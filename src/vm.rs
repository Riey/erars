mod context;
mod save_data;
mod variable;

use std::{io, path::PathBuf};

use anyhow::{anyhow, bail, Result};
use arrayvec::ArrayVec;
use itertools::Itertools;
use pad::PadStr;
use rand::Rng;
use smol_str::SmolStr;
use strum::Display;

use hashbrown::HashMap;

use erars_ast::{
    BeginType, BinaryOperator, BuiltinCommand, BuiltinMethod, BuiltinVariable, EventType,
    PrintFlags, ScriptPosition, UnaryOperator, Value,
};
use erars_compiler::{Instruction, ParserContext, ReplaceInfo};

use self::variable::SerializableVariableStorage;
pub use self::{
    context::{Callstack, LocalValue, VmContext},
    variable::{UniformVariable, VariableStorage, VmVariable},
};

use crate::ui::{ConsoleResult, FontStyle, InputRequest};
use crate::{
    function::{FunctionBody, FunctionDic},
    ui::ConsoleSender,
};

const SAVE_COUNT: usize = 20;

macro_rules! report_error {
    ($tx:expr, $($t:tt)+) => {
        log::error!($($t)+);
        $tx.print_line(format!($($t)+));
    };
}
macro_rules! get_arg {
    ($arg:expr) => {
        get_arg!(@opt $arg).ok_or_else(|| anyhow!("매개변수가 부족합니다"))?
    };
    (@opt $arg:expr) => {
        $arg.next()
    };
    (@var $arg:expr) => {
        match $arg.next() {
            Some(LocalValue::VarRef(r)) => r,
            Some(LocalValue::Value(_)) => bail!("매개변수가 VarRef가 아닙니다"),
            None => bail!("매개변수가 부족합니다"),
        }
    };
    (@value $arg:expr, $ctx:expr) => {
        get_arg!(@opt @value $arg, $ctx).ok_or_else(|| anyhow!("매개변수가 부족합니다"))?
    };
    (@opt @value $arg:expr, $ctx:expr) => {
        match $arg.next() {
            Some(LocalValue::VarRef(r)) => Some($ctx.read_var_ref(&r)?),
            Some(LocalValue::Value(v)) => Some(v),
            None => None,
        }
    };
    (@$t:ty: $arg:expr, $ctx:expr) => {
        get_arg!(@opt @$t: $arg, $ctx).ok_or_else(|| anyhow!("매개변수가 부족합니다"))?
    };
    (@opt @$t:ty: $arg:expr, $ctx:expr) => {
        get_arg!(@opt @value $arg, $ctx).and_then(|v| <$t>::try_from(v).ok())
    };
}

#[derive(Display, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Workflow {
    Return,
    Exit,
}

pub struct TerminalVm {
    dic: FunctionDic,
    sav_path: PathBuf,
}

impl TerminalVm {
    pub fn new(function_dic: FunctionDic, game_path: PathBuf) -> Self {
        Self {
            dic: function_dic,
            sav_path: game_path.join("sav"),
        }
    }

    fn run_instruction(
        &self,
        func_name: &str,
        goto_labels: &HashMap<SmolStr, u32>,
        inst: &Instruction,
        cursor: &mut usize,
        tx: &mut ConsoleSender,
        ctx: &mut VmContext,
    ) -> Result<Option<Workflow>> {
        log::trace!(
            "[{func_name}] `{inst:?}[{cursor}]`, stack: {stack:?}, call_stack: {call_stack:?}",
            stack = ctx.stack(),
            call_stack = ctx.call_stack(),
        );

        match inst {
            Instruction::ReportPosition(pos) => ctx.update_position(pos.clone()),
            Instruction::LoadInt(n) => ctx.push(*n),
            Instruction::LoadStr(s) => ctx.push(s.to_string()),
            Instruction::Nop => {}
            Instruction::Pop => drop(ctx.pop()?),
            Instruction::Duplicate => ctx.dup(),
            Instruction::DuplicatePrev => ctx.dup_prev(),
            Instruction::StoreResult => match ctx.pop_value()? {
                Value::Int(i) => ctx.var.set_result(i),
                Value::String(s) => ctx.var.set_results(s),
            },
            Instruction::ReadVar => {
                let value = ctx.pop_value()?;
                ctx.push(value);
            }
            Instruction::EvalFormString => {
                let form = ctx.pop_str()?;
                let parser_ctx = ParserContext::new(ctx.header_info.clone(), "FORMS.ERB".into());
                let expr = erars_compiler::normal_form_str(&parser_ctx)(&form).unwrap().1;
                let insts = erars_compiler::compile_expr(expr).unwrap();

                for inst in insts.iter() {
                    self.run_instruction(func_name, goto_labels, inst, &mut 0, tx, ctx)?;
                }
            }
            Instruction::GotoLabel => {
                *cursor = goto_labels[ctx.pop_str()?.as_str()] as usize;
            }
            Instruction::LoadExternVarRef(c) => {
                let func_extern = ctx.pop_str()?;
                let name = ctx.pop_str()?;
                let args = ctx.take_arg_list(*c)?;
                ctx.push_var_ref(name, func_extern, args);
            }
            Instruction::LoadVarRef(c) => {
                let name = ctx.pop_str()?;
                let args = ctx.take_arg_list(*c)?;
                ctx.push_var_ref(name, func_name.to_string(), args);
            }
            Instruction::StoreVar => {
                let var_ref = ctx.pop_var_ref()?;
                let value = ctx.pop_value()?;

                ctx.set_var_ref(&var_ref, value)?;
            }
            Instruction::ReuseLastLine => {
                let s = ctx.pop_str()?;
                tx.reuse_last_line(s);
                tx.request_redraw();
            }
            Instruction::Print(flags) => {
                let s = ctx.pop_str()?;
                if flags.contains(PrintFlags::LEFT_ALIGN) {
                    tx.printlc(&s);
                } else if flags.contains(PrintFlags::RIGHT_ALIGN) {
                    tx.printrc(&s);
                } else {
                    tx.print(s);
                }
                if flags.contains(PrintFlags::NEWLINE) {
                    tx.new_line();
                }
                tx.request_redraw();

                if flags.contains(PrintFlags::WAIT) {
                    if tx.input(InputRequest::Anykey) == ConsoleResult::Quit {
                        return Ok(Some(Workflow::Exit));
                    }
                }
            }
            Instruction::TryCall(c) => {
                let func = ctx.pop_str()?;
                let args = ctx.take_value_list(*c)?;

                match self.try_call(&func, &args, tx, ctx)? {
                    Some(Workflow::Return) => ctx.push(true),
                    Some(Workflow::Exit) => return Ok(Some(Workflow::Exit)),
                    None => {
                        ctx.push(false);
                    }
                }
            }
            Instruction::Call(c) => {
                let func = ctx.pop_str()?;
                let args = ctx.take_value_list(*c)?;

                match self.call(&func, &args, tx, ctx)? {
                    Workflow::Exit => return Ok(Some(Workflow::Exit)),
                    Workflow::Return => {}
                }
            }
            Instruction::Begin(b) => {
                ctx.begin = Some(*b);
                return Ok(Some(Workflow::Exit));
            }
            Instruction::CallEvent(ty) => match self.call_event(*ty, tx, ctx)? {
                Workflow::Return => {}
                Workflow::Exit => return Ok(Some(Workflow::Exit)),
            },
            Instruction::ConcatString(c) => {
                let args = ctx.take_value_list(*c)?;
                let ret = args.into_iter().fold(String::new(), |s, l| s + l.into_str().as_str());
                ctx.push(ret);
            }
            Instruction::Times(t) => {
                let arg = ctx.pop_int()?;
                let ret = (arg as f32 * t.into_inner()) as i64;
                ctx.push(ret);
            }
            Instruction::UnaryOperator(op) => match op {
                UnaryOperator::Not => {
                    let operand = ctx.pop_value()?.as_bool();
                    ctx.push(!operand);
                }
                UnaryOperator::Minus => {
                    let operand = ctx.pop_int()?;
                    ctx.push(-operand);
                }
            },
            Instruction::BinaryOperator(op) => {
                let rhs = ctx.pop_value()?;
                let lhs = ctx.pop_value()?;

                let ret = match op {
                    BinaryOperator::Add => match lhs {
                        Value::Int(i) => Value::Int(i + rhs.try_into_int()?),
                        Value::String(s) => Value::String(s + rhs.into_str().as_str()),
                    },
                    BinaryOperator::Mul => match lhs {
                        Value::Int(i) => Value::Int(i * rhs.try_into_int()?),
                        Value::String(s) => {
                            Value::String(s.repeat(usize::try_from(rhs.try_into_int()?)?))
                        }
                    },
                    BinaryOperator::Sub => Value::Int(lhs.try_into_int()? - rhs.try_into_int()?),
                    BinaryOperator::Div => Value::Int(lhs.try_into_int()? / rhs.try_into_int()?),
                    BinaryOperator::Rem => Value::Int(lhs.try_into_int()? % rhs.try_into_int()?),
                    BinaryOperator::Less => {
                        Value::Int((lhs.try_into_int()? < rhs.try_into_int()?).into())
                    }
                    BinaryOperator::LessOrEqual => {
                        Value::Int((lhs.try_into_int()? <= rhs.try_into_int()?).into())
                    }
                    BinaryOperator::Greater => {
                        Value::Int((lhs.try_into_int()? > rhs.try_into_int()?).into())
                    }
                    BinaryOperator::GreaterOrEqual => {
                        Value::Int((lhs.try_into_int()? >= rhs.try_into_int()?).into())
                    }
                    BinaryOperator::Equal => Value::Int(i64::from(lhs == rhs)),
                    BinaryOperator::NotEqual => Value::Int(i64::from(lhs != rhs)),
                    BinaryOperator::And => Value::Int(i64::from(lhs.as_bool() && rhs.as_bool())),
                    BinaryOperator::Or => Value::Int(i64::from(lhs.as_bool() || rhs.as_bool())),
                    BinaryOperator::Xor => Value::Int(i64::from(lhs.as_bool() ^ rhs.as_bool())),
                    BinaryOperator::BitAnd => {
                        Value::Int(i64::from(lhs.try_into_int()? & rhs.try_into_int()?))
                    }
                    BinaryOperator::BitOr => {
                        Value::Int(i64::from(lhs.try_into_int()? | rhs.try_into_int()?))
                    }
                    BinaryOperator::BitXor => {
                        Value::Int(i64::from(lhs.try_into_int()? ^ rhs.try_into_int()?))
                    }
                    BinaryOperator::Lhs => {
                        Value::Int(i64::from(lhs.try_into_int()? << rhs.try_into_int()?))
                    }
                    BinaryOperator::Rhs => {
                        Value::Int(i64::from(lhs.try_into_int()? >> rhs.try_into_int()?))
                    }
                };

                ctx.push(ret);
            }
            Instruction::Goto(no) => {
                *cursor = *no as usize;
            }
            Instruction::GotoIfNot(no) => {
                let cond = ctx.pop_value()?.as_bool();
                if !cond {
                    *cursor = *no as usize;
                }
            }
            Instruction::GotoIf(no) => {
                let cond = ctx.pop_value()?.as_bool();
                if cond {
                    *cursor = *no as usize;
                }
            }
            Instruction::SetAlignment(align) => {
                tx.set_align(*align);
            }
            Instruction::PadStr(align) => {
                use erars_ast::Alignment;

                let size = ctx.pop_int()?;
                let text = match ctx.pop_value()? {
                    Value::String(s) => s,
                    Value::Int(i) => i.to_string(),
                };

                let align = match align {
                    Alignment::Left => pad::Alignment::Left,
                    Alignment::Center => pad::Alignment::Middle,
                    Alignment::Right => pad::Alignment::Right,
                };

                ctx.push(text.pad_to_width_with_alignment(size as usize, align));
            }
            Instruction::BuiltinVar(var, c) => {
                let args = ctx.take_arg_list(*c)?;

                use BuiltinVariable::*;

                let value = match var {
                    GamebaseVersion => Value::Int(0),
                    GamebaseAuthor => "Riey".into(),
                    GamebaseYear => Value::Int(2022),
                    GamebaseTitle => "eraTHYMKR".into(),
                    GamebaseInfo => "".into(),
                    GamebaseCode => Value::Int(0),
                    CharaNum => (ctx.var.character_len() as i64).into(),
                    ItemPrice => {
                        let arg = args[0] as u32;
                        ctx.header_info.item_price.get(&arg).copied().unwrap_or(0).into()
                    }

                    AblName | TalentName | ItemName | FlagName | ExName | ExpName | MarkName
                    | CflagName | CstrName | StrName | TstrName | EquipName | TequipName
                    | PalamName | SourceName | StainName | TcvarName | GlobalName | GlobalsName
                    | SaveStrName => {
                        let name = <&str>::from(var).strip_suffix("NAME").unwrap();
                        let arg = args[0] as u32;
                        ctx.header_info
                            .var_name_var
                            .get(name)
                            .and_then(|d| Some(d.get(&arg)?.as_str()))
                            .unwrap_or("")
                            .into()
                    }
                    Rand => {
                        let max = args[0];
                        Value::Int(ctx.var.rng().gen_range(0..max) as i64)
                    }
                };

                ctx.push(value);
            }
            Instruction::BuiltinMethod(meth, c) => {
                let mut args = ctx.take_list(*c).collect::<Vec<_>>().into_iter();

                macro_rules! check_arg_count {
                    ($expect:expr) => {
                        if *c != $expect {
                            bail!("메소드 {meth}의 매개변수는 {}개여야합니다.", $expect);
                        }
                    };
                    ($min:expr, $max:expr) => {
                        if *c < $min || *c > $max {
                            bail!("메소드 {meth}의 매개변수는 {}~{}개여야합니다.", $min, $max);
                        }
                    };
                    (@atleast $expect:expr) => {
                        if *c < $expect {
                            bail!("메소드 {meth}의 매개변수는 {}개 이상이여야합니다.", $expect);
                        }
                    };
                }

                macro_rules! csv_method {
                    ($field:ident) => {
                        check_arg_count!(1, 2);
                        let no = get_arg!(@i64: args, ctx) as u32;
                        let sp = get_arg!(@opt @i64: args, ctx);

                        if sp != Some(0) {
                            log::warn!("Ignore SP feature");
                        }

                        let csv = ctx
                            .header_info
                            .character_templates
                            .get(&no)
                            .map(|csv| csv.$field.clone())
                            .unwrap_or_default();

                        ctx.push(csv);
                    };

                    (@arr $field:ident) => {
                        check_arg_count!(2, 3);
                        let no = get_arg!(@i64: args, ctx) as u32;
                        let idx = get_arg!(@i64: args, ctx) as u32;
                        let sp = get_arg!(@opt @i64: args, ctx);

                        if sp != Some(0) {
                            log::warn!("Ignore SP feature");
                        }

                        let csv = ctx
                            .header_info
                            .character_templates
                            .get(&no)
                            .and_then(|csv| csv.$field.get(&idx).cloned())
                            .unwrap_or_default();

                        ctx.push(csv);
                    };
                }

                match meth {
                    BuiltinMethod::CsvName => {
                        csv_method!(name);
                    }
                    BuiltinMethod::CsvNickName => {
                        csv_method!(nick_name);
                    }
                    BuiltinMethod::CsvCallName => {
                        csv_method!(call_name);
                    }
                    BuiltinMethod::CsvMasterName => {
                        csv_method!(master_name);
                    }
                    BuiltinMethod::CsvCstr => {
                        csv_method!(@arr cstr);
                    }
                    BuiltinMethod::CsvTalent => {
                        csv_method!(@arr talent);
                    }
                    BuiltinMethod::CsvAbl => {
                        csv_method!(@arr abl);
                    }
                    BuiltinMethod::CsvBase => {
                        csv_method!(@arr base);
                    }
                    BuiltinMethod::CsvEx => {
                        csv_method!(@arr ex);
                    }
                    BuiltinMethod::CsvExp => {
                        csv_method!(@arr exp);
                    }
                    BuiltinMethod::CsvEquip => {
                        csv_method!(@arr equip);
                    }
                    BuiltinMethod::CsvMark => {
                        csv_method!(@arr mark);
                    }
                    BuiltinMethod::CsvRelation => {
                        csv_method!(@arr relation);
                    }
                    BuiltinMethod::CsvJuel => {
                        csv_method!(@arr juel);
                    }
                    BuiltinMethod::CsvCflag => {
                        csv_method!(@arr cflag);
                    }
                    BuiltinMethod::FindChara => {
                        check_arg_count!(1, 4);
                        let mut key = get_arg!(@var args);
                        let value = get_arg!(@value args, ctx);

                        let start = get_arg!(@opt @usize: args, ctx).unwrap_or(0);
                        let end =
                            get_arg!(@opt @usize: args, ctx).unwrap_or(ctx.var.character_len());

                        key.idxs.insert(0, start);

                        let mut ret = -1;

                        for chara_idx in start..end {
                            key.idxs[0] = chara_idx;

                            if value == ctx.read_var_ref(&key)? {
                                ret = chara_idx as i64;
                                break;
                            }
                        }

                        ctx.push(ret);
                    }
                    BuiltinMethod::FindCharaData => {
                        log::warn!("FIND_CHARADATA");
                        ctx.push(0);
                    }
                    BuiltinMethod::ChkCharaData => {
                        log::warn!("CHKCHARADATA");
                        ctx.push(1);
                    }
                    BuiltinMethod::Rand => {
                        check_arg_count!(1, 2);
                        let n1 = get_arg!(@i64: args, ctx);
                        let n2 = get_arg!(@opt @i64: args, ctx);

                        let ret = match n2 {
                            Some(max) => ctx.var.rng().gen_range(n1..max),
                            None => ctx.var.rng().gen_range(0..n1),
                        };

                        ctx.push(ret);
                    }
                    BuiltinMethod::Power => {
                        check_arg_count!(2);
                        let x = get_arg!(@i64: args, ctx);
                        let y = get_arg!(@i64: args, ctx);
                        ctx.push(x ^ y);
                    }
                    BuiltinMethod::Sqrt => {
                        check_arg_count!(1);
                        let x = get_arg!(@i64: args, ctx);
                        ctx.push((x as f32).sqrt() as i64);
                    }
                    BuiltinMethod::BarStr => {
                        check_arg_count!(3);

                        let var = get_arg!(@i64: args, ctx);
                        let max = get_arg!(@i64: args, ctx).max(1);
                        let length = get_arg!(@i64: args, ctx).max(0);

                        ctx.push(make_bar_str(&ctx.header_info.replace, var, max, length));
                    }
                    BuiltinMethod::Escape => {
                        check_arg_count!(1);
                        let s = get_arg!(@String: args, ctx);
                        ctx.push(regex::escape(&s));
                    }
                    BuiltinMethod::Replace => {
                        check_arg_count!(3);
                        let base = get_arg!(@String: args, ctx);
                        let from = get_arg!(@String: args, ctx);
                        let to = get_arg!(@String: args, ctx);

                        let regex = regex::Regex::new(&from)?;
                        ctx.push(regex.replace_all(&base, &to).into_owned());
                    }
                    BuiltinMethod::StrLenS => {
                        check_arg_count!(1);
                        let s = get_arg!(@String: args, ctx);
                        ctx.push(ctx.encoding().encode(&s).0.as_ref().len() as i64);
                    }
                    BuiltinMethod::StrLenSU => {
                        check_arg_count!(1);
                        let s = get_arg!(@String: args, ctx);
                        ctx.push(s.chars().count() as i64);
                    }
                    BuiltinMethod::SumArray => {
                        check_arg_count!(2, 3);

                        let var_ref = get_arg!(@var args);
                        let start = get_arg!(@opt @usize: args, ctx).unwrap_or(0);
                        let end = get_arg!(@opt @usize: args, ctx);

                        let target = ctx.var.read_int("TARGET", &[])?;
                        let var = ctx.var.get_maybe_local_var(func_name, &var_ref.name)?.1;

                        let var = match var {
                            UniformVariable::Character(cvar) => {
                                let c_idx =
                                    var_ref.idxs.first().copied().unwrap_or(target.try_into()?);
                                &mut cvar[c_idx]
                            }
                            UniformVariable::Normal(var) => var,
                        }
                        .as_int()?;

                        let slice = match end {
                            Some(end) => &var[start..end],
                            None => &var[start..],
                        };

                        let ret = slice.iter().sum::<i64>();
                        ctx.push(ret);
                    }
                    BuiltinMethod::ToStr => {
                        check_arg_count!(1, 2);
                        let value = get_arg!(@i64: args, ctx);
                        let format = get_arg!(@opt @String: args, ctx);
                        let ret = match format {
                            Some(_) => {
                                format!("{00}", value)
                            }
                            None => value.to_string(),
                        };

                        ctx.push(ret);
                    }
                    BuiltinMethod::ToInt => {
                        check_arg_count!(1);

                        match get_arg!(@String: args, ctx).parse() {
                            Ok(i) => ctx.push(Value::Int(i)),
                            Err(_) => ctx.push(0),
                        }
                    }
                    BuiltinMethod::Max => {
                        check_arg_count!(@atleast 1);

                        let mut max = get_arg!(@value args, ctx);

                        for arg in args {
                            max = max.max(ctx.reduce_local_value(arg)?);
                        }

                        ctx.push(max);
                    }
                    BuiltinMethod::Min => {
                        check_arg_count!(@atleast 1);

                        let mut min = get_arg!(@value args, ctx);

                        for arg in args {
                            min = min.min(ctx.reduce_local_value(arg)?);
                        }

                        ctx.push(min);
                    }
                    BuiltinMethod::Limit => {
                        check_arg_count!(3);
                        let v = get_arg!(@i64: args, ctx);
                        let low = get_arg!(@i64: args, ctx);
                        let high = get_arg!(@i64: args, ctx);

                        ctx.push(v.clamp(low, high));
                    }
                    BuiltinMethod::Abs => {
                        check_arg_count!(1);
                        let v = get_arg!(@i64: args, ctx);
                        ctx.push(v.abs());
                    }
                    BuiltinMethod::Sign => {
                        check_arg_count!(1);
                        let v = get_arg!(@i64: args, ctx);
                        ctx.push(-v);
                    }
                    BuiltinMethod::InRange => {
                        check_arg_count!(3);
                        let v = get_arg!(@i64: args, ctx);
                        let l = get_arg!(@i64: args, ctx);
                        let h = get_arg!(@i64: args, ctx);
                        ctx.push(v >= l && v <= h);
                    }
                    BuiltinMethod::Log => {
                        check_arg_count!(1);
                        let v = get_arg!(@i64: args, ctx);
                        ctx.push((v as f32).ln() as i64);
                    }
                    BuiltinMethod::Log10 => {
                        check_arg_count!(1);
                        let v = get_arg!(@i64: args, ctx);
                        ctx.push((v as f32).log10() as i64);
                    }
                    BuiltinMethod::LineIsEmpty => {
                        check_arg_count!(0);
                        ctx.push(tx.line_is_empty());
                    }
                    BuiltinMethod::GroupMatch => {
                        check_arg_count!(@atleast 1);
                        let value = ctx.reduce_local_value(args.next().unwrap())?;
                        let mut ret = 0i64;

                        for arg in args {
                            if value == ctx.reduce_local_value(arg)? {
                                ret += 1;
                            }
                        }

                        ctx.push(ret);
                    }
                    BuiltinMethod::GetBit => {
                        check_arg_count!(2);
                        let l = get_arg!(@i64: args, ctx);
                        let r = get_arg!(@i64: args, ctx);
                        ctx.push((l >> r) & 1);
                    }

                    BuiltinMethod::StrCount => {
                        check_arg_count!(2);
                        let text = get_arg!(@String: args, ctx);
                        let m = get_arg!(@String: args, ctx);

                        let r = regex::Regex::new(&m)?;
                        ctx.push(r.find_iter(&text).count() as i64);
                    }
                    BuiltinMethod::SubString => {
                        check_arg_count!(1, 3);
                        let text = get_arg!(@String: args, ctx);
                        let start = get_arg!(@opt @usize: args, ctx).unwrap_or(0);
                        let length = get_arg!(@opt @usize: args, ctx);

                        let bytes = ctx.encoding().encode(&text).0;

                        let sub_bytes = match length {
                            Some(length) => &bytes.as_ref()[start..(start + length)],
                            None => &bytes.as_ref()[start..],
                        };

                        let sub_str = ctx.encoding().decode(sub_bytes).0;
                        ctx.push(sub_str.into_owned());
                    }

                    BuiltinMethod::SubStringU => {
                        check_arg_count!(1, 3);
                        let text = get_arg!(@String: args, ctx);
                        let start = get_arg!(@opt @usize: args, ctx).unwrap_or(0);
                        let length = get_arg!(@opt @usize: args, ctx);

                        let mut chars = text.chars().skip(start);

                        let mut ret = String::new();

                        match length {
                            Some(length) => {
                                for _ in 0..length {
                                    ret.push(chars.next().unwrap());
                                }
                            }
                            None => {
                                for ch in chars {
                                    ret.push(ch);
                                }
                            }
                        };

                        ctx.push(ret);
                    }

                    BuiltinMethod::Unicode => {
                        check_arg_count!(1);
                        let code = get_arg!(@i64: args, ctx).try_into()?;

                        ctx.push(
                            char::from_u32(code)
                                .ok_or_else(|| {
                                    anyhow!("u32 {code} is not valid unicode codepoint")
                                })?
                                .to_string(),
                        );
                    }

                    BuiltinMethod::GetDefColor => {
                        check_arg_count!(0);
                        ctx.push(0xFFFFFF);
                    }
                    BuiltinMethod::GetDefBgColor => {
                        check_arg_count!(0);
                        ctx.push(0);
                    }
                    BuiltinMethod::GetFont => {
                        check_arg_count!(0);
                        ctx.push(tx.font().to_string());
                    }

                    BuiltinMethod::GetColor => {
                        check_arg_count!(0);
                        ctx.push(tx.color() as i64);
                    }
                    BuiltinMethod::GetBgColor => {
                        check_arg_count!(0);
                        ctx.push(tx.bg_color() as i64);
                    }
                    BuiltinMethod::GetFocusColor => {
                        check_arg_count!(0);
                        ctx.push(tx.hl_color() as i64);
                    }
                    BuiltinMethod::GetChara => {
                        check_arg_count!(1, 2);

                        let no = get_arg!(@i64: args, ctx);
                        let _sp = get_arg!(@opt @i64: args, ctx);

                        let idx = ctx.var.get_chara(no)?;

                        ctx.push(idx.map(|i| i as i64).unwrap_or(-1));
                    }
                    BuiltinMethod::GetPalamLv => {
                        check_arg_count!(2);

                        let value = get_arg!(@i64: args, ctx);
                        let max = get_arg!(@i64: args, ctx);

                        let var = ctx.var.get_var("PALAMLV")?.1.assume_normal().as_int()?;

                        let mut ret = max;

                        for (lv, lv_value) in var.iter().enumerate() {
                            if lv as i64 > max {
                                break;
                            }
                            if value <= *lv_value {
                                ret = lv as i64;
                                break;
                            }
                        }

                        ctx.push(ret);
                    }
                    BuiltinMethod::GetExpLv => {
                        check_arg_count!(2);

                        let value = get_arg!(@i64: args, ctx);
                        let max = get_arg!(@i64: args, ctx);

                        let var = ctx.var.get_var("EXPLV")?.1.assume_normal().as_int()?;

                        let mut ret = max;

                        for (lv, lv_value) in var.iter().enumerate() {
                            if lv as i64 > max {
                                break;
                            }
                            if value <= *lv_value {
                                ret = lv as i64;
                                break;
                            }
                        }

                        ctx.push(ret);
                    }

                    BuiltinMethod::GetTime => {
                        check_arg_count!(0);
                        let now = time::OffsetDateTime::now_local()?;

                        ctx.push(format!(
                            "{year:04}年{month:02}月{day:02}日 {hour:02}:{minute:02}:{second:02}",
                            year = now.year(),
                            month = now.month() as u8,
                            day = now.day(),
                            hour = now.hour(),
                            minute = now.minute(),
                            second = now.second()
                        ));
                    }

                    BuiltinMethod::ChkData => {
                        check_arg_count!(1);
                        let idx = get_arg!(@i64: args, ctx);

                        let (ret, rets) = match self::save_data::read_save_data(&self.sav_path, idx)
                        {
                            Ok(data) => (0, data.description),
                            Err(err) => (err, "セーブデータのバーションが異なります".into()),
                        };

                        ctx.var.set_results(rets);
                        ctx.push(ret);
                    }
                }
            }
            Instruction::BuiltinCommand(com, c) => {
                let mut args = ctx.take_list(*c).collect::<ArrayVec<_, 8>>().into_iter();

                match com {
                    BuiltinCommand::UpCheck => {
                        let names = ctx.header_info.var_name_var.get("PALAM").unwrap();
                        let target = ctx.var.read_int("TARGET", &[])?.try_into()?;
                        ctx.var.upcheck(tx, target, names)?;
                    }
                    BuiltinCommand::CUpCheck => {
                        let target = get_arg!(@usize: args, ctx);
                        let names = ctx.header_info.var_name_var.get("PALAM").unwrap();
                        ctx.var.cupcheck(tx, target, names)?;
                    }
                    BuiltinCommand::Restart => {
                        *cursor = 0;
                    }
                    BuiltinCommand::SetBit => {
                        let v = get_arg!(@var args);
                        let idx = get_arg!(@usize: args, ctx);
                        let i = ctx.ref_int_var_ref(&v)?;
                        *i |= 1 << idx;
                    }
                    BuiltinCommand::ClearBit => {
                        let v = get_arg!(@var args);
                        let idx = get_arg!(@usize: args, ctx);
                        let i = ctx.ref_int_var_ref(&v)?;
                        *i &= !(1 << idx);
                    }
                    BuiltinCommand::InvertBit => {
                        let v = get_arg!(@var args);
                        let idx = get_arg!(@usize: args, ctx);
                        let i = ctx.ref_int_var_ref(&v)?;
                        *i ^= 1 << idx;
                    }
                    BuiltinCommand::ArrayShift => {
                        let v = get_arg!(@var args);
                        let empty_value = get_arg!(@value args, ctx);
                        let start = get_arg!(@usize: args, ctx);
                        let count = get_arg!(@usize: args, ctx);

                        let target = if let Some(idx) = v.idxs.get(0).copied() {
                            idx
                        } else {
                            ctx.var.read_int("TARGET", &[])?.try_into()?
                        };
                        let (info, var) = ctx.var.get_maybe_local_var(&v.func_name, &v.name)?;
                        let var = var.as_vm_var(target);

                        if info.is_str {
                            let var = var.as_str()?;
                            let empty_value = empty_value.try_into()?;
                            array_shift(var, empty_value, start, count)?;
                        } else {
                            let var = var.as_int()?;
                            let empty_value = empty_value.try_into()?;
                            array_shift(var, empty_value, start, count)?;
                        }
                    }
                    BuiltinCommand::Throw => {
                        let msg = get_arg!(@opt @String: args, ctx);

                        match msg {
                            Some(msg) => bail!("스크립트에서 예외발생: {msg}"),
                            None => bail!("스크립트에서 예외발생"),
                        }
                    }
                    BuiltinCommand::Varset => {
                        let var = get_arg!(@var args);
                        let value = get_arg!(@opt @value args, ctx);
                        let start = get_arg!(@opt @usize: args, ctx);
                        let end = get_arg!(@opt @usize: args, ctx);

                        let target = ctx.var.read_int("TARGET", &[])?;
                        let (info, var, idx) = ctx.resolve_var_ref_raw(&var)?;
                        let (chara_idx, idx) = info.calculate_single_idx(&idx);

                        match (value, start, end) {
                            (None, None, None) => {
                                *var = UniformVariable::new(info);
                            }
                            (Some(value), start, end) => {
                                let var = match var {
                                    UniformVariable::Character(cvar) => {
                                        &mut cvar[chara_idx.unwrap_or_else(|| target as usize)]
                                    }
                                    UniformVariable::Normal(var) => var,
                                };
                                let start = start.unwrap_or(0);
                                let end = end.unwrap_or(info.size.last().copied().unwrap_or(1));

                                for i in start..end {
                                    var.set(idx + i, value.clone())?;
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                    BuiltinCommand::CVarset => {
                        let var = get_arg!(@var args);
                        let index = get_arg!(@usize: args, ctx);
                        let value = get_arg!(@opt @value args, ctx);
                        let start = get_arg!(@opt @usize: args, ctx);

                        let (info, var) = ctx.var.get_var(&var.name)?;

                        let value = value.unwrap_or_else(|| {
                            if info.is_str {
                                Value::String(String::new())
                            } else {
                                Value::Int(0)
                            }
                        });

                        let start = start.unwrap_or(0);

                        let cvar = var.assume_chara_vec();

                        for var in &mut cvar[start..] {
                            var.set(index, value.clone())?;
                        }
                    }
                    BuiltinCommand::Split => {
                        let s = get_arg!(@String: args, ctx);
                        let delimiter = get_arg!(@String: args, ctx);
                        let mut var = get_arg!(@var args);

                        for (idx, part) in s.split(delimiter.as_str()).enumerate() {
                            var.idxs.push(idx);

                            ctx.set_var_ref(&var, part.into())?;

                            var.idxs.pop();
                        }
                    }
                    BuiltinCommand::Bar => {
                        let var = get_arg!(@i64: args, ctx);
                        let max = get_arg!(@i64: args, ctx).max(1);
                        let length = get_arg!(@i64: args, ctx).max(0);

                        tx.print(make_bar_str(&ctx.header_info.replace, var, max, length));
                    }
                    BuiltinCommand::ReturnF => {
                        let ret = get_arg!(@value args, ctx);

                        if args.next().is_some() {
                            bail!("RETURNF는 한개의 값만 반환할 수 있습니다.");
                        }

                        drop(ctx.return_func()?);

                        ctx.push(ret);

                        return Ok(Some(Workflow::Return));
                    }
                    BuiltinCommand::Return => {
                        drop(ctx.return_func()?);

                        let mut result_idx = 0usize;
                        let mut results_idx = 0usize;

                        let args: ArrayVec<_, 8> = args
                            .map(|v| match v {
                                LocalValue::Value(v) => Ok(v),
                                LocalValue::VarRef(v) => ctx.read_var_ref(&v),
                            })
                            .try_collect()?;

                        let ((_, result), (_, results)) =
                            ctx.var.get_var2("RESULT".into(), "RESULTS".into()).unwrap();
                        let result = result.assume_normal();
                        let results = results.assume_normal();

                        for arg in args {
                            match arg {
                                Value::Int(i) => {
                                    result.as_int()?[result_idx] = i;
                                    result_idx += 1;
                                }
                                Value::String(s) => {
                                    results.as_str()?[results_idx] = s;
                                    results_idx += 1;
                                }
                            }
                        }

                        return Ok(Some(Workflow::Return));
                    }
                    BuiltinCommand::DrawLine => {
                        tx.draw_line(ctx.header_info.replace.drawline_str.clone());
                        tx.request_redraw();
                    }
                    BuiltinCommand::CustomDrawLine => {
                        let s = get_arg!(@String: args, ctx);
                        tx.draw_line(s);
                        tx.request_redraw();
                    }
                    BuiltinCommand::FontStyle => {
                        let style: u32 = get_arg!(@i64: args, ctx).try_into()?;
                        let style = FontStyle::from_bits_truncate(style);
                        tx.set_style(style);
                    }
                    BuiltinCommand::GetStyle => {
                        ctx.push(tx.style().bits() as i64);
                    }
                    BuiltinCommand::ChkFont => {
                        log::warn!("TODO: CHKFONT");
                        ctx.push(0);
                    }
                    BuiltinCommand::SetFont => {
                        let font = get_arg!(@String: args, ctx);
                        tx.set_font(font);
                    }
                    BuiltinCommand::FontBold => {
                        tx.set_style(FontStyle::BOLD);
                    }
                    BuiltinCommand::FontRegular => {
                        tx.set_style(FontStyle::NORMAL);
                    }
                    BuiltinCommand::FontItalic => {
                        tx.set_style(FontStyle::ITALIC);
                    }
                    BuiltinCommand::SetColor => {
                        let c = get_arg!(@i64: args, ctx);

                        let (r, g, b) = match get_arg!(@opt @i64: args, ctx) {
                            Some(g) => {
                                let b = get_arg!(@i64: args, ctx);
                                (c as u8, g as u8, b as u8)
                            }
                            None => {
                                let [r, g, b, _] = (c as u32).to_le_bytes();
                                (r, g, b)
                            }
                        };

                        tx.set_color(r, g, b);
                    }
                    BuiltinCommand::SetColorByName | BuiltinCommand::SetBgColorByName => {
                        bail!("SETCOLORBYNAME");
                    }
                    BuiltinCommand::SetBgColor => {
                        let c = get_arg!(@i64: args, ctx);

                        let (r, g, b) = match get_arg!(@opt @i64: args, ctx) {
                            Some(g) => {
                                let b = get_arg!(@i64: args, ctx);
                                (c as u8, g as u8, b as u8)
                            }
                            None => {
                                let [r, g, b, _] = (c as u32).to_le_bytes();
                                (r, g, b)
                            }
                        };

                        tx.set_bg_color(r, g, b);
                    }
                    BuiltinCommand::ResetColor => {
                        tx.set_color(0xFF, 0xFF, 0xFF);
                    }
                    BuiltinCommand::ResetBgColor => {
                        tx.set_bg_color(0, 0, 0);
                    }
                    BuiltinCommand::Wait => match tx.input(InputRequest::EnterKey) {
                        ConsoleResult::Value(_) => {}
                        ConsoleResult::Quit => return Ok(Some(Workflow::Exit)),
                    },
                    BuiltinCommand::WaitAnykey => match tx.input(InputRequest::Anykey) {
                        ConsoleResult::Value(_) => {}
                        ConsoleResult::Quit => return Ok(Some(Workflow::Exit)),
                    },
                    BuiltinCommand::Input
                    | BuiltinCommand::InputS
                    | BuiltinCommand::TInput
                    | BuiltinCommand::TInputS
                    | BuiltinCommand::TOneInput
                    | BuiltinCommand::TOneInputS
                    | BuiltinCommand::OneInput
                    | BuiltinCommand::OneInputS => {
                        let req = match com {
                            BuiltinCommand::InputS
                            | BuiltinCommand::OneInputS
                            | BuiltinCommand::TInputS
                            | BuiltinCommand::TOneInputS => InputRequest::Str,
                            BuiltinCommand::Input
                            | BuiltinCommand::OneInput
                            | BuiltinCommand::TInput
                            | BuiltinCommand::TOneInput => InputRequest::Int,
                            _ => unreachable!(),
                        };

                        if matches!(
                            com,
                            BuiltinCommand::TInput
                                | BuiltinCommand::TInputS
                                | BuiltinCommand::TOneInputS
                                | BuiltinCommand::TOneInput
                        ) {
                            log::warn!("[TODO] {com}: timeout is not implemented");
                        }

                        if matches!(
                            com,
                            BuiltinCommand::OneInput
                                | BuiltinCommand::OneInputS
                                | BuiltinCommand::TOneInputS
                                | BuiltinCommand::TOneInput
                        ) {
                            log::warn!("[TODO] {com}: one input is not implemented");
                        }

                        match tx.input(req) {
                            ConsoleResult::Quit => {
                                return Ok(Some(Workflow::Exit));
                            }
                            ConsoleResult::Value(ret) => match ret {
                                Value::Int(i) => *ctx.var.ref_int("RESULT", &[])? = i,
                                Value::String(s) => *ctx.var.ref_str("RESULTS", &[])? = s,
                            },
                        }
                    }
                    BuiltinCommand::Quit => {
                        log::info!("Run QUIT");
                        tx.exit();
                        return Ok(Some(Workflow::Exit));
                    }
                    BuiltinCommand::SwapChara => {
                        let a = get_arg!(@usize: args, ctx);
                        let b = get_arg!(@usize: args, ctx);

                        ctx.var.swap_chara(a, b);
                    }
                    BuiltinCommand::SortChara => bail!("SORTCHARA"),
                    BuiltinCommand::PickupChara => bail!("PICKUPCHARA"),
                    BuiltinCommand::AddChara => {
                        let no = get_arg!(@i64: args, ctx).try_into()?;
                        let template = ctx
                            .header_info
                            .character_templates
                            .get(&no)
                            .ok_or_else(|| anyhow!("존재하지 않는 캐릭터 번호입니다({no})"))?;

                        let idx = ctx.var.character_len();

                        ctx.var.add_chara();
                        ctx.var.set_character_template(idx, template)?;
                    }
                    BuiltinCommand::AddDefChara => {
                        let idx = ctx.var.character_len();

                        ctx.var.add_chara();

                        match ctx.header_info.character_templates.get(&0) {
                            Some(template) => {
                                ctx.var.set_character_template(idx, template)?;
                            }
                            None => {}
                        }
                    }
                    BuiltinCommand::Redraw => {
                        tx.request_redraw();
                    }
                    BuiltinCommand::ClearLine => {
                        tx.clear_line(get_arg!(@usize: args, ctx));
                    }
                    BuiltinCommand::CallTrain => {
                        bail!("CALLTRAIN");
                    }
                    BuiltinCommand::DelChara => {
                        let idx = get_arg!(@i64: args, ctx);
                        ctx.var.del_chara(idx.try_into()?);
                    }
                    BuiltinCommand::ResetData => {
                        ctx.var.reset_data(&ctx.header_info.replace)?;
                    }
                    BuiltinCommand::SaveData => {
                        let idx = get_arg!(@i64: args, ctx);
                        let description = get_arg!(@String: args, ctx);

                        log::info!("Save {idx}: {description}");

                        self::save_data::write_save_data(
                            &self.sav_path,
                            idx,
                            &ctx.var,
                            description,
                        );
                    }
                    BuiltinCommand::LoadData => {
                        let idx = get_arg!(@i64: args, ctx);

                        match self::save_data::read_save_data(&self.sav_path, idx) {
                            Ok(data) => {
                                ctx.var.load_serializable(data, &ctx.header_info.replace)?;
                            }
                            _ => {}
                        }
                    }
                    BuiltinCommand::DelData => {
                        let idx = get_arg!(@i64: args, ctx);

                        let _ = self::save_data::delete_save_data(&self.sav_path, idx);
                    }
                    BuiltinCommand::SaveGlobal => {
                        self::save_data::write_global_data(&self.sav_path, &ctx.var);
                    }
                    BuiltinCommand::LoadGlobal => {
                        match self::save_data::read_global_data(&self.sav_path) {
                            Ok(data) => {
                                ctx.var.load_global_serializable(data, &ctx.header_info.replace)?;
                                ctx.var.set_result(1);
                            }
                            _ => {
                                ctx.var.set_result(0);
                            }
                        }
                    }
                    BuiltinCommand::Swap => {
                        let v1 = get_arg!(@var args);
                        let v2 = get_arg!(@var args);

                        let temp1 = ctx.read_var_ref(&v1)?;
                        let temp2 = ctx.read_var_ref(&v2)?;

                        ctx.set_var_ref(&v1, temp2)?;
                        ctx.set_var_ref(&v2, temp1)?;
                    }
                    BuiltinCommand::SaveNos => {
                        let nos = ctx.config.save_nos;
                        ctx.push(nos as i64);
                    }
                    BuiltinCommand::SaveGame => {
                        if self.run_save_game(tx, ctx)? == Workflow::Exit {
                            return Ok(Some(Workflow::Exit));
                        }
                    }
                    BuiltinCommand::LoadGame => {
                        if self.run_load_game(tx, ctx)? != Workflow::Exit {
                            // begin shop
                            ctx.begin = Some(BeginType::Shop);
                        }

                        return Ok(Some(Workflow::Exit));
                    }
                    BuiltinCommand::PutForm => {
                        let arg = get_arg!(@String: args, ctx);
                        match ctx.put_form_buf.as_mut() {
                            Some(buf) => {
                                buf.push_str(&arg);
                            }
                            None => {
                                bail!("PUTFORM called in no @SAVEINFO function");
                            }
                        }
                    }
                    BuiltinCommand::ResetStain => {
                        let chara = get_arg!(@usize: args, ctx);
                        let stain = ctx.var.get_var("STAIN")?.1.assume_chara(chara);
                        let stain_init = &ctx.header_info.replace.stain_init;
                        stain.as_int()?[..stain_init.len()].copy_from_slice(&stain_init);
                    }
                    BuiltinCommand::SaveChara => bail!("SAVECHARA"),
                    BuiltinCommand::LoadChara => bail!("LOADCHARA"),
                }
            }
            _ => bail!("TODO: {:?}", inst),
        }

        Ok(None)
    }

    fn load_savs(&self) -> HashMap<usize, SerializableVariableStorage> {
        let mut savs = HashMap::new();

        for i in 0..SAVE_COUNT {
            if let Ok(sav) = save_data::read_save_data(&self.sav_path, i as i64) {
                savs.insert(i, sav);
            }
        }

        savs
    }

    fn print_sav_data_list(
        savs: &HashMap<usize, SerializableVariableStorage>,
        tx: &mut ConsoleSender,
    ) {
        for i in 0..SAVE_COUNT {
            match savs.get(&i) {
                Some(sav) => {
                    tx.print_line(format!("[{i:02}] - {}", sav.description));
                }
                None => {
                    tx.print_line(format!("[{i:02}] - NO DATA"));
                }
            }
        }

        tx.print_line("[100] Return".into());
    }

    fn run_load_game(&self, tx: &mut ConsoleSender, ctx: &mut VmContext) -> Result<Workflow> {
        let mut savs = self.load_savs();

        loop {
            Self::print_sav_data_list(&savs, tx);
            match tx.input_int() {
                None => return Ok(Workflow::Exit),
                Some(100) => return Ok(Workflow::Return),
                Some(i) if i >= 0 && i < SAVE_COUNT as i64 => {
                    if let Some(sav) = savs.remove(&(i as usize)) {
                        ctx.var.load_serializable(sav, &ctx.header_info.replace)?;
                        break;
                    }
                }
                Some(_) => continue,
            }
        }

        if self.try_call("SYSTEM_LOADEND", &[], tx, ctx)? == Some(Workflow::Exit) {
            return Ok(Workflow::Exit);
        }

        self.call_event(EventType::Load, tx, ctx)
    }

    fn run_save_game(&self, tx: &mut ConsoleSender, ctx: &mut VmContext) -> Result<Workflow> {
        let savs = self.load_savs();

        loop {
            Self::print_sav_data_list(&savs, tx);

            match tx.input_int() {
                None => return Ok(Workflow::Exit),
                Some(100) => break,
                Some(i) if i >= 0 && i < SAVE_COUNT as i64 => {
                    let i = i as usize;
                    let write = if savs.contains_key(&i) {
                        tx.print_line(format!("SAVE {i} already exists. Overwrite?"));
                        tx.print_line(format!("[0] Yes [1] No"));
                        loop {
                            match tx.input_int() {
                                None => return Ok(Workflow::Exit),
                                Some(0) => break true,
                                Some(1) => break false,
                                Some(_) => continue,
                            }
                        }
                    } else {
                        true
                    };

                    if write {
                        ctx.put_form_buf = Some(String::new());
                        if self.try_call("SAVEINFO", &[], tx, ctx)? == Some(Workflow::Exit) {
                            return Ok(Workflow::Exit);
                        }
                        save_data::write_save_data(
                            &self.sav_path,
                            i as i64,
                            &ctx.var,
                            ctx.put_form_buf.take().unwrap(),
                        );
                        break;
                    }
                }
                _ => continue,
            }
        }

        Ok(Workflow::Return)
    }

    fn run_body(
        &self,
        func_name: &str,
        goto_labels: &HashMap<SmolStr, u32>,
        body: &FunctionBody,
        tx: &mut ConsoleSender,
        ctx: &mut VmContext,
    ) -> Result<Workflow> {
        let mut cursor = 0;

        let insts = body.body();

        while let Some(inst) = insts.get(cursor) {
            cursor += 1;
            match self.run_instruction(func_name, goto_labels, inst, &mut cursor, tx, ctx) {
                Ok(None) => {}
                Ok(Some(Workflow::Return)) => return Ok(Workflow::Return),
                Ok(Some(flow)) => return Ok(flow),
                Err(err) => {
                    return Err(err);
                }
            }
        }

        // exit without RETURN/RETURNF

        if body.is_function() {
            ctx.push(0);
        } else if body.is_functions() {
            ctx.push("");
        } else {
            ctx.var.set_result(0);
        }

        Ok(Workflow::Return)
    }

    fn call_internal(
        &self,
        label: &str,
        args: &[Value],
        tx: &mut ConsoleSender,
        ctx: &mut VmContext,
        body: &FunctionBody,
    ) -> Result<Workflow> {
        log::trace!("CALL {label}({args:?})");
        ctx.new_func(Ok(label.into()), body.file_path().clone());

        let mut args = args.iter().cloned();

        for (var_idx, default_value, arg_indices) in body.args().iter() {
            let (info, var) = ctx.var.get_maybe_local_var(label, var_idx)?;
            let var = var.assume_normal();
            let idx = info.calculate_single_idx(arg_indices).1;

            let arg = args.next().or_else(|| default_value.clone());

            if info.is_str {
                var.as_str()?[idx] = match arg {
                    Some(Value::String(s)) => s,
                    Some(_) => bail!("Argument type mismatched"),
                    None => String::new(),
                };
            } else {
                var.as_int()?[idx] = match arg {
                    Some(Value::Int(s)) => s,
                    Some(_) => bail!("Argument type mismatched"),
                    None => 0,
                };
            }
        }

        let ret = self.run_body(label, body.goto_labels(), body, tx, ctx)?;

        ctx.end_func();

        Ok(ret)
    }

    #[inline]
    fn call(
        &self,
        label: &str,
        args: &[Value],
        tx: &mut ConsoleSender,
        ctx: &mut VmContext,
    ) -> Result<Workflow> {
        self.call_internal(label, args, tx, ctx, self.dic.get_func(label)?)
    }

    #[inline]
    fn try_call(
        &self,
        label: &str,
        args: &[Value],
        tx: &mut ConsoleSender,
        ctx: &mut VmContext,
    ) -> Result<Option<Workflow>> {
        match self.dic.get_func_opt(label) {
            Some(body) => self.call_internal(label, args, tx, ctx, body).map(Some),
            None => Ok(None),
        }
    }

    fn call_event(
        &self,
        ty: EventType,
        tx: &mut ConsoleSender,
        ctx: &mut VmContext,
    ) -> Result<Workflow> {
        self.dic.get_event(ty).run(|body| {
            let label: &str = ty.into();
            ctx.new_func(Err(ty), body.file_path().clone());
            let ret = self.run_body(label, body.goto_labels(), body, tx, ctx)?;
            ctx.end_func();

            Ok(ret)
        })
    }

    fn begin(&self, ty: BeginType, tx: &mut ConsoleSender, ctx: &mut VmContext) -> Result<()> {
        log::trace!("Begin {ty}");

        /// 해당 함수를 호출하고 존재할경우 true, 반대면 false를 반환함
        ///
        /// 해당 함수에서 게임이 종료되면 그대로 종료
        macro_rules! call {
            ($name:expr) => {
                match self.try_call($name, &[], tx, ctx)? {
                    Some(Workflow::Exit) => return Ok(()),
                    Some(Workflow::Return) => true,
                    None => false,
                }
            };
        }

        macro_rules! call_event {
            ($ty:expr) => {
                if self.call_event($ty, tx, ctx)? == Workflow::Exit {
                    return Ok(());
                }
            };
        }

        match ty {
            BeginType::Title => {
                if !call!("SYSTEM_TITLE") {
                    todo!("Default TITLE");
                }
                Ok(())
            }
            BeginType::First => {
                call_event!(EventType::First);
                Ok(())
            }
            BeginType::Train => {
                ctx.var.reset_train_data()?;
                call_event!(EventType::Train);

                while ctx.begin.is_none() {
                    let com_no = match ctx.var.read_int("NEXTCOM", &[])? {
                        no if no >= 0 => no,
                        _ => {
                            call!("SHOW_STATUS");

                            for (no, name) in ctx.header_info.clone().var_name_var["TRAIN"].iter() {
                                if call!(&format!("COM_ABLE{no}")) && ctx.var.get_result() != 0
                                    || ctx.header_info.replace.comable_init != 0
                                {
                                    tx.printrc(&format!("{name}[{no:3}]"));
                                }
                            }

                            call!("SHOW_USERCOM");

                            ctx.var.reset_var("UP")?;
                            ctx.var.reset_var("DOWN")?;
                            ctx.var.reset_var("LOSEBASE")?;
                            ctx.var.reset_var("CUP")?;
                            ctx.var.reset_var("CDOWN")?;
                            ctx.var.reset_var("DOWNBASE")?;

                            match tx.input(InputRequest::Int) {
                                ConsoleResult::Quit => return Ok(()),
                                ConsoleResult::Value(Value::String(_)) => unreachable!(),
                                ConsoleResult::Value(Value::Int(no)) => {
                                    ctx.var.set_result(no);

                                    let com_exists = match no.try_into() {
                                        Ok(no) => ctx
                                            .header_info
                                            .var_name_var
                                            .get("TRAIN")
                                            .map(|v| v.contains_key(&no))
                                            .unwrap_or(false),
                                        _ => false,
                                    };

                                    if com_exists {
                                        no
                                    } else {
                                        call!("USERCOM");

                                        continue;
                                    }
                                }
                            }
                        }
                    };

                    ctx.var.reset_var("NOWEX")?;

                    *ctx.var.ref_int("SELECTCOM", &[])? = com_no;

                    call_event!(EventType::Com);
                    call!(&format!("COM{com_no}"));

                    if ctx.var.get_result() == 0 {
                        continue;
                    }

                    call!("SOURCE_CHECK");

                    ctx.var.reset_var("SOURCE")?;

                    call_event!(EventType::ComEnd);
                }

                Ok(())
            }
            BeginType::AfterTrain => {
                call_event!(EventType::End);

                Ok(())
            }
            BeginType::AblUp => {
                while ctx.begin.is_none() {
                    call!("SHOW_JUEL");
                    call!("SHOW_ABLUP_SELECT");

                    loop {
                        match tx.input(InputRequest::Int) {
                            ConsoleResult::Quit => return Ok(()),
                            ConsoleResult::Value(Value::Int(i)) => {
                                ctx.var.set_result(i);

                                if matches!(i, 0..=99) {
                                    if call!(&format!("ABLUP{i}")) {
                                        break;
                                    }
                                } else {
                                    call!("USERABLUP");
                                    break;
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                }

                Ok(())
            }
            BeginType::TurnEnd => {
                call_event!(EventType::TurnEnd);

                Ok(())
            }
            BeginType::Shop => {
                call_event!(EventType::Shop);

                while ctx.begin.is_none() {
                    call!("SHOW_SHOP");

                    match tx.input(InputRequest::Int) {
                        ConsoleResult::Quit => break,
                        ConsoleResult::Value(Value::Int(i)) => {
                            ctx.var.set_result(i);

                            log::debug!("SHOP: get {i}");

                            if i >= 0 && i < ctx.header_info.replace.sell_item_count {
                                let sales = ctx.var.read_int("ITEMSALES", &[i as usize])?;

                                log::debug!("sales: {sales}");

                                if sales != 0 {
                                    let price = ctx
                                        .header_info
                                        .item_price
                                        .get(&(i as u32))
                                        .copied()
                                        .unwrap_or_default()
                                        as i64;
                                    let money = ctx.var.ref_int("MONEY", &[])?;

                                    if *money >= price {
                                        *money -= price;
                                        drop(money);
                                        *ctx.var.ref_int("ITEM", &[i as usize])? += 1;
                                    }
                                }
                            } else {
                                call!("USERSHOP");
                            }
                        }
                        ConsoleResult::Value(Value::String(_)) => {
                            log::error!("콘솔에서 잘못된 타입의 응답을 보냈습니다.");
                            break;
                        }
                    }
                }

                Ok(())
            }
        }
    }

    fn report_stack(stack: &Callstack, tx: &mut ConsoleSender, position: Option<ScriptPosition>) {
        fn read_source(path: &str, pos: &ScriptPosition) -> io::Result<Option<String>> {
            use io::{BufRead, BufReader};
            BufReader::new(std::fs::File::open(path)?)
                .lines()
                .nth(pos.line as usize)
                .transpose()
        }

        let position = position.unwrap_or(stack.script_position.clone());

        match &stack.func_name {
            Ok(name) => match read_source(stack.file_path.as_str(), &position) {
                Ok(Some(s)) => {
                    let source = s.replace("\n", "\\n");
                    report_error!(
                        tx,
                        "    at function {name}{position} `{}` [{source}]",
                        stack.file_path,
                    );
                }
                _ => {
                    report_error!(
                        tx,
                        "    at function {name}@{position} `{}`",
                        stack.file_path,
                    );
                }
            },
            Err(ty) => match read_source(stack.file_path.as_str(), &position) {
                Ok(Some(s)) => {
                    let source = s.replace("\n", "\\n");
                    report_error!(tx, "    at {ty}{position} `{}` [{source}]", stack.file_path,);
                }
                _ => {
                    report_error!(tx, "    at {ty}@{position} `{}`", stack.file_path,);
                }
            },
        }
    }

    pub fn start(&self, tx: &mut ConsoleSender, ctx: &mut VmContext) -> Result<()> {
        while let Some(begin) = ctx.take_begin() {
            match self.begin(begin, tx, ctx) {
                Ok(()) => {}
                Err(err) => {
                    tx.new_line();
                    report_error!(tx, "VM failed with: {err}");

                    ctx.update_last_call_stack();

                    for stack in ctx.call_stack().iter().rev() {
                        Self::report_stack(stack, tx, None);
                    }

                    return Err(err);
                }
            }
        }

        Ok(())
    }
}

fn make_bar_str(replace: &ReplaceInfo, var: i64, max: i64, length: i64) -> String {
    let bar_length = ((var as f32 / max as f32).clamp(0.0, 1.0) * length as f32) as usize;
    let blank = length as usize - bar_length;

    let mut ret = String::with_capacity(length as usize);

    ret.push('[');

    for _ in 0..bar_length {
        ret.push_str(&replace.bar_str1);
    }

    for _ in 0..blank {
        ret.push_str(&replace.bar_str2);
    }

    ret.push(']');

    ret
}

fn array_shift<T: Clone>(
    arr: &mut [T],
    empty_value: T,
    start: usize,
    count: usize,
) -> anyhow::Result<()> {
    if start >= arr.len() {
        bail!("ARRAYSHIFT start value exceed");
    }

    let arr = &mut arr[start..];

    if count < arr.len() {
        arr.rotate_right(count);
        arr[..count].fill(empty_value);
    } else {
        arr.fill(empty_value);
    }

    Ok(())
}

#[test]
fn shift_test() {
    let mut arr = [1, 1, 1, 1];
    array_shift(&mut arr, 0, 1, 2).unwrap();
    k9::assert_equal!(arr, [1, 0, 0, 1]);
    arr.fill(1);
    array_shift(&mut arr, 0, 1, 10).unwrap();
    k9::assert_equal!(arr, [1, 0, 0, 0]);
}
