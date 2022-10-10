mod context;
mod function;
mod save_data;
mod system_func;
mod variable;

use std::{collections::BTreeSet, path::PathBuf};

use anyhow::{anyhow, bail, Result};
use arrayvec::ArrayVec;
use context::FunctionIdentifier;
use itertools::Itertools;
use pad::PadStr;
use rand::Rng;
use strum::Display;

use hashbrown::HashMap;

use erars_ast::{
    BeginType, BinaryOperator, BuiltinCommand, BuiltinMethod, BuiltinVariable, EventType,
    PrintFlags, UnaryOperator, Value,
};
use erars_compiler::{Instruction, ParserContext, ReplaceInfo};
use erars_ui::{FontStyle, InputRequest, InputRequestType, Timeout, VirtualConsole};

pub use self::{
    context::{Callstack, LocalValue, VmContext},
    function::FunctionDic,
    variable::{UniformVariable, VariableStorage, VmVariable},
};
use crate::{function::FunctionBody, variable::SerializableVariableStorage};

macro_rules! report_error {
    ($tx:expr, $($t:tt)+) => {
        log::error!($($t)+);
        $tx.print_line(format!($($t)+));
    };
}

macro_rules! ret_set_state {
    ($ctx:expr, $state:expr) => {
        return Ok(InstructionWorkflow::SwitchState($state));
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

#[derive(Display, Debug, Clone, PartialEq, Eq)]
pub enum Workflow {
    Return,
    SwitchState(SystemState),
    GotoState(SystemState),
    Begin(BeginType),
    Input { req: InputRequest },
    Redraw,
    Exit,
}

#[derive(Display, Debug, Clone, derivative::Derivative)]
#[derivative(PartialEq, Eq)]
pub enum SystemState {
    LoadGame(
        #[derivative(PartialEq = "ignore")] Option<HashMap<usize, SerializableVariableStorage>>,
    ),
    SaveGame,
    SaveData,
    LoadData(
        i64,
        #[derivative(PartialEq = "ignore")] Option<SerializableVariableStorage>,
    ),
    BeginTitle,
    BeginShop,
    BeginTrain {
        com_no: u32,
        com_able_no: u32,
        printc_count: u32,
    },
    BeginFirst,
    BeginTurnEnd,
    BeginAfterTrain,
    BeginAblUp,
    DoTrain(u32),
    CallTrain(Vec<u32>, Option<u32>),
}

impl From<BeginType> for SystemState {
    fn from(ty: BeginType) -> Self {
        match ty {
            BeginType::AblUp => Self::BeginAblUp,
            BeginType::AfterTrain => Self::BeginAfterTrain,
            BeginType::Shop => Self::BeginShop,
            BeginType::Title => Self::BeginTitle,
            BeginType::Train => Self::BeginTrain {
                com_no: 0,
                com_able_no: 0,
                printc_count: 0,
            },
            BeginType::First => Self::BeginFirst,
            BeginType::TurnEnd => Self::BeginTurnEnd,
        }
    }
}

impl Default for SystemState {
    fn default() -> Self {
        Self::BeginTitle
    }
}

#[derive(Debug)]
enum InstructionWorkflow {
    Normal,
    Exit,
    Goto(u32),
    GotoLabel {
        label: String,
        is_try: bool,
    },
    Return,
    SwitchState(SystemState),
    CallEvent(EventType),
    Call {
        name: String,
        args: Vec<Value>,
        is_try: bool,
        is_jump: bool,
    },
    Input {
        req: InputRequest,
    },
    Redraw,
}

impl Default for Workflow {
    fn default() -> Self {
        Self::Return
    }
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
        inst: &Instruction,
        tx: &mut VirtualConsole,
        ctx: &mut VmContext,
    ) -> Result<InstructionWorkflow> {
        log::trace!(
            "[{func_name}] `{inst:?}`, stack: {stack:?}, call_stack: {call_stack:?}",
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
                    match self.run_instruction(func_name, inst, tx, ctx)? {
                        InstructionWorkflow::Normal => {}
                        other => bail!("EvalFromString can't do flow control"),
                    }
                }
            }
            Instruction::GotoLabel => {
                return Ok(InstructionWorkflow::GotoLabel {
                    label: ctx.pop_str()?,
                    is_try: false,
                });
            }
            Instruction::TryGotoLabel => {
                return Ok(InstructionWorkflow::GotoLabel {
                    label: ctx.pop_str()?,
                    is_try: true,
                })
            }
            Instruction::LoadExternVarRef(c) => {
                let func_extern = ctx.pop_str()?;
                let name = ctx.pop_str()?;
                let args = ctx.take_arg_list(None, *c)?;
                ctx.push_var_ref(name, func_extern, args);
            }
            Instruction::LoadVarRef(c) => {
                let name = ctx.pop_str()?;
                let args = ctx.take_arg_list(Some(&name), *c)?;
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
                if flags.contains(PrintFlags::WAIT) {
                    return Ok(InstructionWorkflow::Input {
                        req: InputRequest {
                            generation: tx.input_gen(),
                            ty: InputRequestType::AnyKey,
                            is_one: false,
                            timeout: None,
                        },
                    });
                }
            }
            Instruction::TryCall(c)
            | Instruction::TryJump(c)
            | Instruction::Jump(c)
            | Instruction::Call(c) => {
                let func = ctx.pop_str()?;
                let args = ctx.take_value_list(*c)?;

                return Ok(InstructionWorkflow::Call {
                    name: func,
                    args,
                    is_try: matches!(inst, Instruction::TryCall(_) | Instruction::TryJump(_)),
                    is_jump: matches!(inst, Instruction::Jump(_) | Instruction::TryJump(_)),
                });
            }
            Instruction::Begin(b) => {
                ret_set_state!(ctx, (*b).into());
            }
            Instruction::CallEvent(ty) => {
                return Ok(InstructionWorkflow::CallEvent(*ty));
            }
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
                    BinaryOperator::BitAnd => Value::Int(lhs.try_into_int()? & rhs.try_into_int()?),
                    BinaryOperator::BitOr => Value::Int(lhs.try_into_int()? | rhs.try_into_int()?),
                    BinaryOperator::BitXor => Value::Int(lhs.try_into_int()? ^ rhs.try_into_int()?),
                    BinaryOperator::Lhs => Value::Int(lhs.try_into_int()? << rhs.try_into_int()?),
                    BinaryOperator::Rhs => Value::Int(lhs.try_into_int()? >> rhs.try_into_int()?),
                };

                ctx.push(ret);
            }
            Instruction::Goto(no) => {
                return Ok(InstructionWorkflow::Goto(*no));
            }
            Instruction::GotoIfNot(no) => {
                let cond = ctx.pop_value()?.as_bool();
                if !cond {
                    return Ok(InstructionWorkflow::Goto(*no));
                }
            }
            Instruction::GotoIf(no) => {
                let cond = ctx.pop_value()?.as_bool();
                if cond {
                    return Ok(InstructionWorkflow::Goto(*no));
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
                let args = ctx.take_arg_list(None, *c)?;

                use BuiltinVariable::*;

                let value = match var {
                    GamebaseCode => ctx.header_info.gamebase.code.into(),
                    GamebaseVersion => ctx.header_info.gamebase.version.into(),
                    GamebaseAllowVersion => ctx.header_info.gamebase.allow_version.into(),
                    GamebaseDefaultChara => ctx.header_info.gamebase.default_chara.into(),
                    GamebaseNoItem => ctx.header_info.gamebase.no_item.into(),
                    GamebaseAuthor => ctx.header_info.gamebase.author.clone().into(),
                    GamebaseYear => ctx.header_info.gamebase.year.clone().into(),
                    GamebaseTitle => ctx.header_info.gamebase.title.clone().into(),
                    GamebaseInfo => ctx.header_info.gamebase.info.clone().into(),

                    LastLoadNo => ctx.lastload_no.into(),
                    LastLoadText => ctx.lastload_text.clone().into(),
                    LastLoadVersion => ctx.lastload_version.into(),

                    CharaNum => (ctx.var.character_len() as i64).into(),
                    LineCount => (tx.line_count() as i64).into(),
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
                        let end = get_arg!(@opt @usize: args, ctx)
                            .unwrap_or_else(|| ctx.var.character_len());

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
                        ctx.push(0i64);
                    }
                    BuiltinMethod::ChkCharaData => {
                        log::warn!("CHKCHARADATA");
                        ctx.push(1i64);
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
                    BuiltinMethod::StrFind => {
                        check_arg_count!(2, 3);
                        let s = get_arg!(@String: args, ctx);
                        let find = get_arg!(@String: args, ctx);
                        let start = get_arg!(@opt @usize: args, ctx).unwrap_or(0);

                        let encoding = ctx.encoding();
                        let bytes = encoding.encode(&s).0;
                        let find = encoding.encode(&find).0;

                        let pos = twoway::find_bytes(&bytes.as_ref()[start..], find.as_ref())
                            .map_or(-1, |n| n as i64);
                        ctx.push(pos);
                    }
                    BuiltinMethod::StrFindU => {
                        check_arg_count!(2, 3);
                        let s = get_arg!(@String: args, ctx);
                        let find = get_arg!(@String: args, ctx);
                        let start = get_arg!(@opt @usize: args, ctx).unwrap_or(0);

                        let start_len = s.chars().take(start).map(char::len_utf8).sum();

                        let pos = s[start_len..]
                            .split_once(&find)
                            .map_or(-1, |(left, _)| left.chars().count() as i64);
                        ctx.push(pos);
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
                    BuiltinMethod::IsSkip => {
                        ctx.push(tx.skipdisp());
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
                            Err(_) => ctx.push(0i64),
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
                        ctx.push(0xFFFFFFu32);
                    }
                    BuiltinMethod::GetDefBgColor => {
                        check_arg_count!(0);
                        ctx.push(0i64);
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

                        let (ret, rets) = match self::save_data::read_save_data(
                            &self.sav_path,
                            &ctx.header_info,
                            idx,
                        ) {
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
                        return Ok(InstructionWorkflow::Goto(0));
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

                        let target = if let Some(idx) = v.idxs.first().copied() {
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
                                        &mut cvar[chara_idx.unwrap_or(target as usize)]
                                    }
                                    UniformVariable::Normal(var) => var,
                                };
                                let start = start.unwrap_or(0);
                                let end =
                                    end.unwrap_or_else(|| info.size.last().copied().unwrap_or(1));

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

                        return Ok(InstructionWorkflow::Return);
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
                            ctx.var.get_var2("RESULT", "RESULTS").unwrap();
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

                        return Ok(InstructionWorkflow::Return);
                    }
                    BuiltinCommand::DrawLine => {
                        tx.draw_line(ctx.header_info.replace.drawline_str.clone());
                    }
                    BuiltinCommand::CustomDrawLine => {
                        let s = get_arg!(@String: args, ctx);
                        tx.draw_line(s);
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
                        ctx.push(0i64);
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
                        let name = get_arg!(@String: args, ctx);

                        let rgb: css_color::Srgb = match name.parse() {
                            Ok(color) => color,
                            Err(_) => {
                                bail!("Unknown color name {name}");
                            }
                        };

                        let (r, g, b) = (
                            (rgb.red * 255.0) as u8,
                            (rgb.green * 255.0) as u8,
                            (rgb.blue * 255.0) as u8,
                        );

                        if *com == BuiltinCommand::SetColorByName {
                            tx.set_color(r, g, b);
                        } else {
                            tx.set_bg_color(r, g, b);
                        }
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
                    BuiltinCommand::Wait | BuiltinCommand::WaitAnykey => {
                        return Ok(InstructionWorkflow::Input {
                            req: InputRequest::normal(
                                tx.input_gen(),
                                if *com == BuiltinCommand::Wait {
                                    InputRequestType::EnterKey
                                } else {
                                    InputRequestType::AnyKey
                                },
                            ),
                        });
                    }
                    BuiltinCommand::SkipDisp => {
                        let arg = ctx.pop_int()?;
                        tx.set_skipdisp(arg != 0);
                        ctx.var.set_result(0);
                    }
                    BuiltinCommand::NoSkip => {
                        ctx.prev_skipdisp = Some(tx.skipdisp());
                        tx.set_skipdisp(true);
                    }
                    BuiltinCommand::EndNoSkip => match ctx.prev_skipdisp.take() {
                        Some(ret) => tx.set_skipdisp(ret),
                        None => bail!("ENDNOSKIP without NOSKIP"),
                    },
                    BuiltinCommand::Input
                    | BuiltinCommand::InputS
                    | BuiltinCommand::TInput
                    | BuiltinCommand::TInputS
                    | BuiltinCommand::TOneInput
                    | BuiltinCommand::TOneInputS
                    | BuiltinCommand::OneInput
                    | BuiltinCommand::OneInputS => {
                        fn to_time(time: time::OffsetDateTime) -> i128 {
                            time.unix_timestamp_nanos()
                        }

                        let req = match com {
                            BuiltinCommand::InputS => {
                                InputRequest::normal(tx.input_gen(), InputRequestType::Str)
                            }
                            BuiltinCommand::Input => {
                                InputRequest::normal(tx.input_gen(), InputRequestType::Int)
                            }
                            BuiltinCommand::OneInputS => {
                                InputRequest::oneinput(tx.input_gen(), InputRequestType::Str)
                            }
                            BuiltinCommand::OneInput => {
                                InputRequest::oneinput(tx.input_gen(), InputRequestType::Int)
                            }
                            BuiltinCommand::TInputS => InputRequest {
                                generation: tx.input_gen(),
                                ty: InputRequestType::Str,
                                is_one: false,
                                timeout: Some(Timeout {
                                    timeout: to_time(
                                        time::OffsetDateTime::now_utc()
                                            + time::Duration::milliseconds(
                                                get_arg!(@i64: args, ctx),
                                            ),
                                    ),
                                    default_value: get_arg!(@Value: args, ctx),
                                    show_timer: get_arg!(@opt @bool: args, ctx).unwrap_or(true),
                                    timeout_msg: get_arg!(@opt @String: args, ctx),
                                }),
                            },
                            BuiltinCommand::TInput => InputRequest {
                                generation: tx.input_gen(),
                                ty: InputRequestType::Int,
                                is_one: false,
                                timeout: Some(Timeout {
                                    timeout: to_time(
                                        time::OffsetDateTime::now_utc()
                                            + time::Duration::milliseconds(
                                                get_arg!(@i64: args, ctx),
                                            ),
                                    ),
                                    default_value: get_arg!(@Value: args, ctx),
                                    show_timer: get_arg!(@opt @bool: args, ctx).unwrap_or(true),
                                    timeout_msg: get_arg!(@opt @String: args, ctx),
                                }),
                            },
                            BuiltinCommand::TOneInputS => InputRequest {
                                generation: tx.input_gen(),
                                ty: InputRequestType::Str,
                                is_one: true,
                                timeout: Some(Timeout {
                                    timeout: to_time(
                                        time::OffsetDateTime::now_utc()
                                            + time::Duration::milliseconds(
                                                get_arg!(@i64: args, ctx),
                                            ),
                                    ),
                                    default_value: get_arg!(@Value: args, ctx),
                                    show_timer: get_arg!(@opt @bool: args, ctx).unwrap_or(true),
                                    timeout_msg: get_arg!(@opt @String: args, ctx),
                                }),
                            },
                            BuiltinCommand::TOneInput => InputRequest {
                                generation: tx.input_gen(),
                                ty: InputRequestType::Int,
                                is_one: true,
                                timeout: Some(Timeout {
                                    timeout: to_time(
                                        time::OffsetDateTime::now_utc()
                                            + time::Duration::milliseconds(
                                                get_arg!(@i64: args, ctx),
                                            ),
                                    ),
                                    default_value: get_arg!(@Value: args, ctx),
                                    show_timer: get_arg!(@opt @bool: args, ctx).unwrap_or(true),
                                    timeout_msg: get_arg!(@opt @String: args, ctx),
                                }),
                            },
                            _ => unreachable!(),
                        };

                        return Ok(InstructionWorkflow::Input { req });
                    }
                    BuiltinCommand::Quit => {
                        log::info!("Run QUIT");
                        return Ok(InstructionWorkflow::Exit);
                    }
                    BuiltinCommand::SwapChara => {
                        let a = get_arg!(@usize: args, ctx);
                        let b = get_arg!(@usize: args, ctx);

                        ctx.var.swap_chara(a, b);
                    }
                    BuiltinCommand::SortChara => bail!("SORTCHARA"),
                    BuiltinCommand::PickupChara => {
                        let list = args
                            .map(|v| ctx.reduce_local_value(v).and_then(usize::try_from))
                            .collect::<Result<BTreeSet<_>>>()?;

                        let target = ctx.var.read_int("TARGET", &[])?;
                        let master = ctx.var.read_int("MASTER", &[])?;
                        let assi = ctx.var.read_int("ASSI", &[])?;

                        let recalculate_idx = |chara_idx: i64| match usize::try_from(chara_idx) {
                            Ok(idx) => list
                                .iter()
                                .find_position(|i| **i == idx)
                                .map(|(idx, _)| idx as i64)
                                .unwrap_or(-1),
                            _ => chara_idx,
                        };

                        *ctx.var.ref_int("TARGET", &[])? = recalculate_idx(target);
                        *ctx.var.ref_int("MASTER", &[])? = recalculate_idx(master);
                        *ctx.var.ref_int("ASSI", &[])? = recalculate_idx(assi);

                        ctx.var.del_chara_list(&list);
                    }
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
                        return Ok(InstructionWorkflow::Redraw);
                    }
                    BuiltinCommand::ClearLine => {
                        tx.clear_line(get_arg!(@usize: args, ctx));
                    }
                    BuiltinCommand::DoTrain => {
                        let arg = get_arg!(@u32: args, ctx);

                        ret_set_state!(ctx, SystemState::DoTrain(arg));
                    }
                    BuiltinCommand::CallTrain => {
                        let count = get_arg!(@usize: args, ctx);
                        let commands = ctx.var.get_var("SELECTCOM")?.1.assume_normal().as_int()?
                            [..count]
                            .iter()
                            // CallTrain works reverse order
                            .rev()
                            .map(|c| u32::try_from(*c).map_err(anyhow::Error::from))
                            .collect::<Result<Vec<u32>>>()?;

                        ret_set_state!(ctx, SystemState::CallTrain(commands, None));
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
                            &ctx.header_info,
                            description,
                        );
                    }
                    BuiltinCommand::LoadData => {
                        let idx = get_arg!(@i64: args, ctx);

                        if let Ok(sav) =
                            self::save_data::read_save_data(&self.sav_path, &ctx.header_info, idx)
                        {
                            ret_set_state!(ctx, SystemState::LoadData(idx, Some(sav)));
                        }
                    }
                    BuiltinCommand::DelData => {
                        let idx = get_arg!(@i64: args, ctx);

                        let _ = self::save_data::delete_save_data(&self.sav_path, idx);
                    }
                    BuiltinCommand::SaveGlobal => {
                        self::save_data::write_global_data(
                            &self.sav_path,
                            &ctx.var,
                            &ctx.header_info,
                        );
                    }
                    BuiltinCommand::LoadGlobal => {
                        match self::save_data::read_global_data(&self.sav_path, &ctx.header_info) {
                            Ok(data) => {
                                ctx.var.load_global_serializable(data, &ctx.header_info)?;
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
                        ret_set_state!(ctx, SystemState::SaveGame);
                    }
                    BuiltinCommand::LoadGame => {
                        ret_set_state!(ctx, SystemState::LoadGame(None));
                    }
                    BuiltinCommand::PutForm => {
                        let arg = get_arg!(@String: args, ctx);

                        anyhow::ensure!(
                            ctx.put_form_enabled,
                            "PUTFORM called in no @SAVEINFO function"
                        );

                        ctx.var.ref_str("SAVEDATA_TEXT", &[])?.push_str(&arg);
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
        }

        Ok(InstructionWorkflow::Normal)
    }

    fn run_body(
        &self,
        func_identifier: FunctionIdentifier,
        body: &FunctionBody,
        tx: &mut VirtualConsole,
        ctx: &mut VmContext,
        mut cursor: usize,
    ) -> Result<Workflow> {
        let insts = body.body();
        let func_name = func_identifier.name();

        while let Some(inst) = insts.get(cursor) {
            cursor += 1;
            use InstructionWorkflow::*;

            match self.run_instruction(func_name, inst, tx, ctx) {
                Ok(Normal) => {}
                Ok(Exit) => return Ok(Workflow::Exit),
                Ok(Goto(pos)) => {
                    cursor = pos as usize;
                }
                Ok(GotoLabel { label, is_try }) => match body.goto_labels().get(label.as_str()) {
                    Some(pos) => {
                        cursor = *pos as usize;
                    }
                    None => {
                        if is_try {
                            ctx.push(false);
                        } else {
                            bail!("Label {label} is not founded");
                        }
                    }
                },
                Ok(Return) => return Ok(Workflow::Return),
                Ok(CallEvent(ty)) => {
                    ctx.push_current_call_stack(
                        func_identifier.clone(),
                        body.file_path().clone(),
                        cursor,
                    );
                    match self.call_event(ty, tx, ctx, 0, 0)? {
                        Workflow::Return => {
                            ctx.pop_call_stack();
                            continue;
                        }
                        other => {
                            return Ok(other);
                        }
                    }
                }
                Ok(Call {
                    name,
                    args,
                    is_jump,
                    is_try,
                }) => {
                    if !is_jump {
                        ctx.push_current_call_stack(
                            func_identifier.clone(),
                            body.file_path().clone(),
                            cursor,
                        );
                    }
                    match self.try_call(&name, &args, tx, ctx)? {
                        Some(Workflow::Return) => {
                            if is_jump {
                                return Ok(Workflow::Return);
                            } else {
                                ctx.pop_call_stack();
                                if is_try {
                                    ctx.push(true);
                                }
                                continue;
                            }
                        }
                        Some(other) => {
                            return Ok(other);
                        }
                        None => {
                            if is_try {
                                ctx.push(false);
                            } else {
                                bail!("Function {name} is not found");
                            }
                        }
                    }
                }
                Ok(Redraw) => {
                    ctx.push_current_call_stack(
                        func_identifier.clone(),
                        body.file_path().clone(),
                        cursor,
                    );
                    return Ok(Workflow::Redraw);
                }
                Ok(SwitchState(state)) => {
                    ctx.push_current_call_stack(
                        func_identifier.clone(),
                        body.file_path().clone(),
                        cursor,
                    );
                    return Ok(Workflow::SwitchState(state));
                }
                Ok(Input { req }) => {
                    ctx.push_current_call_stack(
                        func_identifier.clone(),
                        body.file_path().clone(),
                        cursor,
                    );
                    return Ok(Workflow::Input { req });
                }
                Err(err) => {
                    return Err(err);
                }
            }
        }

        // exit without RETURN/RETURNF

        if body.is_function() {
            ctx.push(0i64);
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
        tx: &mut VirtualConsole,
        ctx: &mut VmContext,
        body: &FunctionBody,
    ) -> Result<Workflow> {
        log::trace!("CALL {label}({args:?})");

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

        let ret = self.run_body(FunctionIdentifier::Normal(label.into()), body, tx, ctx, 0)?;

        Ok(ret)
    }

    #[inline]
    fn call(
        &self,
        label: &str,
        args: &[Value],
        tx: &mut VirtualConsole,
        ctx: &mut VmContext,
    ) -> Result<Workflow> {
        self.call_internal(label, args, tx, ctx, self.dic.get_func(label)?)
    }

    #[inline]
    fn try_call(
        &self,
        label: &str,
        args: &[Value],
        tx: &mut VirtualConsole,
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
        tx: &mut VirtualConsole,
        ctx: &mut VmContext,
        event_idx: usize,
        event_cursor: usize,
    ) -> Result<Workflow> {
        self.dic.get_event(ty).run(event_idx, |body, idx| {
            let label: &str = ty.into();
            let ret = self.run_body(
                FunctionIdentifier::Event(ty, idx),
                body,
                tx,
                ctx,
                event_cursor,
            )?;

            Ok(ret)
        })
    }

    fn try_run_call_stack(&self, tx: &mut VirtualConsole, ctx: &mut VmContext) -> Result<Workflow> {
        while let Some(stack) = ctx.pop_call_stack() {
            match stack.func_name {
                FunctionIdentifier::Normal(name) => {
                    let body = self.dic.get_func(&name).unwrap();
                    match self.run_body(
                        FunctionIdentifier::Normal(name),
                        body,
                        tx,
                        ctx,
                        stack.instruction_pos,
                    )? {
                        Workflow::Return => continue,
                        other => return Ok(other),
                    }
                }
                FunctionIdentifier::Event(ty, idx) => {
                    match self.call_event(ty, tx, ctx, idx, stack.instruction_pos)? {
                        Workflow::Return => continue,
                        other => return Ok(other),
                    }
                }
            }
        }
        Ok(Workflow::Return)
    }

    pub fn run_state(&self, tx: &mut VirtualConsole, ctx: &mut VmContext) -> Result<VmResult> {
        match self.try_run_call_stack(tx, ctx)? {
            Workflow::Return => {}
            Workflow::Begin(ty) => {
                ctx.state.clear();
                ctx.clear_call_stack();
                ctx.state.push((ty.into(), 0));
            }
            Workflow::Exit => {
                ctx.state.clear();
                ctx.clear_call_stack();
                return Ok(VmResult::Exit);
            }
            Workflow::Redraw => return Ok(VmResult::Redraw),
            Workflow::Input { req } => return Ok(VmResult::NeedInput { req }),
            Workflow::SwitchState(new_state) => {
                ctx.state.push((new_state, 0));
            }
            Workflow::GotoState(new_state) => {
                ctx.state.push((new_state, 0));
            }
        }

        let mut phase = 0;
        let (mut current_state, mut phase) = match ctx.state.pop() {
            Some(s) => s,
            None => return Ok(VmResult::Exit),
        };

        let ret = loop {
            log::info!("Run {current_state:?}[{phase}]");
            match current_state.run(self, tx, ctx, &mut phase)? {
                Some(Workflow::Return) => {
                    match ctx.state.pop() {
                        Some(s) => {
                            current_state = s.0;
                            phase = s.1;
                        },
                        None => return Ok(VmResult::Exit),
                    };
                }
                Some(Workflow::Exit) => {
                    ctx.state.clear();
                    ctx.clear_call_stack();
                    return Ok(VmResult::Exit);
                }
                Some(Workflow::Redraw) => {
                    break Ok(VmResult::Redraw);
                }
                Some(Workflow::Input { req }) => break Ok(VmResult::NeedInput { req }),
                Some(Workflow::Begin(ty)) => {
                    ctx.state.clear();
                    ctx.clear_call_stack();
                    current_state = ty.into();
                    phase = 0;
                }
                Some(Workflow::GotoState(new_state)) => {
                    current_state = new_state;
                    phase = 0;
                }
                Some(Workflow::SwitchState(new_state)) => {
                    ctx.state.push((current_state, phase));
                    current_state = new_state;
                    phase = 0;
                }
                None => {
                    continue;
                }
            }
        };

        ctx.state.push((current_state, phase));

        ret
    }
}

pub enum VmResult {
    Exit,
    Redraw,
    NeedInput { req: InputRequest },
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
