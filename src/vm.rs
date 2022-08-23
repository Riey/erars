mod context;
mod variable;

use std::{io, iter};

use anyhow::{anyhow, bail, Result};
use arrayvec::ArrayVec;
use itertools::Itertools;
use smol_str::SmolStr;
use strum::Display;

use hashbrown::HashMap;

use erars_ast::{
    BeginType, BinaryOperator, BuiltinCommand, EventType, PrintFlags, ScriptPosition,
    UnaryOperator, Value,
};
use erars_compiler::{Instruction, ParserContext};

pub use self::{
    context::{Callstack, LocalValue, VmContext},
    variable::{UniformVariable, VariableStorage, VmVariable},
};

use crate::function::{FunctionBody, FunctionDic};
use crate::ui::{ConsoleChannel, ConsoleMessage, ConsoleResult, InputRequest};

macro_rules! report_error {
    ($chan:expr, $($t:tt)+) => {
        log::error!($($t)+);
        $chan.send_msg(ConsoleMessage::Print(format!($($t)+)));
        $chan.send_msg(ConsoleMessage::NewLine);
    };
}

#[derive(Display, Debug, Clone, Copy)]
enum Workflow {
    Return,
    Exit,
}

pub struct TerminalVm {
    dic: FunctionDic,
}

impl TerminalVm {
    pub fn new(function_dic: FunctionDic) -> Self {
        Self { dic: function_dic }
    }

    fn run_instruction(
        &self,
        func_name: &str,
        goto_labels: &HashMap<SmolStr, u32>,
        inst: &Instruction,
        cursor: &mut usize,
        chan: &ConsoleChannel,
        ctx: &mut VmContext,
    ) -> Result<Option<Workflow>> {
        log::trace!(
            "[{func_name}] `{inst:?}`, stack: {stack:?}",
            stack = ctx.stack()
        );

        match inst {
            Instruction::ReportPosition(pos) => ctx.update_position(pos.clone()),
            Instruction::LoadInt(n) => ctx.push(*n),
            Instruction::LoadStr(s) => ctx.push(s.to_string()),
            Instruction::Nop => {}
            Instruction::Pop => drop(ctx.pop()),
            Instruction::Duplicate => ctx.dup(),
            Instruction::DuplicatePrev => ctx.dup_prev(),
            Instruction::ReadVar => {
                let value = ctx.pop_value()?;
                ctx.push(value);
            }
            Instruction::EvalFormString => {
                let form = ctx.pop_str()?;
                let parser_ctx = ParserContext::new(ctx.header_info.clone(), "FORMS.ERB".into());
                let expr = erars_compiler::normal_form_str(&parser_ctx)(&form)
                    .unwrap()
                    .1;
                let insts = erars_compiler::compile_expr(expr).unwrap();

                for inst in insts.iter() {
                    self.run_instruction(func_name, goto_labels, inst, &mut 0, chan, ctx)?;
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
                ctx.reuse_last_line(chan, s);
                chan.request_redraw();
            }
            Instruction::Print(flags) => {
                let s = ctx.pop_str()?;
                ctx.print(chan, s);
                if flags.contains(PrintFlags::NEWLINE) {
                    ctx.new_line(chan);
                }
                chan.request_redraw();

                // TODO: PRINTW
            }
            Instruction::CallMethod(c) => {
                let func = ctx.pop_str()?;
                match func.as_str() {
                    "CSVNAME" => {
                        let no = ctx.pop_int()? as u32;

                        let csv = ctx
                            .header_info
                            .character_templates
                            .get(&no)
                            .map(|csv| csv.name.clone())
                            .unwrap_or_default();
                        ctx.push(csv);
                    }
                    "CSVNICKNAME" => {
                        let no = ctx.pop_int()? as u32;

                        let csv = ctx
                            .header_info
                            .character_templates
                            .get(&no)
                            .map(|csv| csv.nick_name.clone())
                            .unwrap_or_default();
                        ctx.push(csv);
                    }
                    "CSVCALLNAME" => {
                        let no = ctx.pop_int()? as u32;

                        let csv = ctx
                            .header_info
                            .character_templates
                            .get(&no)
                            .map(|csv| csv.call_name.clone())
                            .unwrap_or_default();
                        ctx.push(csv);
                    }
                    "CSVCSTR" => {
                        let idx = ctx.pop_int()? as u32;
                        let no = ctx.pop_int()? as u32;

                        let csv = ctx
                            .header_info
                            .character_templates
                            .get(&no)
                            .and_then(|csv| csv.cstr.get(&idx).cloned())
                            .unwrap_or_default();
                        ctx.push(csv);
                    }
                    "STRLENS" => {
                        let s = ctx.pop_str()?;
                        ctx.push(encoding_rs::SHIFT_JIS.encode(&s).0.as_ref().len() as i64);
                    }
                    "STRLENSU" => {
                        let s = ctx.pop_str()?;
                        ctx.push(s.len() as i64);
                    }
                    "TOSTR" => {
                        let ret;

                        match *c {
                            1 => {
                                let value = ctx.pop_int()?;
                                ret = value.to_string();
                            }
                            2 => {
                                let _format = ctx.pop_str()?;
                                let value = ctx.pop_int()?;
                                ret = format!("{00}", value);
                            }
                            _ => bail!("TOSTR의 매개변수는 1개 또는 2개여야 합니다."),
                        }

                        ctx.push(ret);
                    }
                    "MAX" => {
                        let args = ctx.take_value_list(*c)?.into_iter();

                        ctx.push(args.max().unwrap_or(Value::Int(0)));
                    }
                    "MIN" => {
                        let args = ctx.take_value_list(*c)?.into_iter();

                        ctx.push(args.min().unwrap_or(Value::Int(0)));
                    }
                    "LIMIT" => {
                        if *c != 3 {
                            bail!("LIMIT의 매개변수는 3개여야합니다");
                        } else {
                            let high = ctx.pop_int()?;
                            let low = ctx.pop_int()?;
                            let v = ctx.pop_int()?;

                            ctx.push(v.clamp(low, high));
                        }
                    }
                    "LINEISEMPTY" => {
                        let line_is_empty = ctx.line_is_empty();
                        ctx.push(line_is_empty);
                    }
                    "GETCHARA" => {
                        let no = ctx.pop_int()?;

                        let idx = ctx.var.get_chara(no)?;

                        ctx.push(idx.map(|i| i as i64).unwrap_or(-1));
                    }
                    label => {
                        let args = ctx.take_value_list(*c)?;

                        match self.call(label, args.as_slice(), chan, ctx)? {
                            Workflow::Exit => return Ok(Some(Workflow::Exit)),
                            Workflow::Return => {}
                        }
                    }
                }
            }
            Instruction::TryCall(c) => {
                let func = ctx.pop_str()?;
                let args = ctx.take_value_list(*c)?;

                match self.try_call(&func, &args, chan, ctx)? {
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

                match self.call(&func, &args, chan, ctx)? {
                    Workflow::Return => {}
                    Workflow::Exit => return Ok(Some(Workflow::Exit)),
                }
            }
            Instruction::Begin(b) => {
                ctx.begin = Some(*b);
                return Ok(Some(Workflow::Exit));
            }
            Instruction::ConcatString(c) => {
                let args = ctx.take_value_list(*c)?;
                let ret = args
                    .into_iter()
                    .fold(String::new(), |s, l| s + l.into_str().as_str());
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
                chan.send_msg(ConsoleMessage::Alignment(*align));
            }
            Instruction::PadStr(align) => {
                use erars_ast::Alignment;
                use pad::{Alignment as PadAlign, PadStr};

                let size = ctx.pop_int()?;
                let text = match ctx.pop_value()? {
                    Value::String(s) => s,
                    Value::Int(i) => i.to_string(),
                };

                let align = match align {
                    Alignment::Left => PadAlign::Left,
                    Alignment::Center => PadAlign::Middle,
                    Alignment::Right => PadAlign::Right,
                };

                ctx.push(text.pad_to_width_with_alignment(size as usize, align));
            }
            Instruction::Command(com, c) => {
                let mut args = ctx.take_list(*c).collect::<ArrayVec<_, 8>>().into_iter();

                macro_rules! get_arg {
                    () => {
                        get_arg!(@opt).ok_or_else(|| anyhow!("매개변수가 부족합니다"))?
                    };
                    (@opt) => {
                        args.next()
                    };
                    (@var) => {
                        match args.next() {
                            Some(LocalValue::VarRef(r)) => r,
                            Some(LocalValue::Value(_)) => bail!("매개변수가 VarRef가 아닙니다"),
                            None => bail!("매개변수가 부족합니다"),
                        }
                    };
                    (@value) => {
                        get_arg!(@opt @value).ok_or_else(|| anyhow!("매개변수가 부족합니다"))?
                    };
                    (@opt @value) => {
                        match args.next() {
                            Some(LocalValue::VarRef(r)) => Some(ctx.read_var_ref(&r)?),
                            Some(LocalValue::Value(v)) => Some(v),
                            None => None,
                        }
                    };
                    (@$t:ty) => {
                        get_arg!(@opt @$t).ok_or_else(|| anyhow!("매개변수가 부족합니다"))?
                    };
                    (@opt @$t:ty) => {
                        get_arg!(@opt @value).and_then(|v| <$t>::try_from(v).ok())
                    };
                }

                match com {
                    BuiltinCommand::Restart => {
                        *cursor = 0;
                    }
                    BuiltinCommand::Unicode => {
                        let code = get_arg!(@i64).try_into()?;

                        ctx.push(
                            char::from_u32(code)
                                .ok_or_else(|| {
                                    anyhow!("u32 {code} is not valid unicode codepoint")
                                })?
                                .to_string(),
                        );
                    }
                    BuiltinCommand::Throw => {
                        let msg = get_arg!(@opt @String);

                        match msg {
                            Some(msg) => bail!("스크립트에서 예외발생: {msg}"),
                            None => bail!("스크립트에서 예외발생"),
                        }
                    }
                    BuiltinCommand::Varset => {
                        let var = get_arg!(@var);
                        let value = get_arg!(@opt @value);
                        let start = get_arg!(@opt @usize);
                        let end = get_arg!(@opt @usize);

                        let (info, var, args) = ctx.resolve_var_ref_raw(&var)?;

                        match (value, start, end) {
                            (None, None, None) => {
                                *var = UniformVariable::new(info);
                            }
                            (Some(value), start, end) => {
                                let var = var.assume_normal();
                                let start = start.unwrap_or(0);
                                let end = end.unwrap_or(info.size.last().copied().unwrap_or(1));

                                for i in start..end {
                                    let args = args.clone().chain(Some(i));
                                    var.set(args, value.clone())?;
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                    BuiltinCommand::Split => {
                        let s = get_arg!(@String);
                        let delimiter = get_arg!(@String);
                        let mut var = get_arg!(@var);

                        for (idx, part) in s.split(delimiter.as_str()).enumerate() {
                            var.idxs.push(idx);

                            ctx.set_var_ref(&var, part.into())?;

                            var.idxs.pop();
                        }
                    }
                    BuiltinCommand::Bar => {
                        let var = get_arg!(@i64);
                        let max = get_arg!(@i64).max(1);
                        let length = get_arg!(@i64).max(0);

                        let bar_length =
                            ((var as f32 / max as f32).clamp(0.0, 1.0) * length as f32) as usize;
                        let blank = length as usize - bar_length;

                        let mut ret = String::with_capacity(length as usize);

                        ret.push('[');

                        const FILL_CHAR: char = '*';
                        const BLANK_CHAR: char = '.';

                        for _ in 0..bar_length {
                            ret.push(FILL_CHAR);
                        }

                        for _ in 0..blank {
                            ret.push(BLANK_CHAR);
                        }

                        ret.push(']');

                        ctx.print(chan, ret);
                    }
                    BuiltinCommand::ReturnF => {
                        let ret = get_arg!(@value);

                        if args.next().is_some() {
                            log::warn!("RETURNF는 한개의 값만 반환할 수 있습니다.");
                        }

                        let left_stack = ctx.return_func()?.collect::<ArrayVec<_, 8>>();

                        if !left_stack.is_empty() {
                            log::warn!("반환되는 함수에 값이 남아있습니다. 프로그램이 잘못되었습니다: {left_stack:?}");
                        }

                        ctx.push(ret);

                        return Ok(Some(Workflow::Return));
                    }
                    BuiltinCommand::Return => {
                        let left_stack = ctx.return_func()?.collect::<ArrayVec<_, 8>>();

                        if !left_stack.is_empty() {
                            log::warn!("반환되는 함수에 값이 남아있습니다. 프로그램이 잘못되었습니다: {left_stack:?}");
                        }

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
                                value @ Value::Int(_) => {
                                    result.set(iter::once(result_idx), value)?;
                                    result_idx += 1;
                                }
                                value @ Value::String(_) => {
                                    results.set(iter::once(results_idx), value)?;
                                    results_idx += 1;
                                }
                            }
                        }

                        return Ok(Some(Workflow::Return));
                    }
                    BuiltinCommand::StrLenS => {
                        let s = get_arg!(@String);

                        ctx.var
                            .set_result(encoding_rs::SHIFT_JIS.encode(&s).0.as_ref().len() as i64);
                    }
                    BuiltinCommand::StrLenSU => {
                        let s = get_arg!(@String);

                        ctx.var.set_result(s.len() as i64);
                    }
                    BuiltinCommand::DrawLine => {
                        chan.send_msg(ConsoleMessage::DrawLine);
                        chan.request_redraw();
                    }
                    BuiltinCommand::FontStyle => {
                        log::warn!("TODO: fontstyle({})", get_arg!(@i64));
                    }
                    BuiltinCommand::FontBold
                    | BuiltinCommand::FontRegular
                    | BuiltinCommand::FontItalic => {
                        log::warn!("TODO: {com}");
                    }
                    BuiltinCommand::SetColor => {
                        let c = get_arg!(@i64);

                        let (r, g, b) = match get_arg!(@opt @i64) {
                            Some(g) => {
                                let b = get_arg!(@i64);
                                (c as u8, g as u8, b as u8)
                            }
                            None => {
                                let [r, g, b, _] = (c as u32).to_le_bytes();
                                (r, g, b)
                            }
                        };

                        ctx.set_color(chan, r, g, b);
                    }
                    BuiltinCommand::ResetColor => {
                        ctx.set_color(chan, 0xFF, 0xFF, 0xFF);
                    }
                    BuiltinCommand::ResetBgColor => {
                        log::warn!("TODO: RESETBGCOLOR");
                        // chan.send_msg(ConsoleMessage::SetColor(0xFF, 0xFF, 0xFF));
                    }
                    BuiltinCommand::GetColor => {
                        ctx.push(ctx.color() as i64);
                    }
                    BuiltinCommand::GetBgColor => {
                        ctx.push(ctx.bg_color() as i64);
                    }
                    BuiltinCommand::GetFocusColor => {
                        ctx.push(ctx.hl_color() as i64);
                    }
                    BuiltinCommand::Input | BuiltinCommand::InputS => {
                        let req = match com {
                            BuiltinCommand::InputS => InputRequest::Str,
                            BuiltinCommand::Input => InputRequest::Int,
                            _ => unreachable!(),
                        };
                        match input(chan, req) {
                            ConsoleResult::Quit => {
                                log::info!("User Quit");
                                return Ok(Some(Workflow::Exit));
                            }
                            ConsoleResult::Value(ret) => {
                                ctx.var
                                    .get_var(if req == InputRequest::Int {
                                        "RESULT"
                                    } else {
                                        "RESULTS"
                                    })
                                    .unwrap()
                                    .1
                                    .assume_normal()
                                    .set(iter::empty(), ret)?;
                            }
                        }
                    }
                    BuiltinCommand::Quit => {
                        log::info!("Run QUIT");
                        chan.exit();
                        return Ok(Some(Workflow::Exit));
                    }
                    BuiltinCommand::AddChara => {
                        let no = get_arg!(@i64).try_into()?;
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
                    BuiltinCommand::GetChara => {
                        let no = get_arg!(@i64);

                        let idx = ctx.var.get_chara(no)?;

                        ctx.var.set_result(idx.map(|i| i as i64).unwrap_or(-1));
                    }
                    BuiltinCommand::DelChara => {
                        let idx = get_arg!(@i64);
                        ctx.var.del_chara(idx.try_into()?);
                    }
                    BuiltinCommand::ResetData => {
                        ctx.var.reset_data();
                    }
                    BuiltinCommand::LoadGlobal
                    | BuiltinCommand::SaveGlobal
                    | BuiltinCommand::SaveData
                    | BuiltinCommand::LoadData => {
                        log::warn!("TODO: Save/Load");
                    }
                    _ => {
                        bail!("TODO: {}({:?})", com, args.collect_vec());
                    }
                }
            }
            _ => bail!("TODO: {:?}", inst),
        }

        Ok(None)
    }

    fn run_body(
        &self,
        func_name: &str,
        goto_labels: &HashMap<SmolStr, u32>,
        body: &FunctionBody,
        chan: &ConsoleChannel,
        ctx: &mut VmContext,
    ) -> Result<Workflow> {
        let mut cursor = 0;

        let insts = body.body();

        while let Some(inst) = insts.get(cursor) {
            cursor += 1;
            match self.run_instruction(func_name, goto_labels, inst, &mut cursor, chan, ctx) {
                Ok(None) => {}
                Ok(Some(Workflow::Return)) => break,
                Ok(Some(flow)) => return Ok(flow),
                Err(err) => {
                    return Err(err);
                }
            }
        }

        Ok(Workflow::Return)
    }

    fn call_internal(
        &self,
        label: &str,
        args: &[Value],
        chan: &ConsoleChannel,
        ctx: &mut VmContext,
        body: &FunctionBody,
    ) -> Result<Workflow> {
        log::trace!("CALL {label}({args:?})");
        ctx.new_func(Ok(label.into()), body.file_path().clone());

        let mut args = args.iter().cloned();

        for (var_idx, default_value, arg_indices) in body.args().iter() {
            let var = ctx.var.get_maybe_local_var(label, var_idx)?;

            var.1.assume_normal().set(
                arg_indices.iter().copied(),
                args.next()
                    .or_else(|| default_value.clone())
                    .unwrap_or_else(|| {
                        if var.0.is_str {
                            Value::String("".into())
                        } else {
                            Value::Int(0)
                        }
                    }),
            )?;
        }

        let ret = self.run_body(label, body.goto_labels(), body, chan, ctx)?;

        ctx.end_func();

        Ok(ret)
    }

    #[inline]
    fn call(
        &self,
        label: &str,
        args: &[Value],
        chan: &ConsoleChannel,
        ctx: &mut VmContext,
    ) -> Result<Workflow> {
        self.call_internal(label, args, chan, ctx, self.dic.get_func(label)?)
    }

    #[inline]
    fn try_call(
        &self,
        label: &str,
        args: &[Value],
        chan: &ConsoleChannel,
        ctx: &mut VmContext,
    ) -> Result<Option<Workflow>> {
        match self.dic.get_func_opt(label) {
            Some(body) => self.call_internal(label, args, chan, ctx, body).map(Some),
            None => Ok(None),
        }
    }

    fn call_event(&self, ty: EventType, chan: &ConsoleChannel, ctx: &mut VmContext) -> Result<()> {
        self.dic.get_event(ty).run(|body| {
            let label: &str = ty.into();
            ctx.new_func(Err(ty), body.file_path().clone());
            self.run_body(label, body.goto_labels(), body, chan, ctx)?;
            ctx.end_func();

            Ok(())
        })
    }

    fn begin(&self, ty: BeginType, chan: &ConsoleChannel, ctx: &mut VmContext) -> Result<()> {
        log::trace!("Begin {ty}");

        match ty {
            BeginType::Title => {
                self.call("SYSTEM_TITLE".into(), &[], chan, ctx)?;
                Ok(())
            }
            BeginType::First => {
                self.call_event(EventType::First, chan, ctx)?;
                Ok(())
            }
            BeginType::Shop => {
                self.call_event(EventType::Shop, chan, ctx)?;

                while ctx.begin.is_none() {
                    self.call("SHOW_SHOP", &[], chan, ctx)?;

                    match input(chan, InputRequest::Int) {
                        ConsoleResult::Quit => break,
                        ConsoleResult::Value(Value::Int(i)) => {
                            ctx.var.set_result(i);

                            log::debug!("SHOP: get {i}");

                            if i >= 0 && i < 100 {
                                let sales = *ctx
                                    .var
                                    .get_var("ITEMSALES")?
                                    .1
                                    .assume_normal()
                                    .get_int(iter::once(i as usize))?;

                                log::debug!("sales: {sales}");

                                #[allow(unreachable_code)]
                                #[allow(unused_variables)]
                                if sales != 0 {
                                    let price = todo!("ITEMPRICE");
                                    let money = ctx
                                        .var
                                        .get_var("MONEY")?
                                        .1
                                        .assume_normal()
                                        .get_int(iter::empty())?;

                                    if *money >= price {
                                        *money -= price;
                                        drop(money);
                                        *ctx.var
                                            .get_var("ITEM")?
                                            .1
                                            .assume_normal()
                                            .get_int(iter::once(i as usize))? += 1;
                                    }
                                }
                            } else {
                                self.call("USERSHOP", &[], chan, ctx)?;
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
            _ => bail!("TODO: {}", ty),
        }
    }

    fn report_stack(stack: &Callstack, chan: &ConsoleChannel, position: Option<ScriptPosition>) {
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
                        chan,
                        "    at function {name}{position} `{}` [{source}]",
                        stack.file_path,
                    );
                }
                _ => {
                    report_error!(
                        chan,
                        "    at function {name}@{position} `{}`",
                        stack.file_path,
                    );
                }
            },
            Err(ty) => match read_source(stack.file_path.as_str(), &position) {
                Ok(Some(s)) => {
                    let source = s.replace("\n", "\\n");
                    report_error!(
                        chan,
                        "    at {ty}{position} `{}` [{source}]",
                        stack.file_path,
                    );
                }
                _ => {
                    report_error!(chan, "    at {ty}@{position} `{}`", stack.file_path,);
                }
            },
        }
    }

    pub fn start(&self, chan: &ConsoleChannel, ctx: &mut VmContext) -> Result<()> {
        while let Some(begin) = ctx.take_begin() {
            match self.begin(begin, chan, ctx) {
                Ok(()) => {}
                Err(err) => {
                    report_error!(chan, "VM failed with: {err}");

                    ctx.update_last_call_stack();

                    for stack in ctx.call_stack().iter().rev() {
                        Self::report_stack(stack, chan, None);
                    }

                    return Err(err);
                }
            }
        }

        Ok(())
    }
}

fn input(chan: &ConsoleChannel, req: InputRequest) -> ConsoleResult {
    chan.send_msg(ConsoleMessage::Input(req));
    let ret = chan.recv_ret();
    log::trace!("Console Recv {ret:?}");
    ret
}
