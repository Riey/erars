use anyhow::{ensure, Context};
use std::fmt::Write as _;
use erars_ast::Alignment;
use erars_compiler::EraConfigKey;
use erars_ui::image::MixedNum;
use tinyvec::ArrayVec;

use crate::{
    context::VariableRef,
    dotnet_number::format_arg,
    graphics::{ColorMatrix, Font, ImageResolver, Pen, Rect, SpriteColor, MAX_IMAGE_SIZE},
    variable::{DataEntryStyle, KnownVariableNames as Var},
};

use super::*;

const BASE_TIME: time::OffsetDateTime = time::PrimitiveDateTime::new(
    if let Ok(d) = time::Date::from_ordinal_date(1, 1) {
        d
    } else {
        unreachable!()
    },
    time::Time::MIDNIGHT,
)
.assume_utc();

macro_rules! conv_workflow {
    ($workflow:expr) => {
        match $workflow {
            Workflow::Return => {}
            other => return Ok(other.into()),
        }
    };
}

/// A print-driven repaint: Emuera's unforced `RefreshStrings(false)`, which
/// `REDRAW 0` suppresses (`EmueraConsole.cs:1440`).
fn redraw_print(ctx: &mut VmContext, tx: &mut VirtualConsole) -> anyhow::Result<()> {
    if tx.redraw_enabled() {
        ctx.redraw(tx)?;
    }
    Ok(())
}

// macro_rules! call {
//     ($vm:expr, $name:expr, $tx:expr, $ctx:expr) => {
//         call!($vm, $name, &[], $tx, $ctx)
//     };
//     ($vm:expr, $name:expr, $args:expr, $tx:expr, $ctx:expr) => {
//         conv_workflow!($vm.call($name, $args, $tx, $ctx)?)
//     };
// }

macro_rules! try_call {
    ($vm:expr, $name:expr, $tx:expr, $ctx:expr) => {
        try_call!($vm, $name, &[], $tx, $ctx)
    };
    ($vm:expr, $name:expr, $args:expr, $tx:expr, $ctx:expr) => {
        match $vm.try_call($name, $args, $tx, $ctx)? {
            Some(Workflow::Return) => true,
            None => false,
            Some(other) => return Ok(other.into()),
        }
    };
}

macro_rules! call_event {
    ($vm:expr, $ty:expr, $tx:expr, $ctx:expr) => {
        conv_workflow!($vm.call_event($ty, $tx, $ctx)?)
    };
}

macro_rules! get_arg {
    ($arg:expr) => {
        get_arg!(@opt $arg).ok_or_else(|| anyhow!("매개변수가 부족합니다"))?
    };
    (@opt $arg:expr) => {
        $arg.next()
    };
    (@key $arg:expr, $ctx:expr) => {
        match $arg.next() {
            Some(LocalValue::InternedStr(key)) => key,
            Some(v) => {
                let s: String = $ctx.reduce_local_value(v)?.try_into().context("매개변수의 형식이 잘못되었습니다.")?;
                $ctx.var.interner().get_or_intern(&s)
            }
            None => bail!("매개변수가 부족합니다"),
        }
    };
    (@var $arg:expr) => {
        match $arg.next() {
            Some(LocalValue::VarRef(r)) => r,
            Some(_) => bail!("매개변수가 VarRef가 아닙니다"),
            None => bail!("매개변수가 부족합니다"),
        }
    };
    (@value $arg:expr, $ctx:expr) => {
        get_arg!(@opt @value $arg, $ctx).ok_or_else(|| anyhow!("매개변수가 부족합니다"))?
    };
    (@opt @value $arg:expr, $ctx:expr) => {
        match $arg.next() {
            Some(v) => Some($ctx.reduce_local_value(v)?),
            None => None,
        }
    };
    (@$t:ty: $arg:expr, $ctx:expr) => {
        get_arg!(@opt @$t: $arg, $ctx).ok_or_else(|| anyhow!("매개변수가 부족합니다"))?
    };
    (@opt @$t:ty: $arg:expr, $ctx:expr) => {
        match get_arg!(@opt @value $arg, $ctx) {
            Some(v) => Some(<$t>::try_from(v).context("매개변수의 형식이 잘못되었습니다.")?),
            None => None,
        }
    };
}

pub(super) fn run_instruction(
    vm: &TerminalVm,
    func_name: StrKey,
    inst: Instruction,
    tx: &mut VirtualConsole,
    ctx: &mut VmContext,
) -> Result<InstructionWorkflow> {
    if let Some(pos) = inst.as_report_position() {
        ctx.update_position(pos);
    } else if let Some(i) = inst.as_load_int() {
        ctx.push(i as i64);
    } else if let Some(r) = inst.as_load_int_suffix() {
        let l = ctx.pop_int()? as i32;
        #[cfg(target_endian = "big")]
        compile_error!("Big endian not supported");
        let i: i64 = unsafe { std::mem::transmute((l, r)) };
        ctx.push(i);
    } else if let Some(s) = inst.as_load_str() {
        ctx.push_strkey(s);
    } else if inst.is_duplicate() {
        ctx.dup();
    } else if inst.is_duplicate_prev() {
        ctx.dup_prev();
    } else if inst.is_store_result() {
        match ctx.pop_value()? {
            Value::Int(i) => ctx.var.set_result(i),
            Value::String(s) => ctx.var.set_results(s),
        }
    } else if inst.is_store_var() {
        let var_ref = ctx.pop_var_ref()?;
        let value = ctx.pop_value()?;

        ctx.set_var_ref(&var_ref, value)?;
    } else if inst.is_pop() {
        drop(ctx.pop()?);
    } else if inst.is_read_var() {
        let value = ctx.pop_value()?;
        ctx.push(value);
    } else if inst.is_eval_form_string() {
        let form = ctx.pop_str()?;

        return Ok(InstructionWorkflow::EvalFormString(form));
    } else if inst.is_goto_label() {
        return Ok(InstructionWorkflow::GotoLabel {
            label: ctx.pop_strkey()?,
            is_try: false,
        });
    } else if inst.is_try_goto_label() {
        return Ok(InstructionWorkflow::GotoLabel {
            label: ctx.pop_strkey()?,
            is_try: true,
        });
    } else if let Some(c) = inst.as_load_extern_varref() {
        let func_extern = ctx.pop_strkey()?;
        let name = ctx.pop_strkey()?;
        let args = ctx.take_arg_list(None, c)?;
        ctx.push_var_ref(name, func_extern, args)?;
    } else if let Some(c) = inst.as_load_var_ref() {
        let name = ctx.pop_strkey()?;
        let args = ctx.take_arg_list(Some(name), c)?;
        ctx.push_var_ref(name, func_name, args)?;
    } else if inst.is_load_count_var_ref() {
        ctx.push_var_ref(ctx.var.known_key(Var::Count), func_name, ArgVec::new())?;
    } else if inst.is_reuse_lastline() {
        let s = ctx.pop_str()?;
        tx.reuse_last_line(s);
        redraw_print(ctx, tx)?;
    } else if let Some(flags) = inst.as_print_button() {
        let value = ctx.pop_value()?;
        let text = ctx.pop_str()?;
        if flags.contains(PrintFlags::LEFT_ALIGN) {
            tx.print_button_lc(text, value);
        } else if flags.contains(PrintFlags::RIGHT_ALIGN) {
            tx.print_button_rc(text, value);
        } else {
            tx.print_button(text, value);
        }
        redraw_print(ctx, tx)?;
    } else if let Some(flags) = inst.as_print() {
        let s = ctx.pop_str()?;

        if flags.contains(PrintFlags::DEBUG) {
            // Emuera's debug console is a flat `StringBuilder`: `DebugPrint`
            // appends the text and `DebugNewLine` appends a line break, so the
            // `L`/`W` forms end the pending line and the bare forms do not
            // (`EmueraConsole.cs:1837-1854`). DEBUGPRINT is not a PRINTK
            // function, so `FORCEKANA` never applies to it.
            log::debug!("{s}");
            tx.debug_print(s, flags.contains(PrintFlags::NEWLINE));
            return Ok(InstructionWorkflow::Normal);
        }

        // Emuera applies `ConvertStringType` only to the PRINTK family and
        // PRINTDATAK (`Instraction.Child.cs:149-150`, `:229-230`) — never to a
        // plain PRINT, however `FORCEKANA` is set.
        let s = if flags.contains(PrintFlags::FORCE_KANA) {
            tx.force_kana().convert(s)
        } else {
            s
        };

        let prev_color = if flags.contains(PrintFlags::DEFAULT_COLOR) {
            let c = tx.color();
            tx.reset_color();
            Some(c)
        } else {
            None
        };

        if flags.contains(PrintFlags::LEFT_ALIGN) {
            tx.printlc(&s);
        } else if flags.contains(PrintFlags::RIGHT_ALIGN) {
            tx.printrc(&s);
        } else if flags.contains(PrintFlags::PLAIN) {
            tx.print_plain(s);
        } else {
            tx.print(s);
        }

        if let Some(prev_color) = prev_color {
            let erars_ui::Color([r, g, b]) = erars_ui::Color::from(prev_color);
            tx.set_color(r, g, b);
        }

        if flags.contains(PrintFlags::NEWLINE) {
            tx.new_line();
        }

        if flags.contains(PrintFlags::WAIT) {
            let gen = tx.input_gen();
            // Emuera force-paints whenever the console settles into a wait
            // (`EmueraConsole.cs:1184`), even with REDRAW off.
            ctx.input_redraw(
                tx,
                InputRequest {
                    generation: gen,
                    ty: InputRequestType::AnyKey,
                    is_one: false,
                    timeout: None,
                },
            )?;
        } else {
            redraw_print(ctx, tx)?;
        }
    } else if let Some(c) = inst.as_try_call().or_else(|| inst.as_try_jump()) {
        let args = ctx.take_list(c).collect::<Vec<_>>();
        let func = ctx.pop_strkey()?;

        match vm.try_call(func, &args, tx, ctx)? {
            Some(Workflow::Return) => {
                if inst.is_try_jump() {
                    return Ok(Workflow::Return.into());
                }
                ctx.push(true);
            }
            Some(other) => return Ok(other.into()),
            None => {
                ctx.push(false);
            }
        }
    } else if let Some(c) = inst.as_jump().or_else(|| inst.as_call()) {
        let args = ctx.take_list(c).collect::<Vec<_>>();
        let func = ctx.pop_strkey()?;

        match vm.call(func, &args, tx, ctx)? {
            Workflow::Return => {
                if inst.is_jump() {
                    return Ok(Workflow::Return.into());
                }
            }
            other => return Ok(other.into()),
        }
    } else if let Some(b) = inst.as_begin() {
        return Ok(Workflow::Begin(b).into());
    } else if let Some(ty) = inst.as_call_event() {
        call_event!(vm, ty, tx, ctx);
    } else if let Some(c) = inst.as_concat_string() {
        let args = ctx.take_value_list(c)?;
        let ret = args.into_iter().fold(String::new(), |s, l| s + l.into_str().as_str());
        ctx.push(ret);
    } else if let Some(t) = inst.as_times() {
        let arg = ctx.pop_int()?;
        let ret = (arg as f32 * t.into_inner()) as i64;
        ctx.push(ret);
    } else if let Some(op) = inst.as_unaryop() {
        match op {
            UnaryOperator::Not => {
                let operand = ctx.pop_value()?.as_bool();
                ctx.push(!operand);
            }
            UnaryOperator::Minus => {
                let operand = ctx.pop_int()?;
                ctx.push(-operand);
            }
        }
    } else if let Some(op) = inst.as_binop() {
        let rhs = ctx.pop_value()?;
        let lhs = ctx.pop_value()?;

        let ret = match op {
            BinaryOperator::Add => match lhs {
                Value::Int(i) => Value::Int(i + rhs.try_into_int()?),
                Value::String(s) => Value::String(s + rhs.into_str().as_str()),
            },
            BinaryOperator::Mul => match lhs {
                Value::Int(i) => Value::Int(i * rhs.try_into_int()?),
                Value::String(s) => Value::String(s.repeat(usize::try_from(rhs.try_into_int()?)?)),
            },
            BinaryOperator::Sub => Value::Int(lhs.try_into_int()? - rhs.try_into_int()?),
            BinaryOperator::Div => Value::Int(lhs.try_into_int()? / rhs.try_into_int()?),
            BinaryOperator::Rem => Value::Int(lhs.try_into_int()? % rhs.try_into_int()?),
            BinaryOperator::Less => Value::Int((lhs.try_into_int()? < rhs.try_into_int()?).into()),
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
            BinaryOperator::Nand => Value::Int(i64::from(!(lhs.as_bool() && rhs.as_bool()))),
            BinaryOperator::Nor => Value::Int(i64::from(!(lhs.as_bool() || rhs.as_bool()))),
            BinaryOperator::Xor => Value::Int(i64::from(lhs.as_bool() ^ rhs.as_bool())),
            BinaryOperator::BitAnd => Value::Int(lhs.try_into_int()? & rhs.try_into_int()?),
            BinaryOperator::BitOr => Value::Int(lhs.try_into_int()? | rhs.try_into_int()?),
            BinaryOperator::BitXor => Value::Int(lhs.try_into_int()? ^ rhs.try_into_int()?),
            BinaryOperator::Lhs => Value::Int(lhs.try_into_int()? << rhs.try_into_int()?),
            BinaryOperator::Rhs => Value::Int(lhs.try_into_int()? >> rhs.try_into_int()?),
        };

        ctx.push(ret);
    } else if let Some(no) = inst.as_goto() {
        return Ok(InstructionWorkflow::Goto(no));
    } else if let Some(no) = inst.as_goto_if_not() {
        let cond = ctx.pop_value()?.as_bool();
        if !cond {
            return Ok(InstructionWorkflow::Goto(no));
        }
    } else if let Some(no) = inst.as_goto_if() {
        let cond = ctx.pop_value()?.as_bool();
        if cond {
            return Ok(InstructionWorkflow::Goto(no));
        }
    } else if let Some(align) = inst.as_set_aligment() {
        tx.set_align(align);
    } else if let Some(align) = inst.as_pad_str() {
        let width = ctx.pop_int()?;
        let text = match ctx.pop_value()? {
            Value::String(s) => s,
            Value::Int(i) => i.to_string(),
        };
        let text_cells = tx.cells(&text);

        ctx.push(cells::pad_str_cells(text, width, align, text_cells));
    } else if let Some(var) = inst.as_builtin_var() {
        let c = ctx.pop_int()? as u32;
        let args = ctx.take_arg_list(None, c)?;

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
            Rand => {
                // `RAND` needs its argument: Emuera's `ReduceVariable` refuses
                // the bare form with 「RANDの引数が省略されています」
                // (`GameData/Variable/VariableParser.cs:170-177`). erars checks
                // arity at run time throughout, so the refusal lands here.
                ensure!(
                    !args.is_empty(),
                    "RAND의 인수가 생략되었습니다"
                );
                // Emuera raises a script error instead of sampling an empty
                // range: `RandToken.GetIntValue` throws
                // `RANDの引数に0以下の値({i})が指定されました` whenever the
                // argument is not positive
                // (`GameData/Variable/VariableToken.cs:1459-1465`).
                let max = args[0];
                ensure!(max > 0, "RAND: 인수에 0 이하의 값({max})이 지정됐습니다");
                Value::Int(ctx.var.rng().gen_range(0..max) as i64)
            }
            DrawLineStr => {
                // `getDefStBar` — the bar Emuera baked from `DRAWLINE文字` at
                // start-up (`GameProc/Process.cs:117`). `PrintBar` prints this
                // very string, so `DRAWLINE` and `DRAWLINESTR` agree.
                let unit = &ctx.header_info.replace.drawline_str;
                Value::String(tx.bar_string(unit).unwrap_or_default())
            }
            IsTimeout => Value::Int(ctx.is_timeout as i64),
            MoneyLabel => Value::String(ctx.header_info.replace.money_unit.clone()),
        };

        ctx.push(value);
    } else if let Some(meth) = inst.as_builtin_method() {
        return run_builtin_method(meth, func_name, tx, ctx);
    } else if let Some(com) = inst.as_builtin_command() {
        return run_builtin_command(com, func_name, vm, tx, ctx);
    } else if let Some(idx) = inst.as_load_default_argument() {
        let target_func_name = match ctx
            .stack()
            .iter()
            .rev()
            .nth(idx as usize)
            .context("Invalid index for LoadDefaultArgument")?
        {
            LocalValue::InternedStr(name) => *name,
            LocalValue::Value(Value::String(name)) => ctx.var.interner().get_or_intern(name),
            _ => bail!("LoadDefaultArgument need function name"),
        };

        let body = vm.dic.get_func(target_func_name)?;

        let arg = body
            .args()
            .get(idx as usize)
            .context("LoadDefaultArgument argument is out of range")?;

        match arg.2.as_ref() {
            Some(default_value) => match default_value {
                InlineValue::Int(i) => ctx.push(*i),
                InlineValue::String(s, _) => ctx.push_strkey(*s),
            },
            None => match ctx.var.get_maybe_local_var(target_func_name, arg.0)?.0.is_str {
                true => ctx.push(String::new()),
                false => ctx.push(0i64),
            },
        }
    } else {
        if !inst.is_nop() && !inst.is_debug() {
            bail!("Unimplemented instruction: {inst:?}");
        }
    }

    Ok(InstructionWorkflow::Normal)
}

fn run_save_game(
    vm: &TerminalVm,
    tx: &mut VirtualConsole,
    ctx: &mut VmContext,
) -> Result<Workflow> {
    let mut savs = crate::save::load_local_list(&ctx.sav_dir)?;
    print_sav_data_list(&savs, tx);

    loop {
        match ctx.input_int_redraw(tx)? {
            100 => break Ok(Workflow::Return),
            i if i >= 0 && i < SAVE_COUNT as i64 => {
                let i = i as u32;
                let write = if savs.contains_key(&i) {
                    tx.print_line(format!("SAVE {i} already exists. Overwrite?"));
                    tx.print_line("[0] Yes [1] No".into());

                    loop {
                        match ctx.input_int_redraw(tx)? {
                            0 => break true,
                            1 => break false,
                            _ => continue,
                        }
                    }
                } else {
                    true
                };

                if write {
                    ctx.put_form_enabled = true;
                    try_call!(vm, "SAVEINFO", tx, ctx);
                    ctx.put_form_enabled = false;
                    let description = std::mem::take(ctx.var.ref_str("SAVEDATA_TEXT", &[])?);
                    let sav = ctx.var.get_serializable(&ctx.header_info, description);
                    crate::save::write_save_data(&ctx.sav_dir, i, &sav)?;
                    savs.insert(i, Either::Left(sav));
                }
            }
            _ => {}
        }
    }
}

fn run_load_game(tx: &mut VirtualConsole, ctx: &mut VmContext) -> Result<Option<u32>> {
    let mut savs = crate::save::load_local_list(&ctx.sav_dir)?;
    print_sav_data_list(&savs, tx);

    loop {
        match ctx.input_int_redraw(tx)? {
            100 => break Ok(None),
            i if i >= 0 && i < SAVE_COUNT as i64 => {
                if let Some(_) = savs.remove(&(i as u32)) {
                    break Ok(Some(i as u32));
                }
            }
            _ => {}
        }
    }
}

fn run_load_data(
    vm: &TerminalVm,
    tx: &mut VirtualConsole,
    ctx: &mut VmContext,
    idx: u32,
) -> Result<Workflow> {
    // Emuera runs `CheckData` before loading and refuses anything that is not
    // `EraDataState.OK` — a missing slot included — with
    // 「不正なデータをロードしようとしました」
    // (`GameProc/Process.ScriptProc.cs:814-828`). erars used to unwrap here,
    // which turned a script-level mistake into a process abort.
    let sav = crate::save::read_save_data(&ctx.sav_dir, idx)?
        .ok_or_else(|| anyhow!("부정한 데이터를 로드하려고 했습니다"))?
        .to_local_data()?;

    ctx.lastload_text = sav.description.clone();
    ctx.lastload_no = idx;
    ctx.lastload_version = sav.version;
    ctx.var.load_serializable(sav, &ctx.header_info)?;

    try_call!(vm, "SYSTEM_LOADEND", tx, ctx);
    call_event!(vm, EventType::Load, tx, ctx);

    Ok(Workflow::Begin(BeginType::Shop))
}

/// Emuera keeps the pending `CALLTRAIN` commands in `Process.coms` and drives
/// them from the train state machine; erars runs them in this loop instead, so
/// `STOPCALLTRAIN` announces itself through `VmContext` and is picked up before
/// the next command starts.
fn run_call_train(
    vm: &TerminalVm,
    tx: &mut VirtualConsole,
    ctx: &mut VmContext,
    commands: Vec<u32>,
    is_do_train: bool,
) -> Result<Workflow> {
    // A command function may start a train of its own, and `try_call!` can
    // leave this frame through any workflow, so the previous state is put back
    // whatever happens.
    let prev_running = std::mem::replace(&mut ctx.call_train_running, !is_do_train);
    let prev_stopped = std::mem::replace(&mut ctx.call_train_stopped, false);

    let ret = run_train_commands(vm, tx, ctx, commands, is_do_train);

    ctx.call_train_running = prev_running;
    ctx.call_train_stopped = prev_stopped;

    ret
}

fn run_train_commands(
    vm: &TerminalVm,
    tx: &mut VirtualConsole,
    ctx: &mut VmContext,
    commands: Vec<u32>,
    is_do_train: bool,
) -> Result<Workflow> {
    for command in commands {
        // Emuera's `ClearCommands` empties the queue, which the state machine
        // only notices once the command running at the time has finished.
        if ctx.call_train_stopped {
            break;
        }

        try_call!(vm, "SHOW_STATUS", tx, ctx);

        ctx.var.prepare_train_data()?;
        ctx.var.reset_var("NOWEX")?;
        *ctx.var.ref_int("SELECTCOM", &[])? = command as i64;

        call_event!(vm, EventType::Com, tx, ctx);
        try_call!(vm, &format!("COM{command}"), tx, ctx);

        if ctx.var.get_result() == 0 {
            continue;
        }

        try_call!(vm, "SOURCE_CHECK", tx, ctx);
        ctx.var.reset_var("SOURCE")?;
        call_event!(vm, EventType::ComEnd, tx, ctx);
    }

    if !is_do_train {
        try_call!(vm, "CALLTRAINEND", tx, ctx);
    }

    Ok(Workflow::Return)
}

const SAVE_COUNT: u32 = 20;

fn print_sav_data_list(savs: &SaveList, tx: &mut VirtualConsole) {
    for i in 0..SAVE_COUNT {
        match savs.get(&i) {
            Some(
                Either::Left(SerializableVariableStorage { description, .. })
                | Either::Right(RawSaveData { description, .. }),
            ) => {
                tx.print_line(format!("[{i:02}] - {description}"));
            }
            None => {
                tx.print_line(format!("[{i:02}] - NO DATA"));
            }
        }
    }

    tx.print_line("[100] Return".into());
}

/// Emuera `ScriptVersionText` (`GameData/GameBase.cs:31-44`): the thousands
/// digit, a dot, then either all three low digits or — when the version is a
/// multiple of ten — only the upper two of them.
fn script_version_text(version: u32, out: &mut String) {
    use std::fmt::Write;

    let _ = write!(out, "{}.", version / 1000);
    if version % 10 != 0 {
        let _ = write!(out, "{:03}", version % 1000);
    } else {
        let _ = write!(out, "{:02}", version % 1000 / 10);
    }
}

/// Emuera's built-in title screen, used when the game defines no
/// `@SYSTEM_TITLE` (`GameProc/Process.SystemProc.cs:193-211`). `[0]` resets
/// every variable, registers the master and the default character, and enters
/// `@BEGIN FIRST` (`:223-234`); `[1]` runs `@TITLE_LOADGAME` when the game
/// defines one and otherwise the standard load menu (`:235-245`), then redraws
/// the title (`endTitleLoadgame`, `:268-271`). Any other value reprints the
/// prompt (`:246-252`).
fn run_default_title(
    vm: &TerminalVm,
    tx: &mut VirtualConsole,
    ctx: &mut VmContext,
) -> Result<Workflow> {
    let mut buf = String::new();

    loop {
        tx.draw_line(ctx.header_info.replace.drawline_str.clone());
        tx.new_line();

        tx.set_align(Alignment::Center);
        tx.print_line(ctx.header_info.gamebase.title.clone());
        if ctx.header_info.gamebase.version != 0 {
            buf.clear();
            script_version_text(ctx.header_info.gamebase.version, &mut buf);
            tx.print_line(std::mem::take(&mut buf));
        }
        tx.print_line(ctx.header_info.gamebase.author.clone());
        tx.print_line(format!("({})", ctx.header_info.gamebase.year));
        tx.new_line();
        tx.print_line(ctx.header_info.gamebase.info.clone());
        tx.set_align(Alignment::Left);

        tx.draw_line(ctx.header_info.replace.drawline_str.clone());
        tx.new_line();
        tx.print_line(format!("[0] {}", ctx.header_info.replace.system_menu0));
        tx.print_line(format!("[1] {}", ctx.header_info.replace.system_menu1));

        let selected = loop {
            match ctx.input_int_redraw(tx)? {
                i @ (0 | 1) => break i,
                // `deleteLine(1)` + `PrintTemporaryLine(InvalidValue)`
                // (`_Library/EvilMask/Lang.cs:942`) and ask again.
                _ => tx.print_line("無効な値です".into()),
            }
        };

        if selected == 0 {
            ctx.var.reset_data(&ctx.header_info)?;
            add_chara_from_template(0, false, ctx)?;
            let default_chara = ctx.header_info.gamebase.default_chara;
            if default_chara > 0 {
                add_chara_from_template(default_chara as i64, false, ctx)?;
            }
            tx.draw_line(ctx.header_info.replace.drawline_str.clone());
            tx.new_line();
            return Ok(Workflow::Begin(BeginType::First));
        }

        if !try_call!(vm, "TITLE_LOADGAME", tx, ctx) {
            // `beginLoadGameOpening` (`:820-825`) asks for a slot and loads it.
            tx.print_line("何番をロードしますか？".into());
            if let Some(idx) = run_load_game(tx, ctx)? {
                return run_load_data(vm, tx, ctx, idx);
            }
        }
    }
}

pub fn run_begin(
    vm: &TerminalVm,
    ty: BeginType,
    tx: &mut VirtualConsole,
    ctx: &mut VmContext,
) -> Result<Workflow> {
    log::info!("Begin {ty}");

    match ty {
        BeginType::Title => {
            // Emuera `GotoTitle` (`EmueraConsole.cs:2309`) restores repainting,
            // the only place a sticky `REDRAW 0` is undone.
            tx.reset_redraw();
            if !try_call!(vm, "SYSTEM_TITLE", tx, ctx) {
                return run_default_title(vm, tx, ctx);
            }
        }
        BeginType::First => {
            call_event!(vm, EventType::First, tx, ctx);
        }
        BeginType::Train => {
            ctx.var.reset_train_data()?;
            call_event!(vm, EventType::Train, tx, ctx);
            let train_key = ctx.var.known_key(Var::Train);
            let mut comables = vec![
                0;
                *ctx.header_info.var_name_var[&train_key].last_key_value().unwrap().0
                    as usize
                    + 1
            ];

            loop {
                let com_no = match ctx.var.read_int(Var::NextCom, &[])? {
                    no if no >= 0 => no,
                    _ => {
                        comables.fill(ctx.header_info.replace.comable_init);
                        try_call!(vm, "SHOW_STATUS", tx, ctx);

                        let mut printc_count = 0;

                        for (no, name) in ctx.header_info.clone().var_name_var[&train_key].iter() {
                            if try_call!(vm, &format!("COM_ABLE{no}"), tx, ctx) {
                                comables[*no as usize] = ctx.var.get_result();
                            }

                            if comables[*no as usize] != 0 {
                                if ctx.config.printc_count != 0
                                    && printc_count == ctx.config.printc_count
                                {
                                    printc_count = 0;
                                    tx.new_line();
                                }
                                tx.printrc(&format!("{name}[{no:3}]"));
                                printc_count += 1;
                            }
                        }

                        tx.new_line();

                        try_call!(vm, "SHOW_USERCOM", tx, ctx);

                        ctx.var.prepare_train_data()?;

                        let no = ctx.input_int_redraw(tx)?;
                        ctx.var.set_result(no);

                        let com_exists = match no.try_into() {
                            Ok(no) => ctx
                                .header_info
                                .var_name_var
                                .get(&train_key)
                                .map(|v| v.contains_key(&no))
                                .unwrap_or(false),
                            _ => false,
                        };

                        if com_exists && comables[no as usize] != 0 {
                            no
                        } else {
                            try_call!(vm, "USERCOM", tx, ctx);
                            continue;
                        }
                    }
                };

                ctx.var.reset_var("NOWEX")?;

                *ctx.var.ref_int("SELECTCOM", &[])? = com_no;

                call_event!(vm, EventType::Com, tx, ctx);
                try_call!(vm, &format!("COM{com_no}"), tx, ctx);

                if ctx.var.get_result() == 0 {
                    continue;
                }

                try_call!(vm, "SOURCE_CHECK", tx, ctx);

                ctx.var.reset_var("SOURCE")?;

                call_event!(vm, EventType::ComEnd, tx, ctx);
            }
        }
        BeginType::AfterTrain => {
            call_event!(vm, EventType::End, tx, ctx);
        }
        BeginType::AblUp => loop {
            try_call!(vm, "SHOW_JUEL", tx, ctx);
            try_call!(vm, "SHOW_ABLUP_SELECT", tx, ctx);

            loop {
                let i = ctx.input_int_redraw(tx)?;
                ctx.var.set_result(i);

                if matches!(i, 0..=99) {
                    if try_call!(vm, &format!("ABLUP{i}"), tx, ctx) {
                        break;
                    }
                } else {
                    try_call!(vm, "USERABLUP", tx, ctx);
                    break;
                }
            }
        },
        BeginType::TurnEnd => {
            call_event!(vm, EventType::TurnEnd, tx, ctx);
        }
        BeginType::Shop => {
            call_event!(vm, EventType::Shop, tx, ctx);

            loop {
                try_call!(vm, "SHOW_SHOP", tx, ctx);

                let i = ctx.input_int_redraw(tx)?;
                ctx.var.set_result(i);

                if i >= 0 && i < ctx.header_info.replace.sell_item_count {
                    let sales = ctx.var.read_int("ITEMSALES", &[i as u32])?;

                    if sales != 0 {
                        let price = ctx
                            .header_info
                            .item_price
                            .get(&(i as u32))
                            .copied()
                            .unwrap_or_default() as i64;
                        let money = ctx.var.ref_int("MONEY", &[])?;

                        if *money >= price {
                            *money -= price;
                            *ctx.var.ref_int("ITEM", &[i as u32])? += 1;
                        }
                    }
                } else {
                    try_call!(vm, "USERSHOP", tx, ctx);
                }
            }
        }
    }

    Ok(Workflow::Return)
}

fn to_time(time: u32) -> i128 {
    (time::OffsetDateTime::now_utc() + time::Duration::milliseconds(time as i64))
        .unix_timestamp_nanos()
}

fn get_single_var(
    func_name: StrKey,
    var_ref: VariableRef,
    ctx: &mut VmContext,
) -> Result<&mut VmVariable> {
    let target = ctx.var.read_int(Var::Target, &[])?;
    let var = ctx.var.get_maybe_local_var(func_name, var_ref.name)?.1;

    Ok(match var {
        UniformVariable::Character(cvar) => {
            let c_idx = var_ref.idxs.first().copied().unwrap_or(target.try_into()?);
            &mut cvar[c_idx as usize]
        }
        UniformVariable::Normal(var) => var,
    })
}

fn range_end_opt<T>(arr: &mut [T], start: usize, end: Option<usize>) -> Result<&mut [T]> {
    match end {
        Some(end) => arr.get_mut(start..end.min(arr.len())),
        _ => arr.get_mut(start..),
    }
    .ok_or_else(|| anyhow!("Array index out of bound"))
}

/// Emuera `ReadGraphics`: a Graphics id is a non-negative `int`.
fn graphics_id(meth: BuiltinMethod, id: i64) -> Result<u32> {
    if id < 0 {
        bail!("메소드 {meth}의 Graphics ID는 음수일 수 없습니다: {id}");
    }
    if id > i32::MAX as i64 {
        bail!("메소드 {meth}의 Graphics ID가 너무 큽니다: {id}");
    }
    Ok(id as u32)
}

/// `GCREATE`'s width/height: `1..=MAX_IMAGE_SIZE`, anything else is an error.
fn graphics_extent(meth: BuiltinMethod, what: &str, v: i64) -> Result<u32> {
    if v <= 0 {
        bail!("메소드 {meth}의 {what}는 0보다 커야합니다: {v}");
    }
    if v > MAX_IMAGE_SIZE {
        bail!("메소드 {meth}의 {what}는 {MAX_IMAGE_SIZE} 이하여야합니다: {v}");
    }
    Ok(v as u32)
}

/// Emuera `ReadColor`: `0xAARRGGBB` packed into an int.
fn graphics_color(meth: BuiltinMethod, color: i64) -> Result<u32> {
    u32::try_from(color)
        .map_err(|_| anyhow!("메소드 {meth}의 색상값이 범위를 벗어났습니다: {color}"))
}

/// Emuera `ReadPoint`: every coordinate must fit in an `int`.
fn graphics_coord(meth: BuiltinMethod, v: i64) -> Result<i32> {
    i32::try_from(v).map_err(|_| anyhow!("메소드 {meth}의 좌표가 범위를 벗어났습니다: {v}"))
}

/// A `CBGSET*` depth: an `int`, and never 0 — that depth is the text's
/// (`Creator.Method.cs:6578-6580`, `GameView/EmueraConsole.cs:109-111`).
fn cbg_zdepth(meth: BuiltinMethod, v: i64) -> Result<i32> {
    match i32::try_from(v) {
        Ok(0) | Err(_) => {
            bail!("메소드 {meth}의 zdepth는 0이 아닌 int 범위의 값이여야합니다: {v}")
        }
        Ok(z) => Ok(z),
    }
}

/// Emuera `ReadRectangle` (`Creator.Method.cs:5155-5175`): four consecutive
/// `int` arguments, of which the two extents may be negative — a negative one
/// mirrors — but never zero.
fn read_rect(
    meth: BuiltinMethod,
    args: &mut std::vec::IntoIter<LocalValue>,
    ctx: &mut VmContext,
) -> Result<Rect> {
    let x = graphics_coord(meth, get_arg!(@i64: args, ctx))?;
    let y = graphics_coord(meth, get_arg!(@i64: args, ctx))?;
    let width = rect_extent(meth, "width", get_arg!(@i64: args, ctx))?;
    let height = rect_extent(meth, "height", get_arg!(@i64: args, ctx))?;
    Ok(Rect::new(x, y, width, height))
}

/// One extent of a `ReadRectangle`: an `int`, and `w64 == 0` is rejected
/// alongside the range (`Creator.Method.cs:5167`, `:5171`).
fn rect_extent(meth: BuiltinMethod, what: &str, v: i64) -> Result<i32> {
    match i32::try_from(v) {
        Ok(0) | Err(_) => {
            bail!("메소드 {meth}의 {what}는 0이 아닌 int 범위의 값이여야합니다: {v}")
        }
        Ok(e) => Ok(e),
    }
}

/// Emuera `ReadColormatrix`: the optional trailing argument of `GDRAWG` and
/// `GDRAWSPRITE` is a 5x5 block of an integer array, every entry scaled by
/// `1 / 256`.
fn read_color_matrix_opt(
    meth: BuiltinMethod,
    args: &mut std::vec::IntoIter<LocalValue>,
    ctx: &mut VmContext,
) -> Result<Option<ColorMatrix>> {
    let Some(arg) = args.next() else {
        return Ok(None);
    };
    let LocalValue::VarRef(var_ref) = arg else {
        bail!("메소드 {meth}의 색상 행렬 인수는 2차원 이상의 정수 배열 변수여야합니다");
    };

    // `character_len` is read first: `resolve_var_ref_raw` borrows `ctx`.
    let chara_len = ctx.var.character_len();
    let (info, var, idxs) = ctx.resolve_var_ref_raw(&var_ref)?;
    if info.is_str {
        bail!("메소드 {meth}의 색상 행렬 인수는 정수 배열이어야합니다");
    }
    let dim = info.size.len();
    if dim < 2 {
        // DELIBERATE divergence from `Creator.Method.cs:5186`, `:5212`: both
        // the `IsArray2D` and the `IsArray3D` test fail for a one-dimensional
        // array, so `cm[x]` stays null and the matrix read throws
        // `NullReferenceException` — Emuera's own internal-error box, with no
        // script line. A diagnostic is the only reachable behaviour.
        bail!("메소드 {meth}의 색상 행렬 인수는 2차원 이상의 배열이어야합니다");
    }

    // A character variable's first index is the character number and the two
    // that follow are the block's origin: `GetArrayChara((int)p.Index1)` with
    // `e1 = p.Index2` and `e2 = p.Index3` (`Creator.Method.cs:5188-5193`).
    // Only a 2D per-character array is readable — Emuera's 3D character
    // branch is `throw new NotImplCodeEE()` (`:5214-5217`), so refusing it is
    // faithful.
    let (chara, block) = if info.is_chara {
        ensure!(
            dim == 2,
            "메소드 {meth}의 색상 행렬 인수로 3차원 이상의 캐릭터 변수는 사용할 수 없습니다"
        );
        let (chara, rest) = idxs.split_first().map_or((0, &[][..]), |(c, rest)| (*c, rest));
        ensure!(chara < chara_len, "존재하지 않는 캐릭터 번호입니다({chara})");
        (Some(chara), rest)
    } else {
        (None, &idxs[..])
    };

    // Emuera reads `array[.., row + x, col + y]`; unspecified leading indices
    // default to 0.
    let mut start = vec![0u32; dim];
    start[..block.len().min(dim)].copy_from_slice(&block[..block.len().min(dim)]);
    let (rows, cols) = (info.size[dim - 2], info.size[dim - 1]);
    let (row, col) = (start[dim - 2], start[dim - 1]);
    if row + 5 > rows || col + 5 > cols {
        bail!("메소드 {meth}의 색상 행렬이 배열 범위를 벗어났습니다: {row}, {col}");
    }

    let base = start.iter().zip(info.size.iter()).fold(0usize, |acc, (idx, len)| {
        acc * *len as usize + *idx as usize
    });
    let data = match chara {
        Some(no) => var.assume_chara(no).as_int()?,
        None => var.assume_normal().as_int()?,
    };

    let mut raw = [[0i64; 5]; 5];
    for (x, row) in raw.iter_mut().enumerate() {
        for (y, cell) in row.iter_mut().enumerate() {
            *cell = *data
                .get(base + x * cols as usize + y)
                .ok_or_else(|| anyhow!("메소드 {meth}의 색상 행렬이 배열 범위를 벗어났습니다"))?;
        }
    }

    Ok(Some(ColorMatrix::from_scaled_ints(raw)))
}

/// Emuera `GetSaveDataPathGraphics`: `<sav_dir>/imgNNNN.png`.
fn graphics_file_path(ctx: &VmContext, file_no: i64) -> Option<std::path::PathBuf> {
    if file_no < 0 || file_no > i32::MAX as i64 {
        return None;
    }
    Some(ctx.sav_dir.join(format!("img{file_no:04}.png")))
}

/// `GCREATEFROMFILE`'s path resolution (`Creator.Method.cs:5928-5937`): an
/// absolute path is used as given, a relative one is taken against
/// `Program.ContentDir` — `<game>/resources` — unless `isRelative` asks for
/// the process directory instead. Missing files return `None`, which is
/// Emuera's `File.Exists` guard rather than an error.
///
/// DELIBERATE: the separator is normalised. Game scripts write Windows
/// separators (`"タイトル画像\\タイトル001.webp"`) that .NET resolves natively
/// and Unix would take for one filename component.
fn content_path(ctx: &VmContext, name: &str, is_relative: bool) -> Option<std::path::PathBuf> {
    if name.is_empty() {
        return None;
    }
    let name: std::borrow::Cow<str> = if std::path::MAIN_SEPARATOR != '\\' && name.contains('\\') {
        name.replace('\\', "/").into()
    } else {
        name.into()
    };
    let path = std::path::Path::new(name.as_ref());
    let path = if path.is_absolute() || is_relative {
        path.to_path_buf()
    } else {
        ctx.content_dir.join(path)
    };
    path.is_file().then_some(path)
}

/// Emuera `VariableEvaluator.AddCharacter_UseSp`: append a character and stamp
/// the CHARA CSV template with this number onto it.
fn add_chara_from_template(no: i64, sp: bool, ctx: &mut VmContext) -> Result<()> {
    let template = ctx
        .header_info
        .chara_template(no, sp, ctx.config.use_sp_chara)
        .ok_or_else(|| anyhow!("존재하지 않는 캐릭터 번호입니다({no})"))?;

    let idx = ctx.var.character_len();

    ctx.var.add_chara();
    ctx.var.set_character_template(idx, template)
}

/// Emuera `CharacterData.Asc/DescCharacterComparison`: compare on the key, then
/// on the pre-sort index. The index tiebreak makes both directions stable — a
/// descending sort is *not* a reversed ascending sort.
fn chara_order<T: Ord>(keys: &[T], pinned: Option<u32>, is_forward: bool) -> Vec<u32> {
    let mut order: Vec<u32> =
        (0..keys.len() as u32).filter(|c| Some(*c) != pinned).collect();

    if is_forward {
        order.sort_by(|&a, &b| {
            keys[a as usize].cmp(&keys[b as usize]).then(a.cmp(&b))
        });
    } else {
        order.sort_by(|&a, &b| {
            keys[b as usize].cmp(&keys[a as usize]).then(a.cmp(&b))
        });
    }

    // `fixMaster`: MASTER was lifted out before the sort and goes back into the
    // slot it already occupied (`CharacterList.Insert((int)MASTER, ...)`).
    if let Some(p) = pinned {
        order.insert(p as usize, p);
    }

    order
}

/// `SORTCHARA` — Emuera `VariableEvaluator.SortChara(key, elem, order, fixMaster: true)`.
///
/// Stable-sorts the character list by a per-chara key, pins `MASTER` to its
/// current slot, and remaps `TARGET` / `ASSI` so they keep pointing at their own
/// characters.
fn sort_chara(key: &VariableRef, is_forward: bool, ctx: &mut VmContext) -> Result<()> {
    let len = ctx.var.character_len();
    if len <= 1 {
        return Ok(());
    }

    let master = ctx.var.read_int(Var::Master, &[])?;
    let target = ctx.var.read_int(Var::Target, &[])?;
    let assi = ctx.var.read_int(Var::Assi, &[])?;
    let in_range = |i: i64| i >= 0 && i < len as i64;

    let pinned = in_range(master).then(|| master as u32);
    // With MASTER pinned a two-chara list has a single movable chara, and
    // Emuera returns before touching anything.
    if pinned.is_some() && len <= 2 {
        return Ok(());
    }

    let (info, var, idxs) = ctx.resolve_var_ref_raw(key)?;
    ensure!(
        info.is_chara,
        "SORTCHARA의 정렬키는 캐릭터 변수여야합니다: {}",
        key.name
    );
    // Emuera reads the element subscripts that follow the character index
    // (`GetElementInt(1..)`); erars' own index math folds them into the same
    // flat offset, and the character index it picks is overwritten per chara.
    let (_, elem) = info.calculate_single_idx(&idxs);
    let elem = elem as usize;
    let is_str = info.is_str;

    let UniformVariable::Character(charas) = var else {
        bail!("SORTCHARA의 정렬키는 캐릭터 변수여야합니다: {}", key.name);
    };
    ensure!(
        charas.len() >= len as usize,
        "SORTCHARA: 캐릭터 변수 {}의 길이가 캐릭터 수보다 짧습니다",
        key.name
    );

    let oob = || anyhow!("SORTCHARA: 정렬키 {}의 첨자 {elem}이(가) 범위를 벗어났습니다", key.name);
    let order = if is_str {
        let mut keys = Vec::with_capacity(len as usize);
        for chara in charas[..len as usize].iter_mut() {
            keys.push(chara.as_str()?.get(elem).ok_or_else(oob)?.clone());
        }
        chara_order(&keys, pinned, is_forward)
    } else {
        let mut keys = Vec::with_capacity(len as usize);
        for chara in charas[..len as usize].iter_mut() {
            keys.push(*chara.as_int()?.get(elem).ok_or_else(oob)?);
        }
        chara_order(&keys, pinned, is_forward)
    };

    // Realise `order[new] == old` with the whole-list swap primitive.
    // `slot[old]` tracks where the chara that started at `old` currently lives.
    let mut slot: Vec<u32> = (0..len).collect();
    let mut occupant: Vec<u32> = (0..len).collect();
    for new in 0..len {
        let cur = slot[order[new as usize] as usize];
        if cur != new {
            ctx.var.swap_chara(new, cur);
            occupant.swap(new as usize, cur as usize);
            slot[occupant[new as usize] as usize] = new;
            slot[occupant[cur as usize] as usize] = cur;
        }
    }

    // MASTER never moved. TARGET and ASSI follow their charas.
    if in_range(target) {
        *ctx.var.ref_int(Var::Target, &[])? = slot[target as usize] as i64;
    }
    if in_range(assi) {
        *ctx.var.ref_int(Var::Assi, &[])? = slot[assi as usize] as i64;
    }

    Ok(())
}

/// The methods Emuera refuses outright while `描画インターフェース` is `WINAPI`.
///
/// The whole `G*` surface is GDI+, so 35 method bodies in
/// `GameData/Function/Creator.Method.cs` open with
/// `if (Config.TextDrawingMode == TextDrawingMode.WINAPI) throw new
/// CodeEE(Lang.Error.GDIPlusOnly.Text)` — `GCREATE` at `:5875`, `GDRAWTEXT` at
/// `:5533`, `GFILLRECTANGLE` at `:6159`, `CBGSETG` at `:6570`, `GLOAD` at
/// `:7080`. The guard is the first statement of each, before the id argument is
/// even read, so nothing is created and nothing is drawn.
///
/// The set is exactly Emuera's, which is narrower than "everything that touches
/// a bitmap": the sprite accessors (`SPRITECREATED`, `SPRITEWIDTH`,
/// `SPRITEHEIGHT`, `SPRITEPOSX`, `SPRITEPOSY`, `SPRITESETPOS`, `SPRITEMOVE`,
/// `SPRITEDISPOSE`, `SPRITEGETCOLOR`) and the CBG list operations
/// (`CBGSETSPRITE`, `CBGCLEAR`, `CBGCLEARBUTTON`, `CBGREMOVEBMAP`,
/// `CBGREMOVERANGE`) carry no guard and keep answering — in `CBGSETCIMG`'s case
/// because the fork commented its own guard out (`:6627`).
fn gdiplus_only(meth: BuiltinMethod) -> bool {
    use BuiltinMethod::*;

    matches!(
        meth,
        GCreate
            | GCreated
            | GCreateFromFile
            | GDispose
            | GClear
            | GWidth
            | GHeight
            | GGetColor
            | GSetColor
            | GSetBrush
            | GSetPen
            | GSetFont
            | GDrawG
            | GDrawGWithMask
            | GDrawSprite
            | GDrawText
            | GFillRectangle
            | GSave
            | GLoad
            | SpriteCreate
            | SpriteAnimeCreate
            | SpriteAnimeAddFrame
            | CbgSetG
            | CbgSetBmapG
            | CbgSetButtonSprite
    )
}

fn run_builtin_method(
    meth: BuiltinMethod,
    func_name: StrKey,
    tx: &mut VirtualConsole,
    ctx: &mut VmContext,
) -> Result<InstructionWorkflow> {
    let c = ctx.pop_int()? as u32;
    let mut args = ctx.take_list(c).collect::<Vec<_>>().into_iter();

    macro_rules! check_arg_count {
        ($expect:expr) => {
            if c != $expect {
                bail!("메소드 {meth}의 매개변수는 {}개여야합니다. {c}", $expect);
            }
        };
        ($min:expr, $max:expr) => {
            if c < $min || c > $max {
                bail!("메소드 {meth}의 매개변수는 {}~{}개여야합니다.", $min, $max);
            }
        };
        (@atleast $expect:expr) => {
            if c < $expect {
                bail!("메소드 {meth}의 매개변수는 {}개 이상이여야합니다.", $expect);
            }
        };
        (@atmost $expect:expr) => {
            if c > $expect {
                bail!("메소드 {meth}의 매개변수는 {}개 이하여야합니다.", $expect);
            }
        };
    }

    macro_rules! csv_method {
        ($field:ident) => {
            check_arg_count!(1, 2);
            let no = get_arg!(@i64: args, ctx);
            let sp = get_arg!(@opt @i64: args, ctx).is_some_and(|sp| sp != 0);
            ensure!(
                !sp || ctx.config.use_sp_chara,
                "SP캐릭터 기능은 「SPキャラを使用する」 설정이 켜져야 사용할 수 있습니다"
            );

            let csv = ctx
                .header_info
                .chara_template(no, sp, ctx.config.use_sp_chara)
                .map(|csv| csv.$field.clone())
                .unwrap_or_default();

            ctx.push(csv);
        };

        (@arr $field:ident) => {
            check_arg_count!(2, 3);
            let no = get_arg!(@i64: args, ctx);
            let idx = get_arg!(@i64: args, ctx) as u32;
            let sp = get_arg!(@opt @i64: args, ctx).is_some_and(|sp| sp != 0);
            ensure!(
                !sp || ctx.config.use_sp_chara,
                "SP캐릭터 기능은 「SPキャラを使用する」 설정이 켜져야 사용할 수 있습니다"
            );

            let csv = ctx
                .header_info
                .chara_template(no, sp, ctx.config.use_sp_chara)
                .and_then(|csv| csv.$field.get(&idx).cloned())
                .unwrap_or_default();

            ctx.push(csv);
        };
    }

    // Message text from `_Library/EvilMask/Lang.cs:1158`. erars evaluates a
    // call's arguments before the method runs, so an argument's own side
    // effects have already happened here — the same is true of every arity
    // check above.
    ensure!(
        !(ctx.config.text_drawing_mode == erars_compiler::TextDrawingMode::WinApi
            && gdiplus_only(meth)),
        "{meth} 함수: 묘화 옵션이 WINAPI일 때에는 사용할 수 없습니다"
    );

    match meth {
        BuiltinMethod::GCreate => {
            check_arg_count!(3);
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let width = graphics_extent(meth, "Width", get_arg!(@i64: args, ctx))?;
            let height = graphics_extent(meth, "Height", get_arg!(@i64: args, ctx))?;
            let ret = ctx.graphics.create(id, width, height);
            ctx.push(ret);
        }
        BuiltinMethod::GCreateFromFile => {
            // `GraphicsCreateFromFileMethod` (`Creator.Method.cs:5913-5962`):
            // `(int id, str filename, {int isRelative})`. Every failure is a
            // soft 0 — already created, missing file, undecodable content, or
            // an axis over `MAX_IMAGESIZE`.
            if !(2..=3).contains(&c) {
                bail!("메소드 {meth}의 매개변수는 2개 또는 3개여야합니다. {c}");
            }
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let name = get_arg!(@String: args, ctx);
            let is_relative = get_arg!(@opt @i64: args, ctx).unwrap_or(0) != 0;
            let ret = match content_path(ctx, &name, is_relative) {
                Some(path) => ctx.graphics.load_image(id, &path),
                None => false,
            };
            ctx.push(ret);
        }
        BuiltinMethod::GDrawText => {
            // `GraphicsDrawStringMethod` (`Creator.Method.cs:5531-5565`):
            // `(int id, str text, {int x, int y})` drawn through GDI+, with
            // the extent `MeasureString` reports in `RESULT:1`/`:2`. Its
            // `WINAPI` refusal (`:5533-5535`) is one of the 35 `gdiplus_only`
            // handles.
            if c != 2 && c != 4 {
                bail!("메소드 {meth}의 매개변수는 2개 또는 4개여야합니다. {c}");
            }
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let text = get_arg!(@String: args, ctx);
            // 2 args → `GDrawString(text, 0, 0)` (`:5540-5542`); 4 args →
            // `ReadPoint(Name, exm, arguments, 2)` (`:5544-5547`).
            let (x, y) = if c == 4 {
                let x = graphics_coord(meth, get_arg!(@i64: args, ctx))?;
                let y = graphics_coord(meth, get_arg!(@i64: args, ctx))?;
                (x, y)
            } else {
                (0, 0)
            };
            let [r, g, b] = ctx.config.fore_color;
            let env = crate::graphics::TextEnv {
                family: &ctx.config.font_family,
                // `GlobalStatic.Console.StringStyle.FontStyle`
                // (`GraphicsImage.cs:127`); `erars_ui::FontStyle` carries the
                // same four bits in the same order.
                console_style: i64::from(tx.style().bits()),
                // `Config.ForeColor` is opaque.
                fore_color: 0xFF00_0000 | u32::from(r) << 16 | u32::from(g) << 8 | u32::from(b),
                // `<game>/font/*` joins the chain; `content_dir` is
                // `<game>/resources`, so its parent is the game directory.
                game_dir: ctx.content_dir.parent().unwrap_or(&ctx.content_dir),
                lang: ctx.config.lang,
            };
            match ctx.graphics.draw_text(id, &text, i64::from(x), i64::from(y), &env) {
                // `if (!g.IsCreated) return 0` happens before the measurement,
                // so `RESULT` keeps whatever it held (`:5536-5538`).
                None => ctx.push(0i64),
                Some((width, height)) => {
                    // `resultArray[1] = (Int64)size.Width` — a C# float→long
                    // cast truncates toward zero (`:5562-5564`).
                    *ctx.var.ref_int(Var::Result, &[1])? = width as i64;
                    *ctx.var.ref_int(Var::Result, &[2])? = height as i64;
                    ctx.push(1i64);
                }
            }
        }
        BuiltinMethod::SpriteAnimeCreate => {
            check_arg_count!(3);
            let name = get_arg!(@String: args, ctx);
            let width = graphics_extent(meth, "Width", get_arg!(@i64: args, ctx))?;
            let height = graphics_extent(meth, "Height", get_arg!(@i64: args, ctx))?;
            let ret = ctx.graphics.sprite_anime_create(name, width, height);
            ctx.push(ret);
        }
        BuiltinMethod::SpriteAnimeAddFrame => {
            check_arg_count!(9);
            let name = get_arg!(@String: args, ctx);
            let gid = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let rect = read_rect(meth, &mut args, ctx)?;
            let offset_x = graphics_coord(meth, get_arg!(@i64: args, ctx))?;
            let offset_y = graphics_coord(meth, get_arg!(@i64: args, ctx))?;
            let delay = get_arg!(@i64: args, ctx);
            let ret = ctx
                .graphics
                .sprite_anime_add_frame(&name, gid, rect, offset_x, offset_y, delay);
            ctx.push(ret);
        }
        BuiltinMethod::SetAnimeTimer => {
            // `SetAnimeTimerMethod` (`Creator.Method.cs:6808-6815`): the range
            // check is `int.MinValue..=short.MaxValue`, then
            // `Console.setRedrawTimer`.
            check_arg_count!(1);
            let ms = get_arg!(@i64: args, ctx);
            if ms < i32::MIN as i64 || ms > i16::MAX as i64 {
                bail!(
                    "메소드 {meth}의 1번째 인수가 범위를 벗어났습니다: {ms} ({}~{})",
                    i32::MIN,
                    i32::MAX
                );
            }
            tx.set_redraw_timer(ms as i32);
            ctx.push(1i64);
        }
        BuiltinMethod::ClientWidth => {
            check_arg_count!(0);
            // `EmueraConsole.ClientWidth` is `MainPicBox.Width`, which
            // `MainWindow.cs:530` sizes to `Config.WindowX`.
            ctx.push(ctx.config.window_width as i64);
        }
        BuiltinMethod::ClientHeight => {
            check_arg_count!(0);
            // `MainPicBox.Height`: `MainWindow.cs:530` leaves out the input
            // strip, and `ConfigDialog.cs:756` reads the configured height
            // back as `MainPicBox.Height + Config.LineHeight`.
            ctx.push(
                ctx.config.window_height.saturating_sub(ctx.config.line_height) as i64
            );
        }
        BuiltinMethod::GCreated => {
            check_arg_count!(1);
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            ctx.push(ctx.graphics.created(id));
        }
        BuiltinMethod::GDispose => {
            check_arg_count!(1);
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let ret = ctx.graphics.dispose(id);
            ctx.push(ret);
        }
        BuiltinMethod::GClear => {
            if c != 2 && c != 6 {
                bail!("메소드 {meth}의 매개변수는 2개 또는 6개여야합니다. {c}");
            }
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let color = graphics_color(meth, get_arg!(@i64: args, ctx))?;
            let ret = if c == 2 {
                ctx.graphics.clear(id, color)
            } else {
                let rect = read_rect(meth, &mut args, ctx)?;
                ctx.graphics.clear_rect(id, color, rect)
            };
            ctx.push(ret);
        }
        BuiltinMethod::GWidth => {
            check_arg_count!(1);
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            ctx.push(ctx.graphics.width(id));
        }
        BuiltinMethod::GHeight => {
            check_arg_count!(1);
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            ctx.push(ctx.graphics.height(id));
        }
        BuiltinMethod::GGetColor => {
            check_arg_count!(3);
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let x = graphics_coord(meth, get_arg!(@i64: args, ctx))?;
            let y = graphics_coord(meth, get_arg!(@i64: args, ctx))?;
            // The only graphics method that reports failure as -1.
            ctx.push(ctx.graphics.get_color(id, x as i64, y as i64).map_or(-1, i64::from));
        }
        BuiltinMethod::GSetColor => {
            check_arg_count!(4);
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let color = graphics_color(meth, get_arg!(@i64: args, ctx))?;
            let x = graphics_coord(meth, get_arg!(@i64: args, ctx))?;
            let y = graphics_coord(meth, get_arg!(@i64: args, ctx))?;
            let ret = ctx.graphics.set_color(id, color, x as i64, y as i64);
            ctx.push(ret);
        }
        BuiltinMethod::GSetBrush => {
            check_arg_count!(2);
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let color = graphics_color(meth, get_arg!(@i64: args, ctx))?;
            let ret = ctx.graphics.set_brush(id, color);
            ctx.push(ret);
        }
        BuiltinMethod::GSetPen => {
            check_arg_count!(3);
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let color = graphics_color(meth, get_arg!(@i64: args, ctx))?;
            let width = get_arg!(@i64: args, ctx);
            let ret = ctx.graphics.set_pen(id, Pen { color, width });
            ctx.push(ret);
        }
        BuiltinMethod::GSetFont => {
            check_arg_count!(3, 4);
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let name = get_arg!(@String: args, ctx);
            let size = get_arg!(@i64: args, ctx);
            let style = get_arg!(@opt @i64: args, ctx).unwrap_or(0);
            let ret = ctx.graphics.set_font(id, Font { name, size, style });
            ctx.push(ret);
        }
        BuiltinMethod::GDrawG => {
            check_arg_count!(10, 11);
            let dest = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let src = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let dest_rect = read_rect(meth, &mut args, ctx)?;
            let src_rect = read_rect(meth, &mut args, ctx)?;
            let cm = read_color_matrix_opt(meth, &mut args, ctx)?;
            let ret = ctx.graphics.draw_g(dest, src, dest_rect, src_rect, cm.as_ref());
            ctx.push(ret);
        }
        BuiltinMethod::GDrawGWithMask => {
            check_arg_count!(5);
            let dest = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let src = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let mask = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let x = graphics_coord(meth, get_arg!(@i64: args, ctx))?;
            let y = graphics_coord(meth, get_arg!(@i64: args, ctx))?;
            let ret = ctx.graphics.draw_g_with_mask(dest, src, mask, x, y);
            ctx.push(ret);
        }
        BuiltinMethod::GDrawSprite => {
            if !matches!(c, 2 | 4 | 6 | 7) {
                bail!("메소드 {meth}의 매개변수는 2, 4, 6 또는 7개여야합니다. {c}");
            }
            let dest = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let name = get_arg!(@String: args, ctx);

            // Emuera falls back to the sprite's own size for the 2/4 argument
            // forms, so the destination rect needs the sprite up front.
            let (x, y) = if c >= 4 {
                (
                    graphics_coord(meth, get_arg!(@i64: args, ctx))?,
                    graphics_coord(meth, get_arg!(@i64: args, ctx))?,
                )
            } else {
                (0, 0)
            };
            let (width, height) = if c >= 6 {
                (
                    graphics_coord(meth, get_arg!(@i64: args, ctx))?,
                    graphics_coord(meth, get_arg!(@i64: args, ctx))?,
                )
            } else {
                (
                    ctx.graphics.sprite_width(&name) as i32,
                    ctx.graphics.sprite_height(&name) as i32,
                )
            };
            let cm = read_color_matrix_opt(meth, &mut args, ctx)?;
            let ret =
                ctx.graphics
                    .draw_sprite(dest, &name, Rect::new(x, y, width, height), cm.as_ref());
            ctx.push(ret);
        }
        BuiltinMethod::GSave => {
            check_arg_count!(2);
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let file_no = get_arg!(@i64: args, ctx);
            let ret = match graphics_file_path(ctx, file_no) {
                Some(path) => ctx.graphics.save_image(id, &path),
                None => false,
            };
            ctx.push(ret);
        }
        BuiltinMethod::GLoad => {
            check_arg_count!(2);
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let file_no = get_arg!(@i64: args, ctx);
            let ret = match graphics_file_path(ctx, file_no) {
                Some(path) => ctx.graphics.load_image(id, &path),
                None => false,
            };
            ctx.push(ret);
        }
        BuiltinMethod::GFillRectangle => {
            check_arg_count!(5);
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let rect = read_rect(meth, &mut args, ctx)?;
            // No colour argument: the fill uses the bitmap's `GSETBRUSH`
            // colour, and without one Emuera builds a
            // `SolidBrush(Config.BackColor)` on the spot
            // (`Content/GraphicsImage.cs:194-202`). Its `TextDrawingMode ==
            // WINAPI` guard (`Creator.Method.cs:6159-6161`) is one of the 35
            // `gdiplus_only` handles.
            let no_brush = 0xFF00_0000 | u32::from(erars_ui::Color(ctx.config.bg_color));
            let ret = ctx.graphics.fill_rect(id, rect, no_brush);
            ctx.push(ret);
        }

        BuiltinMethod::SpriteCreate => {
            if c != 2 && c != 6 {
                bail!("메소드 {meth}의 매개변수는 2개 또는 6개여야합니다. {c}");
            }
            let name = get_arg!(@String: args, ctx);
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let rect = if c == 6 {
                Some(read_rect(meth, &mut args, ctx)?)
            } else {
                None
            };
            let ret = ctx
                .graphics
                .sprite_create(name, id, rect)
                .map_err(|()| anyhow!("메소드 {meth}: 지정한 영역이 Graphics {id}의 밖입니다"))?;
            ctx.push(ret);
        }
        BuiltinMethod::SpriteCreated => {
            check_arg_count!(1);
            let name = get_arg!(@String: args, ctx);
            ctx.push(ctx.graphics.sprite_created(&name));
        }
        BuiltinMethod::SpriteWidth => {
            check_arg_count!(1);
            let name = get_arg!(@String: args, ctx);
            ctx.push(ctx.graphics.sprite_width(&name));
        }
        BuiltinMethod::SpriteHeight => {
            check_arg_count!(1);
            let name = get_arg!(@String: args, ctx);
            ctx.push(ctx.graphics.sprite_height(&name));
        }
        BuiltinMethod::SpritePosX => {
            check_arg_count!(1);
            let name = get_arg!(@String: args, ctx);
            ctx.push(ctx.graphics.sprite_pos_x(&name));
        }
        BuiltinMethod::SpritePosY => {
            check_arg_count!(1);
            let name = get_arg!(@String: args, ctx);
            ctx.push(ctx.graphics.sprite_pos_y(&name));
        }
        BuiltinMethod::SpriteSetPos => {
            check_arg_count!(3);
            let name = get_arg!(@String: args, ctx);
            let x = graphics_coord(meth, get_arg!(@i64: args, ctx))?;
            let y = graphics_coord(meth, get_arg!(@i64: args, ctx))?;
            let ret = ctx.graphics.sprite_set_pos(&name, x, y);
            ctx.push(ret);
        }
        BuiltinMethod::SpriteMove => {
            check_arg_count!(3);
            let name = get_arg!(@String: args, ctx);
            let x = graphics_coord(meth, get_arg!(@i64: args, ctx))?;
            let y = graphics_coord(meth, get_arg!(@i64: args, ctx))?;
            let ret = ctx.graphics.sprite_move(&name, x, y);
            ctx.push(ret);
        }
        BuiltinMethod::SpriteDispose => {
            check_arg_count!(1);
            let name = get_arg!(@String: args, ctx);
            let ret = ctx.graphics.sprite_dispose(&name);
            ctx.push(ret);
        }
        BuiltinMethod::SpriteGetColor => {
            check_arg_count!(3);
            let name = get_arg!(@String: args, ctx);
            let x = graphics_coord(meth, get_arg!(@i64: args, ctx))?;
            let y = graphics_coord(meth, get_arg!(@i64: args, ctx))?;
            match ctx.graphics.sprite_get_color(&name, x, y) {
                // 他と違って失敗は0ではなく負の値 (`Creator.Method.cs:5828`).
                SpriteColor::Missing => ctx.push(-1i64),
                SpriteColor::Unsupported => {
                    bail!("메소드 {meth}는 애니메이션 스프라이트에 사용할 수 없습니다: {name}")
                }
                // DELIBERATE: the packed `0xAARRGGBB` the wiki documents.
                // `Creator.Method.cs:5840` writes
                // `((Int64)c.A) << 24 + c.R << 16 + c.G << 8 + c.B`, and in C#
                // `+` binds tighter than `<<`, so that is
                // `((A << (24+R)) << (16+G)) << (8+B)` — for all but a handful
                // of colours the bits shift straight off the top and the
                // method answers 0. Reproducing the typo would make the only
                // pixel-reading sprite method useless. Recorded in
                // `docs/research/2026-09-03-emuera-command-gap.md` §5.
                SpriteColor::Color(argb) => ctx.push(argb),
            }
        }
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
        BuiltinMethod::MesSkip | BuiltinMethod::MouseSkip => {
            check_arg_count!(0);
            // Emuera `MesSkipMethod` (`Creator.Method.cs:2522`) reads
            // `Console.MesSkip`, the live "user is fast-forwarding" flag, not
            // `Process.SkipPrint` (that one is ISSKIP).
            ctx.push(tx.mes_skip());
        }
        BuiltinMethod::FindElement | BuiltinMethod::FindLastElement => {
            check_arg_count!(2, 5);
            let var = get_arg!(@var args);
            let value = get_arg!(@value args, ctx);
            let start = get_arg!(@opt @usize: args, ctx).unwrap_or(0);
            let end = get_arg!(@opt @usize: args, ctx);
            let exact_match = get_arg!(@opt @i64: args, ctx).map_or(false, |i| i != 0);

            let (info, var, _) = ctx.resolve_var_ref(&var)?;

            ensure!(info.size.len() == 1, "{meth} only work with 1D variable");

            let pos = if info.is_str {
                let value = value.try_into_str()?;
                let regex = regex::Regex::new(&if exact_match {
                    format!("^{value}$")
                } else {
                    value
                })
                .context("Parse FINDELEMENT argument")?;
                let var = var.as_str()?;
                let arr = range_end_opt(var, start, end)?;

                if meth == BuiltinMethod::FindElement {
                    arr.iter().position(|v| regex.is_match(v))
                } else {
                    arr.iter().rposition(|v| regex.is_match(v))
                }
            } else {
                let value = value.try_into_int()?;
                let var = var.as_int()?;
                let arr = range_end_opt(var, start, end)?;

                if meth == BuiltinMethod::FindElement {
                    arr.iter().position(|v| *v == value)
                } else {
                    arr.iter().rposition(|v| *v == value)
                }
            };

            ctx.push(pos.map_or(-1, |p| (p + start) as i64));
        }
        BuiltinMethod::FindChara | BuiltinMethod::FindLastChara => {
            check_arg_count!(1, 4);
            let forward = meth == BuiltinMethod::FindChara;
            let mut key = get_arg!(@var args);
            let value = get_arg!(@value args, ctx);

            let start = get_arg!(@opt @u32: args, ctx).unwrap_or(0);
            let end = get_arg!(@opt @u32: args, ctx).unwrap_or_else(|| ctx.var.character_len());

            key.idxs.insert(0, start);

            let mut ret = -1;

            // Emuera scans the same half-open range `[start, end)`;
            // `FINDLASTCHARA` walks it backwards.
            for i in start..end {
                let chara_idx = if forward { i } else { end - 1 - (i - start) };
                key.idxs[0] = chara_idx;

                if value == ctx.read_var_ref(&key)? {
                    ret = chara_idx as i64;
                    break;
                }
            }

            ctx.push(ret);
        }
        BuiltinMethod::FindCharaData => {
            // Emuera `FindFilesMethod(EraSaveFileType.CharVar)`
            // (`Creator.Method.cs:2443-2481`): the matching `chara_*.dat` names go
            // into `RESULTS` — as many as fit — and the *total* count is returned.
            // The pattern argument is optional and defaults to `*`.
            ensure!(c <= 1, "메소드 {meth}의 매개변수는 0~1개여야합니다.");
            let pattern = get_arg!(@opt @String: args, ctx);
            let names =
                crate::save::find_dat_files(&ctx.sav_dir, true, pattern.as_deref().unwrap_or("*"));

            let results = ctx.var.get_var(Var::ResultS)?.1.assume_normal().as_str()?;
            for (slot, name) in results.iter_mut().zip(&names) {
                slot.clone_from(name);
            }

            ctx.push(names.len() as i64);
        }
        BuiltinMethod::ArrayMSort => {
            let refs = take_var_refs(args.len() as u32, &mut args)?;
            let ret = array_msort(&refs, ctx)?;
            ctx.push(ret);
        }
        BuiltinMethod::ChkCharaData => {
            check_arg_count!(1);
            let name = get_arg!(@String: args, ctx);

            // Emuera `CheckDataByFilename`: the state code goes to the stack and
            // the human-readable reason to RESULTS.
            let (ret, rets) = match crate::save::read_chara_data(&ctx.sav_dir, &name) {
                Ok(Some(sav)) => {
                    if sav.code != ctx.header_info.gamebase.code {
                        (2, "異なるゲームのセーブデータです".into())
                    } else if sav.version < ctx.header_info.gamebase.allow_version {
                        (3, "セーブデータのバーションが異なります".into())
                    } else {
                        (0, sav.description)
                    }
                }
                Ok(None) => (1, "----".into()),
                Err(err) => {
                    log::warn!("CHKCHARADATA {name}: {err}");
                    (4, "セーブデータが壊れています".into())
                }
            };

            ctx.var.set_results(rets);
            ctx.push(ret as i64);
        }
        BuiltinMethod::SaveText => {
            check_arg_count!(2, 4);
            let text = get_arg!(@String: args, ctx);
            let idx = get_arg!(@i64: args, ctx);
            // Emuera's `force_savdir` selects its un-redirected save directory
            // and `force_UTF8` overrides `Config.SaveEncode`. erars has a
            // single `sav_dir` and writes UTF-8 unconditionally, so both are
            // accepted with nothing left to switch.
            let _force_savdir = get_arg!(@opt @i64: args, ctx);
            let _force_utf8 = get_arg!(@opt @i64: args, ctx);

            let ret = (0..=i32::MAX as i64).contains(&idx)
                && crate::save::write_text_data(&ctx.sav_dir, idx as u32, &text);

            ctx.push(ret as i64);
        }
        BuiltinMethod::LoadText => {
            check_arg_count!(1, 3);
            let idx = get_arg!(@i64: args, ctx);
            let _force_savdir = get_arg!(@opt @i64: args, ctx);
            let _force_utf8 = get_arg!(@opt @i64: args, ctx);

            let ret = if (0..=i32::MAX as i64).contains(&idx) {
                crate::save::read_text_data(&ctx.sav_dir, idx as u32)
            } else {
                String::new()
            };

            ctx.push(ret);
        }
        BuiltinMethod::ChkVarData => {
            check_arg_count!(1);
            let name = get_arg!(@String: args, ctx);

            // Emuera `CheckDataByFilename(getSaveDataPathV(name), Var)`.
            let (ret, rets) = match crate::save::read_var_data(&ctx.sav_dir, &name) {
                Ok(Some(sav)) => {
                    if sav.code != ctx.header_info.gamebase.code {
                        (2, "異なるゲームのセーブデータです".into())
                    } else if sav.version < ctx.header_info.gamebase.allow_version {
                        (3, "セーブデータのバーションが異なります".into())
                    } else {
                        (0, sav.description)
                    }
                }
                Ok(None) => (1, "----".into()),
                Err(err) => {
                    log::warn!("CHKVARDATA {name}: {err}");
                    (4, "セーブデータが壊れています".into())
                }
            };

            ctx.var.set_results(rets);
            ctx.push(ret as i64);
        }
        BuiltinMethod::AllSames => {
            let mut all_same = true;

            let init = get_arg!(@opt @value args, ctx);

            if let Some(init) = init {
                while let Some(other) = get_arg!(@opt @value args, ctx) {
                    if init != other {
                        all_same = false;
                        break;
                    }
                }
            }

            ctx.push(all_same);
        }
        BuiltinMethod::NoSames => {
            let mut no_same = true;

            let init = get_arg!(@opt @value args, ctx);

            if let Some(init) = init {
                while let Some(other) = get_arg!(@opt @value args, ctx) {
                    if init == other {
                        no_same = false;
                        break;
                    }
                }
            }

            ctx.push(no_same);
        }
        BuiltinMethod::Rand => {
            check_arg_count!(1, 2);
            let n1 = get_arg!(@i64: args, ctx);
            let n2 = get_arg!(@opt @i64: args, ctx);

            // Emuera `RandMethod` (`Creator.Method.cs:2941-2962`): one argument
            // is a maximum over `[0, max)`, two are `[min, max)`, and an empty
            // range raises one of two `CodeEE`s chosen by whether `min` is 0
            // (`_Library/EvilMask/Lang.cs:1136-1137`).
            let (min, max) = match n2 {
                Some(max) => (n1, max),
                None => (0, n1),
            };
            ensure!(
                max > min,
                "{}",
                if min == 0 {
                    format!("RAND関数: 最大値に0以下の値({max})が指定されました")
                } else {
                    format!("RAND関数: 最大値に最小値以下の値({max})が指定されました")
                }
            );

            let ret = ctx.var.rng().gen_range(min..max);
            ctx.push(ret);
        }
        BuiltinMethod::Power => {
            check_arg_count!(2);
            let x = get_arg!(@i64: args, ctx);
            let y = get_arg!(@i64: args, ctx);

            ctx.push(pow_i64(x, y)?);
        }
        BuiltinMethod::Sqrt => {
            check_arg_count!(1);
            let x = get_arg!(@i64: args, ctx);
            ctx.push((x as f32).sqrt() as i64);
        }
        BuiltinMethod::MoneyStr => {
            check_arg_count!(1, 2);

            let value = get_arg!(@i64: args, ctx);
            let arg = get_arg!(@opt @String: args, ctx);

            let number = match arg.as_deref() {
                Some(format) => format_arg(meth, value, format)?,
                None => value.to_string(),
            };

            let ret = if ctx.header_info.replace.unit_forward {
                format!("{}{number}", ctx.header_info.replace.money_unit)
            } else {
                format!("{number}{}", ctx.header_info.replace.money_unit)
            };

            ctx.push(ret);
        }
        BuiltinMethod::Cbrt => {
            check_arg_count!(1);
            let x = get_arg!(@i64: args, ctx);
            ensure!(x >= 0, "CBRT関数: 第1引数に負の値({x})が指定されました");
            // `Math.Pow(x, 1.0/3.0)` truncated, NOT `cbrt`: the two disagree
            // on perfect cubes because `pow` lands just under the integer —
            // Emuera returns 3 for `CBRT(64)` (`Creator.Method.cs:3086-3101`).
            ctx.push((x as f64).powf(1.0 / 3.0) as i64);
        }
        BuiltinMethod::Exponent => {
            check_arg_count!(1);
            let x = get_arg!(@i64: args, ctx);
            // `Math.Exp` with Emuera's three result guards
            // (`Creator.Method.cs:3148-3170`).
            let ret = (x as f64).exp();
            ensure!(!ret.is_nan(), "EXPONENT関数: 計算結果が非数値です");
            ensure!(ret.is_finite(), "EXPONENT関数: 計算結果が無限大です");
            ensure!(
                ret < i64::MAX as f64 && ret > i64::MIN as f64,
                "EXPONENT関数: 計算結果({ret})が64ビット符号付き整数の範囲外です"
            );
            ctx.push(ret as i64);
        }
        BuiltinMethod::ColorFromRgb => {
            check_arg_count!(3);
            // Emuera reads and range-checks one argument at a time, so the
            // first out-of-range component is the one reported
            // (`Creator.Method.cs:2693-2718`).
            let mut rgb = [0i64; 3];
            for idx in 0..3 {
                let v = get_arg!(@i64: args, ctx);
                ensure!(
                    (0..=255).contains(&v),
                    "COLOR_FROMRGB関数: 第{}引数({v})が0から255の範囲外です",
                    idx + 1
                );
                rgb[idx] = v;
            }
            ctx.push((rgb[0] << 16) + (rgb[1] << 8) + rgb[2]);
        }
        BuiltinMethod::ColorFromName => {
            check_arg_count!(1);
            let name = get_arg!(@String: args, ctx);
            // `Color.FromName` hands back a zero-alpha colour for a name it
            // does not know, which Emuera turns into `-1`; the one name it
            // refuses outright is `transparent`
            // (`Creator.Method.cs:2666-2691`).
            ensure!(
                !name.eq_ignore_ascii_case("transparent"),
                "無色透明(Transparent)は色として指定できません"
            );
            // DELIBERATE: the name set is CSS's, as `SETCOLORBYNAME` already
            // uses (`css_color`), not .NET's `KnownColor`. The two agree on
            // every HTML colour name; .NET's extra *system* names
            // (`Control`, `ActiveBorder`, …) name desktop theme colours a
            // terminal has no access to and resolve to `-1` here. Anything
            // that is not a bare name — `#rrggbb`, `rgb(…)` — is also `-1`,
            // matching `Color.FromName`, which only knows names.
            let named = !name.is_empty() && name.chars().all(|c| c.is_ascii_alphabetic());
            let ret = match named.then(|| name.parse::<css_color::Srgb>()) {
                Some(Ok(rgb)) => {
                    let (r, g, b) = (
                        (rgb.red * 255.0).round() as i64,
                        (rgb.green * 255.0).round() as i64,
                        (rgb.blue * 255.0).round() as i64,
                    );
                    (r << 16) + (g << 8) + b
                }
                _ => -1,
            };
            ctx.push(ret);
        }
        BuiltinMethod::PrintCLength => {
            check_arg_count!(0);
            let width = ctx
                .config
                .get_config(EraConfigKey::PrintcWidth, &ctx.header_info.replace)
                .try_into_int()?;
            ctx.push(width);
        }
        BuiltinMethod::GetLineStr => {
            check_arg_count!(1);
            let unit = get_arg!(@String: args, ctx);
            ensure!(!unit.is_empty(), "GETLINESTR関数: 第1引数が空文字列です");
            // `getStBar` (`EmueraConsole.Print.cs:632-649`) — the very string
            // `CUSTOMDRAWLINE` prints and `DRAWLINESTR` reports.
            ctx.push(tx.bar_string(&unit).unwrap_or_default());
        }
        BuiltinMethod::StrForm => {
            check_arg_count!(1);
            let form = get_arg!(@String: args, ctx);
            // The argument is a FORM string only known at run time, so it
            // takes the same detour `EvalFormString` already exists for: the
            // VM parses and compiles it and the expanded value lands on the
            // stack in this method's place
            // (`Creator.Method.cs:4840-4871`, `terminal_vm.rs:79`).
            return Ok(InstructionWorkflow::EvalFormString(form));
        }
        BuiltinMethod::GetSpChara => {
            check_arg_count!(1);
            // Emuera checks the compatibility option before it even reads the
            // argument (`Creator.Method.cs:2010-2026`).
            ensure!(
                ctx.config.use_sp_chara,
                "SPキャラ関係の機能は標準では使用できません(互換性オプション「SPキャラを使用する」をONにしてください)"
            );
            let no = get_arg!(@i64: args, ctx);
            let idx = ctx.var.get_chara_with_sp(no, true)?;
            ctx.push(idx.map(|i| i as i64).unwrap_or(-1));
        }
        BuiltinMethod::BarStr => {
            check_arg_count!(3);

            let var = get_arg!(@i64: args, ctx);
            let max = get_arg!(@i64: args, ctx);
            let length = get_arg!(@i64: args, ctx);

            ctx.push(make_bar_str(&ctx.header_info.replace, var, max, length)?);
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
            let start = get_arg!(@opt @i64: args, ctx);

            ctx.push(cells::strfind_cells(&s, &find, start, |c| tx.char_cells(c)));
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
            ctx.push(tx.cells(&s) as i64);
        }
        BuiltinMethod::StrLenSU => {
            check_arg_count!(1);
            let s = get_arg!(@String: args, ctx);
            // Emuera returns `str.Length` (`Creator.Method.cs:4200`), a UTF-16
            // code-unit count, so an astral character counts as 2.
            ctx.push(s.encode_utf16().count() as i64);
        }
        BuiltinMethod::SumArray | BuiltinMethod::MaxArray | BuiltinMethod::MinArray => {
            check_arg_count!(1, 3);

            let var_ref = get_arg!(@var args);
            let start = get_arg!(@opt @usize: args, ctx).unwrap_or(0);
            let end = get_arg!(@opt @usize: args, ctx);

            let var = get_single_var(func_name, var_ref, ctx)?.as_int()?;
            let slice = range_end_opt(var, start, end)?.iter();
            let ret = match meth {
                BuiltinMethod::SumArray => slice.sum::<i64>(),
                BuiltinMethod::MaxArray => slice.max().copied().unwrap_or(0),
                _ => slice.min().copied().unwrap_or(0),
            };
            ctx.push(ret);
        }
        BuiltinMethod::Match => {
            check_arg_count!(2, 4);

            let var_ref = get_arg!(@var args);
            let value = get_arg!(@value args, ctx);
            let start = get_arg!(@opt @usize: args, ctx).unwrap_or(0);
            let end = get_arg!(@opt @usize: args, ctx);

            let var = get_single_var(func_name, var_ref, ctx)?;

            let ret = match value {
                Value::Int(i) => range_end_opt(var.as_int()?, start, end)?
                    .iter()
                    .filter(|v| **v == i)
                    .count(),
                Value::String(s) => range_end_opt(var.as_str()?, start, end)?
                    .iter()
                    .filter(|v| **v == s)
                    .count(),
            };

            ctx.push(ret as i64);
        }
        BuiltinMethod::SumCArray | BuiltinMethod::MaxCArray | BuiltinMethod::MinCArray => {
            check_arg_count!(1, 3);

            let var_ref = get_arg!(@var args);
            let start = get_arg!(@opt @usize: args, ctx).unwrap_or(0);
            let end = get_arg!(@opt @usize: args, ctx);

            let (info, var) = ctx.var.get_maybe_local_var(func_name, var_ref.name)?;
            ensure!(
                !info.is_str && info.is_chara,
                "{meth} only work with character int variable"
            );

            let var = var.assume_chara_vec();
            let (_, idx) = info.calculate_single_idx(&var_ref.idxs);
            ensure!(
                idx < info.full_size() as u32,
                "Index out of bound {idx} over {}",
                info.full_size()
            );

            let slice = range_end_opt(var, start, end)?
                .iter_mut()
                .map(|v| v.as_int().unwrap()[idx as usize]);
            let ret = match meth {
                BuiltinMethod::SumCArray => slice.sum::<i64>(),
                BuiltinMethod::MaxCArray => slice.max().unwrap_or(0),
                _ => slice.min().unwrap_or(0),
            };

            ctx.push(ret);
        }
        BuiltinMethod::CMatch => {
            check_arg_count!(2, 4);

            let var_ref = get_arg!(@var args);
            let value = get_arg!(@value args, ctx);
            let start = get_arg!(@opt @usize: args, ctx).unwrap_or(0);
            let chara_len = ctx.var.character_len() as usize;
            // Emuera defaults the end of the span to CHARANUM and range-checks
            // both ends against it, so CMATCH over an empty character list is
            // always an error (`GameData/Function/Creator.Method.cs:3364-3367`,
            // 「{0}関数: 範囲指定がキャラクタ配列の範囲を超えています({1}～{2})」).
            let end = get_arg!(@opt @usize: args, ctx).unwrap_or(chara_len);
            ensure!(
                start < chara_len && end <= chara_len,
                "{meth} 함수: 범위 지정이 캐릭터 배열의 범위를 넘었습니다({start}~{end})"
            );

            let mut ret = 0;
            let (info, var) = ctx.var.get_maybe_local_var(func_name, var_ref.name)?;
            // The first argument is declared `ArgType.CharacterData` in Emuera
            // (`GameData/Function/Creator.Method.cs:3308`), so a plain variable
            // is rejected rather than reinterpreted.
            ensure!(info.is_chara, "{meth} 함수의 첫 번째 인수는 캐릭터 변수여야 합니다");
            let var = var.assume_chara_vec();
            let (_, idx) = info.calculate_single_idx(&var_ref.idxs);
            ensure!(
                idx < info.full_size() as u32,
                "Index out of bound {idx} over {}",
                info.full_size()
            );

            let vars = range_end_opt(var, start, Some(end))?.iter_mut();

            match value {
                Value::Int(i) => {
                    for var in vars {
                        if i == var.as_int()?[idx as usize] {
                            ret += 1;
                        }
                    }
                }
                Value::String(s) => {
                    for var in vars {
                        if s == var.as_str()?[idx as usize] {
                            ret += 1;
                        }
                    }
                }
            }

            ctx.push(ret);
        }
        BuiltinMethod::IsSkip => {
            ctx.push(tx.skipdisp());
        }
        BuiltinMethod::Convert => {
            check_arg_count!(1, 2);

            let value = get_arg!(@i64: args, ctx);
            let radix = get_arg!(@i64: args, ctx);

            let ret = match radix {
                2 => format!("{value:b}"),
                8 => format!("{value:o}"),
                10 => format!("{value}"),
                16 => format!("{value:x}"),
                _ => bail!(
                    "CONVERT only accept 2, 8, 10, 16 for second argument, but give `{radix}`"
                ),
            };

            ctx.push(ret);
        }
        // `TOSTR(value, format)` is `Int64.ToString(format)`
        // (`Creator.Method.cs:4430-4447`), the same formatter `MONEYSTR` uses.
        BuiltinMethod::ToStr => {
            check_arg_count!(1, 2);
            let value = get_arg!(@i64: args, ctx);
            let arg = get_arg!(@opt @String: args, ctx);
            let ret = match arg.as_deref() {
                Some(format) => format_arg(meth, value, format)?,
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
            ctx.push(v.signum());
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
            let start = get_arg!(@opt @i64: args, ctx).unwrap_or(0);
            let length = get_arg!(@opt @i64: args, ctx);

            ctx.push(cells::substring_cells(&text, start, length, |c| {
                tx.char_cells(c)
            }));
        }

        BuiltinMethod::SubStringU => {
            check_arg_count!(1, 3);
            let text = get_arg!(@String: args, ctx);
            let start = get_arg!(@opt @usize: args, ctx).unwrap_or(0);
            let length = get_arg!(@opt @usize: args, ctx);

            let chars = text.chars().skip(start);

            let mut ret = String::new();

            match length {
                Some(length) => {
                    ret.extend(chars.take(length));
                }
                None => {
                    ret.extend(chars);
                }
            };

            ctx.push(ret);
        }

        // `CHARATU(str, pos)` — Emuera `CharAtMethod`
        // (`Creator.Method.cs:4812-4819`).
        //
        // DELIBERATE: `pos` indexes `char`s, not UTF-16 code units. C#
        // indexes `string` directly, so on an astral character it would hand
        // back a lone surrogate; erars has no way to represent that in a
        // `String`, and the whole `*U` family here already counts `char`s
        // (`SubStringU`, above). Identical for every BMP string, which is all
        // either corpus contains.
        BuiltinMethod::CharAtU => {
            check_arg_count!(2);
            let text = get_arg!(@String: args, ctx);
            let pos = get_arg!(@i64: args, ctx);

            let c = usize::try_from(pos)
                .ok()
                .and_then(|pos| text.chars().nth(pos));

            ctx.push(match c {
                Some(c) => c.to_string(),
                // `pos < 0 || pos >= str.Length` (`:4816-4817`).
                None => String::new(),
            });
        }

        // The live input surface. Emuera reads the OS on every call, so these
        // never cache: a script polling `GETKEY` in a loop must see the key
        // go up.
        BuiltinMethod::GetKey | BuiltinMethod::GetKeyTriggered => {
            check_arg_count!(1);
            let keycode = get_arg!(@i64: args, ctx);
            let ret = ctx.get_key(keycode, meth == BuiltinMethod::GetKeyTriggered)?;
            ctx.push(ret);
        }
        BuiltinMethod::MouseX | BuiltinMethod::MouseY => {
            check_arg_count!(0);
            let state = ctx.system.input_state()?;
            ctx.push(if meth == BuiltinMethod::MouseX {
                state.mouse_x
            } else {
                state.mouse_y
            });
        }
        BuiltinMethod::IsActive => {
            check_arg_count!(0);
            // `EmueraConsole.IsActive` is `{ get { return true; } }` in this
            // fork (`GameView/EmueraConsole.cs:276-277`), so the method is a
            // constant. Upstream Emuera tested real window focus; erars
            // follows the implementation of record (see §5.11).
            ctx.push(1i64);
        }

        // The console-background plane. `tx.cbg` outlives the game the way
        // Emuera's `cbgList` does — see `erars_ui::cbg`.
        BuiltinMethod::CbgClear => {
            check_arg_count!(0);
            Arc::make_mut(&mut tx.cbg).clear();
            ctx.push(1i64);
        }
        BuiltinMethod::CbgClearButton => {
            check_arg_count!(0);
            Arc::make_mut(&mut tx.cbg).clear_button();
            ctx.push(1i64);
        }
        BuiltinMethod::CbgRemoveBmap => {
            check_arg_count!(0);
            Arc::make_mut(&mut tx.cbg).clear_button_map();
            ctx.push(1i64);
        }
        BuiltinMethod::CbgRemoveRange => {
            check_arg_count!(2);
            // `unchecked { CBG_ClearRange((int)x64, (int)y64) }`
            // (`Creator.Method.cs:6508-6512`) — the only CBG argument that is
            // truncated instead of range-checked.
            let zmin = get_arg!(@i64: args, ctx) as i32;
            let zmax = get_arg!(@i64: args, ctx) as i32;
            Arc::make_mut(&mut tx.cbg).clear_range(zmin, zmax);
            ctx.push(1i64);
        }
        BuiltinMethod::CbgSetG => {
            check_arg_count!(4);
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            let (x, y, z) = (
                get_arg!(@i64: args, ctx),
                get_arg!(@i64: args, ctx),
                get_arg!(@i64: args, ctx),
            );
            // `!g.IsCreated` is tested before the coordinates are validated,
            // so a missing bitmap answers 0 rather than raising on a bad
            // coordinate (`:6572-6573`).
            let Some(sprite) = ctx.graphics.bitmap_sprite(id) else {
                ctx.push(0i64);
                return Ok(InstructionWorkflow::Normal);
            };
            let x = graphics_coord(meth, x)?;
            let y = graphics_coord(meth, y)?;
            let z = cbg_zdepth(meth, z)?;
            Arc::make_mut(&mut tx.cbg).set_image(sprite, x, y, z);
            ctx.push(1i64);
        }
        BuiltinMethod::CbgSetSprite => {
            check_arg_count!(4);
            let name = get_arg!(@String: args, ctx);
            let (x, y, z) = (
                get_arg!(@i64: args, ctx),
                get_arg!(@i64: args, ctx),
                get_arg!(@i64: args, ctx),
            );
            let Some(sprite) = ctx.graphics.sprite_geometry(&name) else {
                ctx.push(0i64);
                return Ok(InstructionWorkflow::Normal);
            };
            let x = graphics_coord(meth, x)?;
            let y = graphics_coord(meth, y)?;
            let z = cbg_zdepth(meth, z)?;
            Arc::make_mut(&mut tx.cbg).set_image(sprite, x, y, z);
            ctx.push(1i64);
        }
        BuiltinMethod::CbgSetBmapG => {
            check_arg_count!(1);
            let id = graphics_id(meth, get_arg!(@i64: args, ctx))?;
            if !ctx.graphics.created(id) {
                ctx.push(0i64);
                return Ok(InstructionWorkflow::Normal);
            }
            // The `false` of "this bitmap is already the map" is dropped:
            // `CBGSETBMAPG` returns 1 whenever the bitmap exists
            // (`:6606-6608`).
            Arc::make_mut(&mut tx.cbg).set_button_map(id);
            ctx.push(1i64);
        }
        BuiltinMethod::CbgSetButtonSprite => {
            check_arg_count!(6, 7);
            let button = get_arg!(@i64: args, ctx);
            let name = get_arg!(@String: args, ctx);
            let name_b = get_arg!(@String: args, ctx);
            let (x, y, z) = (
                get_arg!(@i64: args, ctx),
                get_arg!(@i64: args, ctx),
                get_arg!(@i64: args, ctx),
            );
            // The tooltip is read — Emuera evaluates it — and dropped: erars
            // has no tooltip surface at all, not for `<button title>` either.
            // Recorded in §5.11.
            let _tooltip = get_arg!(@opt @String: args, ctx);

            // A button value is a 24-bit colour, because that is what the
            // button map's pixel can hold (`:6687-6689`).
            if !(0..=0xFF_FFFF).contains(&button) {
                ctx.push(0i64);
                return Ok(InstructionWorkflow::Normal);
            }
            // Both sprites may be missing; the entry is registered anyway
            // (`:6692-6696` has no `IsCreated` guard).
            let sprite = ctx.graphics.sprite_geometry(&name);
            let sprite_b = ctx.graphics.sprite_geometry(&name_b);
            let x = graphics_coord(meth, x)?;
            let y = graphics_coord(meth, y)?;
            let z = cbg_zdepth(meth, z)?;
            Arc::make_mut(&mut tx.cbg)
                .set_button_image(button as u32, sprite, sprite_b, x, y, z);
            ctx.push(1i64);
        }

        BuiltinMethod::HtmlEscape => {
            check_arg_count!(1);
            let s = get_arg!(@String: args, ctx);
            ctx.push(html::escape(&s));
        }

        BuiltinMethod::HtmlToPlainText => {
            check_arg_count!(1);
            let s = get_arg!(@String: args, ctx);
            ctx.push(html::to_plain_text(&s)?);
        }

        // Emuera's own argument list is `{ Int }` with `OmitStart = 0`
        // (`Creator.Method.cs:5029-5031`), i.e. nought or one.
        BuiltinMethod::HtmlGetPrintedStr => {
            check_arg_count!(@atmost 1);
            let line_no = get_arg!(@opt @i64: args, ctx).unwrap_or(0);
            if line_no < 0 {
                bail!("HTML_GETPRINTEDSTR: 인수를 0 미만으로 할 수 없습니다: {line_no}");
            }

            let fore = tx.default_color();
            let family = ctx.config.font_family.clone();
            ctx.push(html::get_printed_str(tx, line_no, fore, &family));
        }

        BuiltinMethod::HtmlPopPrintingStr => {
            check_arg_count!(0);

            let fore = tx.default_color();
            let family = ctx.config.font_family.clone();
            ctx.push(html::pop_printing_str(tx, fore, &family));
        }

        BuiltinMethod::Unicode => {
            check_arg_count!(1);
            let code = get_arg!(@u32: args, ctx);

            ctx.push(
                char::from_u32(code)
                    .ok_or_else(|| anyhow!("u32 {code} is not valid unicode codepoint"))?
                    .to_string(),
            );
        }

        BuiltinMethod::EncodeToUni => {
            check_arg_count!(1, 2);
            let s = get_arg!(@String: args, ctx);
            let pos = get_arg!(@opt @usize: args, ctx).unwrap_or(0);

            ctx.push(s.chars().nth(pos).map(|c| c as u32).ok_or_else(|| {
                anyhow!("ENCODETOUNI start position {pos} has exceed char count of {s}")
            })?);
        }

        BuiltinMethod::ToUpper => {
            check_arg_count!(1);
            let s = get_arg!(@String: args, ctx);
            ctx.push(s.to_uppercase());
        }

        BuiltinMethod::ToLower => {
            check_arg_count!(1);
            let s = get_arg!(@String: args, ctx);
            ctx.push(s.to_lowercase());
        }

        BuiltinMethod::ToHalf => {
            check_arg_count!(1);
            let s = get_arg!(@String: args, ctx);
            ctx.push(erars_ui::kana::to_half(s));
        }

        BuiltinMethod::ToFull => {
            check_arg_count!(1);
            let s = get_arg!(@String: args, ctx);
            ctx.push(erars_ui::kana::to_full(s));
        }

        BuiltinMethod::IsNumeric => {
            check_arg_count!(1);
            let s = get_arg!(@String: args, ctx);
            ctx.push(s.parse::<i64>().is_ok());
        }

        BuiltinMethod::GetDefColor => {
            check_arg_count!(0);
            ctx.push(u32::from(tx.default_color()));
        }
        BuiltinMethod::GetDefBgColor => {
            check_arg_count!(0);
            ctx.push(u32::from(erars_ui::Color(ctx.config.bg_color)));
        }
        BuiltinMethod::GetFont => {
            check_arg_count!(0);
            ctx.push(tx.font().to_string());
        }
        BuiltinMethod::ChkFont => {
            check_arg_count!(1);
            // Emuera `ChkFontMethod` (`Creator.Method.cs:2363`) walks
            // `InstalledFontCollection.Families` for an exact name match.
            let name = get_arg!(@String: args, ctx);
            let found = ctx.system.chk_font(&name)?;
            ctx.push(found);
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
        BuiltinMethod::GetStyle => {
            ctx.push(tx.style().bits() as i64);
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
        BuiltinMethod::GetNum => {
            check_arg_count!(2);

            let key = get_arg!(@var args).name;
            let name = get_arg!(@key args, ctx);

            let ret = ctx
                .header_info
                .var_names
                .get(&key)
                .and_then(|names| names.get(&name))
                .copied()
                .map_or(-1, |n| n as i64);
            ctx.push(ret);
        }

        BuiltinMethod::StrJoin => {
            check_arg_count!(1, 4);

            let var = get_arg!(@var args);
            let delimiter = get_arg!(@opt @String: args, ctx);
            let delimiter = delimiter.as_deref().unwrap_or(",");
            let start = get_arg!(@opt @usize: args, ctx).unwrap_or(0);
            let count = get_arg!(@opt @usize: args, ctx).unwrap_or(usize::MAX);
            let (info, var, _) = ctx.resolve_var_ref(&var)?;

            if info.size.len() != 1 {
                bail!("STRJOIN only work with 1D variable");
            }

            let var = var.as_str()?;

            let end = start.saturating_add(count).min(var.len());

            let ret = var[start..end].join(delimiter);

            ctx.push(ret);
        }

        BuiltinMethod::GetTime => {
            check_arg_count!(0);
            let now = time::OffsetDateTime::now_local()?;

            ctx.push(get_time(now));
        }

        BuiltinMethod::GetTimeS => {
            check_arg_count!(0);
            let now = time::OffsetDateTime::now_local()?;

            ctx.push(get_times(now));
        }

        BuiltinMethod::GetSecond => {
            check_arg_count!(0);
            let diff = time::OffsetDateTime::now_utc() - BASE_TIME;
            ctx.push(diff.whole_seconds());
        }

        BuiltinMethod::GetMillisecond => {
            check_arg_count!(0);
            let diff = time::OffsetDateTime::now_utc() - BASE_TIME;
            ctx.push(diff.whole_milliseconds() as i64);
        }

        BuiltinMethod::CurrentAlign => {
            check_arg_count!(0);
            // Emuera `CurrentAlignMethod` (`Creator.Method.cs:2633-2650`) has
            // `ReturnType = typeof(string)` and yields "LEFT"/"CENTER"/"RIGHT", so at
            // line-head position it writes RESULTS, not RESULT.
            ctx.push(match tx.align() {
                Alignment::Left => "LEFT",
                Alignment::Center => "CENTER",
                Alignment::Right => "RIGHT",
            });
        }

        BuiltinMethod::CurrentRedraw => {
            check_arg_count!(0);
            // Emuera `CurrentRedrawMethod` (`Creator.Method.cs:2660`):
            // `Console.Redraw != ConsoleRedraw.None`.
            ctx.push(tx.redraw_enabled());
        }

        BuiltinMethod::SaveNos => {
            let nos = ctx.config.save_nos;
            ctx.push(nos as i64);
        }

        BuiltinMethod::GetConfig => {
            check_arg_count!(1);
            let key = get_arg!(@String: args, ctx);
            // `Creator.Method.cs:4988-5006` — empty string, unknown name and
            // type mismatch are three distinct errors.
            ensure!(!key.is_empty(), "GETCONFIG関数: 第1引数が空文字列です");
            let key = key.parse::<EraConfigKey>().map_err(|_| {
                anyhow!("GETCONFIG関数: 文字列\"{key}\"は適切なコンフィグ名ではありません")
            })?;
            let value = ctx.config.get_config(key, &ctx.header_info.replace);
            let value = value.try_into_int().map_err(|_| {
                anyhow!("GETCONFIG関数: 型が違います (GETCONFIGS関数を使用してください)")
            })?;
            ctx.push(value);
        }

        BuiltinMethod::GetConfigS => {
            check_arg_count!(1);
            let key = get_arg!(@String: args, ctx);
            ensure!(!key.is_empty(), "GETCONFIGS関数: 第1引数が空文字列です");
            let key = key.parse::<EraConfigKey>().map_err(|_| {
                anyhow!("GETCONFIGS関数: 文字列\"{key}\"は適切なコンフィグ名ではありません")
            })?;
            let value = ctx.config.get_config(key, &ctx.header_info.replace);
            let value = value.try_into_str().map_err(|_| {
                anyhow!("GETCONFIGS関数: 型が違います (GETCONFIG関数を使用してください)")
            })?;
            ctx.push(value);
        }

        BuiltinMethod::PrintCPerLine => {
            check_arg_count!(0);
            let count = ctx
                .config
                .get_config(EraConfigKey::PrintcCount, &ctx.header_info.replace)
                .try_into_int()?;
            ctx.push(count);
        }

        BuiltinMethod::VarSize => {
            check_arg_count!(1, 2);
            let var = get_arg!(@String: args, ctx).to_uppercase();
            let dim = get_arg!(@opt @usize: args, ctx).unwrap_or(0);

            let var_ref = ctx.make_var_ref(func_name, &var, ArrayVec::new())?;
            let info = ctx.resolve_var_ref_raw(&var_ref)?.0;

            let ret = if let Some(ret) = info.size.get(dim) {
                *ret
            } else if info.size.is_empty() && dim == 0 {
                // 0D var has size 1
                1
            } else {
                bail!("VARSIZE exceed dimension of variable {name} dim is {dim} but variable's dimension is {var_dim}", name = var, var_dim = info.size.len());
            };

            ctx.push(ret);
        }

        BuiltinMethod::ExistCsv => {
            check_arg_count!(1, 2);
            let no = get_arg!(@i64: args, ctx);
            // Emuera `EXISTCSV` takes an optional SP flag and rejects it when
            // the SP-character option is off.
            let sp = get_arg!(@opt @i64: args, ctx).is_some_and(|sp| sp != 0);
            ensure!(
                !sp || ctx.config.use_sp_chara,
                "SP캐릭터 기능은 「SPキャラを使用する」 설정이 켜져야 사용할 수 있습니다"
            );
            ctx.push(
                ctx.header_info
                    .chara_template(no, sp, ctx.config.use_sp_chara)
                    .is_some(),
            );
        }

        BuiltinMethod::ChkData => {
            check_arg_count!(1);
            let idx = get_arg!(@u32: args, ctx);

            let (ret, rets) = match crate::save::read_save_data(&ctx.sav_dir, idx)? {
                Some(sav) => {
                    if sav.code != ctx.header_info.gamebase.code {
                        (2, None)
                    } else if sav.version < ctx.header_info.gamebase.allow_version {
                        (3, None)
                    } else {
                        (0, Some(sav.description))
                    }
                }
                None => (1, None),
            };

            ctx.var
                .set_results(rets.unwrap_or_else(|| "セーブデータのバーションが異なります".into()));
            ctx.push(ret as i64);
        }
    }

    Ok(InstructionWorkflow::Normal)
}

/// HTML_TAGSPLIT's tokenizer — Emuera `HtmlManager.HtmlTagSplit`
/// (`HtmlManager.cs:397-425`). Text runs and `<...>` tags alternate; an empty
/// text run is never emitted, and a `<` with no later `>` fails the whole
/// split (`None`).
fn html_tag_split(s: &str) -> Option<Vec<&str>> {
    let mut out = Vec::new();
    let mut rest = s;

    while !rest.is_empty() {
        match rest.find('<') {
            None => {
                out.push(rest);
                break;
            }
            Some(0) => {}
            Some(open) => {
                out.push(&rest[..open]);
                rest = &rest[open..];
            }
        }

        // Emuera measures from the `<` it stopped at, so a tag is everything
        // through the next `>` inclusive.
        let close = rest.find('>')?;
        out.push(&rest[..=close]);
        rest = &rest[close + 1..];
    }

    Some(out)
}

fn run_builtin_command(
    com: BuiltinCommand,
    func_name: StrKey,
    vm: &TerminalVm,
    tx: &mut VirtualConsole,
    ctx: &mut VmContext,
) -> Result<InstructionWorkflow> {
    let c = ctx.pop_int()? as u32;
    let mut args = ctx.take_list(c).collect::<Vec<_>>().into_iter();

    match com {
        BuiltinCommand::HtmlPrint => {
            // `SP_HTML_PRINT` (`ArgumentBuilder.cs:355-391`): a string plus an
            // optional int, and neither `NotEnoughArguments` nor `TooManyArg`
            // is recoverable.
            ensure!(
                (1..=2).contains(&c),
                "{com}: 인수는 1~2개여야합니다 (인수 {c}개)"
            );
            let s = get_arg!(@String: args, ctx);
            let to_print_buffer = get_arg!(@opt @i64: args, ctx).unwrap_or(0) != 0;
            let images = ImageResolver::new(&ctx.graphics, ctx.config.font_size as i32);
            crate::html::html_print(&s, to_print_buffer, tx, images)?;
        }
        BuiltinCommand::HtmlPrintIsland => {
            // Fork `HTML_PRINT_ISLAND value, {layer = 0}`: the same markup as
            // `HTML_PRINT`, kept as a free-floating overlay that outlives
            // scrolling, input and further printing, and draws in front of
            // every `PRINT`. Islands accumulate — printing to a layer stacks
            // on what is already there rather than replacing it, because
            // `Data/ERB/RPG/ダンジョンアタック/SYSTEM_DUNGEON.ERB:2630-2641`
            // covers the dungeon view with one island and then centres a
            // second island's text inside *that* cover. Only
            // `HTML_PRINT_ISLAND_CLEAR` takes an island down.
            //
            // DELIBERATE: the string `ISLAND` does not occur anywhere in the
            // fork and the wiki has no page for it, so there is no reference
            // implementation to follow; the semantics above are derived from
            // every eramegaten_p_kr call site, and the layer order (a higher
            // number draws later, hence on top) from
            // `Data/ERB/関数/汎用組み込み関数/メッセージ/MESSAGE_POPUP.ERB:22-39`,
            // which dims the screen on `L_LAYER_NO - 1` under its popup on
            // `L_LAYER_NO`. See §5 of
            // `docs/research/2026-09-03-emuera-command-gap.md`.
            ensure!(c <= 2, "{com}: 인수는 최대 2개입니다 (인수 {c}개)");
            let s = get_arg!(@opt @String: args, ctx).unwrap_or_default();
            let layer = get_arg!(@opt @i64: args, ctx).unwrap_or(0);
            // The markup builds its own lines, so an island never touches the
            // log: not its scroll position, not LINECOUNT, not CLEARLINE.
            let mut island = tx.sub_console();
            let images = ImageResolver::new(&ctx.graphics, ctx.config.font_size as i32);
            let result = crate::html::html_print(&s, false, &mut island, images);
            // Tag diagnostics come first: a broken island adds nothing, so
            // what is already on screen stays instead of gaining a
            // half-parsed overlay.
            result?;
            tx.print_island(layer, island.into_lines());
        }
        BuiltinCommand::HtmlPrintIslandClear => {
            // Fork `HTML_PRINT_ISLAND_CLEAR {layer}` — discard one layer, or
            // every island when the argument is omitted. Both forms are live
            // in the corpus: `MESSAGE_POPUP.ERB:38-39` clears exactly the two
            // layers it printed so a caller's islands survive, while
            // `SHOW_STATUS/SHOW_STATUS_WINDOW.ERB:1111` clears everything
            // before reprinting its own layers.
            ensure!(c <= 1, "{com}: 인수는 최대 1개입니다 (인수 {c}개)");
            tx.clear_islands(get_arg!(@opt @i64: args, ctx));
        }
        BuiltinCommand::MatchAll => {
            // Fork `MATCHALL var, value, {start, end}`: count the elements of
            // `var` equal to `value` into `RESULT:0` and list the matching
            // indices from `RESULT:1` on, dropping anything past the end of
            // `RESULT` without lowering the count. `end` is exclusive.
            //
            // DELIBERATE: the fork's five-argument
            // `MATCHALL var, index, value, start, end` form, which scans
            // `VAR:I:index` of a one-dimensional character variable, is
            // refused rather than guessed at — erars carries the index inside
            // the variable reference, so accepting it positionally would
            // silently read `value` as the index.
            ensure!(
                (2..=4).contains(&c),
                "{com}: 인수는 2~4개여야합니다 (인수 {c}개)"
            );
            let var_ref = get_arg!(@var args);
            let value = get_arg!(@value args, ctx);
            let start = get_arg!(@opt @usize: args, ctx).unwrap_or(0);
            let end = get_arg!(@opt @usize: args, ctx);

            let var = get_single_var(func_name, var_ref, ctx)?;
            let hits: Vec<i64> = match value {
                Value::Int(i) => range_end_opt(var.as_int()?, start, end)?
                    .iter()
                    .enumerate()
                    .filter(|(_, v)| **v == i)
                    .map(|(idx, _)| (start + idx) as i64)
                    .collect(),
                Value::String(s) => range_end_opt(var.as_str()?, start, end)?
                    .iter()
                    .enumerate()
                    .filter(|(_, v)| **v == s)
                    .map(|(idx, _)| (start + idx) as i64)
                    .collect(),
            };

            let result = ctx.var.get_var(Var::Result)?.1.assume_normal().as_int()?;
            result[0] = hits.len() as i64;
            for (slot, hit) in result.iter_mut().skip(1).zip(hits) {
                *slot = hit;
            }
        }
        BuiltinCommand::PrintAbl
        | BuiltinCommand::PrintTalent
        | BuiltinCommand::PrintMark
        | BuiltinCommand::PrintExp => {
            ensure!(c <= 1, "{com}: 인수는 최대 1개입니다");
            // Emuera's INT_EXPRESSION builder substitutes 0 for an omitted
            // argument (`ArgumentBuilder.cs:1228`) — not TARGET.
            let idx = get_arg!(@opt @u32: args, ctx).unwrap_or(0);
            let (value_var, name_var, style) = match com {
                BuiltinCommand::PrintAbl => ("ABL", "ABLNAME", DataEntryStyle::Level),
                BuiltinCommand::PrintTalent => ("TALENT", "TALENTNAME", DataEntryStyle::Bracket),
                BuiltinCommand::PrintMark => ("MARK", "MARKNAME", DataEntryStyle::Level),
                _ => ("EXP", "EXPNAME", DataEntryStyle::Value),
            };
            ctx.var.print_chara_data(tx, idx, value_var, name_var, style)?;
        }
        BuiltinCommand::PrintPalam => {
            ensure!(c <= 1, "PRINT_PALAM: 인수는 최대 1개입니다");
            let idx = get_arg!(@opt @u32: args, ctx).unwrap_or(0);
            let printc_count = ctx.config.printc_count as usize;
            ctx.var.print_palam(tx, idx, printc_count)?;
        }
        BuiltinCommand::PrintItem => {
            ensure!(c == 0, "PRINT_ITEM: 인수를 받지 않습니다");
            ctx.var.print_item(tx)?;
        }
        BuiltinCommand::PrintShopItem => {
            ensure!(c == 0, "PRINT_SHOPITEM: 인수를 받지 않습니다");
            let printc_count = ctx.config.printc_count as usize;
            let replace = &ctx.header_info.replace;
            let (money_unit, unit_forward) = (replace.money_unit.clone(), replace.unit_forward);
            ctx.var
                .print_shop_item(tx, printc_count, &money_unit, unit_forward)?;
        }
        BuiltinCommand::HtmlTagSplit => {
            ensure!(
                (1..=3).contains(&c),
                "HTML_TAGSPLIT: 인수는 1~3개여야 합니다"
            );
            let html = get_arg!(@String: args, ctx);
            // Emuera defaults the destinations to RESULTS and RESULT:0
            // (`ArgumentBuilder.cs:1897-1911`).
            let dest = match args.next() {
                Some(LocalValue::VarRef(r)) => r,
                Some(_) => bail!("HTML_TAGSPLIT: 두 번째 인수가 변수가 아닙니다"),
                None => ctx.make_var_ref(func_name, Var::ResultS, ArgVec::new())?,
            };
            let count = match args.next() {
                Some(LocalValue::VarRef(r)) => r,
                Some(_) => bail!("HTML_TAGSPLIT: 세 번째 인수가 변수가 아닙니다"),
                None => ctx.make_var_ref(func_name, Var::Result, ArgVec::new())?,
            };

            match html_tag_split(&html) {
                // A `<` with no closing `>`: only the count is written, and it
                // reports -1 (`Instraction.Child.cs:288-292`).
                None => ctx.set_var_ref(&count, Value::Int(-1))?,
                Some(tokens) => {
                    // Emuera writes the count before the copy, so it reports
                    // every token even when the destination cannot hold them.
                    ctx.set_var_ref(&count, Value::Int(tokens.len() as i64))?;

                    let (info, var, _) = ctx.resolve_var_ref_raw(&dest)?;
                    ensure!(
                        info.is_str && !info.is_chara && info.size.len() == 1,
                        "HTML_TAGSPLIT의 두 번째 인수는 캐릭터 변수가 아닌 1차원 문자열 배열이어야 합니다: {}",
                        dest.name
                    );
                    let dest = var.assume_normal().as_str()?;
                    // `Array.Copy(strs, output, min(lengths))` fills from index
                    // 0 and leaves any surplus element as it was.
                    for (slot, token) in dest.iter_mut().zip(tokens) {
                        slot.clear();
                        slot.push_str(token);
                    }
                }
            }
        }
        BuiltinCommand::InputMouseKey => {
            ensure!(c <= 1, "INPUTMOUSEKEY: 인수는 최대 1개입니다");
            // Emuera arms the limit only for a positive time
            // (`Instraction.Child.cs:1933`).
            let time = get_arg!(@opt @i64: args, ctx).unwrap_or(0);
            let req = InputRequest {
                generation: tx.input_gen(),
                ty: InputRequestType::MouseKey,
                is_one: false,
                timeout: (time > 0).then(|| Timeout {
                    timeout: to_time(time as u32),
                    default_value: Value::Int(0),
                    show_timer: false,
                    timeout_msg: None,
                }),
            };

            // A wait that needs a value ends message skip, and
            // `PrimitiveMouseKey` is one (`InputRequest.NeedValue`,
            // `EmueraConsole.cs:1160`).
            tx.set_mes_skip(false);
            let ev = ctx.input_mouse_key(tx, req)?;
            for (no, value) in [ev.kind, ev.code, ev.x, ev.y, ev.mask, ev.button]
                .into_iter()
                .enumerate()
            {
                *ctx.var.ref_int(Var::Result, &[no as u32])? = value;
            }
            if let Some(s) = ev.button_str {
                *ctx.var.ref_str(Var::ResultS, &[0])? = s;
            }
        }
        BuiltinCommand::UpCheck => {
            let palam = ctx.var.known_key(Var::Palam);
            let names = ctx.header_info.var_name_var.get(&palam).unwrap();
            let target = ctx.var.read_int(Var::Target, &[])?.try_into()?;
            ctx.var.upcheck(tx, target, names)?;
        }
        BuiltinCommand::CUpCheck => {
            let palam = ctx.var.known_key(Var::Palam);
            let target = get_arg!(@u32: args, ctx);
            let names = ctx.header_info.var_name_var.get(&palam).unwrap();
            ctx.var.cupcheck(tx, target, names)?;
        }
        BuiltinCommand::GetTime => {
            let now = time::OffsetDateTime::now_local()?;

            ctx.var.set_result(get_time(now));
            ctx.var.set_results(get_times(now));
        }
        BuiltinCommand::Restart => {
            drop(ctx.return_func()?);
            return Ok(InstructionWorkflow::Goto(0));
        }
        BuiltinCommand::Power => {
            let out = get_arg!(@var args);
            let x = get_arg!(@i64: args, ctx);
            let y = get_arg!(@i64: args, ctx);
            *ctx.ref_int_var_ref(&out)? = pow_i64(x, y)?;
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
            // Emuera `Process.ScriptProc.cs:606-638`: the variable must be a plain
            // 1-D array, the shift is *signed* and a zero shift returns before the
            // remaining arguments are even range-checked, a negative `start` or
            // `count` aborts, and a zero `count` is a no-op.
            let v = get_arg!(@var args);
            let shift = get_arg!(@i64: args, ctx);
            let empty_value = get_arg!(@value args, ctx);
            let start = get_arg!(@opt @i64: args, ctx).unwrap_or(0);
            let count = get_arg!(@opt @i64: args, ctx);

            if shift != 0 && count != Some(0) {
                ensure!(
                    start >= 0,
                    "ARRAYSHIFT関数: 第4引数に負の値({start})が指定されました"
                );
                if let Some(count) = count {
                    ensure!(
                        count >= 0,
                        "ARRAYSHIFT関数: 第5引数に負の値({count})が指定されました"
                    );
                }

                let (info, var, _) = ctx.resolve_var_ref(&v)?;
                ensure!(
                    info.size.len() == 1,
                    "ARRAYSHIFTは1次元配列および配列型キャラクタ変数のみに対応しています"
                );

                let start = start as usize;
                let count = count.map(|c| c as usize);

                if info.is_str {
                    let var = var.as_str()?;
                    let empty_value = empty_value.try_into()?;
                    array_shift(var, empty_value, shift, start, count)?;
                } else {
                    let var = var.as_int()?;
                    let empty_value = empty_value.try_into()?;
                    array_shift(var, empty_value, shift, start, count)?;
                }
            }
        }
        BuiltinCommand::ArrayRemove => {
            let v = get_arg!(@var args);
            let start = get_arg!(@usize: args, ctx);
            let count = get_arg!(@i64: args, ctx).try_into().unwrap_or(usize::MAX);

            let (info, var, _) = ctx.resolve_var_ref(&v)?;

            if info.is_str {
                let var = var.as_str()?;
                array_remove(var, start, count)?;
            } else {
                let var = var.as_int()?;
                array_remove(var, start, count)?;
            }
        }
        BuiltinCommand::ArrayCopy => {
            // Emuera `SP_COPY_ARRAY` (`ArgumentBuilder.cs:2236-2301`) takes two
            // *strings* holding variable names, resolved through the identifier
            // dictionary at run time (`Process.ScriptProc.cs:681-718`), so the script
            // form is `ARRAYCOPY "SRC", "DST"`. Character variables and a dimension
            // or element-type mismatch are all refused there.
            let original = get_arg!(@String: args, ctx);
            let target = get_arg!(@String: args, ctx);

            let (original_key, target_key) = {
                let interner = ctx.var.interner();
                (interner.get_or_intern(&original), interner.get_or_intern(&target))
            };

            let [(original_info, original_var), (target_info, target_var)] = ctx
                .var
                .get_maybe_local_var2(func_name, original_key, func_name, target_key)?;

            for (idx, info) in [(1, &*original_info), (2, &*target_info)] {
                ensure!(
                    !info.is_chara,
                    "命令ARRAYCOPYの第{idx}引数がキャラクタ変数です"
                );
                ensure!(
                    (1..=3).contains(&info.size.len()),
                    "命令ARRAYCOPYの第{idx}引数が配列変数ではありません"
                );
            }
            ensure!(
                original_info.size.len() == target_info.size.len(),
                "ARRAYCOPYの引数の配列の次元が一致しません"
            );
            ensure!(
                original_info.is_str == target_info.is_str,
                "ARRAYCOPYの引数の型が一致しません"
            );

            let original_size = original_info.size.clone();
            let target_size = target_info.size.clone();
            let original_var = original_var.assume_normal();
            let target_var = target_var.assume_normal();

            if original_info.is_str {
                copy_array(
                    original_var.as_str()?,
                    &original_size,
                    target_var.as_str()?,
                    &target_size,
                );
            } else {
                copy_array(
                    original_var.as_int()?,
                    &original_size,
                    target_var.as_int()?,
                    &target_size,
                );
            }
        }
        BuiltinCommand::ArraySort => {
            let v = get_arg!(@var args);
            let is_forward = get_arg!(@bool: args, ctx);
            let start = get_arg!(@opt @usize: args, ctx);
            let count = get_arg!(@opt @usize: args, ctx);

            let (info, var, _) = ctx.resolve_var_ref(&v)?;

            let start = start.unwrap_or(0);
            let end = count
                .unwrap_or(usize::MAX)
                .saturating_add(start)
                .min(info.size[0] as usize);

            ensure!(start <= end, "start must be less than or equal to end");

            if info.is_str {
                let var = var.as_str()?;
                let arr = var.get_mut(start..end).context("ARRAYSORT out of range")?;
                if is_forward {
                    arr.sort();
                } else {
                    arr.sort_by(|a, b| b.cmp(a));
                }
            } else {
                let var = var.as_int()?;
                let arr = var.get_mut(start..end).context("ARRAYSORT out of range")?;
                if is_forward {
                    arr.sort();
                } else {
                    arr.sort_by(|a, b| b.cmp(a));
                }
            }
        }
        BuiltinCommand::ArrayMove => {
            // DELIBERATE EXTENSION, NOT EMUERA. `ARRAYMOVE` appears nowhere in the
            // Emuera source (zero matches across the whole tree), in the EM+EE
            // instruction index, or in the era-wiki command tables; erars inherited
            // the name in `erars-lexer/src/inst.rs:247` and there is no reference
            // behaviour to be faithful to. It is kept — rather than made a parse
            // error — because existing erars scripts may use it, and it is defined
            // here as an in-place block move of `count` elements from `start` to
            // `move_to`, clamped to the array.
            let v = get_arg!(@var args);
            let move_to = get_arg!(@usize: args, ctx);
            let count = get_arg!(@usize: args, ctx);
            let start = get_arg!(@opt @usize: args, ctx).unwrap_or(0);

            let (info, var, _) = ctx.resolve_var_ref(&v)?;

            if info.is_str {
                array_move(var.as_str()?, move_to, count, start)?;
            } else {
                array_move(var.as_int()?, move_to, count, start)?;
            }
        }
        BuiltinCommand::ArrayMSort => {
            // Emuera has no `FunctionCode.ARRAYMSORT`: the name lives only in the
            // method table (`Creator.cs:112`), and `FunctionIdentifier.cs:433-440`
            // folds every such method into the statement dictionary behind
            // `METHOD_Instruction`, which stores the `Int64` result in `RESULT`
            // (`Instraction.Child.cs:486-497`). This arm is that statement form; the
            // expression form is `BuiltinMethod::ArrayMSort`.
            let refs = take_var_refs(c, &mut args)?;
            let ret = array_msort(&refs, ctx)?;
            ctx.var.set_result(ret);
        }
        BuiltinCommand::Throw => {
            let msg = get_arg!(@opt @String: args, ctx);

            match msg {
                Some(msg) => {
                    bail!("스크립트에서 예외발생: {msg}")
                }
                None => bail!("스크립트에서 예외발생"),
            }
        }
        BuiltinCommand::Varset => {
            let var = get_arg!(@var args);
            let value = get_arg!(@opt @value args, ctx);
            let start = get_arg!(@opt @u32: args, ctx);
            let end = get_arg!(@opt @u32: args, ctx);

            let target = ctx.var.read_int(Var::Target, &[])?;
            let (info, var, idx) = ctx.resolve_var_ref_raw(&var)?;
            let (chara_idx, idx) = info.calculate_single_idx(&idx);

            match (value, start, end) {
                (None, None, None) => {
                    var.reset(&vm.header, info);
                }
                (Some(value), start, end) => {
                    let var = match var {
                        UniformVariable::Character(cvar) => {
                            &mut cvar[chara_idx.unwrap_or(target as u32) as usize]
                        }
                        UniformVariable::Normal(var) => var,
                    };
                    let start = start.unwrap_or(0);
                    let end = end.unwrap_or_else(|| info.size.last().copied().unwrap_or(1));

                    for i in start..end {
                        var.set(idx + i, value.clone())?;
                    }
                }
                _ => unreachable!(),
            }
        }
        BuiltinCommand::CVarset => {
            let var = get_arg!(@var args);
            let index = get_arg!(@u32: args, ctx);
            let value = get_arg!(@opt @value args, ctx);
            let start = get_arg!(@opt @usize: args, ctx);
            let end = get_arg!(@opt @usize: args, ctx);

            // Emuera bounds both ends by CHARANUM and refuses either one out of
            // range, then orders them
            // (`GameProc/Function/Instraction.Child.cs:1472-1499`;
            // 「命令CVARSETの第4引数({0})がキャラクタの範囲外です」). erars used
            // to drop the fifth argument and walk to the end of the character
            // vector instead.
            let chara_len = ctx.var.character_len() as usize;
            let mut start = start.unwrap_or(0);
            let mut end = end.unwrap_or(chara_len);
            ensure!(
                start < chara_len,
                "명령 CVARSET의 4번째 인수({start})가 캐릭터 범위를 벗어났습니다"
            );
            ensure!(
                end <= chara_len,
                "명령 CVARSET의 5번째 인수({end})가 캐릭터 범위를 벗어났습니다"
            );
            if start > end {
                std::mem::swap(&mut start, &mut end);
            }

            let name = var.name;
            let (info, var) = ctx.var.get_var(name)?;

            // `SP_CVAR_SET_ArgumentBuilder` requires character data
            // (`GameProc/Function/ArgumentBuilder.cs:1763-1767`) and the
            // instruction re-checks it
            // (`Instraction.Child.cs:1500-1501`,
            // 「命令CVARSETにキャラクタ変数でない変数"{0}"が渡されました」).
            ensure!(
                info.is_chara,
                "명령 CVARSET에 캐릭터 변수가 아닌 변수 \"{name}\"가 전달되었습니다"
            );

            let value = value.unwrap_or_else(|| {
                if info.is_str {
                    Value::String(String::new())
                } else {
                    Value::Int(0)
                }
            });

            let cvar = var.assume_chara_vec();

            for var in &mut cvar[start..end] {
                var.set(index, value.clone())?;
            }
        }
        BuiltinCommand::Split => {
            let s = get_arg!(@String: args, ctx);
            let delimiter = get_arg!(@String: args, ctx);
            let mut var = get_arg!(@var args);

            for (idx, part) in s.split(delimiter.as_str()).enumerate() {
                var.idxs.push(idx as u32);

                ctx.set_var_ref(&var, part.into())?;

                var.idxs.pop();
            }
        }
        BuiltinCommand::Bar => {
            let var = get_arg!(@i64: args, ctx);
            let max = get_arg!(@i64: args, ctx);
            let length = get_arg!(@i64: args, ctx);

            tx.print(make_bar_str(&ctx.header_info.replace, var, max, length)?);
        }
        BuiltinCommand::BarL => {
            let var = get_arg!(@i64: args, ctx);
            let max = get_arg!(@i64: args, ctx);
            let length = get_arg!(@i64: args, ctx);

            tx.print(make_bar_str(&ctx.header_info.replace, var, max, length)?);
            tx.new_line();
        }
        BuiltinCommand::EncodeToUni => {
            let s = get_arg!(@String: args, ctx);
            let result = ctx.var.get_var(Var::Result)?.1.assume_normal().as_int()?;
            result[0] = s.len() as i64;

            for (idx, b) in s.as_bytes().iter().enumerate() {
                result[idx + 1] = *b as i64;
            }
        }
        BuiltinCommand::ReturnF => {
            let ret = get_arg!(@opt @value args, ctx);

            if args.next().is_some() {
                bail!("RETURNF는 한개의 값만 반환할 수 있습니다.");
            }

            drop(ctx.return_func()?);

            let ret = match ret {
                Some(ret) => ret,
                None => {
                    let func = vm.dic.get_func(func_name)?;
                    if func.is_function {
                        Value::ZERO
                    } else {
                        Value::EMPTY
                    }
                }
            };

            ctx.push(ret);

            return Ok(Workflow::Return.into());
        }
        BuiltinCommand::Return => {
            drop(ctx.return_func()?);

            let mut result_idx = 0usize;
            let mut results_idx = 0usize;

            let args: Vec<_> = args.map(|v| ctx.reduce_local_value(v)).try_collect()?;

            let ((_, result), (_, results)) = ctx.var.get_var2("RESULT", "RESULTS").unwrap();
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

            return Ok(Workflow::Return.into());
        }
        BuiltinCommand::DrawLine => {
            tx.draw_line(ctx.header_info.replace.drawline_str.clone());
        }
        BuiltinCommand::CustomDrawLine => {
            let s = get_arg!(@String: args, ctx);
            tx.draw_line(s);
        }
        BuiltinCommand::FontStyle => {
            let style: u32 = get_arg!(@opt @i64: args, ctx).unwrap_or(0).try_into()?;
            let style = FontStyle::from_bits_truncate(style);
            tx.set_style(style);
        }
        BuiltinCommand::SetFont => {
            let font =
                get_arg!(@opt @String: args, ctx).unwrap_or_else(|| ctx.config.font_family.clone());
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
                    let erars_ui::Color([r, g, b]) = erars_ui::Color::from(c as u32);
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

            if com == BuiltinCommand::SetColorByName {
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
                    let erars_ui::Color([r, g, b]) = erars_ui::Color::from(c as u32);
                    (r, g, b)
                }
            };

            tx.set_bg_color(r, g, b);
        }
        BuiltinCommand::ResetColor => {
            tx.reset_color();
        }
        BuiltinCommand::ResetBgColor => {
            let [r, g, b] = ctx.config.bg_color;
            tx.set_bg_color(r, g, b);
        }
        BuiltinCommand::Twait => {
            let time = get_arg!(@u32: args, ctx);
            let force_wait = get_arg!(@i64: args, ctx) != 0;

            let ty = if force_wait {
                InputRequestType::ForceEnterKey
            } else {
                InputRequestType::EnterKey
            };

            let gen = tx.input_gen();
            ctx.input_redraw(
                tx,
                InputRequest {
                    generation: gen,
                    ty,
                    is_one: false,
                    timeout: Some(Timeout {
                        timeout: to_time(time),
                        default_value: Value::Int(0),
                        show_timer: false,
                        timeout_msg: None,
                    }),
                },
            )?;
        }
        BuiltinCommand::Wait | BuiltinCommand::WaitAnykey | BuiltinCommand::ForceWait => {
            let gen = tx.input_gen();
            ctx.input_redraw(
                tx,
                InputRequest::normal(
                    gen,
                    if com == BuiltinCommand::Wait {
                        InputRequestType::EnterKey
                    } else if com == BuiltinCommand::ForceWait {
                        InputRequestType::ForceEnterKey
                    } else {
                        InputRequestType::AnyKey
                    },
                ),
            )?;
        }
        BuiltinCommand::SkipDisp => {
            // `Process.ScriptProc.cs:571-578`: one required int expression
            // (`FunctionIdentifier.cs:317`, `INT_EXPRESSION`), and RESULT
            // receives the new skip state rather than a constant.
            //
            // The argument has to come out of `args`: `run_builtin_command`
            // already moved every argument off the value stack with
            // `take_list(c)`, so popping here read the *caller's* frame and left
            // this command's own argument behind — every `SKIPDISP` corrupted
            // the stack, which the wiki coverage sweep caught as a
            // 「다른 함수의 스택을 침범했습니다」 on a well-formed call.
            ensure!(c == 1, "SKIPDISP은 인수가 하나여야합니다. {c}");
            let arg = get_arg!(@i64: args, ctx);
            tx.set_skipdisp(arg != 0);
            ctx.var.set_result(i64::from(arg != 0));
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
                        timeout: to_time(get_arg!(@u32: args, ctx)),
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
                        timeout: to_time(get_arg!(@u32: args, ctx)),
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
                        timeout: to_time(get_arg!(@u32: args, ctx)),
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
                        timeout: to_time(get_arg!(@u32: args, ctx)),
                        default_value: get_arg!(@Value: args, ctx),
                        show_timer: get_arg!(@opt @bool: args, ctx).unwrap_or(true),
                        timeout_msg: get_arg!(@opt @String: args, ctx),
                    }),
                },
                _ => unreachable!(),
            };

            let ty = req.ty;

            let ret = ctx.input_redraw(tx, req)?;

            match (ty, ret) {
                (InputRequestType::Int, Some(Value::Int(i))) => {
                    ctx.var.set_result(i);
                }
                (InputRequestType::Str, Some(Value::String(s))) => {
                    ctx.var.set_results(s);
                }
                (InputRequestType::Str, Some(Value::Int(i))) => {
                    ctx.var.set_results(i.to_string());
                }
                (_, _) => {
                    bail!("Invalid input returned");
                }
            }
        }
        BuiltinCommand::Quit => {
            log::info!("Run QUIT");
            return Ok(Workflow::Exit.into());
        }
        BuiltinCommand::SwapChara => {
            let a = get_arg!(@u32: args, ctx);
            let b = get_arg!(@u32: args, ctx);

            // `VariableEvaluator.SwapChara` refuses either index outside the
            // registered list with 「存在しない登録キャラクタを入れ替えようとしました」
            // (`GameData/Variable/VariableEvaluator.cs:1179-1183`) and returns
            // early when both name the same character.
            let chara_len = ctx.var.character_len();
            ensure!(
                a < chara_len && b < chara_len,
                "존재하지 않는 등록 캐릭터를 교체하려고 했습니다"
            );
            if a != b {
                ctx.var.swap_chara(a, b);
            }
        }
        BuiltinCommand::SortChara => {
            let key = get_arg!(@var args);
            let is_forward = get_arg!(@bool: args, ctx);
            sort_chara(&key, is_forward, ctx)?;
        }
        BuiltinCommand::PickupChara => {
            let list = args
                .map(|v| ctx.reduce_local_value(v).and_then(u32::try_from))
                .collect::<Result<BTreeSet<_>>>()?;
            ensure!(c >= 1, "캐릭터 번호를 하나 이상 지정해야 합니다");

            // The set is sorted, so the largest index is the only one that can
            // be out of range — and `character_len` must not count phantoms.
            if let Some(no) = list.last() {
                ensure!(
                    *no < ctx.var.character_len(),
                    "캐릭터 범위를 벗어났습니다({no})"
                );
            }

            let target = ctx.var.read_int("TARGET", &[])?;
            let master = ctx.var.read_int("MASTER", &[])?;
            let assi = ctx.var.read_int("ASSI", &[])?;

            let recalculate_idx = |chara_idx: i64| match u32::try_from(chara_idx) {
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

            ctx.var.pickup_chara(&list);
        }
        BuiltinCommand::CopyChara => {
            let from = get_arg!(@u32: args, ctx);
            let to = get_arg!(@u32: args, ctx);

            ensure!(from < ctx.var.character_len());
            ensure!(to < ctx.var.character_len());

            ctx.var.copy_chara(from, to);
        }
        // Emuera `ADDCHARA_Instruction` takes `INT_ANY` — one or more numbers,
        // one character added per number. `ADDSPCHARA` is the same instruction
        // with `isSp`.
        BuiltinCommand::AddChara | BuiltinCommand::AddSpChara => {
            let is_sp = com == BuiltinCommand::AddSpChara;
            ensure!(
                !is_sp || ctx.config.use_sp_chara,
                "SP캐릭터 기능은 「SPキャラを使用する」 설정이 켜져야 사용할 수 있습니다"
            );
            ensure!(c >= 1, "캐릭터 번호를 하나 이상 지정해야 합니다");

            while let Some(no) = get_arg!(@opt @i64: args, ctx) {
                add_chara_from_template(no, is_sp, ctx)?;
            }
        }
        BuiltinCommand::AddVoidChara => {
            // Emuera `AddPseudoCharacter`: an empty template, so every variable
            // keeps the value a fresh character is created with.
            ctx.var.add_chara();
        }
        BuiltinCommand::DelAllChara => {
            ctx.var.del_all_chara();
        }
        BuiltinCommand::AddCopyChara => {
            let idx = get_arg!(@u32: args, ctx);
            ensure!(idx < ctx.var.character_len(), "캐릭터 범위를 벗어났습니다");
            ctx.var.add_copy_chara(idx);
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
            ensure!(c == 1, "REDRAW: 인수는 정확히 1개여야 합니다");
            // Emuera `EmueraConsole.SetRedraw` (`EmueraConsole.cs:1398`): bit 0
            // enables repainting, bit 1 additionally repaints right now. The
            // flag is sticky until `BEGIN TITLE` (`GotoTitle`, `:2309`).
            if tx.set_redraw(get_arg!(@i64: args, ctx)) {
                ctx.redraw(tx)?;
            }
        }
        BuiltinCommand::ClearLine => {
            tx.clear_line(get_arg!(@usize: args, ctx));
        }
        BuiltinCommand::ForceKana => {
            // Emuera `ExpressionMediator.ForceKana` (`ExpressionMediator.cs:37-45`)
            // takes a required 0..=3 *mode*, not a flag: 0 off, 1 hiragana→katakana,
            // 2 katakana→hiragana, 3 katakana→hiragana plus half-width→full-width.
            // Anything outside the range aborts with `OoRForcekanaArg`.
            let flag = get_arg!(@i64: args, ctx);
            let mode = erars_ui::ForceKana::from_flag(flag)
                .ok_or_else(|| anyhow!("FORCEKANA의 인자는 0~3이어야 합니다: {flag}"))?;
            tx.set_force_kana(mode);
        }
        BuiltinCommand::DebugClear => {
            tx.clear_debug();
        }
        BuiltinCommand::ClearTextBox => {
            // Emuera's `CLEARTEXTBOX` is `richTextBox1.Clear()`
            // (`Process.ScriptProc.cs:747-749` -> `Forms/MainWindow.cs:1071-1074`).
            // `richTextBox1` is the one-line *input field* pinned to the bottom of
            // the window (`MainWindow.cs:532-533`, and `:364-371` reads it back for
            // keyboard macros); the console itself is drawn on `mainPicBox` from
            // `displayLineList`. So the command discards half-typed input and never
            // touches the log — which is why `OUTPUTLOG` still dumps everything
            // printed before it.
            //
            // DELIBERATE NO-OP: erars front-ends have no live edit box. Input is a
            // request/response round trip (`InputRequest`/`ConsoleResult`), so at
            // the moment this command can run there is never any partial input to
            // discard. Clearing the console history instead — as erars used to —
            // destroyed state Emuera keeps and corrupted `OUTPUTLOG`.
        }
        BuiltinCommand::PrintSpace => {
            // Emuera draws a blank rectangle whose width is `n` percent of the line
            // height (`ConsoleShapePart.cs:56-62`, `Instraction.Child.cs:375-399`).
            // DELIBERATE DEVIATION: erars-ui has no pixel surface, so the blank is
            // rendered as `n` space cells — the unit changes from percent-of-line-
            // height to character cells. Non-positive widths draw nothing, matching
            // Emuera's degenerate rectangle.
            let n = get_arg!(@i64: args, ctx);
            if n > 0 {
                tx.print(" ".repeat(n.min(SHAPE_CELL_LIMIT) as usize));
            }
        }
        BuiltinCommand::PrintRect => {
            // Emuera `PRINT_RECT` takes one *or* four numeric parameters and fills a
            // pixel rectangle with the current colour (`Instraction.Child.cs:342-373`
            // → `ConsoleShapePart.cs:64-85`): one parameter is a width as a percentage
            // of the line height, one line tall; four are x, y, w, h in pixels, valid
            // only when `x >= 0 && w > 0 && h > 0`. Any other shape becomes a
            // `ConsoleErrorShapePart` that renders the literal tag text
            // (`ConsoleShapePart.cs:88-92`, `:198-204`).
            // DELIBERATE DEVIATION: without a pixel surface the fill is approximated
            // with U+2588 FULL BLOCK cells; the width parameter is read as a cell
            // count, `x` as a leading blank run, and `y`/`h` are dropped because a
            // text line has no sub-line geometry.
            ensure!((1..=4).contains(&c), "PRINT_RECT의 인자는 1개 또는 4개여야 합니다");
            let p0 = get_arg!(@i64: args, ctx);

            if c == 1 {
                if p0 > 0 {
                    tx.print("\u{2588}".repeat(p0.min(SHAPE_CELL_LIMIT) as usize));
                }
            } else if c == 4 {
                let y = get_arg!(@i64: args, ctx);
                let w = get_arg!(@i64: args, ctx);
                let h = get_arg!(@i64: args, ctx);

                if p0 >= 0 && w > 0 && h > 0 {
                    let mut buf = String::new();
                    for _ in 0..p0.min(SHAPE_CELL_LIMIT) {
                        buf.push(' ');
                    }
                    for _ in 0..w.min(SHAPE_CELL_LIMIT) {
                        buf.push('\u{2588}');
                    }
                    tx.print(buf);
                } else {
                    tx.print(format!("<shape type='rect' param='{p0}, {y}, {w}, {h}'>"));
                }
            } else {
                // Emuera's error shape for the 2- and 3-parameter forms.
                let mut buf = format!("<shape type='rect' param='{p0}");
                while let Some(v) = get_arg!(@opt @i64: args, ctx) {
                    buf.push_str(", ");
                    let _ = write!(buf, "{v}");
                }
                buf.push_str("'>");
                tx.print(buf);
            }
        }
        BuiltinCommand::OutputLog => {
            // Shared with `@OUTPUT`; see [`crate::debug_console::output_log`].
            let name = get_arg!(@opt @String: args, ctx);
            crate::debug_console::output_log(tx, ctx, name.as_deref().unwrap_or(""))?;
        }
        BuiltinCommand::Await => {
            // Emuera `AWAIT` (`Instraction.Child.cs:2192-2216`) takes an optional wait
            // in milliseconds, rejecting negatives and anything over 10000, then does
            // `DoEvents` followed by `Thread.Sleep(time)` when the time is positive
            // (`EmueraConsole.cs:544-556`). It never reads input.
            let ms = get_arg!(@opt @i64: args, ctx);

            if let Some(ms) = ms {
                ensure!(ms >= 0, "AWAITの待機時間が負の値です:{ms}");
                ensure!(ms <= 10000, "AWAITの待機時間が10秒を超えています:{ms}");
            }

            ctx.redraw(tx)?;

            if let Some(ms) = ms.filter(|&ms| ms > 0) {
                std::thread::sleep(std::time::Duration::from_millis(ms as u64));
            }
        }
        BuiltinCommand::TooltipSetColor => {
            // Emuera `TOOLTIP_SETCOLOR` (`Instraction.Child.cs:1847-1867`) is
            // `SP_SWAP`: two packed 24-bit RGB integers, foreground then background,
            // each range-checked against 0..=0xFFFFFF.
            let fore = get_arg!(@i64: args, ctx);
            let back = get_arg!(@i64: args, ctx);

            for (idx, v) in [(1, fore), (2, back)] {
                ensure!(
                    (0..=0xFFFFFF).contains(&v),
                    "{idx}番目の引数がカラーコードとして不正な値です"
                );
            }

            tx.set_tooltip_color(Some(unpack_rgb(fore)), Some(unpack_rgb(back)));
        }
        BuiltinCommand::TooltipSetDelay => {
            // Emuera `Instraction.Child.cs:1870-1889`: 0..=i32::MAX milliseconds.
            let ms = get_arg!(@i64: args, ctx);
            ensure!(
                (0..=i32::MAX as i64).contains(&ms),
                "TOOLTIP_SETDELAYの引数が範囲外です:{ms}"
            );
            tx.set_tooltip_delay(ms as u32);
        }
        BuiltinCommand::TooltipSetDuration => {
            // Emuera `Instraction.Child.cs:1892-1913`: 0..=i32::MAX milliseconds,
            // then clamped to `short.MaxValue` before it reaches the tooltip.
            let ms = get_arg!(@i64: args, ctx);
            ensure!(
                (0..=i32::MAX as i64).contains(&ms),
                "TOOLTIP_SETDURATIONの引数が範囲外です:{ms}"
            );
            tx.set_tooltip_duration(ms.min(i16::MAX as i64) as u32);
        }
        BuiltinCommand::PrintImg => {
            // `SP_PRINT_IMG_ArgumentBuilder` (`ArgumentBuilder.cs:245-305`)
            // dispatches the trailing arguments on their *type*, not their
            // position: a string is the `srcb` alternate in slot 2 or the
            // `srcm` hit mask in slot 3, and only while no number has been
            // seen yet; numbers fill three slots. `PRINT_IMG_Instruction`
            // (`Instraction.Child.cs:330-337`) then passes them as
            // height = param[1], width = param[0], ypos = param[2], i.e. the
            // surface order is `name[, srcb][, srcm], width, height, ypos`.
            let name = get_arg!(@String: args, ctx);
            let mut src_b: Option<String> = None;
            let mut src_m: Option<String> = None;
            let mut param = ArrayVec::<[MixedNum; 3]>::new();
            // Emuera counts the *surface* arguments from 2, since `name` is 1.
            let mut arg_count = 2usize;

            while let Some(v) = get_arg!(@opt @value args, ctx) {
                match v {
                    Value::String(s) => {
                        // `:283-288`: a string after any number, or past slot
                        // 3, is `IncorrectArg`.
                        ensure!(
                            param.is_empty() && arg_count <= 3,
                            "PRINT_IMG의 {arg_count}번째 인자가 잘못되었습니다"
                        );
                        if arg_count == 2 {
                            src_b = Some(s.into());
                        } else {
                            src_m = Some(s.into());
                        }
                    }
                    Value::Int(n) => {
                        ensure!(param.len() < 3, "PRINT_IMG의 인자가 너무 많습니다");
                        // erars' argument parser has no `px` keyword, so every
                        // number arrives as a font-size percentage. Emuera's
                        // `px` form (`ArgumentBuilder.cs:298`) is an EM
                        // private-build extension with no call site in either
                        // corpus; the HTML `<img>` surface does support it.
                        // `(int)` in `Instraction.Child.cs:334` truncates.
                        param.push(MixedNum::percent(n as i32));
                    }
                }
                arg_count += 1;
            }

            // `if (strb == string.Empty) strb = null;` (`:328`).
            let src_b = src_b.filter(|s| !s.is_empty());
            let resolver = ImageResolver::new(&ctx.graphics, ctx.config.font_size as i32);
            match resolver.resolve(
                &name,
                src_b.as_deref(),
                src_m.as_deref(),
                param.first().copied(),
                param.get(1).copied(),
                param.get(2).copied(),
            ) {
                Ok(image) => tx.print_image(std::sync::Arc::new(image)),
                // A missing resource is not a script error: Emuera prints the
                // reconstructed tag as text (`ConsoleImagePart.cs:69-73`).
                Err(alt) => tx.print(alt),
            }
        }
        BuiltinCommand::DoTrain => {
            let com_no = get_arg!(@u32: args, ctx);

            conv_workflow!(run_call_train(vm, tx, ctx, vec![com_no], true)?);
        }
        BuiltinCommand::CallTrain => {
            let count = get_arg!(@usize: args, ctx);
            let selectcom = ctx.var.get_var("SELECTCOM")?.1.assume_normal().as_int()?;

            // Emuera's `SetCommnds` (`Process.cs:224-237`) raises
            // `CalltrainArgMoreThanSelectcom` here; without the check the slice
            // below panics instead.
            ensure!(
                count < selectcom.len(),
                "CALLTRAIN 명령의 인수가 SELECTCOM의 요소수를 넘었습니다. {count}"
            );

            let commands = selectcom[..count]
                .iter()
                // CallTrain works reverse order
                .rev()
                .map(|c| u32::try_from(*c).context("CallTrain command convert"))
                .collect::<Result<Vec<u32>>>()?;

            conv_workflow!(run_call_train(vm, tx, ctx, commands, false)?);
        }
        // Emuera `ADDCHARA_Instruction(flagDel: true)` also takes `INT_ANY`, and
        // deletes every listed character in one pass.
        BuiltinCommand::DelChara => {
            let chara_len = ctx.var.character_len();
            let mut list = BTreeSet::new();
            ensure!(c >= 1, "캐릭터 번호를 하나 이상 지정해야 합니다");

            while let Some(idx) = get_arg!(@opt @i64: args, ctx) {
                let idx: u32 = idx
                    .try_into()
                    .map_err(|_| anyhow!("캐릭터 범위를 벗어났습니다({idx})"))?;
                ensure!(idx < chara_len, "캐릭터 범위를 벗어났습니다({idx})");
                ensure!(list.insert(idx), "캐릭터 번호가 중복되었습니다({idx})");
            }

            ctx.var.del_chara_list(&list);
        }
        BuiltinCommand::ResetData => {
            ctx.var.reset_data(&ctx.header_info)?;
        }
        BuiltinCommand::SaveData => {
            let idx = get_arg!(@u32: args, ctx);
            let description = get_arg!(@String: args, ctx);

            log::info!("Save {idx}: {description}");

            let var = ctx.var.get_serializable(&ctx.header_info, description);
            crate::save::write_save_data(&ctx.sav_dir, idx, &var)?;
        }
        BuiltinCommand::LoadData => {
            let idx = get_arg!(@u32: args, ctx);

            conv_workflow!(run_load_data(vm, tx, ctx, idx)?);
        }
        BuiltinCommand::DelData => {
            let idx = get_arg!(@u32: args, ctx);

            crate::save::delete_save_data(&ctx.sav_dir, idx)?;
        }
        BuiltinCommand::SaveGlobal => {
            crate::save::write_global_data(
                &ctx.sav_dir,
                &ctx.var.get_global_serializable(&ctx.header_info),
            )?;
        }
        BuiltinCommand::LoadGlobal => {
            if let Some(global_sav) = crate::save::read_global_data(&ctx.sav_dir)? {
                ctx.var
                    .load_global_serializable(global_sav.to_global_data()?, &ctx.header_info)?;
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
        BuiltinCommand::SaveGame => {
            conv_workflow!(run_save_game(vm, tx, ctx)?);
        }
        BuiltinCommand::LoadGame => match run_load_game(tx, ctx)? {
            Some(idx) => {
                conv_workflow!(run_load_data(vm, tx, ctx, idx)?);
            }
            None => {}
        },
        BuiltinCommand::PutForm => {
            let arg = get_arg!(@String: args, ctx);

            anyhow::ensure!(
                ctx.put_form_enabled,
                "PUTFORM called in no @SAVEINFO function"
            );

            ctx.var.ref_str("SAVEDATA_TEXT", &[])?.push_str(&arg);
        }
        BuiltinCommand::ResetStain => {
            let chara = get_arg!(@u32: args, ctx);
            // `VariableEvaluator.SetDefaultStain` refuses an unregistered
            // character with 「定義していないキャラクタを参照しようとしました」
            // (`GameData/Variable/VariableEvaluator.cs:1664-1670`).
            ensure!(
                chara < ctx.var.character_len(),
                "정의하지 않은 캐릭터를 참조하려고 했습니다"
            );
            let stain = ctx.var.get_var("STAIN")?.1.assume_chara(chara);
            let stain_init = &ctx.header_info.replace.stain_init;
            stain.as_int()?[..stain_init.len()].copy_from_slice(&stain_init);
        }
        BuiltinCommand::SaveChara => {
            let name = get_arg!(@String: args, ctx);
            let description = get_arg!(@String: args, ctx);

            let chara_len = ctx.var.character_len();
            let mut charas = Vec::new();
            let mut seen = BTreeSet::new();

            while let Some(idx) = get_arg!(@opt @i64: args, ctx) {
                let idx: u32 = idx
                    .try_into()
                    .map_err(|_| anyhow!("캐릭터 번호가 범위를 벗어났습니다({idx})"))?;
                ensure!(idx < chara_len, "캐릭터 번호가 범위를 벗어났습니다({idx})");
                // Emuera rejects a repeated index rather than saving it twice.
                ensure!(seen.insert(idx), "캐릭터 번호가 중복되었습니다({idx})");

                charas.push(ctx.var.extract_chara(idx));
            }

            ensure!(!charas.is_empty(), "저장할 캐릭터를 지정해야 합니다");

            crate::save::write_chara_data(
                &ctx.sav_dir,
                &name,
                &crate::save::SerializableCharaData {
                    description,
                    code: ctx.header_info.gamebase.code,
                    version: ctx.header_info.gamebase.version,
                    charas,
                },
            )?;
        }
        BuiltinCommand::LoadChara => {
            let name = get_arg!(@String: args, ctx);

            // Emuera silently reports failure through RESULT instead of raising:
            // a missing, foreign or outdated file is simply not loaded.
            let loaded = match crate::save::read_chara_data(&ctx.sav_dir, &name)? {
                Some(sav)
                    if sav.code == ctx.header_info.gamebase.code
                        && sav.version >= ctx.header_info.gamebase.allow_version =>
                {
                    let sav = sav.to_chara_data()?;

                    for chara in sav.charas {
                        let idx = ctx.var.character_len();
                        ctx.var.add_chara();
                        ctx.var.restore_chara(idx, chara);
                    }

                    true
                }
                _ => false,
            };

            ctx.var.set_result(loaded as i64);
        }
        BuiltinCommand::SaveVar => {
            ensure!(c >= 3, "SAVEVAR의 매개변수가 부족합니다");
            let name = get_arg!(@String: args, ctx);
            let description = get_arg!(@String: args, ctx);

            let mut variables = hashbrown::HashMap::with_capacity(c as usize - 2);

            for arg in args {
                let LocalValue::VarRef(r) = arg else {
                    bail!("매개변수가 VarRef가 아닙니다");
                };

                // Emuera rejects private and local variables outright; a
                // function-scoped declaration shadowing a global must not be
                // silently replaced by the global here either.
                ensure!(
                    !ctx.var.is_local_var(r.func_name, r.name),
                    "지역 변수 {}는 저장할 수 없습니다",
                    r.name
                );

                let var = ctx.var.extract_global_var(r.name)?;

                // Emuera's builder rejects a repeated variable rather than
                // writing two entries under the same key.
                ensure!(
                    variables.insert(r.name, var).is_none(),
                    "변수가 중복되었습니다({})",
                    r.name
                );
            }

            crate::save::write_var_data(
                &ctx.sav_dir,
                &name,
                &crate::save::SerializableVarData {
                    description,
                    code: ctx.header_info.gamebase.code,
                    version: ctx.header_info.gamebase.version,
                    variables,
                },
            )?;
        }
        BuiltinCommand::LoadVar => {
            let name = get_arg!(@String: args, ctx);

            // Emuera reports failure through RESULT instead of raising: a
            // missing, foreign or outdated file leaves every variable alone.
            let loaded = match crate::save::read_var_data(&ctx.sav_dir, &name)? {
                Some(sav)
                    if sav.code == ctx.header_info.gamebase.code
                        && sav.version >= ctx.header_info.gamebase.allow_version =>
                {
                    for (name, var) in sav.to_var_data()?.variables {
                        ctx.var.restore_global_var(name, var);
                    }

                    true
                }
                _ => false,
            };

            ctx.var.set_result(loaded as i64);
        }
        BuiltinCommand::ResetGlobal => {
            ctx.var.reset_global_data();
        }
        BuiltinCommand::SaveNos => {
            let nos = ctx.config.save_nos as i64;

            // Emuera's SP_GETINT form: with no argument the count goes to
            // RESULT, otherwise into the named variable.
            match args.next() {
                Some(LocalValue::VarRef(r)) => ctx.set_var_ref(&r, nos.into())?,
                Some(_) => bail!("매개변수가 VarRef가 아닙니다"),
                None => ctx.var.set_result(nos),
            }
        }
        BuiltinCommand::PrintCPerLine => {
            let count = ctx.config.printc_count as i64;

            // Same `SP_GETINT` shape as SAVENOS
            // (`Process.ScriptProc.cs:552-556`).
            match args.next() {
                Some(LocalValue::VarRef(r)) => ctx.set_var_ref(&r, count.into())?,
                Some(_) => bail!("매개변수가 VarRef가 아닙니다"),
                None => ctx.var.set_result(count),
            }
        }

        BuiltinCommand::Assert => {
            ensure!(c == 1, "ASSERT문은 인수가 하나여야합니다. {c}");

            // Emuera flags ASSERT as DEBUG_FUNC, so a run without `-Debug`
            // skips the line without even building its argument
            // (`Process.ScriptProc.cs:35-38`). erars drops the whole statement
            // at preprocess time instead (`crates/erars-compiler/src/parser.rs:2524`),
            // so reaching this arm at all means debug mode is on and the
            // argument must be evaluated.
            ensure!(get_arg!(@i64: args, ctx) != 0, "ASSERT문의 인수가 0입니다");
        }
        BuiltinCommand::StopCallTrain => {
            ensure!(c == 0, "STOPCALLTRAIN은 인수를 받지 않습니다. {c}");

            // Emuera guards on `isCTrain`, which is set only by CALLTRAIN, so
            // this is a no-op outside one and during DOTRAIN.
            ctx.call_train_stopped = ctx.call_train_running;
        }
        BuiltinCommand::Ref | BuiltinCommand::RefByName => {
            ensure!(c == 2, "{com}은 인수가 두개여야합니다. {c}");
            let target = get_arg!(@key args, ctx);

            // REFBYNAME names the source at run time. Emuera uppercases that
            // name with `Config.ICVariable` before looking it up, as VARSIZE
            // already does here; a name nothing was ever interned under cannot
            // be a variable, so it fails like any other mismatch.
            let src = match com {
                BuiltinCommand::RefByName => ctx
                    .var
                    .interner()
                    .get(get_arg!(@String: args, ctx).to_uppercase()),
                _ => Some(get_arg!(@key args, ctx)),
            };

            let bound = bind_ref(ctx, func_name, target, src)?;
            ctx.var.set_result(bound as i64);
        }

        BuiltinCommand::Randomize => {
            let seed = get_arg!(@i64: args, ctx);
            ctx.var.randomize(seed);
        }
        BuiltinCommand::DumpRand => {
            ctx.var.dump_rand();
        }
        BuiltinCommand::InitRand => {
            ctx.var.init_rand();
        }
    }

    Ok(InstructionWorkflow::Normal)
}

/// `REF` / `REFBYNAME`: point the `#DIM REF` variable `target` at `src`,
/// answering `false` for the mismatches that make Emuera's
/// `ReferenceToken.MatchType` refuse a binding (`VariableToken.cs:500-525`)
/// rather than raising. Every refusal leaves the reference unbound, as
/// `MatchType` reaches `RESULT = 0` only through `SetRef(null)`.
///
/// `src` is `None` when `REFBYNAME` was handed a name that was never interned,
/// which no variable can answer to.
///
/// A reference is a name alias here, not an array pointer: `make_var_ref`
/// rewrites every access to `target` into an access to the source, so the
/// source's own `VariableInfo` keeps governing bounds, dimensions and type.
/// That is why `MatchType`'s element-type and dimension-count checks have no
/// counterpart — `#DIM REF` deliberately drops the declared type of a
/// reference (`crates/erars-compiler/src/parser/expr.rs`, "REF variable is 0D
/// int") — and Emuera's own `REF` raises `NotImplCodeEE` before reaching any
/// of this, so no script can depend on the refusal.
fn bind_ref(
    ctx: &mut VmContext,
    func_name: StrKey,
    target: StrKey,
    src: Option<StrKey>,
) -> Result<bool> {
    // Emuera raises `trerror.ArgIsNotRef` while loading, before MatchType can
    // run. Only a *private* reference can be bound here, because
    // `make_var_ref` looks the indirection up among the current function's
    // locals.
    let is_ref = ctx.var.is_local_var(func_name, target)
        && ctx.var.get_local_var(func_name, target)?.0.is_ref;
    ensure!(is_ref, "{target}는 참조형 변수여야합니다");

    // The `(name, func)` pair `call_internal` packs for a `#DIM REF` argument
    // and `make_var_ref` reads back, or `0` — an impossible `lasso` key, hence
    // the unbound marker — for anything MatchType turns down.
    let packed = 'resolve: {
        let Some(src) = src else { break 'resolve 0 };

        // A source that is itself a reference binds to what it currently points
        // at, as MatchType inspects the resolved `rother`. One level suffices:
        // a bound reference always stores an already-resolved name. An unbound
        // one is `CanNotOmitRefToVar`, a refusal and not an error.
        let Ok(src) = ctx.make_var_ref(func_name, src, ArgVec::new()) else {
            break 'resolve 0;
        };

        // Emuera resolves REF's source while loading and reports an unknown
        // variable there; erars only has the name at run time, so it joins
        // MatchType's refusals. A pseudo variable (`IsCalc`) is an
        // `Expr::BuiltinVar`, never a `VariableStorage` entry, so it fails
        // here too.
        let Ok((info, _)) = ctx.var.get_maybe_local_var(src.func_name, src.name) else {
            break 'resolve 0;
        };

        // MatchType with allowChara = false.
        if info.is_const || info.is_chara {
            break 'resolve 0;
        }

        unsafe { std::mem::transmute((src.name.to_u32(), src.func_name.to_u32())) }
    };

    match ctx.var.get_local_var(func_name, target)?.1 {
        UniformVariable::Normal(VmVariable::Int(v)) => v[0] = packed,
        _ => bail!("참조 변수 {target}의 저장소가 정수가 아닙니다"),
    }

    Ok(packed != 0)
}

fn get_time(now: time::OffsetDateTime) -> i64 {
    format!(
        "{year:04}{month:02}{day:02}{hour:02}{minute:02}{second:02}{milli:03}",
        year = now.year(),
        month = now.month() as u8,
        day = now.day(),
        hour = now.hour(),
        minute = now.minute(),
        second = now.second(),
        milli = now.millisecond(),
    )
    .parse::<i64>()
    .unwrap()
}

/// `GETTIMES`, and the `RESULTS` half of the `GETTIME` command: Emuera formats
/// both with `DateTime.Now.ToString("yyyy/MM/dd HH:mm:ss")`
/// (`Creator.Method.cs:2871`, `Process.ScriptProc.cs:391`).
fn get_times(now: time::OffsetDateTime) -> String {
    format!(
        "{year:04}/{month:02}/{day:02} {hour:02}:{minute:02}:{second:02}",
        year = now.year(),
        month = now.month() as u8,
        day = now.day(),
        hour = now.hour(),
        minute = now.minute(),
        second = now.second()
    )
}

/// The widest run of cells a single `PRINT_SPACE`/`PRINT_RECT` may emit.
/// Emuera lifted its own size cap (`ConsoleShapePart.cs:76`) but draws into a
/// bounded window; a text line has no such bound, so the cell approximation
/// keeps one to avoid a script hanging the console on a huge parameter.
const SHAPE_CELL_LIMIT: i64 = 4096;

/// Drain `count` variable references off a builtin's argument list.
fn take_var_refs(
    count: u32,
    args: &mut impl Iterator<Item = LocalValue>,
) -> Result<Vec<VariableRef>> {
    (0..count)
        .map(|_| match args.next() {
            Some(LocalValue::VarRef(r)) => Ok(r),
            Some(_) => bail!("매개변수가 VarRef가 아닙니다"),
            None => bail!("매개변수가 부족합니다"),
        })
        .collect()
}

/// Emuera `VariableEvaluator.CopyArray` (`VariableEvaluator.cs:767-833`).
///
/// The overlapping corner of the two arrays is copied, taking the minimum of
/// each dimension *separately* rather than of the flattened lengths, so a
/// narrower destination row does not slide the source's rows out of alignment.
fn copy_array<T: Clone>(src: &[T], src_dims: &[u32], dst: &mut [T], dst_dims: &[u32]) {
    match (src_dims, dst_dims) {
        ([], _) | (_, []) => {}
        ([_], [_]) => {
            let n = src.len().min(dst.len());
            dst[..n].clone_from_slice(&src[..n]);
        }
        ([_, src_rest @ ..], [_, dst_rest @ ..]) => {
            let src_stride: usize = src_rest.iter().map(|&d| d as usize).product();
            let dst_stride: usize = dst_rest.iter().map(|&d| d as usize).product();

            if src_stride == 0 || dst_stride == 0 {
                return;
            }

            for i in 0..(src.len() / src_stride).min(dst.len() / dst_stride) {
                copy_array(
                    &src[i * src_stride..(i + 1) * src_stride],
                    src_rest,
                    &mut dst[i * dst_stride..(i + 1) * dst_stride],
                    dst_rest,
                );
            }
        }
    }
}

/// Emuera `ArrayMultiSortMethod` (`Creator.Method.cs:4022-4171`).
///
/// The first argument is a 1-D key array whose *leading* run defines the sort
/// order; the run stops at the first `0` or empty string (`:4062-4063`,
/// `:4086-4090`), so only that prefix ever moves. Every argument — the key
/// included — is then permuted by that order, a multi-dimensional array carrying
/// whole rows of its first index (`:4122-4163`). An argument whose first
/// dimension is shorter than the run aborts with `0` and leaves the arrays
/// already permuted as they are (`:4106-4107`); otherwise the result is `1`.
///
/// DELIBERATE DEVIATION: `List<T>.Sort` is an unstable introsort, so Emuera
/// leaves the relative order of equal keys unspecified. erars sorts stably,
/// which is deterministic and satisfies everything Emuera guarantees.
fn array_msort(refs: &[VariableRef], ctx: &mut VmContext) -> Result<i64> {
    ensure!(
        refs.len() >= 2,
        "ARRAYMSORT関数:少なくとも2の引数が必要です"
    );

    let order: Vec<u32> = {
        let (info, var, _) = ctx.resolve_var_ref(&refs[0])?;
        ensure!(
            info.size.len() == 1,
            "ARRAYMSORT関数:1番目の引数が一次元配列ではありません"
        );

        if info.is_str {
            let arr = var.as_str()?;
            let n = arr.iter().position(String::is_empty).unwrap_or(arr.len());
            let mut order: Vec<u32> = (0..n as u32).collect();
            order.sort_by(|&a, &b| arr[a as usize].cmp(&arr[b as usize]));
            order
        } else {
            let arr = var.as_int()?;
            let n = arr.iter().position(|&v| v == 0).unwrap_or(arr.len());
            let mut order: Vec<u32> = (0..n as u32).collect();
            order.sort_by_key(|&i| arr[i as usize]);
            order
        }
    };

    for r in refs {
        let (info, var, _) = ctx.resolve_var_ref(r)?;
        // Rows of the first index move as a unit; a 1-D array has a row of one.
        let row: usize = info.size.iter().skip(1).map(|&d| d as usize).product();

        if info.is_str {
            let arr = var.as_str()?;
            if arr.len() < order.len() * row {
                return Ok(0);
            }
            let clone = arr.to_vec();
            permute_rows(arr, &clone, &order, row);
        } else {
            let arr = var.as_int()?;
            if arr.len() < order.len() * row {
                return Ok(0);
            }
            let clone = arr.to_vec();
            permute_rows(arr, &clone, &order, row);
        }
    }

    Ok(1)
}

fn permute_rows<T: Clone>(dst: &mut [T], src: &[T], order: &[u32], row: usize) {
    for (to, &from) in order.iter().enumerate() {
        let (to, from) = (to * row, from as usize * row);
        dst[to..to + row].clone_from_slice(&src[from..from + row]);
    }
}

/// Split Emuera's packed 24-bit `0xRRGGBB` colour code into channels
/// (`Instraction.Child.cs:1863-1864`).
fn unpack_rgb(v: i64) -> erars_ui::Color {
    erars_ui::Color([(v >> 16) as u8, (v >> 8) as u8, v as u8])
}

fn pow_i64(x: i64, y: i64) -> Result<i64> {
    if let Ok(y) = u32::try_from(y) {
        x.checked_pow(y).context("pow_i64 overflow")
    } else if y > 0 {
        if x == 0 || x == 1 {
            // never overflowed
            Ok(x)
        } else {
            // overflowed
            bail!("pow_i64 overflow")
        }
    } else {
        // y is negative
        Ok(0)
    }
}
