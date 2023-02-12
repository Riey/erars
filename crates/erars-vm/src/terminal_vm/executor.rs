use anyhow::{ensure, Context};
use erars_ast::Alignment;
use erars_compiler::EraConfigKey;
use html5ever::tendril::TendrilSink;
use rcdom::{NodeData, RcDom};
use tinyvec::ArrayVec;

use markup5ever_rcdom as rcdom;

use crate::{context::VariableRef, variable::KnownVariableNames as Var};

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
        #[cfg(target_engian = "big")]
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
        ctx.push_var_ref(name, func_extern, args);
    } else if let Some(c) = inst.as_load_var_ref() {
        let name = ctx.pop_strkey()?;
        let args = ctx.take_arg_list(Some(name), c)?;
        ctx.push_var_ref(name, func_name, args);
    } else if inst.is_load_count_var_ref() {
        ctx.push_var_ref(ctx.var.known_key(Var::Count), func_name, ArgVec::new());
    } else if inst.is_reuse_lastline() {
        let s = ctx.pop_str()?;
        tx.reuse_last_line(s);
        ctx.system.redraw(tx)?;
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
        ctx.system.redraw(tx)?;
    } else if let Some(flags) = inst.as_print() {
        let s = ctx.pop_str()?;

        if flags.contains(PrintFlags::DEBUG) {
            // TODO: check DEBUG
            return Ok(InstructionWorkflow::Normal);
        }

        if flags.contains(PrintFlags::FORCE_KANA) {
            log::error!("Unimplemented: FORCE_KANA");
        }

        let prev_color = if flags.contains(PrintFlags::DEFAULT_COLOR) {
            let c = tx.color();
            // TODO: respect config
            tx.set_color(0xFF, 0xFF, 0xFF);
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
            let [r, g, b, _] = prev_color.to_le_bytes();
            tx.set_color(r, g, b);
        }

        if flags.contains(PrintFlags::NEWLINE) {
            tx.new_line();
        }

        ctx.system.redraw(tx)?;
        if flags.contains(PrintFlags::WAIT) {
            let gen = tx.input_gen();
            ctx.system.input(InputRequest {
                generation: gen,
                ty: InputRequestType::AnyKey,
                is_one: false,
                timeout: None,
            })?;
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
                let max = args[0];
                Value::Int(ctx.var.rng().gen_range(0..max) as i64)
            }
        };

        ctx.push(value);
    } else if let Some(meth) = inst.as_builtin_method() {
        run_builtin_method(meth, func_name, tx, ctx)?;
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
        match ctx.system.input_int_redraw(tx)? {
            100 => break Ok(Workflow::Return),
            i if i >= 0 && i < SAVE_COUNT as i64 => {
                let i = i as u32;
                let write = if savs.contains_key(&i) {
                    tx.print_line(format!("SAVE {i} already exists. Overwrite?"));
                    tx.print_line("[0] Yes [1] No".into());

                    loop {
                        match ctx.system.input_int_redraw(tx)? {
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
        match ctx.system.input_int_redraw(tx)? {
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
    let sav = crate::save::read_save_data(&ctx.sav_dir, idx)?
        .unwrap()
        .to_local_data()?;

    ctx.lastload_text = sav.description.clone();
    ctx.lastload_no = idx;
    ctx.lastload_version = sav.version;
    ctx.var.load_serializable(sav, &ctx.header_info)?;

    try_call!(vm, "SYSTEM_LOADEND", tx, ctx);
    call_event!(vm, EventType::Load, tx, ctx);

    Ok(Workflow::Begin(BeginType::Shop))
}

fn run_call_train(
    vm: &TerminalVm,
    tx: &mut VirtualConsole,
    ctx: &mut VmContext,
    commands: Vec<u32>,
) -> Result<Workflow> {
    for command in commands {
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

    try_call!(vm, "CALLTRAINEND", tx, ctx);

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

pub fn run_begin(
    vm: &TerminalVm,
    ty: BeginType,
    tx: &mut VirtualConsole,
    ctx: &mut VmContext,
) -> Result<Workflow> {
    log::trace!("Begin {ty}");

    match ty {
        BeginType::Title => {
            if !try_call!(vm, "SYSTEM_TITLE", tx, ctx) {
                todo!("Default TITLE");
            }
        }
        BeginType::First => {
            call_event!(vm, EventType::First, tx, ctx);
        }
        BeginType::Train => {
            ctx.var.reset_train_data()?;
            call_event!(vm, EventType::Train, tx, ctx);
            let train_key = ctx.var.known_key(Var::Train);

            loop {
                let com_no = match ctx.var.read_int(Var::NextCom, &[])? {
                    no if no >= 0 => no,
                    _ => {
                        try_call!(vm, "SHOW_STATUS", tx, ctx);

                        let mut printc_count = 0;

                        for (no, name) in ctx.header_info.clone().var_name_var[&train_key].iter() {
                            if try_call!(vm, &format!("COM_ABLE{no}"), tx, ctx)
                                && ctx.var.get_result() != 0
                                || ctx.header_info.replace.comable_init != 0
                            {
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

                        let no = ctx.system.input_int_redraw(tx)?;
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

                        if com_exists {
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
                let i = ctx.system.input_int_redraw(tx)?;
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

                let i = ctx.system.input_int_redraw(tx)?;
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

fn run_builtin_method(
    meth: BuiltinMethod,
    func_name: StrKey,
    tx: &mut VirtualConsole,
    ctx: &mut VmContext,
) -> Result<()> {
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
    }

    macro_rules! csv_method {
        ($field:ident) => {
            check_arg_count!(1, 2);
            let no = get_arg!(@i64: args, ctx);
            let sp = get_arg!(@opt @i64: args, ctx);

            if sp.map_or(false, |sp| sp != 0) {
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
            let no = get_arg!(@i64: args, ctx);
            let idx = get_arg!(@i64: args, ctx) as u32;
            let sp = get_arg!(@opt @i64: args, ctx);

            if sp.map_or(false, |sp| sp != 0) {
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
        BuiltinMethod::SpriteCreated => {
            // log::warn!("SpriteCreated");
            ctx.push(0);
        }
        BuiltinMethod::GCreated => {
            // log::warn!("GCreated");
            ctx.push(0);
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
            // TODO:
            ctx.push(false);
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
        BuiltinMethod::FindChara => {
            check_arg_count!(1, 4);
            let mut key = get_arg!(@var args);
            let value = get_arg!(@value args, ctx);

            let start = get_arg!(@opt @u32: args, ctx).unwrap_or(0);
            let end = get_arg!(@opt @u32: args, ctx).unwrap_or_else(|| ctx.var.character_len());

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
        BuiltinMethod::MoneyStr => {
            check_arg_count!(1, 2);

            let value = get_arg!(@i64: args, ctx);
            let arg = get_arg!(@opt @String: args, ctx);

            if let Some(arg) = arg {
                log::warn!("TODO: str format {arg}");
            }

            let ret = if ctx.header_info.replace.unit_forward {
                format!("{}{value}", ctx.header_info.replace.money_unit)
            } else {
                format!("{value}{}", ctx.header_info.replace.money_unit)
            };

            ctx.push(ret);
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
            let end = get_arg!(@opt @usize: args, ctx);

            let mut ret = 0;
            let (info, var) = ctx.var.get_maybe_local_var(func_name, var_ref.name)?;
            let var = var.assume_chara_vec();
            let (_, idx) = info.calculate_single_idx(&var_ref.idxs);

            let vars = range_end_opt(var, start, end)?.iter_mut();

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
            let start = get_arg!(@opt @i64: args, ctx).unwrap_or(0);
            match usize::try_from(start) {
                Ok(start) => {
                    let bytes = ctx.encoding().encode(&text).0;
                    let length = get_arg!(@opt @i64: args, ctx);

                    if start >= bytes.len() {
                        ctx.push("");
                    } else {
                        let length = length
                            .and_then(|i| usize::try_from(i).ok())
                            .map(|i| i.min(bytes.len() - start));

                        let sub_bytes = match length {
                            Some(length) => bytes.as_ref().get(start..(start + length)),
                            None => bytes.as_ref().get(start..),
                        };

                        match sub_bytes {
                            Some(sub_bytes) => {
                                let sub_str = ctx.encoding().decode(sub_bytes).0;
                                ctx.push(sub_str.into_owned());
                            }
                            None => {
                                ctx.push("");
                            }
                        }
                    }
                }
                _ => {
                    ctx.push("");
                }
            };
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
            ctx.push(String::from_iter(
                s.chars().map(|c| unicode_hfwidth::to_halfwidth(c).unwrap_or(c)),
            ));
        }

        BuiltinMethod::ToFull => {
            check_arg_count!(1);
            let s = get_arg!(@String: args, ctx);
            ctx.push(String::from_iter(
                s.chars().map(|c| unicode_hfwidth::to_fullwidth(c).unwrap_or(c)),
            ));
        }

        BuiltinMethod::IsNumeric => {
            check_arg_count!(1);
            let s = get_arg!(@String: args, ctx);
            ctx.push(s.parse::<i64>().is_ok());
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
        BuiltinMethod::ChkFont => {
            // TODO: CHKFONT
            ctx.push(0i64);
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
            let align = tx.align();
            ctx.push(align as u32);
        }

        BuiltinMethod::CurrentRedraw => {
            ctx.push(0i64);
        }

        BuiltinMethod::SaveNos => {
            let nos = ctx.config.save_nos;
            ctx.push(nos as i64);
        }

        BuiltinMethod::GetConfig => {
            check_arg_count!(1);
            let key = get_arg!(@String: args, ctx);
            match key.parse::<EraConfigKey>() {
                Ok(key) => {
                    ctx.push(
                        ctx.config
                            .get_config(key)
                            .try_into_int()
                            .map_err(|_| anyhow!("Config key {key} has string type"))?,
                    );
                }
                _ => bail!("Invalid config key: {key}"),
            }
        }

        BuiltinMethod::GetConfigS => {
            check_arg_count!(1);
            let key = get_arg!(@String: args, ctx);
            match key.parse::<EraConfigKey>() {
                Ok(key) => {
                    ctx.push(
                        ctx.config
                            .get_config(key)
                            .try_into_str()
                            .map_err(|_| anyhow!("Config key {key} has int type"))?,
                    );
                }
                _ => bail!("Invalid config key: {key}"),
            }
        }

        BuiltinMethod::PrintCPerLine => {
            check_arg_count!(0);
            ctx.push(
                ctx.config
                    .get_config(EraConfigKey::PrintcCount)
                    .try_into_int()
                    .unwrap(),
            );
        }

        BuiltinMethod::VarSize => {
            check_arg_count!(1, 2);
            let var = get_arg!(@String: args, ctx).to_uppercase();
            let dim = get_arg!(@opt @usize: args, ctx).unwrap_or(0);

            let var_ref = ctx.make_var_ref(func_name, &var, ArrayVec::new());
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
            check_arg_count!(1);
            let no = get_arg!(@i64: args, ctx);
            ctx.push(ctx.header_info.character_templates.contains_key(&no));
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

    Ok(())
}

fn html_print(node: &rcdom::Handle, tx: &mut VirtualConsole) {
    let mut newline = false;
    let init_align = tx.align();
    let mut align = init_align;
    match &node.data {
        NodeData::Text { contents } => {
            tx.print(contents.borrow().as_ref().to_owned());
        }
        NodeData::Element { name, attrs, .. } => {
            for attr in attrs.borrow().iter() {
                match &*attr.name.local {
                    "align" => {
                        align = match &*attr.value {
                            "left" => Alignment::Left,
                            "center" => Alignment::Center,
                            "right" => Alignment::Right,
                            other => {
                                log::warn!("TODO: HTML attribute align={other}");
                                continue;
                            }
                        };
                    }
                    _ => {}
                }
            }

            match &*name.local {
                "br" => {
                    newline = true;
                }
                "p" => {
                    newline = true;
                }
                "html" | "body" | "head" => {}
                "img" => {
                    match attrs.borrow().iter().find_map(|attr| {
                        if &*attr.name.local == "src" {
                            Some(&attr.value)
                        } else {
                            None
                        }
                    }) {
                        Some(img_src) => {
                            // TODO print img
                            tx.print(format!("<img src={img_src}>"));
                        }
                        None => {
                            tx.print("<img>".into());
                        }
                    }
                }
                other => {
                    log::warn!("TODO: HTML element {other}");
                }
            }
        }
        NodeData::Document => {}
        NodeData::Doctype { .. } => {}
        NodeData::Comment { .. } => {}
        NodeData::ProcessingInstruction { .. } => {}
    }

    tx.set_align(align);

    for child in node.children.borrow().iter() {
        html_print(child, tx);
    }

    tx.set_align(init_align);

    if newline {
        tx.new_line();
    }
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
        BuiltinCommand::SpriteCreate => {
            log::trace!("TODO: SPRITECREATE");
        }
        BuiltinCommand::HtmlPrint => {
            let s = get_arg!(@String: args, ctx);
            let dom = html5ever::parse_document(RcDom::default(), Default::default())
                .from_utf8()
                .read_from(&mut s.as_bytes())?;

            html_print(&dom.document, tx);
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
            let l = get_arg!(@i64: args, ctx);
            let r = get_arg!(@u32: args, ctx);
            *ctx.ref_int_var_ref(&out)? = l.pow(r);
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
            let shift_count = get_arg!(@usize: args, ctx);
            let empty_value = get_arg!(@value args, ctx);
            let start = get_arg!(@opt @usize: args, ctx).unwrap_or(0);
            let count = get_arg!(@opt @usize: args, ctx).unwrap_or(usize::MAX);
            let end = count.saturating_add(start);

            let (info, var, _) = ctx.resolve_var_ref(&v)?;

            if info.is_str {
                let var = var.as_str()?;
                let empty_value = empty_value.try_into()?;
                array_shift(var, empty_value, shift_count, start, end)?;
            } else {
                let var = var.as_int()?;
                let empty_value = empty_value.try_into()?;
                array_shift(var, empty_value, shift_count, start, end)?;
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
            let original = get_arg!(@var args);
            let target = get_arg!(@var args);

            let target_val = ctx.var.read_int(Var::Target, &[])?.try_into()?;
            let original_target = if let Some(idx) = original.idxs.first().copied() {
                idx
            } else {
                target_val
            };
            let target_target = if let Some(idx) = target.idxs.first().copied() {
                idx
            } else {
                target_val
            };

            let [(original_info, original_var), (target_info, target_var)] =
                ctx.var.get_maybe_local_var2(
                    original.func_name,
                    original.name,
                    target.func_name,
                    target.name,
                )?;

            ensure!(
                original_info.size.len() == target_info.size.len(),
                "ARRAYCOPY size mismatch"
            );
            ensure!(
                original_info.is_str == target_info.is_str,
                "ARRAYCOPY type mismatch"
            );

            let original_var = original_var.as_vm_var(original_target);
            let target_var = target_var.as_vm_var(target_target);

            if original_info.is_str {
                let original_var = original_var.as_str()?;
                let target_var = target_var.as_str()?;
                let count = original_var.len().min(target_var.len());
                target_var[..count].clone_from_slice(&original_var[..count]);
            } else {
                let original_var = original_var.as_int()?;
                let target_var = target_var.as_int()?;
                let count = original_var.len().min(target_var.len());
                target_var[..count].copy_from_slice(&original_var[..count]);
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
            bail!("TODO: ARRAYMOVE");
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

            let (info, var) = ctx.var.get_var(var.name)?;

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
                var.idxs.push(idx as u32);

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
            let style: u32 = get_arg!(@i64: args, ctx).try_into()?;
            let style = FontStyle::from_bits_truncate(style);
            tx.set_style(style);
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
        BuiltinCommand::Twait => {
            let time = get_arg!(@u32: args, ctx);
            let force_wait = get_arg!(@i64: args, ctx) != 0;

            let ty = if force_wait {
                InputRequestType::ForceEnterKey
            } else {
                InputRequestType::EnterKey
            };

            let gen = tx.input_gen();
            ctx.system.input_redraw(
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
            ctx.system.input_redraw(
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

            let ret = ctx.system.input_redraw(tx, req)?;

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

            ctx.var.swap_chara(a, b);
        }
        BuiltinCommand::SortChara => bail!("SORTCHARA"),
        BuiltinCommand::PickupChara => {
            let list = args
                .map(|v| ctx.reduce_local_value(v).and_then(u32::try_from))
                .collect::<Result<BTreeSet<_>>>()?;

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

            ctx.var.del_chara_list(&list);
        }
        BuiltinCommand::CopyChara => {
            let from = get_arg!(@u32: args, ctx);
            let to = get_arg!(@u32: args, ctx);

            ensure!(from < ctx.var.character_len());
            ensure!(to < ctx.var.character_len());

            ctx.var.copy_chara(from, to);
        }
        BuiltinCommand::AddChara => {
            let no = get_arg!(@i64: args, ctx);
            let template = ctx
                .header_info
                .character_templates
                .get(&no)
                .ok_or_else(|| anyhow!("존재하지 않는 캐릭터 번호입니다({no})"))?;

            let idx = ctx.var.character_len();

            ctx.var.add_chara();
            ctx.var.set_character_template(idx, template)?;
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
            ctx.system.redraw(tx)?;
        }
        BuiltinCommand::ClearLine => {
            tx.clear_line(get_arg!(@usize: args, ctx));
        }
        BuiltinCommand::ForceKana => {
            log::error!("FORCEKANA is not implemented!");
        }
        BuiltinCommand::DoTrain => {
            todo!("DOTRAIN")
        }
        BuiltinCommand::CallTrain => {
            let count = get_arg!(@usize: args, ctx);
            let commands = ctx.var.get_var("SELECTCOM")?.1.assume_normal().as_int()?[..count]
                .iter()
                // CallTrain works reverse order
                .rev()
                .map(|c| u32::try_from(*c).context("CallTrain command convert"))
                .collect::<Result<Vec<u32>>>()?;

            conv_workflow!(run_call_train(vm, tx, ctx, commands)?);
        }
        BuiltinCommand::DelChara => {
            let idx = get_arg!(@i64: args, ctx);
            ctx.var.del_chara(idx.try_into()?);
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
            let stain = ctx.var.get_var("STAIN")?.1.assume_chara(chara);
            let stain_init = &ctx.header_info.replace.stain_init;
            stain.as_int()?[..stain_init.len()].copy_from_slice(&stain_init);
        }
        BuiltinCommand::SaveChara => bail!("SAVECHARA"),
        BuiltinCommand::LoadChara => bail!("LOADCHARA"),

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

fn get_times(now: time::OffsetDateTime) -> String {
    format!(
        "{year:04}年{month:02}月{day:02}日 {hour:02}:{minute:02}:{second:02}",
        year = now.year(),
        month = now.month() as u8,
        day = now.day(),
        hour = now.hour(),
        minute = now.minute(),
        second = now.second()
    )
}
