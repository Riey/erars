use anyhow::{ensure, Context};

use crate::variable::KnownVariableNames as Var;

use super::*;

const BASE_TIME: time::OffsetDateTime = time::PrimitiveDateTime::new(
    if let Ok(d) = time::Date::from_ordinal_date(0001, 1) {
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
//         conv_workflow!($vm.call($name, $args, $tx, $ctx).await?)
//     };
// }

macro_rules! try_call {
    ($vm:expr, $name:expr, $tx:expr, $ctx:expr) => {
        try_call!($vm, $name, &[], $tx, $ctx)
    };
    ($vm:expr, $name:expr, $args:expr, $tx:expr, $ctx:expr) => {
        match $vm.try_call($name, $args, $tx, $ctx).await? {
            Some(Workflow::Return) => true,
            None => false,
            Some(other) => return Ok(other.into()),
        }
    };
}

macro_rules! call_event {
    ($vm:expr, $ty:expr, $tx:expr, $ctx:expr) => {
        conv_workflow!($vm.call_event($ty, $tx, $ctx).await?)
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
                let s: String = $ctx.reduce_local_value(v)?.try_into()?;
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
        get_arg!(@opt @value $arg, $ctx).and_then(|v| <$t>::try_from(v).ok())
    };
}

pub(super) async fn run_instruction(
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
    } else if let Some(flags) = inst.as_print() {
        let s = ctx.pop_str()?;

        if flags.contains(PrintFlags::FORCE_KANA) {
            log::error!("Unimplemented: FORCE_KANA");
        }

        let prev_color = if flags.contains(PrintFlags::DEFAULT_COLOR) {
            let c = tx.color();
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
        if flags.contains(PrintFlags::WAIT) {
            let gen = tx.input_gen();
            ctx.system
                .input(
                    tx,
                    InputRequest {
                        generation: gen,
                        ty: InputRequestType::AnyKey,
                        is_one: false,
                        timeout: None,
                    },
                )
                .await?;
        }
    } else if let Some(c) = inst.as_try_call().or_else(|| inst.as_try_jump()) {
        let func = ctx.pop_strkey()?;
        let args = ctx.take_value_list(c)?;

        match vm.try_call(func, &args, tx, ctx).await? {
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
        let func = ctx.pop_strkey()?;
        let args = ctx.take_value_list(c)?;

        match vm.call(func, &args, tx, ctx).await? {
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
            ItemPrice => {
                let arg = args[0] as u32;
                ctx.header_info.item_price.get(&arg).copied().unwrap_or(0).into()
            }

            AblName | TalentName | ItemName | FlagName | ExName | ExpName | MarkName
            | CflagName | CstrName | StrName | TstrName | EquipName | TequipName | PalamName
            | SourceName | StainName | TcvarName | GlobalName | GlobalsName | SaveStrName => {
                let name = <&str>::from(var).strip_suffix("NAME").unwrap();
                let name = ctx.var.interner().get_or_intern(name);
                let arg = args[0] as u32;
                Value::String(
                    ctx.header_info
                        .var_name_var
                        .get(&name)
                        .and_then(|d| Some(ctx.var.interner().resolve(d.get(&arg)?).to_string()))
                        .unwrap_or_default(),
                )
            }
            Rand => {
                let max = args[0];
                Value::Int(ctx.var.rng().gen_range(0..max) as i64)
            }
        };

        ctx.push(value);
    } else if let Some(meth) = inst.as_builtin_method() {
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
                let no = get_arg!(@i64: args, ctx) as u32;
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
                let no = get_arg!(@i64: args, ctx) as u32;
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

                let target = ctx.var.read_int(Var::Target, &[])?;
                let var = ctx.var.get_maybe_local_var(func_name, var_ref.name)?.1;

                let var = match var {
                    UniformVariable::Character(cvar) => {
                        let c_idx = var_ref.idxs.first().copied().unwrap_or(target.try_into()?);
                        &mut cvar[c_idx as usize]
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
                        .ok_or_else(|| anyhow!("u32 {code} is not valid unicode codepoint"))?
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

                ctx.push(
                    format!(
                        "{year}{month}{day}{hour}{minute}{second}",
                        year = now.year(),
                        month = now.month() as u8,
                        day = now.day(),
                        hour = now.hour(),
                        minute = now.minute(),
                        second = now.second()
                    )
                    .parse::<i64>()
                    .unwrap(),
                );
            }

            BuiltinMethod::GetTimeS => {
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

            BuiltinMethod::ChkData => {
                check_arg_count!(1);
                let idx = get_arg!(@u32: args, ctx);

                let (ret, rets) = match ctx.system.load_local(idx).await? {
                    Some(sav) => {
                        if sav.code != ctx.header_info.gamebase.code {
                            (2, None)
                        } else if sav.version < ctx.header_info.gamebase.allow_version {
                            (3, None)
                        } else {
                            ctx.var.load_serializable(sav.clone(), &ctx.header_info)?;
                            (0, Some(sav.description.clone()))
                        }
                    }
                    None => (1, None),
                };

                ctx.var.set_results(
                    rets.unwrap_or_else(|| "セーブデータのバーションが異なります".into()),
                );
                ctx.push(ret as i64);
            }
        }
    } else if let Some(com) = inst.as_builtin_command() {
        let c = ctx.pop_int()? as u32;
        let mut args = ctx.take_list(c).collect::<Vec<_>>().into_iter();

        match com {
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
            BuiltinCommand::Restart => {
                drop(ctx.return_func()?);
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
                let start = get_arg!(@u32: args, ctx);
                let count = get_arg!(@u32: args, ctx);

                let target = if let Some(idx) = v.idxs.first().copied() {
                    idx
                } else {
                    ctx.var.read_int(Var::Target, &[])?.try_into()?
                };
                let (info, var) = ctx.var.get_maybe_local_var(v.func_name, v.name)?;
                let var = var.as_vm_var(target);

                if info.is_str {
                    let var = var.as_str()?;
                    let empty_value = empty_value.try_into()?;
                    array_shift(var, empty_value, start as usize, count as usize)?;
                } else {
                    let var = var.as_int()?;
                    let empty_value = empty_value.try_into()?;
                    array_shift(var, empty_value, start as usize, count as usize)?;
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
                let start = get_arg!(@opt @u32: args, ctx);
                let end = get_arg!(@opt @u32: args, ctx);

                let target = ctx.var.read_int(Var::Target, &[])?;
                let (info, var, idx) = ctx.resolve_var_ref_raw(&var)?;
                let (chara_idx, idx) = info.calculate_single_idx(&idx);

                match (value, start, end) {
                    (None, None, None) => {
                        var.reset(info);
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
            BuiltinCommand::ReturnF => {
                let ret = get_arg!(@value args, ctx);

                if args.next().is_some() {
                    bail!("RETURNF는 한개의 값만 반환할 수 있습니다.");
                }

                drop(ctx.return_func()?);

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
                ctx.system
                    .input(
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
                    )
                    .await?;
            }
            BuiltinCommand::Wait | BuiltinCommand::WaitAnykey | BuiltinCommand::ForceWait => {
                let gen = tx.input_gen();
                ctx.system
                    .input(
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
                    )
                    .await?;
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

                let ret = ctx.system.input(tx, req).await?;

                match (ty, ret) {
                    (InputRequestType::Int, Some(Value::Int(i))) => {
                        ctx.var.set_result(i);
                    }
                    (InputRequestType::Str, Some(Value::String(s))) => {
                        ctx.var.set_results(s);
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
                let no = get_arg!(@u32: args, ctx);
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
                ctx.system.redraw(tx).await?;
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

                conv_workflow!(run_call_train(vm, tx, ctx, commands).await?);
            }
            BuiltinCommand::DelChara => {
                let idx = get_arg!(@i64: args, ctx);
                ctx.var.del_chara(idx.try_into()?);
            }
            BuiltinCommand::ResetData => {
                ctx.var.reset_data(&ctx.header_info.replace)?;
            }
            BuiltinCommand::SaveData => {
                let idx = get_arg!(@u32: args, ctx);
                let description = get_arg!(@String: args, ctx);

                log::info!("Save {idx}: {description}");

                let var = ctx.var.get_serializable(&ctx.header_info, description);
                ctx.system.save_local(idx, &var).await?;
            }
            BuiltinCommand::LoadData => {
                let idx = get_arg!(@u32: args, ctx);

                conv_workflow!(run_load_data(vm, tx, ctx, idx).await?);
            }
            BuiltinCommand::DelData => {
                let idx = get_arg!(@u32: args, ctx);

                ctx.system.remove_local(idx).await?;
            }
            BuiltinCommand::SaveGlobal => {
                ctx.system
                    .save_global(&ctx.var.get_global_serializable(&ctx.header_info))
                    .await?;
            }
            BuiltinCommand::LoadGlobal => {
                if let Some(global_sav) = ctx.system.load_global().await? {
                    ctx.var.load_global_serializable(global_sav, &ctx.header_info)?;
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
                conv_workflow!(run_save_game(vm, tx, ctx).await?);
            }
            BuiltinCommand::LoadGame => match run_load_game(tx, ctx).await? {
                Some(idx) => {
                    conv_workflow!(run_load_data(vm, tx, ctx, idx).await?);
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
        }
    } else {
        if !inst.is_nop() && !inst.is_debug() {
            bail!("Unimplemented instruction: {inst:?}");
        }
    }

    Ok(InstructionWorkflow::Normal)
}

async fn run_save_game(
    vm: &TerminalVm,
    tx: &mut VirtualConsole,
    ctx: &mut VmContext,
) -> Result<Workflow> {
    let mut savs = ctx.system.load_local_list().await?;
    print_sav_data_list(&savs, tx);

    loop {
        match ctx.system.input_int(tx).await? {
            100 => break Ok(Workflow::Return),
            i if i >= 0 && i < SAVE_COUNT as i64 => {
                let i = i as u32;
                let write = if savs.contains_key(&i) {
                    tx.print_line(format!("SAVE {i} already exists. Overwrite?"));
                    tx.print_line("[0] Yes [1] No".into());

                    loop {
                        match ctx.system.input_int(tx).await? {
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
                    ctx.system.save_local(i, &sav).await?;
                    savs.insert(i, sav);
                }
            }
            _ => {}
        }
    }
}

async fn run_load_game(tx: &mut VirtualConsole, ctx: &mut VmContext) -> Result<Option<u32>> {
    let mut savs = ctx.system.load_local_list().await?;
    print_sav_data_list(&savs, tx);

    loop {
        match ctx.system.input_int(tx).await? {
            100 => break Ok(None),
            i if i >= 0 && i < SAVE_COUNT as i64 => {
                if let Some(_) = savs.remove(&(i as u32)) {
                    ctx.system.remove_local(i as u32).await?;
                    break Ok(Some(i as u32));
                }
            }
            _ => {}
        }
    }
}

async fn run_load_data(
    vm: &TerminalVm,
    tx: &mut VirtualConsole,
    ctx: &mut VmContext,
    idx: u32,
) -> Result<Workflow> {
    let sav = ctx.system.load_local(idx).await?.unwrap();

    ctx.lastload_text = sav.description.clone();
    ctx.lastload_no = idx;
    ctx.lastload_version = sav.version;
    ctx.var.load_serializable(sav, &ctx.header_info)?;

    try_call!(vm, "SYSTEM_LOADEND", tx, ctx);
    call_event!(vm, EventType::Load, tx, ctx);

    Ok(Workflow::Begin(BeginType::Shop))
}

async fn run_call_train(
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

pub async fn run_begin(
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

                        let no = ctx.system.input_int(tx).await?;
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
                let i = ctx.system.input_int(tx).await?;
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

                let i = ctx.system.input_int(tx).await?;
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
