use anyhow::{bail, Result};
use erars_ast::EventType;
use erars_ui::{ConsoleSender, InputRequest, InputRequestType};

use crate::{SystemState, TerminalVm, VmContext, Workflow};

macro_rules! call {
    ($vm:expr, $name:expr, $tx:expr, $ctx:expr) => {
        match $vm.try_call($name, &[], $tx, $ctx)? {
            Some(Workflow::Return) | None => None,
            Some(other) => Some(other),
        }
    };
}

macro_rules! call_event {
    ($vm:expr, $ty:expr, $tx:expr, $ctx:expr) => {
        match $vm.call_event($ty, $tx, $ctx)? {
            Workflow::Return => None,
            other => Some(other),
        }
    };
}

fn input_int(tx: &ConsoleSender) -> Option<Workflow> {
    Some(Workflow::Input {
        req: InputRequest::normal(tx.input_gen(), InputRequestType::Int),
    })
}

impl SystemState {
    pub fn run(
        &mut self,
        vm: &TerminalVm,
        tx: &mut ConsoleSender,
        ctx: &mut VmContext,
        phase: &mut usize,
    ) -> Result<Option<Workflow>> {
        use SystemState::*;

        let ret = match self {
            Exited => return Ok(Some(Workflow::Return)),
            BeginTitle => match vm.try_call("SYSTEM_TITLE", &[], tx, ctx)? {
                None => bail!("TODO: default title"),
                Some(ret) => return Ok(Some(ret)),
            },
            BeginShop => match *phase {
                0 => call_event!(vm, EventType::Shop, tx, ctx),
                1 => call!(vm, "SHOW_SHOP", tx, ctx),
                2 => input_int(tx),
                3 => {
                    let i = ctx.current_input.take().unwrap().try_into_int()?;

                    ctx.var.set_result(i);

                    if i >= 0 && i < ctx.header_info.replace.sell_item_count {
                        let sales = ctx.var.read_int("ITEMSALES", &[i as usize])?;

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
                                *ctx.var.ref_int("ITEM", &[i as usize])? += 1;
                            }
                        }

                        *phase = 0;
                        return Ok(None);
                    } else {
                        *phase = 0;
                        return Ok(call!(vm, "USERSHOP", tx, ctx));
                    }
                }
                _ => return Ok(Some(Workflow::Return)),
            },
            BeginTrain {
                com_no,
                com_able_no,
                printc_count,
            } => match *phase {
                0 => {
                    ctx.var.reset_train_data()?;
                    call_event!(vm, EventType::Train, tx, ctx)
                }
                1 => match ctx.var.read_int("NEXTCOM", &[])? {
                    no if no >= 0 => {
                        *com_no = no as u32;
                        *phase = 10;
                        return Ok(None);
                    }
                    _ => {
                        call!(vm, "SHOW_STATUS", tx, ctx)
                    }
                },
                2 => {
                    for (no, _) in
                        ctx.header_info.clone().var_name_var["TRAIN"].range(*com_able_no..)
                    {
                        *com_able_no = *no;
                        match vm.try_call(&format!("COM_ABLE{no}"), &[], tx, ctx)? {
                            Some(Workflow::Return) => {
                                *phase = 3;
                                return Ok(None);
                            }
                            Some(other) => {
                                *phase = 3;
                                return Ok(Some(other));
                            }
                            None => continue,
                        }
                    }

                    tx.new_line();

                    *phase = 4;
                    return Ok(call!(vm, "SHOW_USERCOM", tx, ctx));
                }
                3 => {
                    let no = *com_able_no;
                    let name = &ctx.header_info.var_name_var["TRAIN"][&no];
                    if ctx.var.get_result() != 0 || ctx.header_info.replace.comable_init != 0 {
                        if ctx.config.printc_count != 0 && *printc_count == ctx.config.printc_count as u32 {
                            *printc_count = 0;
                            tx.new_line();
                        }
                        tx.printrc(&format!("{name}[{no:3}]"));
                        *printc_count += 1;
                    }

                    *phase = 2;
                    return Ok(None);
                }
                4 => {
                    ctx.var.prepare_train_data()?;
                    input_int(tx)
                }
                5 => {
                    let no = ctx.current_input.take().unwrap().try_into_int()?;

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
                        *phase = 10;
                        *com_no = no as u32;
                        return Ok(None);
                    } else {
                        *phase = 2;
                        return Ok(call!(vm, "USERCOM", tx, ctx));
                    }
                }
                10 => {
                    ctx.var.reset_var("NOWEX")?;

                    *ctx.var.ref_int("SELECTCOM", &[])? = *com_no as i64;

                    call_event!(vm, EventType::Com, tx, ctx)
                }
                11 => {
                    call!(vm, &format!("COM{com_no}"), tx, ctx)
                }
                12 => {
                    if ctx.var.get_result() == 0 {
                        *phase = 2;
                        return Ok(None);
                    }

                    call!(vm, "SOURCE_CHECK", tx, ctx)
                }
                13 => {
                    ctx.var.reset_var("SOURCE")?;
                    call_event!(vm, EventType::ComEnd, tx, ctx)
                }
                14 => {
                    *phase = 2;
                    return Ok(None);
                }
                _ => return Ok(Some(Workflow::Return)),
            },
            CallTrain(commands, current_com) => {
                let current_com = match current_com {
                    Some(com) => *com,
                    None => match commands.pop() {
                        Some(com) => {
                            *current_com = Some(com);
                            com
                        }
                        None => return Ok(Some(Workflow::Return)),
                    },
                };

                match *phase {
                    0 => {
                        call!(vm, "SHOW_STATUS", tx, ctx)
                    }
                    1 => {
                        ctx.var.prepare_train_data()?;
                        ctx.var.reset_var("NOWEX")?;
                        *ctx.var.ref_int("SELECTCOM", &[])? = current_com as i64;
                        call_event!(vm, EventType::Com, tx, ctx)
                    }
                    2 => {
                        if ctx.var.get_result() == 0 {
                            *phase = 0;
                            return Ok(None);
                        } else {
                            call!(vm, "SOURCE_CHECK", tx, ctx)
                        }
                    }
                    3 => {
                        call!(vm, &format!("COM{current_com}"), tx, ctx)
                    }
                    4 => {
                        call_event!(vm, EventType::ComEnd, tx, ctx)
                    }
                    5 => {
                        ctx.var.reset_var("SOURCE")?;
                        *phase = 0;
                        return Ok(None);
                    }
                    _ => Some(Workflow::Return),
                }
            }
            _ => bail!("TODO: {self:?}"),
        };

        *phase += 1;

        Ok(ret)
    }
}
