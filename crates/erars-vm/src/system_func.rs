use anyhow::{bail, Result};
use erars_ast::{BeginType, EventType};
use erars_ui::{InputRequest, InputRequestType, VirtualConsole};
use hashbrown::HashMap;
use rayon::prelude::*;

use crate::{variable::SerializableVariableStorage, TerminalVm, VmContext, Workflow};

#[derive(strum::Display, Debug, Clone, derivative::Derivative)]
#[derivative(PartialEq, Eq)]
pub enum SystemState {
    LoadGame(
        #[derivative(PartialEq = "ignore")] Option<HashMap<usize, SerializableVariableStorage>>,
    ),
    SaveGame(
        #[derivative(PartialEq = "ignore")] Option<HashMap<usize, SerializableVariableStorage>>,
        Option<usize>,
    ),
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
        match $vm.call_event($ty, $tx, $ctx, 0, 0)? {
            Workflow::Return => None,
            other => Some(other),
        }
    };
}

#[inline]
const fn exit() -> Option<Workflow> {
    Some(Workflow::Exit)
}

#[inline]
const fn ret() -> Option<Workflow> {
    Some(Workflow::Return)
}

#[inline]
const fn begin(ty: BeginType) -> Option<Workflow> {
    Some(Workflow::Begin(ty))
}

fn input_int(tx: &mut VirtualConsole) -> Option<Workflow> {
    Some(Workflow::Input {
        req: InputRequest::normal(tx.input_gen(), InputRequestType::Int),
        set_result: false,
    })
}

impl SystemState {
    pub fn run(
        &mut self,
        vm: &TerminalVm,
        tx: &mut VirtualConsole,
        ctx: &mut VmContext,
        phase: &mut usize,
    ) -> Result<Option<Workflow>> {
        use SystemState::*;

        let ret = match self {
            BeginTitle => match vm.try_call("SYSTEM_TITLE", &[], tx, ctx)? {
                None => bail!("TODO: default title"),
                Some(ret) => return Ok(Some(ret)),
            },
            BeginFirst => call_event!(vm, EventType::First, tx, ctx),
            BeginAfterTrain => call_event!(vm, EventType::End, tx, ctx),
            BeginTurnEnd => call_event!(vm, EventType::TurnEnd, tx, ctx),
            BeginAblUp => match phase {
                0 => call!(vm, "SHOW_JUEL", tx, ctx),
                1 => call!(vm, "SHOW_ABLUP_SELECT", tx, ctx),
                2 => input_int(tx),
                3 => match ctx.pop_int()? {
                    i @ 0..=99 => {
                        call!(vm, &format!("ABLUP{i}"), tx, ctx)
                    }
                    _ => {
                        call!(vm, "USERABLUP", tx, ctx)
                    }
                },
                4 => {
                    *phase = 2;
                    input_int(tx)
                }
                _ => ret(),
            },
            BeginShop => match *phase {
                0 => call_event!(vm, EventType::Shop, tx, ctx),
                1 => call!(vm, "SHOW_SHOP", tx, ctx),
                2 => input_int(tx),
                3 => {
                    let i = ctx.pop_int()?;

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
                _ => ret(),
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
                        if ctx.config.printc_count != 0
                            && *printc_count == ctx.config.printc_count as u32
                        {
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
                    let no = ctx.pop_int()?;

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
                _ => ret(),
            },
            CallTrain(commands, current_com) => {
                let current_com = match current_com {
                    Some(com) => *com,
                    None => match commands.pop() {
                        Some(com) => {
                            *current_com = Some(com);
                            com
                        }
                        None => return Ok(exit()),
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
                    _ => ret(),
                }
            }
            SaveGame(savs, idx) => {
                match phase {
                    0 => {
                        let new_savs = load_savs(vm, ctx);
                        print_sav_data_list(&new_savs, tx);
                        *savs = Some(new_savs);
                        input_int(tx)
                    }
                    1 => {
                        let savs = savs.as_mut().unwrap();
                        match ctx.pop_int()? {
                            100 => return Ok(ret()),
                            i if i >= 0 && i < SAVE_COUNT as i64 => {
                                *idx = Some(i as usize);
                                if savs.contains_key(&(i as usize)) {
                                    tx.print_line(format!("SAVE {i} already exists. Overwrite?"));
                                    tx.print_line("[0] Yes [1] No".into());
                                    input_int(tx)
                                } else {
                                    *phase = 2;
                                    None
                                }
                            }
                            _ => {
                                *phase = 0;
                                input_int(tx)
                            }
                        }
                    }
                    2 => {
                        match ctx.pop_int()? {
                            // YES
                            0 => None,
                            // NO
                            1 => {
                                *phase = 0;
                                input_int(tx)
                            }
                            _ => {
                                *phase = 1;
                                input_int(tx)
                            }
                        }
                    }
                    3 => {
                        ctx.put_form_enabled = true;
                        call!(vm, "SAVEINFO", tx, ctx)
                    }
                    4 => {
                        ctx.put_form_enabled = false;
                        let description = std::mem::take(ctx.var.ref_str("SAVEDATA_TEXT", &[])?);
                        let idx = idx.unwrap();
                        let var = ctx.var.get_serializable(&ctx.header_info, description);
                        savs.as_mut().unwrap().insert(idx, var);
                        *phase = 0;
                        return Ok(None);
                    }
                    _ => ret(),
                }
            }
            DoTrain(_) => bail!("TODO: DoTrain"),
            LoadGame(savs) => {
                match savs {
                    None => {
                        let new_savs = load_savs(vm, ctx);
                        print_sav_data_list(&new_savs, tx);
                        *savs = Some(new_savs);
                    }
                    Some(savs) => match ctx.pop_int()? {
                        100 => return Ok(exit()),
                        i if i >= 0 && i < SAVE_COUNT as i64 => {
                            if let Some(sav) = savs.remove(&(i as usize)) {
                                return Ok(Some(Workflow::SwitchState(SystemState::LoadData(
                                    i,
                                    Some(sav),
                                ))));
                            }
                        }
                        _ => {}
                    },
                }
                input_int(tx)
            }
            LoadData(idx, sav) => match phase {
                0 => {
                    let sav = sav.take().unwrap();
                    ctx.lastload_text = sav.description.clone();
                    ctx.lastload_no = *idx as _;
                    ctx.lastload_version = sav.version;
                    ctx.var.load_serializable(sav, &ctx.header_info)?;
                    call!(vm, "SYSTEM_LOADEND", tx, ctx)
                }
                1 => call_event!(vm, EventType::Load, tx, ctx),
                2 => begin(BeginType::Shop),
                _ => exit(),
            },
        };

        *phase = phase.saturating_add(1);

        Ok(ret)
    }
}

const SAVE_COUNT: usize = 20;

fn load_savs(vm: &TerminalVm, ctx: &mut VmContext) -> HashMap<usize, SerializableVariableStorage> {
    (0..SAVE_COUNT)
        .into_par_iter()
        .filter_map(|i| {
            crate::save_data::read_save_data(&vm.sav_path(), &ctx.header_info, i as i64)
                .ok()
                .map(|d| (i, d))
        })
        .collect()
}

fn print_sav_data_list(
    savs: &HashMap<usize, SerializableVariableStorage>,
    tx: &mut VirtualConsole,
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
