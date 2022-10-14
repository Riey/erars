mod executor;

use std::{
    collections::BTreeSet,
    path::{Path, PathBuf},
};

use crate::*;
use crate::{context::FunctionIdentifier, variable::StrKeyLike};
use anyhow::{anyhow, bail, Result};
use erars_ast::{
    BeginType, BinaryOperator, BuiltinCommand, BuiltinMethod, BuiltinVariable, EventType,
    InlineValue, PrintFlags, StrKey, UnaryOperator, Value,
};
use erars_compiler::{Instruction, ParserContext, ReplaceInfo};
use erars_ui::{FontStyle, InputRequest, InputRequestType, Timeout, VirtualConsole};
use itertools::Itertools;
use rand::Rng;

macro_rules! report_error {
    ($tx:expr, $($t:tt)+) => {
        log::error!($($t)+);
        $tx.print_line(format!($($t)+));
    };
}

pub struct TerminalVm {
    pub dic: FunctionDic,
    pub sav_path: PathBuf,
}

#[derive(Debug)]
enum InstructionWorkflow {
    Normal,
    Exit,
    Goto(u32),
    GotoLabel {
        label: StrKey,
        is_try: bool,
    },
    Return,
    Begin(BeginType),
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
        set_result: bool,
    },
    Redraw,
}

impl TerminalVm {
    pub fn new(function_dic: FunctionDic, game_path: PathBuf) -> Self {
        Self {
            dic: function_dic,
            sav_path: game_path.join("sav"),
        }
    }

    pub fn sav_path(&self) -> &Path {
        &self.sav_path
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
        let func_name = func_identifier.get_key(&ctx.var);

        while let Some(inst) = insts.get(cursor) {
            cursor += 1;
            use InstructionWorkflow::*;

            log::trace!(
                "[{func_name}] `{inst:?}[{cursor}]`, stack: {stack:?}, call_stack: {call_stack:?}",
                func_name = ctx.var.resolve_key(func_name),
                stack = ctx.stack(),
                call_stack = ctx.call_stack(),
            );

            match executor::run_instruction(self, func_name, inst, tx, ctx) {
                Ok(Normal) => {}
                Ok(Exit) => return Ok(Workflow::Exit),
                Ok(Goto(pos)) => {
                    cursor = pos as usize;
                }
                Ok(GotoLabel { label, is_try }) => {
                    match body.goto_labels().iter().find_map(|(cur_label, pos)| {
                        if *cur_label == label {
                            Some(*pos)
                        } else {
                            None
                        }
                    }) {
                        Some(pos) => {
                            cursor = pos as usize;
                        }
                        None => {
                            if is_try {
                                ctx.push(false);
                            } else {
                                bail!(
                                    "Label {label} is not founded",
                                    label = ctx.var.resolve_key(label)
                                );
                            }
                        }
                    }
                }
                Ok(Begin(ty)) => return Ok(Workflow::Begin(ty)),
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
                            if !is_jump {
                                ctx.pop_call_stack();
                            }
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
                Ok(Input { req, set_result }) => {
                    ctx.push_current_call_stack(
                        func_identifier.clone(),
                        body.file_path().clone(),
                        cursor,
                    );
                    return Ok(Workflow::Input { req, set_result });
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
        label: StrKey,
        args: &[Value],
        tx: &mut VirtualConsole,
        ctx: &mut VmContext,
        body: &FunctionBody,
    ) -> Result<Workflow> {
        log::trace!(
            "CALL {label}({args:?})",
            label = ctx.var.interner().resolve(&label)
        );

        let mut args = args.iter().cloned();

        for (var_idx, default_value, arg_indices) in body.args().iter() {
            let (info, var) = ctx.var.get_maybe_local_var(label, *var_idx)?;
            let var = var.assume_normal();
            let idx = info.calculate_single_idx(arg_indices).1;

            let arg = args.next().or_else(|| {
                default_value.clone().map(|v| match v {
                    InlineValue::Int(i) => Value::Int(i),
                    InlineValue::String(s) => Value::String(s.resolve().into()),
                })
            });

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

        let ret = self.run_body(FunctionIdentifier::Normal(label), body, tx, ctx, 0)?;

        Ok(ret)
    }

    // #[inline]
    // fn call(
    //     &self,
    //     label: &str,
    //     args: &[Value],
    //     tx: &mut VirtualConsole,
    //     ctx: &mut VmContext,
    // ) -> Result<Workflow> {
    //     self.call_internal(label, args, tx, ctx, self.dic.get_func(label)?)
    // }

    #[inline]
    pub fn try_call(
        &self,
        label: &str,
        args: &[Value],
        tx: &mut VirtualConsole,
        ctx: &mut VmContext,
    ) -> Result<Option<Workflow>> {
        match ctx.var.interner().get(label) {
            Some(label) => match self.dic.get_func_opt(label) {
                Some(body) => self.call_internal(label, args, tx, ctx, body).map(Some),
                None => Ok(None),
            },
            None => Ok(None),
        }
    }

    pub fn call_event(
        &self,
        ty: EventType,
        tx: &mut VirtualConsole,
        ctx: &mut VmContext,
        event_idx: usize,
        event_cursor: usize,
    ) -> Result<Workflow> {
        self.dic.get_event(ty).run(event_idx, |body, idx| {
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
        let call_stack_base = ctx.last_call_stack_base();

        while let Some(stack) = ctx.pop_call_stack_check(call_stack_base) {
            log::info!("{stack:?}");
            match stack.func_name {
                FunctionIdentifier::Normal(name) => {
                    let body = self.dic.get_func(name)?;
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

    fn run_state_internal(&self, tx: &mut VirtualConsole, ctx: &mut VmContext) -> Result<VmResult> {
        let ret = self.try_run_call_stack(tx, ctx)?;
        log::info!("Callstack ret: {ret:?}");
        match ret {
            Workflow::Return => {}
            Workflow::Begin(ty) => {
                ctx.clear_state();
                ctx.clear_call_stack();
                ctx.push_state(ty.into(), 0);
            }
            Workflow::Exit => {
                return Ok(VmResult::Exit);
            }
            Workflow::Redraw => return Ok(VmResult::Redraw),
            Workflow::Input { req, set_result } => {
                return Ok(VmResult::NeedInput { req, set_result })
            }
            Workflow::SwitchState(new_state) => {
                ctx.push_state(new_state, ctx.stack().len());
            }
            Workflow::GotoState(new_state) => {
                ctx.push_state(new_state, ctx.stack().len());
            }
        }

        let (mut current_state, mut phase, mut call_stack_base) = match ctx.pop_state() {
            Some(s) => s,
            None => return Ok(VmResult::Exit),
        };

        let ret = loop {
            let prev_phase = phase;
            let ret = match current_state.run(self, tx, ctx, &mut phase) {
                Ok(ret) => ret,
                Err(err) => {
                    ctx.push_state_with_phase(current_state, phase, call_stack_base);
                    return Err(err);
                }
            };
            log::info!("Run {current_state:?}[{prev_phase}] -> [{phase}] {ret:?}");
            match ret {
                Some(Workflow::Return) => {
                    match ctx.pop_state() {
                        Some(s) => {
                            current_state = s.0;
                            phase = s.1;
                            call_stack_base = s.2;
                        }
                        None => return Ok(VmResult::Exit),
                    };
                }
                Some(Workflow::Exit) => {
                    ctx.clear_state();
                    ctx.clear_call_stack();
                    return Ok(VmResult::Exit);
                }
                Some(Workflow::Redraw) => {
                    break Ok(VmResult::Redraw);
                }
                Some(Workflow::Input { req, set_result }) => {
                    break Ok(VmResult::NeedInput { req, set_result })
                }
                Some(Workflow::Begin(ty)) => {
                    ctx.clear_state();
                    ctx.clear_call_stack();
                    current_state = ty.into();
                    phase = 0;
                }
                Some(Workflow::GotoState(new_state)) => {
                    current_state = new_state;
                    phase = 0;
                }
                Some(Workflow::SwitchState(new_state)) => {
                    ctx.push_state_with_phase(current_state, phase, ctx.stack().len());
                    current_state = new_state;
                    phase = 0;
                    call_stack_base = ctx.stack().len();
                }
                None => {
                    continue;
                }
            }
        };

        ctx.push_state_with_phase(current_state, phase, call_stack_base);

        ret
    }

    pub fn run_state(&self, tx: &mut VirtualConsole, ctx: &mut VmContext) -> VmResult {
        match self.run_state_internal(tx, ctx) {
            Ok(ret) => ret,
            Err(err) => {
                report_error!(tx, "VM error occurred: {err}");

                while let Some((state, phase, call_stack_base)) = ctx.pop_state() {
                    while let Some(call_stack) = ctx.pop_call_stack_check(call_stack_base) {
                        report_error!(
                            tx,
                            "At function {func} {file}@{line}",
                            func = call_stack.func_name.resolve_key(&ctx.var),
                            file = call_stack.file_path,
                            line = call_stack.script_position.line
                        );
                    }

                    report_error!(tx, "At state {state:?}[{phase}]");
                }

                VmResult::Exit
            }
        }
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
