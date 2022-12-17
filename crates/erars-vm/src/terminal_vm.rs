mod executor;

use std::collections::BTreeSet;

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
}

impl From<Workflow> for InstructionWorkflow {
    fn from(w: Workflow) -> Self {
        Self::Workflow(w)
    }
}

#[derive(Debug)]
enum InstructionWorkflow {
    Normal,
    Workflow(Workflow),
    EvalFormString(String),
    Goto(u32),
    GotoLabel { label: StrKey, is_try: bool },
}

impl TerminalVm {
    pub fn new(function_dic: FunctionDic) -> Self {
        Self { dic: function_dic }
    }

    fn run_body(
        &self,
        func_identifier: FunctionIdentifier,
        body: &FunctionBody,
        tx: &mut VirtualConsole,
        ctx: &mut VmContext,
    ) -> Result<Workflow> {
        let mut cursor = 0;
        let insts = body.body();
        let func_name = func_identifier.get_key(&ctx.var);

        while let Some(inst) = insts.get(cursor).copied() {
            use InstructionWorkflow::*;

            log::trace!(
                "[{func_name}] `{inst:?}[{cursor}]`, stack: {stack:?}, call_stack: {call_stack:?}",
                func_name = ctx.var.resolve_key(func_name),
                stack = ctx.stack(),
                call_stack = ctx.call_stack(),
            );

            match executor::run_instruction(self, func_name, inst, tx, ctx) {
                Ok(Normal) => {
                    cursor += 1;
                }
                Ok(EvalFormString(form)) => {
                    let parser_ctx = ParserContext::new(
                        ctx.header_info.clone(),
                        ctx.var.interner().get_or_intern_static("FORMS.ERB"),
                    );
                    let expr = erars_compiler::normal_form_str(&parser_ctx)(&form).unwrap().1;
                    let insts = Vec::from(erars_compiler::compile_expr(expr).unwrap());

                    for inst in insts {
                        match executor::run_instruction(self, func_name, inst, tx, ctx)? {
                            InstructionWorkflow::Normal => {}
                            _ => bail!("EvalFromString can't do flow control"),
                        }
                    }
                    cursor += 1;
                }
                Ok(Goto(pos)) => {
                    cursor = pos as usize;
                }
                Ok(GotoLabel { label, is_try }) => {
                    match body
                        .goto_labels()
                        .iter()
                        .find_map(|FunctionGotoLabel(cur_label, pos)| {
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
                Ok(Workflow(flow)) => return Ok(flow),
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
        label: FunctionIdentifier,
        args: &[Value],
        tx: &mut VirtualConsole,
        ctx: &mut VmContext,
        body: &FunctionBody,
    ) -> Result<Workflow> {
        log::trace!("CALL {label}({args:?})",);

        let mut args = args.iter().cloned();

        for FunctionArgDef(var_idx, arg_indices, default_value) in body.args().iter() {
            let (info, var) = ctx.var.get_maybe_local_var(label, *var_idx)?;
            let var = var.assume_normal();
            let idx = info.calculate_single_idx(arg_indices).1 as usize;

            let arg = args.next().or_else(|| {
                default_value.clone().map(|v| match v {
                    InlineValue::Int(i) => Value::Int(i),
                    InlineValue::String(s, _) => Value::String(s.resolve().into()),
                })
            });

            if info.is_str {
                var.as_str()?[idx] = match arg {
                    Some(Value::String(s)) => s,
                    _ => String::new(),
                };
            } else {
                var.as_int()?[idx] = match arg {
                    Some(Value::Int(s)) => s,
                    _ => 0,
                };
            }
        }

        ctx.new_func(label, body.file_path);

        let ret = self.run_body(label, body, tx, ctx)?;

        ctx.end_func(label);

        Ok(ret)
    }

    #[inline]
    fn call(
        &self,
        label: impl StrKeyLike,
        args: &[Value],
        tx: &mut VirtualConsole,
        ctx: &mut VmContext,
    ) -> Result<Workflow> {
        let label = label.get_key(&ctx.var);
        self.call_internal(
            FunctionIdentifier::Normal(label),
            args,
            tx,
            ctx,
            self.dic.get_func(label)?,
        )
    }

    #[inline]
    pub fn try_call(
        &self,
        label: impl StrKeyLike,
        args: &[Value],
        tx: &mut VirtualConsole,
        ctx: &mut VmContext,
    ) -> Result<Option<Workflow>> {
        let label = label.get_key(&ctx.var);
        match self.dic.get_func_opt(label) {
            Some(body) => self
                .call_internal(FunctionIdentifier::Normal(label), args, tx, ctx, body)
                .map(Some),
            None => Ok(None),
        }
    }

    pub fn call_event(
        &self,
        ty: EventType,
        tx: &mut VirtualConsole,
        ctx: &mut VmContext,
    ) -> Result<Workflow> {
        for body in self.dic.get_event(ty).iter() {
            match self.run_body(FunctionIdentifier::Event(ty), body, tx, ctx)? {
                Workflow::Return => {}
                other => return Ok(other),
            }
        }

        Ok(Workflow::Return)
    }

    /// Return: Is this normal exit
    pub fn start(&self, tx: &mut VirtualConsole, ctx: &mut VmContext) -> bool {
        let mut begin_ty = Some(BeginType::Title);
        loop {
            let current_ty = match begin_ty.take() {
                Some(ty) => ty,
                None => break true,
            };
            match executor::run_begin(self, current_ty, tx, ctx) {
                Ok(Workflow::Begin(ty)) => {
                    begin_ty = Some(ty);
                }
                Ok(Workflow::Return) | Ok(Workflow::Exit) => {
                    break true;
                }
                Err(err) => {
                    report_error!(tx, "VM error occurred: {err}");

                    while let Some(call_stack) = ctx.pop_call_stack() {
                        report_error!(
                            tx,
                            "At function {func} {file}@{line}",
                            func = call_stack.func_name.resolve_key(&ctx.var),
                            file = call_stack.file_path,
                            line = call_stack.script_position.line
                        );
                    }

                    break false;
                }
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
