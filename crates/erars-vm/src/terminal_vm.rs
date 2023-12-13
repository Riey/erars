mod executor;

use std::collections::BTreeSet;
use std::sync::Arc;

use crate::*;
use crate::{context::FunctionIdentifier, variable::StrKeyLike};
use anyhow::{anyhow, bail, ensure, Context, Result};
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
    pub header: Arc<HeaderInfo>,
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
    pub fn new(function_dic: FunctionDic, header: Arc<HeaderInfo>) -> Self {
        Self {
            dic: function_dic,
            header,
        }
    }

    async fn run_body<S: SystemFunctions>(
        &self,
        func_identifier: FunctionIdentifier,
        body: &FunctionBody,
        tx: &mut VirtualConsole,
        ctx: &mut VmContext<S>,
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

            match executor::run_instruction(self, func_name, inst, tx, ctx).await {
                Ok(Normal) => {
                    cursor += 1;
                }
                Ok(EvalFormString(form)) => {
                    let parser_ctx = ParserContext::new(
                        &*ctx.header_info,
                        ctx.var.interner().get_or_intern_static("FORMS.ERB"),
                    );
                    let expr = erars_compiler::normal_form_str(&parser_ctx)(&form).unwrap().1;
                    let insts = Vec::from(erars_compiler::compile_expr(expr).unwrap());

                    for inst in insts {
                        match executor::run_instruction(self, func_name, inst, tx, ctx).await? {
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

    async fn call_internal<S: SystemFunctions>(
        &self,
        label: FunctionIdentifier,
        args: &[LocalValue],
        tx: &mut VirtualConsole,
        ctx: &mut VmContext<S>,
        body: &FunctionBody,
    ) -> Result<Workflow> {
        log::debug!("CALL {label}({args:?})",);

        let mut args = args.iter().cloned();

        for FunctionArgDef(var_idx, arg_indices, default_value) in body.args().iter() {
            let (info, var) = ctx.var.get_maybe_local_var(label, *var_idx)?;
            if info.is_ref {
                ensure!(arg_indices.is_empty(), "Can't use index for ref var");
                ensure!(
                    default_value.is_none(),
                    "Can't use default value for ref var"
                );

                let arg = args.next().context("Empty args for ref var")?;

                match arg {
                    LocalValue::VarRef(var_ref) => {
                        var.assume_normal().as_int()?[0] = unsafe {
                            std::mem::transmute((var_ref.name.to_u32(), var_ref.func_name.to_u32()))
                        };
                    }
                    _ => bail!("Invalid arg for ref var"),
                }
            } else {
                let var = var.assume_normal();
                let idx = info.calculate_single_idx(arg_indices).1;

                let arg = match args.next() {
                    Some(LocalValue::VarRef(var_ref)) => {
                        let src = ctx.read_var_ref(&var_ref)?;
                        let (_info, var) =
                            ctx.var.get_maybe_local_var(label, *var_idx).context("Set argument")?;
                        var.assume_normal()
                            .set(idx, src)
                            .with_context(|| format!("Set argument {var_idx}"))?;
                        continue;
                    }
                    Some(LocalValue::Value(v)) => Some(v),
                    Some(LocalValue::InternedStr(s)) => Some(Value::String(s.to_string())),
                    None => default_value.clone().map(|v| match v {
                        InlineValue::Int(i) => Value::Int(i),
                        InlineValue::String(s, _) => Value::String(s.resolve().into()),
                    }),
                };

                var.set_or_default(idx, arg).context("Set argument")?;
            }
        }

        ensure!(args.next().is_none(), "Too many args");

        ctx.new_func(label, body.file_path);

        let ret = self.run_body(label, body, tx, ctx).await?;

        ctx.end_func(label);

        Ok(ret)
    }

    #[inline]
    async fn call<S: SystemFunctions>(
        &self,
        label: impl StrKeyLike,
        args: &[LocalValue],
        tx: &mut VirtualConsole,
        ctx: &mut VmContext<S>,
    ) -> Result<Workflow> {
        let label = label.get_key(&ctx.var);
        self.call_internal(
            FunctionIdentifier::Normal(label),
            args,
            tx,
            ctx,
            self.dic.get_func(label)?,
        ).await
    }

    #[inline]
    pub async fn try_call<S: SystemFunctions>(
        &self,
        label: impl StrKeyLike,
        args: &[LocalValue],
        tx: &mut VirtualConsole,
        ctx: &mut VmContext<S>,
    ) -> Result<Option<Workflow>> {
        let label = label.get_key(&ctx.var);
        match self.dic.get_func_opt(label) {
            Some(body) => self
                .call_internal(FunctionIdentifier::Normal(label), args, tx, ctx, body)
                .await
                .map(Some),
            None => Ok(None),
        }
    }

    pub async fn call_event<S: SystemFunctions>(
        &self,
        ty: EventType,
        tx: &mut VirtualConsole,
        ctx: &mut VmContext<S>,
    ) -> Result<Workflow> {
        for body in self.dic.get_event(ty).iter() {
            match self.run_body(FunctionIdentifier::Event(ty), body, tx, ctx).await? {
                Workflow::Return => {}
                other => return Ok(other),
            }
        }

        Ok(Workflow::Return)
    }

    /// Return: Is this normal exit
    pub async fn start<S: SystemFunctions>(&self, tx: &mut VirtualConsole, ctx: &mut VmContext<S>) -> bool {
        let mut begin_ty = Some(BeginType::Title);
        loop {
            let current_ty = match begin_ty.take() {
                Some(ty) => ty,
                None => break true,
            };
            match executor::run_begin(self, current_ty, tx, ctx).await {
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

                    ctx.system.redraw(tx).await.ok();
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
    shift: usize,
    start: usize,
    end: usize,
) -> anyhow::Result<()> {
    ensure!(start < arr.len(), "ARRAYSHIFT start value exceed");

    let end = (start.saturating_add(end)).min(arr.len() - start);

    let arr = &mut arr[start..end];

    if shift < arr.len() {
        arr.rotate_right(shift);
        arr[..shift].fill(empty_value);
    } else {
        arr.fill(empty_value);
    }

    Ok(())
}

#[test]
fn shift_test() {
    let mut arr = [1, 1, 1, 1];
    array_shift(&mut arr, 0, 2, 1, usize::MAX).unwrap();
    k9::assert_equal!(arr, [1, 0, 0, 1]);
    arr.fill(1);
    array_shift(&mut arr, 0, 10, 1, 2).unwrap();
    k9::assert_equal!(arr, [1, 0, 0, 1]);
}

fn array_remove<T: Clone + Default>(
    arr: &mut [T],
    start: usize,
    count: usize,
) -> anyhow::Result<()> {
    ensure!(start < arr.len(), "ARRAYREMOVE start value exceed");

    let arr = &mut arr[start..];

    if count < arr.len() {
        let diff = arr.len() - count;
        arr.rotate_left(count);
        arr[diff..].fill(T::default());
    } else {
        arr.fill(T::default());
    }

    Ok(())
}
