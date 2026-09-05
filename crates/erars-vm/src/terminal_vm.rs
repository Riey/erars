mod cells;
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
                        &*ctx.header_info,
                        ctx.var.interner().get_or_intern_static("FORMS.ERB"),
                    );
                    // `STRFORM` hands user data straight to this path, so a
                    // malformed form string has to be an error, not a panic —
                    // Emuera reports `InvalidFormString`
                    // (`_Library/EvilMask/Lang.cs:1150`,
                    // `Creator.Method.cs:4855-4870`).
                    let expr = match erars_compiler::normal_form_str(&parser_ctx)(&form) {
                        Ok((_, expr)) => expr,
                        Err(err) => {
                            bail!("STRFORM関数: 文字列\"{form}\"の展開エラー: {err}")
                        }
                    };
                    let insts = match erars_compiler::compile_expr(expr) {
                        Ok(insts) => Vec::from(insts),
                        Err(err) => {
                            bail!("STRFORM関数: 文字列\"{form}\"の展開エラー: {err}")
                        }
                    };

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
                                // The instruction after the failed jump reads
                                // this flag; without the advance the same
                                // lookup would be retried forever.
                                ctx.push(false);
                                cursor += 1;
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
        args: &[LocalValue],
        tx: &mut VirtualConsole,
        ctx: &mut VmContext,
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

        let ret = self.run_body(label, body, tx, ctx)?;

        ctx.end_func(label);

        Ok(ret)
    }

    #[inline]
    fn call(
        &self,
        label: impl StrKeyLike,
        args: &[LocalValue],
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
        args: &[LocalValue],
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
                    // `@EXIT` and `@REBOOT` close the window
                    // (`GameView/EmueraConsole.cs:1357-1361`,
                    // `Forms/MainWindow.cs:807-812`). The command runs several
                    // frames below, inside an input request, so it ends the
                    // run by raising this marker rather than by returning a
                    // workflow no input path could carry.
                    if err.downcast_ref::<crate::debug_console::DebugConsoleQuit>().is_some() {
                        log::info!("Run {err}");
                        ctx.redraw(tx).ok();
                        break true;
                    }

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

                    ctx.redraw(tx).ok();
                    break false;
                }
            }
        }
    }
}

/// Emuera `ExpressionMediator.CreateBar` (`ExpressionMediator.cs:122-145`).
///
/// The three range checks are Emuera's own, `TooLongBar` existing to stop a
/// runaway script from filling the console. The fill count is Int64 arithmetic
/// truncated to 32 bits by an `unchecked` cast, then clamped into `[0, length]`.
fn make_bar_str(replace: &ReplaceInfo, var: i64, max: i64, length: i64) -> anyhow::Result<String> {
    anyhow::ensure!(max > 0, "BARの最大値が正の値ではありません");
    anyhow::ensure!(length > 0, "BARの長さが正の値ではありません");
    anyhow::ensure!(length < 100, "BARが長すぎます");

    let bar_length = (var.wrapping_mul(length).wrapping_div(max) as i32 as i64).clamp(0, length);
    let blank = length - bar_length;

    let mut ret = String::with_capacity(
        2 + bar_length as usize * replace.bar_str1.len() + blank as usize * replace.bar_str2.len(),
    );

    ret.push('[');

    for _ in 0..bar_length {
        ret.push_str(&replace.bar_str1);
    }

    for _ in 0..blank {
        ret.push_str(&replace.bar_str2);
    }

    ret.push(']');

    Ok(ret)
}

/// Emuera `VariableEvaluator.ShiftArray` (`VariableEvaluator.cs:522-593` for
/// integers, `:595-665` for strings — the two bodies are identical apart from
/// the element type).
///
/// `shift` is signed: a positive shift moves the window's elements toward
/// higher indices and refills the leading `shift` slots with `empty_value`, a
/// negative one moves them toward lower indices and refills the trailing
/// `|shift|` slots. When the shift is at least as wide as the window every slot
/// in it becomes `empty_value`. Only `arr[start..start + num]` is touched;
/// nothing outside the window moves.
fn array_shift<T: Clone>(
    arr: &mut [T],
    empty_value: T,
    shift: i64,
    start: usize,
    num: Option<usize>,
) -> anyhow::Result<()> {
    ensure!(
        start < arr.len(),
        "命令ARRAYSHIFTの第4引数({start})が配列の範囲を超えています"
    );

    let rest = arr.len() - start;
    let num = num.map_or(rest, |n| n.min(rest));
    let abs_shift = shift.unsigned_abs() as usize;

    // `:538-543`: a shift wider than the whole array clears it outright, but
    // only when the window really is the whole array.
    if abs_shift >= arr.len() && start == 0 && num >= arr.len() {
        arr.fill(empty_value);
        return Ok(());
    }

    let window = &mut arr[start..start + num];

    // `:557-580`: a window no wider than the shift keeps nothing at all.
    let Some(length) = num.checked_sub(abs_shift).filter(|&l| l > 0) else {
        window.fill(empty_value);
        return Ok(());
    };

    if shift > 0 {
        window.rotate_right(abs_shift);
        window[..abs_shift].fill(empty_value);
    } else {
        window.rotate_left(abs_shift);
        window[length..].fill(empty_value);
    }

    Ok(())
}

#[test]
fn shift_test() {
    // Positive shift: the window slides toward higher indices and the vacated
    // leading slots take the default.
    let mut arr = [1, 1, 1, 1];
    array_shift(&mut arr, 0, 2, 1, None).unwrap();
    k9::assert_equal!(arr, [1, 0, 0, 1]);

    // A shift wider than the window clears just the window, not the array.
    arr.fill(1);
    array_shift(&mut arr, 0, 10, 1, Some(2)).unwrap();
    k9::assert_equal!(arr, [1, 0, 0, 1]);

    // Negative shift: `EVENT_NIGHT2.ERB:2453` does `ARRAYSHIFT LOCAL, -1, …, 1, 5`,
    // sliding the window toward lower indices and defaulting its last slot.
    let mut arr = [0, 1, 2, 3, 4, 5, 6, 7];
    array_shift(&mut arr, 9, -1, 1, Some(5)).unwrap();
    k9::assert_equal!(arr, [0, 2, 3, 4, 5, 9, 6, 7]);

    // A shift at least as wide as the whole array, with the window covering it,
    // clears everything (`VariableEvaluator.cs:538-543`).
    let mut arr = [1, 2, 3, 4];
    array_shift(&mut arr, 7, 4, 0, None).unwrap();
    k9::assert_equal!(arr, [7, 7, 7, 7]);

    // Out-of-range start is an error, not a silent clamp.
    let mut arr = [1, 2];
    assert!(array_shift(&mut arr, 0, 1, 2, None).is_err());
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

/// memmove-style block move: copy `count` elements from `start` to `move_to`
/// (overlapping allowed, like C `memmove`). `T: Clone` because the source
/// block is staged before the destination is overwritten (ARRAYMOVE).
fn array_move<T: Clone>(
    arr: &mut [T],
    move_to: usize,
    count: usize,
    start: usize,
) -> anyhow::Result<()> {
    ensure!(start < arr.len(), "ARRAYMOVE start value exceed");
    ensure!(move_to <= arr.len(), "ARRAYMOVE move_to value exceed");

    let count = count.min(arr.len() - start);
    let move_to = move_to.min(arr.len() - count);
    let tmp: Vec<T> = arr[start..start + count].to_vec();

    for (idx, value) in tmp.into_iter().enumerate() {
        arr[move_to + idx] = value;
    }

    Ok(())
}
