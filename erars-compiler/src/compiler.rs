use crate::{CompileError, CompileResult, Instruction};
use erars_ast::{
    BinaryOperator, Expr, FormExpr, FormText, Function, FunctionHeader, SelectCaseCond, Stmt,
    StmtWithPos, Variable,
};
use hashbrown::HashMap;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompiledFunction {
    pub header: FunctionHeader,
    pub goto_labels: HashMap<SmolStr, u32>,
    pub body: Box<[Instruction]>,
}

struct Compiler {
    out: Vec<Instruction>,
    goto_labels: HashMap<SmolStr, u32>,
    continue_marks: Vec<u32>,
    break_marks: Vec<Vec<u32>>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            out: Vec::new(),
            goto_labels: HashMap::new(),
            continue_marks: Vec::new(),
            break_marks: Vec::new(),
        }
    }

    fn push(&mut self, inst: Instruction) {
        self.out.push(inst);
    }

    fn mark(&mut self) -> u32 {
        let ret = self.current_no();
        self.push(Instruction::Nop);
        ret
    }

    fn current_no(&self) -> u32 {
        self.out.len() as u32
    }

    fn insert(&mut self, mark: u32, inst: Instruction) {
        *self
            .out
            .get_mut(mark as usize)
            .unwrap_or_else(|| unreachable!("Invalid mark {}", mark)) = inst;
    }

    fn push_list(
        &mut self,
        args: impl IntoIterator<IntoIter = impl ExactSizeIterator + IntoIterator<Item = Expr>>,
    ) -> CompileResult<u32> {
        let args = args.into_iter();
        let len = args.len();
        for arg in args {
            self.push_expr(arg)?;
        }
        Ok(len as u32)
    }

    fn push_expr(&mut self, expr: Expr) -> CompileResult<()> {
        match expr {
            Expr::IncOpExpr {
                var,
                is_inc,
                is_pre,
            } => {
                let op = if is_inc {
                    BinaryOperator::Add
                } else {
                    BinaryOperator::Sub
                };

                if is_pre {
                    self.push_var(var.clone())?;
                    self.push(Instruction::LoadInt(1));
                    self.push(Instruction::BinaryOperator(op));
                    self.push(Instruction::Duplicate);
                    self.store_var(var.clone())?;
                } else {
                    self.push_var(var.clone())?;
                    self.push(Instruction::ReadVar);
                    self.push(Instruction::Duplicate);
                    self.push(Instruction::LoadInt(1));
                    self.push(Instruction::BinaryOperator(op));
                    self.store_var(var.clone())?;
                }
            }
            Expr::String(s) => self.push(Instruction::LoadStr(s)),
            Expr::Int(i) => self.push(Instruction::LoadInt(i)),
            Expr::BinopExpr(lhs, op, rhs) => {
                match op {
                    // short circuit
                    BinaryOperator::And => {
                        self.push_expr(*lhs)?;
                        let lhs_end = self.mark();
                        self.push_expr(*rhs)?;
                        let rhs_end = self.mark();
                        self.insert(lhs_end, Instruction::GotoIfNot(self.current_no()));
                        self.push(Instruction::LoadInt(0));
                        self.insert(rhs_end, Instruction::Goto(self.current_no()));
                    }
                    // short circuit
                    BinaryOperator::Or => {
                        self.push_expr(*lhs)?;
                        let lhs_end = self.mark();
                        self.push_expr(*rhs)?;
                        let rhs_end = self.mark();
                        self.insert(lhs_end, Instruction::GotoIf(self.current_no()));
                        self.push(Instruction::LoadInt(1));
                        self.insert(rhs_end, Instruction::Goto(self.current_no()));
                    }
                    _ => {
                        self.push_expr(*lhs)?;
                        self.push_expr(*rhs)?;
                        self.push(Instruction::BinaryOperator(op));
                    }
                }
            }
            Expr::UnaryopExpr(expr, op) => {
                self.push_expr(*expr)?;
                self.push(Instruction::UnaryOperator(op));
            }
            Expr::FormText(form) => {
                self.push_form(form)?;
            }
            Expr::CondExpr(cond, if_true, or_false) => {
                self.push_expr(*cond)?;
                let begin = self.mark();
                self.push_expr(*if_true)?;
                let true_end = self.mark();
                self.insert(begin, Instruction::GotoIfNot(true_end + 1));
                self.push_expr(*or_false)?;
                self.insert(true_end, Instruction::Goto(self.current_no()));
            }
            Expr::Method(name, args) => {
                let count = self.push_list(args)?;
                self.push(Instruction::LoadStr(name));
                self.push(Instruction::CallMethod(count));
            }
            Expr::Var(var) => {
                self.push_var(var)?;
            }
        }

        Ok(())
    }

    fn push_var_ref(&mut self, var: Variable) -> CompileResult<()> {
        let count = self.push_list(var.args)?;
        self.push(Instruction::LoadStr(var.var));
        match var.func_extern {
            Some(e) => {
                self.push(Instruction::LoadStr(e));
                self.push(Instruction::LoadExternVarRef(count));
            }
            None => {
                self.push(Instruction::LoadVarRef(count));
            }
        }
        Ok(())
    }

    fn store_var(&mut self, var: Variable) -> CompileResult<()> {
        self.push_var_ref(var)?;
        self.push(Instruction::StoreVar);

        Ok(())
    }

    fn push_var(&mut self, var: Variable) -> CompileResult<()> {
        self.push_var_ref(var)?;

        Ok(())
    }

    fn push_form(&mut self, form: FormText) -> CompileResult<()> {
        let count = 1 + form.other.len() as u32 * 2;
        self.push(Instruction::LoadStr(form.first.into()));
        for (
            FormExpr {
                expr,
                align,
                padding,
            },
            text,
        ) in form.other
        {
            self.push_expr(expr)?;
            if let Some(padding) = padding {
                self.push_expr(padding)?;
                self.push(Instruction::PadStr(align.unwrap_or_default()));
            }
            self.push(Instruction::LoadStr(text.into_boxed_str()));
        }
        self.push(Instruction::ConcatString(count));

        Ok(())
    }

    fn push_for(
        &mut self,
        var: Variable,
        init: Expr,
        end: Expr,
        step: Expr,
        body: Vec<StmtWithPos>,
    ) -> CompileResult<()> {
        // var = init;
        //
        // NOTE: These values are immutable
        //
        // let step = step;
        // let end = end;
        //
        // while var > end {
        //     body
        //     var += step;
        //     goto LOOP;
        // }

        self.push_expr(init)?;
        self.store_var(var.clone())?;
        self.push_expr(step)?;
        self.push(Instruction::ReadVar);
        self.push_expr(end)?;
        self.push(Instruction::ReadVar);

        let loop_mark = self.current_no();

        // compare var with end
        self.push_var(var.clone())?;
        self.push(Instruction::DuplicatePrev);
        self.push(Instruction::BinaryOperator(BinaryOperator::Less));

        let start_mark = self.begin_loop_block();

        for stmt in body {
            self.push_stmt_with_pos(stmt)?;
        }

        // add step to var
        self.push(Instruction::DuplicatePrev);
        self.push_var(var.clone())?;
        self.push(Instruction::BinaryOperator(BinaryOperator::Add));
        self.store_var(var)?;
        self.push(Instruction::Goto(loop_mark));

        self.insert(start_mark, Instruction::GotoIfNot(self.current_no()));

        self.end_loop_block();

        // remove pinned values
        self.push(Instruction::Pop);
        self.push(Instruction::Pop);

        Ok(())
    }

    #[inline]
    pub fn push_stmt_with_pos(&mut self, stmt: StmtWithPos) -> CompileResult<()> {
        self.push(Instruction::ReportPosition(stmt.1));
        self.push_stmt(stmt.0)
    }

    pub fn push_stmt(&mut self, stmt: Stmt) -> CompileResult<()> {
        match stmt {
            Stmt::Print(flags, text) => {
                self.push_expr(text)?;
                self.push(Instruction::Print(flags));
            }
            Stmt::PrintFormS(flags, text) => {
                self.push_expr(text)?;
                self.push(Instruction::EvalFormString);
                self.push(Instruction::Print(flags));
            }
            Stmt::PrintList(flags, args) => {
                let count = self.push_list(args)?;
                self.push(Instruction::ConcatString(count));
                self.push(Instruction::Print(flags));
            }
            Stmt::PrintData(flags, cond, list) => {
                match cond {
                    Some(cond) => self.push_expr(cond)?,
                    None => self.push_var(Variable {
                        var: "RAND".into(),
                        func_extern: None,
                        args: vec![Expr::Int(list.len() as _)],
                    })?,
                }

                let branch = self.current_no();

                for _ in 0..list.len() * 4 {
                    self.mark();
                }

                let mut line_positions = Vec::new();

                for (i, part) in list.into_iter().enumerate() {
                    let start = self.current_no();

                    self.insert(branch + i as u32 * 4 + 0, Instruction::Duplicate);
                    self.insert(branch + i as u32 * 4 + 1, Instruction::LoadInt(i as _));
                    self.insert(
                        branch + i as u32 * 4 + 2,
                        Instruction::BinaryOperator(BinaryOperator::NotEqual),
                    );
                    self.insert(branch + i as u32 * 4 + 3, Instruction::GotoIfNot(start));

                    for line in part {
                        self.push_expr(line)?;
                    }

                    self.push(Instruction::Print(flags));

                    let end = self.mark();
                    line_positions.push(end);
                }

                for end in line_positions {
                    self.insert(end, Instruction::Goto(self.current_no()));
                }
            }
            Stmt::ReuseLastLine(text) => {
                self.push(Instruction::LoadStr(text));
                self.push(Instruction::ReuseLastLine);
            }
            Stmt::Sif(cond, body) => {
                self.push_expr(cond)?;
                let mark = self.mark();
                self.push_stmt_with_pos(*body)?;
                self.insert(mark, Instruction::GotoIfNot(self.current_no()));
            }
            Stmt::SelectCase(cond, cases, case_else) => {
                self.push_expr(cond)?;
                self.push(Instruction::ReadVar);

                let mut ends = Vec::new();
                let mut nexts = Vec::new();
                for (conds, body) in cases {
                    for next in nexts.drain(..) {
                        self.insert(next, Instruction::GotoIfNot(self.current_no()));
                    }

                    for cond in conds {
                        match cond {
                            SelectCaseCond::Single(e) => {
                                self.push(Instruction::Duplicate);
                                self.push_expr(e)?;
                                self.push(Instruction::BinaryOperator(BinaryOperator::Equal));
                            }
                            SelectCaseCond::Is(op, e) => {
                                self.push(Instruction::Duplicate);
                                self.push_expr(e)?;
                                self.push(Instruction::BinaryOperator(op));
                            }
                            SelectCaseCond::To(from, to) => {
                                self.push(Instruction::Duplicate);
                                self.push_expr(from)?;
                                self.push(Instruction::BinaryOperator(
                                    BinaryOperator::GreaterOrEqual,
                                ));
                                self.push(Instruction::DuplicatePrev);
                                self.push_expr(to)?;
                                self.push(Instruction::BinaryOperator(BinaryOperator::LessOrEqual));
                                self.push(Instruction::BinaryOperator(BinaryOperator::And));
                            }
                        }

                        nexts.push(self.mark());
                    }

                    for stmt in body {
                        self.push_stmt_with_pos(stmt)?;
                    }

                    ends.push(self.mark());

                    for next in nexts.drain(..) {
                        self.insert(next, Instruction::GotoIfNot(self.current_no()));
                    }
                }

                self.push(Instruction::Pop);

                if let Some(case_else) = case_else {
                    for stmt in case_else {
                        self.push_stmt_with_pos(stmt)?;
                    }
                }

                for end in ends {
                    self.insert(end, Instruction::Goto(self.current_no()));
                }
            }
            Stmt::Continue => self.push(Instruction::Goto(
                *self
                    .continue_marks
                    .last()
                    .ok_or(CompileError::ContinueNotLoop)?,
            )),
            Stmt::Break => {
                let mark = self.mark();
                self.break_marks
                    .last_mut()
                    .ok_or(CompileError::BreakNotLoop)?
                    .push(mark);
            }
            Stmt::Repeat(end, body) => self.push_for(
                Variable {
                    var: "COUNT".into(),
                    func_extern: None,
                    args: Vec::new(),
                },
                Expr::int(0),
                end,
                Expr::int(1),
                body,
            )?,
            Stmt::Do(cond, body) => {
                let start_mark = self.begin_loop_block();
                for line in body {
                    self.push_stmt_with_pos(line)?;
                }
                self.push_expr(cond)?;
                let end = self.mark();
                self.push(Instruction::Goto(start_mark));
                self.insert(end, Instruction::GotoIfNot(self.current_no()));
                self.end_loop_block();
            }
            Stmt::While(cond, body) => {
                let start_mark = self.begin_loop_block();
                self.push_expr(cond)?;
                let end = self.mark();
                for line in body {
                    self.push_stmt_with_pos(line)?;
                }
                self.push(Instruction::Goto(start_mark));
                self.insert(end, Instruction::GotoIfNot(self.current_no()));
                self.end_loop_block();
            }
            Stmt::For(var, init, end, step, body) => self.push_for(var, init, end, step, body)?,
            Stmt::If(else_ifs, else_part) => {
                let mut end_stack = Vec::with_capacity(32);

                for (cond, body) in else_ifs {
                    self.push_if(cond, body)?;
                    end_stack.push(self.mark());
                }

                for line in else_part {
                    self.push_stmt_with_pos(line)?;
                }

                for end in end_stack {
                    self.insert(end, Instruction::Goto(self.current_no()));
                }
            }
            Stmt::Assign(var, add_op, rhs) => {
                if let Some(add_op) = add_op {
                    self.push_var(var.clone())?;
                    self.push_expr(rhs)?;
                    self.push(Instruction::BinaryOperator(add_op));
                } else {
                    self.push_expr(rhs)?;
                }

                self.store_var(var)?;
            }
            Stmt::Call {
                name,
                args,
                try_body,
                catch_body: catch,
                is_jump,
            } => {
                let count = self.push_list(args)?;
                self.push_expr(name)?;

                if let Some(catch) = catch {
                    if is_jump {
                        self.push(Instruction::TryJump(count));
                    } else {
                        self.push(Instruction::TryCall(count));
                    }
                    let try_top = self.mark();
                    for try_body in try_body {
                        self.push_stmt_with_pos(try_body)?;
                    }
                    let try_end = self.mark();
                    let catch_top = self.current_no();
                    for stmt in catch {
                        self.push_stmt_with_pos(stmt)?;
                    }
                    self.insert(try_top, Instruction::GotoIfNot(catch_top));
                    self.insert(try_end, Instruction::Goto(self.current_no()));
                } else {
                    if is_jump {
                        self.push(Instruction::Jump(count));
                    } else {
                        self.push(Instruction::Call(count));
                    }
                }
            }
            Stmt::Goto {
                label,
                catch_body: catch,
            } => {
                self.push_expr(label)?;

                if let Some(catch) = catch {
                    self.push(Instruction::TryGotoLabel);
                    let catch_top = self.mark();
                    for stmt in catch {
                        self.push_stmt_with_pos(stmt)?;
                    }
                    self.insert(catch_top, Instruction::GotoIfNot(self.current_no()));
                } else {
                    self.push(Instruction::GotoLabel);
                }
            }
            Stmt::Label(label) => {
                if self.goto_labels.insert(label, self.current_no()).is_some() {
                    return Err(CompileError::DuplicatedGotoLabel);
                }
            }
            Stmt::Begin(ty) => {
                self.push(Instruction::Begin(ty));
            }
            Stmt::Alignment(align) => {
                self.push(Instruction::SetAlignment(align));
            }
            Stmt::Command(command, args) => {
                let count = self.push_list(args)?;
                self.push(Instruction::Command(command, count));
            }
            Stmt::Times(var, ratio) => {
                self.push_var(var.clone())?;
                self.push(Instruction::Times(ratio));
                self.store_var(var)?;
            }
        }

        Ok(())
    }

    fn push_if(&mut self, cond: Expr, body: Vec<StmtWithPos>) -> CompileResult<()> {
        self.push_expr(cond)?;
        let begin = self.mark();

        for line in body {
            self.push_stmt_with_pos(line)?;
        }
        self.insert(begin, Instruction::GotoIfNot(self.current_no() + 1));

        Ok(())
    }

    fn begin_loop_block(&mut self) -> u32 {
        let start_mark = self.mark();
        self.continue_marks.push(start_mark);
        self.break_marks.push(Vec::new());
        start_mark
    }

    fn end_loop_block(&mut self) {
        self.continue_marks.pop();

        for break_mark in self.break_marks.pop().unwrap() {
            self.insert(break_mark, Instruction::Goto(self.current_no()));
        }
    }
}

pub fn compile_stmt(stmt: Stmt) -> CompileResult<Box<[Instruction]>> {
    let mut compiler = Compiler::new();

    compiler.push_stmt(stmt)?;

    Ok(compiler.out.into_boxed_slice())
}

pub fn compile_expr(expr: Expr) -> CompileResult<Box<[Instruction]>> {
    let mut compiler = Compiler::new();

    compiler.push_expr(expr)?;

    Ok(compiler.out.into_boxed_slice())
}

pub fn compile(func: Function) -> CompileResult<CompiledFunction> {
    let mut compiler = Compiler::new();

    for stmt in func.body {
        compiler.push_stmt_with_pos(stmt)?;
    }

    Ok(CompiledFunction {
        header: func.header,
        goto_labels: compiler.goto_labels,
        body: compiler.out.into_boxed_slice(),
    })
}
