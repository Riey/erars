use crate::{
    ast::{FormText, SelectCaseCond},
    BinaryOperator, CompileError, CompileResult, Expr, FormExpr, Function, FunctionHeader,
    Instruction, KnownVariables, Stmt, Variable, VariableInterner,
};
use hashbrown::HashMap;
use serde::{Deserialize, Serialize};
use smartstring::{LazyCompact, SmartString};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompiledFunction {
    pub header: FunctionHeader,
    pub goto_labels: HashMap<SmartString<LazyCompact>, u32>,
    pub body: Vec<Instruction>,
}

struct Compiler<'v> {
    var: &'v VariableInterner,
    out: Vec<Instruction>,
    goto_labels: HashMap<SmartString<LazyCompact>, u32>,
    continue_marks: Vec<u32>,
    break_marks: Vec<Vec<u32>>,
}

impl<'v> Compiler<'v> {
    pub fn new(var: &'v VariableInterner) -> Self {
        Self {
            var,
            out: Vec::new(),
            goto_labels: HashMap::new(),
            continue_marks: Vec::new(),
            break_marks: Vec::new(),
        }
    }

    fn mark(&mut self) -> u32 {
        let ret = self.current_no();
        self.out.push(Instruction::Nop);
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
                    self.out.push(Instruction::LoadInt(1));
                    self.out.push(Instruction::BinaryOperator(op));
                    self.out.push(Instruction::Duplicate);
                    self.store_var(var.clone())?;
                } else {
                    self.push_var(var.clone())?;
                    self.out.push(Instruction::Duplicate);
                    self.out.push(Instruction::LoadInt(1));
                    self.out.push(Instruction::BinaryOperator(op));
                    self.store_var(var.clone())?;
                }
            }
            Expr::StringLit(s) => self.out.push(Instruction::LoadStr(s)),
            Expr::IntLit(i) => self.out.push(Instruction::LoadInt(i)),
            Expr::BinopExpr(lhs, op, rhs) => {
                self.push_expr(*lhs)?;
                self.push_expr(*rhs)?;
                self.out.push(Instruction::BinaryOperator(op));
            }
            Expr::UnaryopExpr(expr, op) => {
                self.push_expr(*expr)?;
                self.out.push(Instruction::UnaryOperator(op));
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
                self.out.push(Instruction::LoadStr(name));
                self.out.push(Instruction::CallMethod(count));
            }
            Expr::Var(var) => {
                self.push_var(var)?;
            }
        }

        Ok(())
    }

    fn store_var(&mut self, var: Variable) -> CompileResult<()> {
        let count = self.push_list(var.args)?;
        self.out.push(Instruction::StoreVar(var.var_idx, count));

        Ok(())
    }

    fn push_var(&mut self, var: Variable) -> CompileResult<()> {
        let count = self.push_list(var.args)?;
        self.out.push(Instruction::LoadVar(var.var_idx, count));

        Ok(())
    }

    fn push_form(&mut self, form: FormText) -> CompileResult<()> {
        let count = 1 + form.other.len() as u32 * 2;
        self.out.push(Instruction::LoadStr(form.first));
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
                self.out
                    .push(Instruction::PadStr(align.unwrap_or_default()));
            }
            self.out.push(Instruction::LoadStr(text));
        }
        self.out.push(Instruction::ConcatString(count));

        Ok(())
    }

    fn push_for(
        &mut self,
        var: Variable,
        init: Expr,
        end: Expr,
        step: Expr,
        body: Vec<Stmt>,
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
        self.push_expr(end)?;

        let loop_mark = self.current_no();

        // compare var with end
        self.push_var(var.clone())?;
        self.out.push(Instruction::DuplicatePrev);
        self.out
            .push(Instruction::BinaryOperator(BinaryOperator::Less));

        let start_mark = self.mark();

        self.continue_marks.push(start_mark);
        self.break_marks.push(Vec::new());

        for stmt in body {
            self.push_stmt(stmt)?;
        }

        // add step to var
        self.out.push(Instruction::DuplicatePrev);
        self.push_var(var.clone())?;
        self.out
            .push(Instruction::BinaryOperator(BinaryOperator::Add));
        self.store_var(var)?;
        self.out.push(Instruction::Goto(loop_mark));

        self.continue_marks.pop();
        for break_mark in self.break_marks.pop().unwrap() {
            self.insert(break_mark, Instruction::Goto(self.current_no()));
        }

        self.insert(start_mark, Instruction::GotoIfNot(self.current_no()));

        // remove pinned values
        self.out.push(Instruction::Pop);
        self.out.push(Instruction::Pop);

        Ok(())
    }

    pub fn push_stmt(&mut self, stmt: Stmt) -> CompileResult<()> {
        match stmt {
            Stmt::Print(flags, text) => {
                self.push_expr(text)?;
                self.out.push(Instruction::Print(flags));
            }
            Stmt::PrintList(flags, args) => {
                let count = self.push_list(args)?;
                self.out.push(Instruction::ConcatString(count));
                self.out.push(Instruction::Print(flags));
            }
            Stmt::PrintForm(flags, form) => {
                self.push_form(form)?;
                self.out.push(Instruction::Print(flags));
            }
            Stmt::ReuseLastLine(text) => {
                self.out.push(Instruction::LoadStr(text));
                self.out.push(Instruction::ReuseLastLine);
            }
            Stmt::Sif(cond, body) => {
                self.push_expr(cond)?;
                let mark = self.mark();
                self.push_stmt(*body)?;
                self.insert(mark, Instruction::GotoIfNot(self.current_no()));
            }
            Stmt::SelectCase(cond, cases, case_else) => {
                self.push_expr(cond)?;

                let mut ends = Vec::new();
                let mut nexts = Vec::new();
                for (conds, body) in cases {
                    for next in nexts.drain(..) {
                        self.insert(next, Instruction::GotoIfNot(self.current_no()));
                    }

                    for cond in conds {
                        match cond {
                            SelectCaseCond::Single(e) => {
                                self.out.push(Instruction::Duplicate);
                                self.push_expr(e)?;
                                self.out
                                    .push(Instruction::BinaryOperator(BinaryOperator::Equal));
                            }
                            SelectCaseCond::Is(op, e) => {
                                self.out.push(Instruction::Duplicate);
                                self.push_expr(e)?;
                                self.out.push(Instruction::BinaryOperator(op));
                            }
                            SelectCaseCond::To(from, to) => {
                                self.out.push(Instruction::Duplicate);
                                self.push_expr(from)?;
                                self.out.push(Instruction::BinaryOperator(
                                    BinaryOperator::GreaterOrEqual,
                                ));
                                self.out.push(Instruction::DuplicatePrev);
                                self.push_expr(to)?;
                                self.out
                                    .push(Instruction::BinaryOperator(BinaryOperator::LessOrEqual));
                                self.out
                                    .push(Instruction::BinaryOperator(BinaryOperator::And));
                            }
                        }

                        nexts.push(self.mark());
                    }

                    for stmt in body {
                        self.push_stmt(stmt)?;
                    }

                    ends.push(self.mark());

                    for next in nexts.drain(..) {
                        self.insert(next, Instruction::GotoIfNot(self.current_no()));
                    }
                }

                self.out.push(Instruction::Pop);

                if let Some(case_else) = case_else {
                    for stmt in case_else {
                        self.push_stmt(stmt)?;
                    }
                }

                for end in ends {
                    self.insert(end, Instruction::Goto(self.current_no()));
                }
            }
            Stmt::Continue => self.out.push(Instruction::Goto(
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
                    var_idx: self.var.get_known(KnownVariables::Count),
                    args: Vec::new(),
                },
                Expr::int(0),
                end,
                Expr::int(1),
                body,
            )?,
            Stmt::Do(cond, body) => {
                let start = self.current_no();
                for line in body {
                    self.push_stmt(line)?;
                }
                self.push_expr(cond)?;
                let end = self.mark();
                self.out.push(Instruction::Goto(start));
                self.insert(end, Instruction::GotoIfNot(self.current_no()));
            }
            Stmt::For(var, init, end, step, body) => self.push_for(var, init, end, step, body)?,
            Stmt::If(else_ifs, else_part) => {
                let mut end_stack = Vec::with_capacity(32);

                for (cond, body) in else_ifs {
                    self.push_if(cond, body)?;
                    end_stack.push(self.mark());
                }

                if let Some(else_part) = else_part {
                    for line in else_part {
                        self.push_stmt(line)?;
                    }
                }

                for end in end_stack {
                    self.insert(end, Instruction::Goto(self.current_no()));
                }
            }
            Stmt::Assign(var, add_op, rhs) => {
                if let Some(add_op) = add_op {
                    self.push_var(var.clone())?;
                    self.push_expr(rhs)?;
                    self.out.push(Instruction::BinaryOperator(add_op));
                } else {
                    self.push_expr(rhs)?;
                }

                self.store_var(var)?;
            }
            Stmt::Return(exprs) => {
                self.push_list(exprs)?;
                self.out.push(Instruction::Return);
            }
            Stmt::ReturnF(expr) => {
                self.push_expr(expr)?;
                self.out.push(Instruction::ReturnF);
            }
            Stmt::Call {
                name,
                args,
                catch,
                jump,
            } => {
                let count = self.push_list(args)?;
                self.push_expr(name)?;

                if let Some(catch) = catch {
                    if jump {
                        self.out.push(Instruction::TryJump(count));
                    } else {
                        self.out.push(Instruction::TryCall(count));
                    }
                    let catch_top = self.mark();
                    for stmt in catch {
                        self.push_stmt(stmt)?;
                    }
                    self.insert(catch_top, Instruction::GotoIfNot(self.current_no()));
                } else {
                    if jump {
                        self.out.push(Instruction::Jump(count));
                    } else {
                        self.out.push(Instruction::Call(count));
                    }
                }
            }
            Stmt::Goto { label, catch } => {
                self.push_expr(label)?;

                if let Some(catch) = catch {
                    self.out.push(Instruction::TryGotoLabel);
                    let catch_top = self.mark();
                    for stmt in catch {
                        self.push_stmt(stmt)?;
                    }
                    self.insert(catch_top, Instruction::GotoIfNot(self.current_no()));
                } else {
                    self.out.push(Instruction::GotoLabel);
                }
            }
            Stmt::Label(label) => {
                if self.goto_labels.insert(label, self.current_no()).is_some() {
                    return Err(CompileError::DuplicatedGotoLabel);
                }
            }
            Stmt::Begin(ty) => {
                self.out.push(Instruction::Begin(ty));
            }
            Stmt::Alignment(align) => {
                self.out.push(Instruction::SetAlignment(align));
            }
            Stmt::Command(command, args) => {
                let count = self.push_list(args)?;
                self.out.push(Instruction::Command(command, count));
            }
            Stmt::Varset(var, args) => {
                let varset_count = self.push_list(args)?;
                let arg_count = self.push_list(var.args)?;
                self.out.push(Instruction::Varset {
                    code: var.var_idx,
                    args: arg_count,
                    varset_args: varset_count,
                });
            }
            Stmt::Times(var, ratio) => {
                self.push_var(var.clone())?;
                self.out.push(Instruction::Times(ratio));
                self.store_var(var)?;
            }
        }

        Ok(())
    }

    fn push_if(&mut self, cond: Expr, body: Vec<Stmt>) -> CompileResult<()> {
        self.push_expr(cond)?;
        let begin = self.mark();

        for line in body {
            self.push_stmt(line)?;
        }
        self.insert(begin, Instruction::GotoIfNot(self.current_no()));

        Ok(())
    }
}

pub fn compile(func: Function, var: &VariableInterner) -> CompileResult<CompiledFunction> {
    let mut compiler = Compiler::new(var);

    for stmt in func.body {
        compiler.push_stmt(stmt)?;
    }

    Ok(CompiledFunction {
        header: func.header,
        goto_labels: compiler.goto_labels,
        body: compiler.out,
    })
}
