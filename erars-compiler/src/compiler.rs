use crate::{
    ast::FormText, BinaryOperator, CompileError, CompileResult, Expr, Function, FunctionHeader,
    Instruction, Stmt, Variable,
};
use arrayvec::ArrayVec;
use hashbrown::HashMap;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompiledFunction {
    pub header: FunctionHeader,
    pub body: Vec<Instruction>,
}

#[derive(Default)]
struct Compiler {
    out: Vec<Instruction>,
    marks: HashMap<String, u32>,
    goto_marks: HashMap<String, Vec<u32>>,
    continue_marks: Vec<u32>,
    break_marks: Vec<Vec<u32>>,
}

impl Compiler {
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

    fn push_list(&mut self, args: impl IntoIterator<Item = Expr>) -> CompileResult<()> {
        self.push_list_begin();
        for arg in args {
            self.push_expr(arg)?;
        }
        self.push_list_end();
        Ok(())
    }

    fn push_list_begin(&mut self) {
        self.out.push(Instruction::ListBegin);
    }

    fn push_list_end(&mut self) {
        self.out.push(Instruction::ListEnd);
    }

    fn push_expr(&mut self, expr: Expr) -> CompileResult<()> {
        match expr {
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
                self.push_list(args)?;
                self.out.push(Instruction::LoadStr(name));
                self.out.push(Instruction::CallMethod);
            }
            Expr::Var(var) => {
                self.push_var(var)?;
            }
        }

        Ok(())
    }

    fn get_var(&mut self, var: Variable) -> CompileResult<()> {
        self.push_list(var.args)?;
        self.out.push(Instruction::LoadStr(var.name));

        Ok(())
    }

    fn store_var(&mut self, var: Variable) -> CompileResult<()> {
        self.get_var(var)?;
        self.out.push(Instruction::StoreVar);

        Ok(())
    }

    fn push_var(&mut self, var: Variable) -> CompileResult<()> {
        self.get_var(var)?;
        self.out.push(Instruction::LoadVar);

        Ok(())
    }

    fn push_form(&mut self, form: FormText) -> CompileResult<()> {
        self.push_list_begin();
        self.out.push(Instruction::LoadStr(form.first));
        for (expr, text) in form.other {
            self.push_expr(expr)?;
            self.out.push(Instruction::LoadStr(text));
        }
        self.push_list_end();
        self.out.push(Instruction::ConcatString);

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
        // var = int
        // LOOP:
        // if var > end {
        //     body
        //     var += step;
        //     goto LOOP;
        // }

        self.push_expr(init)?;
        self.store_var(var.clone())?;

        let loop_mark = self.mark();

        self.push_expr(Expr::binary(
            Expr::Var(var.clone()),
            BinaryOperator::Greater,
            end,
        ))?;

        let start_mark = self.mark();

        self.continue_marks.push(start_mark);
        self.break_marks.push(Vec::new());

        for stmt in body {
            self.push_stmt(stmt)?;
        }

        self.get_var(var.clone())?;
        self.push_expr(step)?;
        self.store_var(var)?;
        self.out.push(Instruction::Goto(loop_mark));

        self.continue_marks.pop();
        for break_mark in self.break_marks.pop().unwrap() {
            self.insert(break_mark, Instruction::Goto(self.current_no()));
        }

        self.insert(start_mark, Instruction::GotoIfNot(self.current_no()));

        Ok(())
    }

    pub fn push_stmt(&mut self, stmt: Stmt) -> CompileResult<()> {
        match stmt {
            Stmt::Print(flags, text) => {
                self.out.push(Instruction::LoadStr(text));
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
                    name: "COUNT".into(),
                    args: Vec::new(),
                },
                Expr::int(0),
                end,
                Expr::int(1),
                body,
            )?,
            Stmt::For(var, init, end, step, body) => self.push_for(var, init, end, step, body)?,
            Stmt::If(else_ifs, else_part) => {
                let mut end_stack = ArrayVec::<u32, 24>::new();

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
            Stmt::Call(name, args) => {
                self.push_list(args)?;
                self.out.push(Instruction::LoadStr(name));
                self.out.push(Instruction::Call);
            }
            Stmt::Begin(ty) => {
                self.out.push(Instruction::Begin(ty));
            }
            Stmt::Alignment(align) => {
                self.out.push(Instruction::SetAlignment(align));
            }
            Stmt::Command(name, args) => {
                self.push_list(args)?;
                self.out.push(Instruction::LoadStr(name));
                self.out.push(Instruction::Command);
            }
            Stmt::Label(label) => {
                if self.marks.insert(label, self.current_no()).is_some() {
                    return Err(CompileError::DuplicatedGotoLabel);
                }
            }
            Stmt::Goto(label) => {
                let mark = self.mark();
                self.goto_marks.entry(label.into()).or_default().push(mark);
            }
            Stmt::Varset(var, args) => {
                self.push_list(args)?;
                self.get_var(var)?;
                self.out.push(Instruction::Varset);
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

    pub fn finish(mut self) -> Vec<Instruction> {
        for (label, marks) in self.goto_marks {
            match self.marks.get(&label) {
                Some(label_pos) => {
                    for goto_pos in marks {
                        self.out[goto_pos as usize] = Instruction::Goto(*label_pos);
                    }
                }
                None => unreachable!("Unknown goto label ${}", label),
            }
        }

        self.out
    }
}

pub fn compile(func: Function) -> CompileResult<CompiledFunction> {
    let mut compiler = Compiler::default();

    for stmt in func.body {
        compiler.push_stmt(stmt)?;
    }

    Ok(CompiledFunction {
        header: func.header,
        body: compiler.finish(),
    })
}
