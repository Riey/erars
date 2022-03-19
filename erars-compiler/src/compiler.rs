use crate::{
    ast::FormText, CompileResult, Event, EventFlags, EventType, Expr, Function, FunctionInfo,
    Instruction, Stmt, Variable,
};
use arrayvec::ArrayVec;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompiledFunction {
    pub ty: CompiledFunctionType,
    pub body: Vec<Instruction>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum CompiledFunctionType {
    Normal(String),
    Event(Event),
}

#[derive(Default)]
struct Compiler {
    out: Vec<Instruction>,
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
            Stmt::Call(name, args) => {
                self.push_list(args)?;
                self.out.push(Instruction::LoadStr(name));
                self.out.push(Instruction::Call);
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

    pub fn finish(self) -> Vec<Instruction> {
        self.out
    }
}

pub fn compile(func: Function) -> CompileResult<CompiledFunction> {
    let mut compiler = Compiler::default();

    for stmt in func.body {
        compiler.push_stmt(stmt)?;
    }

    let mut flags = EventFlags::None;

    for info in func.header.infos {
        match info {
            FunctionInfo::EventFlag(flag) => {
                assert_eq!(flags, EventFlags::None);
                flags = flag;
            }
        }
    }

    let ty = match func.header.name.parse::<EventType>() {
        Ok(ty) => CompiledFunctionType::Event(Event { ty, flags }),
        Err(_) => CompiledFunctionType::Normal(func.header.name),
    };

    Ok(CompiledFunction {
        ty,
        body: compiler.finish(),
    })
}
