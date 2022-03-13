use crate::{
    ast::FormText, CompileResult, Expr, Function, FunctionHeader,
    Instruction, Stmt,
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CompiledFunction {
    header: FunctionHeader,
    body: Vec<Instruction>,
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
            _ => todo!(),
        }

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
            _ => todo!(),
        }

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

    Ok(CompiledFunction {
        header: func.header,
        body: compiler.finish(),
    })
}
