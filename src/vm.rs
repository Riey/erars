use anyhow::{anyhow, bail, Result};

use crate::compiler::{Expr, PrintFlags, ProgramLine};
use ahash::AHashMap;

#[derive(Clone, Debug)]
pub enum Variable {
    Int0D(i64),
    Int1D(Vec<i64>),
    Int2D(Vec<Vec<i64>>),
    Int3D(Vec<Vec<Vec<i64>>>),
    Str0D(String),
    Str1D(Vec<String>),
    Str2D(Vec<Vec<String>>),
    Str3D(Vec<Vec<Vec<String>>>),
}

impl Variable {
    pub fn get_int(&self, mut args: impl Iterator<Item = Result<usize>>) -> Result<&i64> {
        macro_rules! a {
            () => {
                args.next().unwrap()?
            };
        }
        match self {
            Variable::Int0D(s) => Ok(s),
            Variable::Int1D(s) => s.get(a!()).ok_or_else(|| anyhow!("Index out of range")),
            Variable::Int2D(s) => s
                .get(a!())
                .unwrap()
                .get(a!())
                .ok_or_else(|| anyhow!("Index out of range")),
            Variable::Int3D(s) => s
                .get(a!())
                .unwrap()
                .get(a!())
                .unwrap()
                .get(a!())
                .ok_or_else(|| anyhow!("Index out of range")),
            _ => bail!("Variable is Str type"),
        }
    }

    pub fn get_str(&self, mut args: impl Iterator<Item = Result<usize>>) -> Result<String> {
        macro_rules! a {
            () => {
                args.next().unwrap()?
            };
        }
        match self {
            Variable::Str0D(s) => Ok(s.clone()),
            Variable::Str1D(s) => s
                .get(a!())
                .ok_or_else(|| anyhow!("Index out of range"))
                .map(Clone::clone),
            Variable::Str2D(s) => s
                .get(a!())
                .unwrap()
                .get(a!())
                .ok_or_else(|| anyhow!("Index out of range"))
                .map(Clone::clone),
            Variable::Str3D(s) => s
                .get(a!())
                .unwrap()
                .get(a!())
                .unwrap()
                .get(a!())
                .ok_or_else(|| anyhow!("Index out of range"))
                .map(Clone::clone),
            _ => self.get_int(args).map(ToString::to_string),
        }
    }
}

pub struct VariableStorage {
    global_var: AHashMap<String, Variable>,
    character_var: AHashMap<String, Vec<Variable>>,
}

impl VariableStorage {
    pub fn new() -> Self {
        let mut global_var = AHashMap::new();
        let character_var = AHashMap::new();

        global_var.insert("MONEY".into(), Variable::Int1D(vec![0; 1000]));

        Self {
            global_var,
            character_var,
        }
    }

    pub fn get_global(&self, name: &str) -> Option<&Variable> {
        self.global_var.get(name)
    }
    pub fn get_chara(&self, name: &str, no: usize) -> Option<&Variable> {
        self.character_var.get(name)?.get(no)
    }
}

pub struct TerminalVm {
    var: VariableStorage,
}

impl TerminalVm {
    pub fn new() -> Self {
        Self {
            var: VariableStorage::new(),
        }
    }

    pub fn eval_int(&self, expr: &Expr) -> Result<i64> {
        match expr {
            Expr::Num(i) => Ok(*i),
            Expr::VarExpr {
                name,
                is_chara,
                args,
            } => {
                let var = if *is_chara {
                    self.var
                        .get_chara(name, self.eval_int(args.last().unwrap())?.try_into()?)
                        .ok_or_else(|| anyhow!("Unknown variable name {}", name))?
                } else {
                    self.var
                        .get_global(name)
                        .ok_or_else(|| anyhow!("Unknown variable name {}", name))?
                };

                var.get_int(
                    args.iter()
                        .map(|e| self.eval_int(e).and_then(|i| Ok(i.try_into()?))),
                )
                .map(Clone::clone)
            }
            _ => bail!("Invalid expr"),
        }
    }

    pub fn eval_str(&self, expr: &Expr) -> Result<String> {
        match expr {
            Expr::Str(s) => Ok(s.clone()),
            Expr::VarExpr {
                name,
                is_chara,
                args,
            } => {
                let var = if *is_chara {
                    self.var
                        .get_chara(name, self.eval_int(args.last().unwrap())?.try_into()?)
                        .ok_or_else(|| anyhow!("Unknown variable name {}", name))?
                } else {
                    self.var
                        .get_global(name)
                        .ok_or_else(|| anyhow!("Unknown variable name {}", name))?
                };

                var.get_str(
                    args.iter()
                        .map(|e| self.eval_int(e).and_then(|i| Ok(i.try_into()?))),
                )
            }
            _ => bail!("Invalid expr"),
        }
    }

    pub fn run(&mut self, line: &ProgramLine) -> Result<()> {
        match line {
            ProgramLine::PrintCom { flags, text } => {
                print!("{}", text.first);

                for (expr, text) in text.pairs.iter() {
                    print!("{}", self.eval_str(expr)?);
                    print!("{}", text);
                }

                if flags.contains(PrintFlags::NEWLINE) {
                    println!("");
                }

                if flags.contains(PrintFlags::WAIT) {}
            }
            ProgramLine::Call { func, args } => {
                println!("CALL {}", func);
            }
        }

        Ok(())
    }
}
