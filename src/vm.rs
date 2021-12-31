use anyhow::{anyhow, bail, Result};
use either::Either;

use crate::compiler::{Expr, PrintFlags, ProgramLine};
use ahash::AHashMap;
use serde::{Serialize, Deserialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct VariableInfo {
    #[serde(default)]
    is_chara: bool,
    #[serde(default)]
    is_str: bool,
    #[serde(default)]
    default_int: i64,
    #[serde(default)]
    size: Vec<usize>,
}

impl VariableInfo {
    pub fn arg_len(&self) -> usize {
        self.size.len() + self.is_chara as usize
    }
}

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
    pub fn new(info: &VariableInfo) -> Self {
        match (info.is_str, info.size.as_slice()) {
            (false, []) => Self::Int0D(info.default_int),
            (false, [a]) => Self::Int1D(vec![info.default_int; *a]),
            (false, [a, b]) => Self::Int2D(vec![vec![info.default_int; *a]; *b]),
            (false, [a, b, c]) => Self::Int3D(vec![vec![vec![info.default_int; *a]; *b]; *c]),
            (true, []) => Self::Str0D(String::new()),
            (true, [a]) => Self::Str1D(vec![String::new(); *a]),
            (true, [a, b]) => Self::Str2D(vec![vec![String::new(); *a]; *b]),
            (true, [a, b, c]) => Self::Str3D(vec![vec![vec![String::new(); *a]; *b]; *c]),
            _ => panic!("size length can't be greater than 3"),
        }
    }

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
    character_len: usize,
    variables: AHashMap<String, (VariableInfo, Either<Variable, Vec<Variable>>)>,
}

impl VariableStorage {
    pub fn new(infos: &AHashMap<String, VariableInfo>) -> Self {
        let variables = infos.iter().map(|(name, info)| {
            let var = if info.is_chara {
                Either::Right(Vec::new())
            } else {
                Either::Left(Variable::new(info))
            };

            (name.clone(), (info.clone(), var))
        }).collect();

        Self {
            character_len: 0,
            variables,
        }
    }

    pub fn add_chara(&mut self) {
        self.character_len += 1;
        for (_, (info, var)) in self.variables.iter_mut() {
            if let Either::Right(cvar) = var {
                cvar.push(Variable::new(info));
            }
        }
    }

    pub fn get_global(&self, name: &str) -> Option<&Variable> {
        if let Either::Left(ref gvar) = self.variables.get(name)?.1 {
            Some(gvar)
        } else {
            None
        }
    }
    pub fn get_chara(&self, name: &str, no: usize) -> Option<&Variable> {
        if let Either::Right(ref cvar) = self.variables.get(name)?.1 {
            cvar.get(no)
        } else {
            None
        }
    }
}

pub struct TerminalVm {
    var: VariableStorage,
}

impl TerminalVm {
    pub fn new(infos: &AHashMap<String, VariableInfo>) -> Self {
        Self {
            var: VariableStorage::new(infos),
        }
    }

    pub fn eval_int(&self, expr: &Expr) -> Result<i64> {
        match expr {
            Expr::Num(i) => Ok(*i),
            Expr::VarExpr {
                name,
                args,
            } => {
                let var = if false {
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
                args,
            } => {
                let var = if false {
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
