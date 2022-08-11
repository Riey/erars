mod expr;

use erars_ast::{Alignment, BeginType, EventFlags, Expr, Function, FunctionInfo, Stmt};
use erars_lexer::{JumpType, Token, PrintType};
use hashbrown::HashMap;
use logos::{internal::LexerInternal, Lexer};
use std::{borrow::Cow, cell::Cell, mem};

pub use crate::error::{ParserError, ParserResult};

macro_rules! error {
    ($lex:expr, $msg:expr) => {{
        return Err((String::from($msg), $lex.span()));
    }};
}

macro_rules! take_ident {
    ($ty:ty, $lex:expr) => {
        match $lex.next() {
            Some(Token::Ident(ident)) => match ident.parse::<$ty>() {
                Ok(ret) => ret,
                Err(_) => error!($lex, format!("Unknown ident: {}", ident)),
            },
            other => error!($lex, format!("Expect ident, found: {:?}", other)),
        }
    };
    ($lex:expr) => {
        match $lex.next() {
            Some(Token::Ident(ident)) => ident,
            other => error!($lex, format!("Expect ident, found: {:?}", other)),
        }
    };
}

macro_rules! try_nom {
    ($lex:expr, $ret:expr) => {
        match $ret {
            Ok(ret) => ret,
            Err(err) => match err {
                nom::Err::Error(err) | nom::Err::Failure(err) => {
                    error!($lex, format!("Expression parsing failed: {:?}", err.code))
                }
                _ => unreachable!(),
            },
        }
    };
}

macro_rules! erb_assert_eq {
    ($lex:expr, $lhs:expr, $rhs:expr, $msg:expr) => {
        if $lhs != $rhs {
            error!($lex, $msg);
        }
    };
}

#[derive(Debug)]
pub struct ParserContext {
    pub macros: HashMap<String, String>,
    pub is_arg: Cell<bool>,
    pub ban_percent: Cell<bool>,
}

impl Default for ParserContext {
    fn default() -> Self {
        Self::new(HashMap::default())
    }
}

impl ParserContext {
    pub fn new(macros: HashMap<String, String>) -> Self {
        Self {
            macros,
            is_arg: Cell::new(false),
            ban_percent: Cell::new(false),
        }
    }

    pub fn is_str_var(&self, ident: &str) -> bool {
        matches!(ident, "NICKNAME" | "NAME" | "CALLNAME" | "LOCALS")
    }

    pub fn replace<'s>(&self, s: &'s str) -> Cow<'s, str> {
        let mut ret = Cow::Borrowed(s);

        while let Some(new) = self.macros.get(ret.as_ref()) {
            ret = Cow::Owned(new.clone());
        }

        ret
    }

    pub fn parse_stmt<'s>(
        &self,
        first: Token<'s>,
        lex: &mut Lexer<'s, Token<'s>>,
    ) -> ParserResult<Stmt> {
        let stmt = match first {
            Token::DirectStmt(stmt) => stmt,
            Token::Alignment => Stmt::Alignment(take_ident!(Alignment, lex)),
            Token::Begin => Stmt::Begin(take_ident!(BeginType, lex)),
            Token::LabelLine(label) => Stmt::Label(label.into()),
            Token::Times(left) => try_nom!(lex, self::expr::times_line(self)(left)).1,
            Token::Print((flags, PrintType::Plain, form)) => {
                Stmt::Print(flags, Expr::str(form))
            }
            Token::Print((flags, PrintType::Form, form)) => {
                let (_, form) = try_nom!(lex, self::expr::normal_form_str(self)(form));
                Stmt::Print(flags, form)
            }
            Token::Print((flags, PrintType::S, form)) => {
                let (_, s) = try_nom!(lex, self::expr::expr(self)(form));
                Stmt::Print(flags, s)
            }
            Token::Print((flags, PrintType::V, form)) => {
                let (_, s) = try_nom!(lex, self::expr::call_arg_list(self)(form));
                Stmt::PrintList(flags, s)
            }
            Token::CallJump((info, args)) => {
                let (name, args) = try_nom!(lex, self::expr::call_jump_line(self, info)(args)).1;

                let catch = if info.is_catch {
                    erb_assert_eq!(
                        lex,
                        lex.next(),
                        Some(Token::Catch),
                        "TRYC 다음에 CATCH가 와야합니다."
                    );

                    let mut catch = Vec::new();

                    loop {
                        match lex.next() {
                            Some(Token::EndCatch) => break Some(catch),
                            Some(tok) => catch.push(self.parse_stmt(tok, lex)?),
                            None => error!(lex, "ENDCATCH없이 끝났습니다."),
                        }
                    }
                } else if info.is_try {
                    Some(Vec::new())
                } else {
                    None
                };

                match info.ty {
                    JumpType::Goto => Stmt::Goto { label: name, catch },
                    JumpType::Call | JumpType::Jump => Stmt::Call {
                        name,
                        args,
                        catch: None,
                        is_jump: info.ty == JumpType::Jump,
                    },
                }
            }
            Token::Sif(cond) => {
                let cond = try_nom!(lex, self::expr::expr(self)(cond)).1;
                let first = match lex.next() {
                    Some(first) => first,
                    None => error!(lex, "Unexpected EOF after SIF"),
                };
                let body = self.parse_stmt(first, lex)?;

                Stmt::Sif(cond, Box::new(body))
            }
            Token::SelectCase(cond) => {
                let cond = try_nom!(lex, self::expr::expr(self)(cond)).1;
                let mut has_else = false;
                let mut body = Vec::new();
                let mut cases: Vec<(_, Vec<Stmt>)> = Vec::new();

                loop {
                    match lex.next() {
                        Some(Token::Case(case)) => {
                            if !cases.is_empty() {
                                cases.last_mut().unwrap().1 = mem::take(&mut body);
                            }
                            let case = try_nom!(lex, self::expr::case_line(self)(case)).1;
                            cases.push((case, Vec::new()));
                        }
                        Some(Token::CaseElse) => {
                            cases.last_mut().unwrap().1 = mem::take(&mut body);
                            has_else = true;
                        }
                        Some(Token::EndSelect) => break,
                        Some(tok) => {
                            body.push(self.parse_stmt(tok, lex)?);
                        }
                        None => error!(lex, "Unexpected EOF after SELECTCASE"),
                    }
                }

                if has_else {
                    Stmt::SelectCase(cond, cases, Some(body))
                } else {
                    cases.last_mut().unwrap().1 = body;
                    Stmt::SelectCase(cond, cases, None)
                }
            }
            Token::For(left) => {
                let (var, init, end, step) = try_nom!(lex, self::expr::for_line(self)(left)).1;
                let mut body = Vec::new();

                loop {
                    match lex.next() {
                        Some(Token::Next) => break,
                        Some(other) => {
                            body.push(self.parse_stmt(other, lex)?);
                        }
                        None => error!(lex, "Unexpected EOF after FOR"),
                    }
                }

                Stmt::For(var, init, end, step, body)
            }
            Token::Repeat(left) => {
                let arg = try_nom!(lex, self::expr::expr(self)(left)).1;
                let mut body = Vec::new();

                loop {
                    match lex.next() {
                        Some(Token::Rend) => break,
                        Some(other) => {
                            body.push(self.parse_stmt(other, lex)?);
                        }
                        None => error!(lex, "Unexpected EOF after FOR"),
                    }
                }

                Stmt::Repeat(arg, body)
            }
            Token::If(left) => {
                let mut is_else = false;
                let mut cond = try_nom!(lex, self::expr::expr(self)(left)).1;
                let mut block = Vec::new();
                let mut if_elses = Vec::new();

                loop {
                    match lex.next() {
                        Some(Token::ElseIf(left)) => {
                            if_elses.push((cond, block));
                            block = Vec::new();
                            cond = try_nom!(lex, self::expr::expr(self)(left)).1;
                        }
                        Some(Token::Else) => {
                            is_else = true;
                            if_elses.push((cond, block));
                            cond = Expr::Int(1);
                            block = Vec::new();
                        }
                        Some(Token::EndIf) => {
                            if !is_else {
                                if_elses.push((cond, block));
                                block = Vec::new();
                            }
                            break;
                        }
                        Some(token) => {
                            block.push(self.parse_stmt(token, lex)?);
                        }
                        None => break,
                    }
                }

                Stmt::If(if_elses, block)
            }
            Token::Ident(var) => {
                let i = cut_line(lex);
                try_nom!(lex, self::expr::assign_line(self, var)(i)).1
            }
            Token::NormalExprCommand((com, args)) => {
                let args = try_nom!(lex, self::expr::expr_list(self)(args)).1;
                Stmt::Command(com, args)
            }
            Token::Varset(args) => try_nom!(lex, self::expr::varset_line(self)(args)).1,
            other => error!(lex, format!("[Stmt] Invalid token: {:?}", other)),
        };

        Ok(stmt)
    }

    pub fn parse<'s>(&self, lex: &mut Lexer<'s, Token<'s>>) -> ParserResult<Vec<Function>> {
        let mut out = Vec::new();
        let mut current_func = Function::default();

        loop {
            match lex.next() {
                Some(Token::At) => {
                    let label = take_ident!(lex);
                    let args = try_nom!(lex, self::expr::function_args(self)(cut_line(lex))).1;
                    if !current_func.header.name.is_empty() {
                        out.push(mem::take(&mut current_func));
                    }
                    current_func.header.name = label.into();
                    current_func.header.args = args;
                }
                Some(Token::Function) => {
                    current_func.header.infos.push(FunctionInfo::Function);
                }
                Some(Token::FunctionS) => {
                    current_func.header.infos.push(FunctionInfo::FunctionS);
                }
                Some(Token::Pri) => {
                    current_func
                        .header
                        .infos
                        .push(FunctionInfo::EventFlag(EventFlags::Pre));
                }
                Some(Token::Later) => {
                    current_func
                        .header
                        .infos
                        .push(FunctionInfo::EventFlag(EventFlags::Later));
                }
                Some(Token::Single) => {
                    current_func
                        .header
                        .infos
                        .push(FunctionInfo::EventFlag(EventFlags::Single));
                }
                Some(Token::Dim) | Some(Token::DimS) => {
                    todo!()
                }
                Some(other) => match self.parse_stmt(other, lex) {
                    Ok(stmt) => current_func.body.push(stmt),
                    Err(err) => {
                        return Err(err);
                    }
                },
                None => break,
            }
        }

        if !current_func.header.name.is_empty() {
            out.push(current_func);
        }

        Ok(out)
    }
}

impl ParserContext {
    pub fn parse_program_str<'s>(&self, s: &'s str) -> ParserResult<Vec<Function>> {
        self.parse(&mut Lexer::new(s))
    }

    pub fn parse_function_str<'s>(&self, s: &'s str) -> ParserResult<Function> {
        self.parse_program_str(s)
            .map(|f| f.into_iter().next().unwrap())
    }

    pub fn parse_expr_str<'s>(&self, s: &'s str) -> ParserResult<Expr> {
        let lex = Lexer::<Token<'s>>::new(s);
        Ok(try_nom!(lex, self::expr::expr(self)(s)).1)
    }

    pub fn parse_body_str<'s>(&self, s: &'s str) -> ParserResult<Vec<Stmt>> {
        let mut lex = Lexer::<Token<'s>>::new(s);
        let mut body = Vec::new();

        loop {
            match lex.next() {
                Some(tok) => body.push(self.parse_stmt(tok, &mut lex)?),
                None => break,
            }
        }

        Ok(body)
    }

    pub fn parse_stmt_str<'s>(&self, s: &'s str) -> ParserResult<Stmt> {
        let mut lex = Lexer::new(s);
        let first = lex.next().unwrap();
        self.parse_stmt(first, &mut lex)
    }
}

fn cut_line<'s>(lex: &mut Lexer<'s, Token<'s>>) -> &'s str {
    let i = lex.remainder();
    let l = i.split_once('\n').map(|(l, _)| l).unwrap_or(i);
    lex.bump_unchecked(l.len());
    l
}
