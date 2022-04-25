use std::cell::Cell;

use serde::{Deserialize, Serialize};
use strum::{Display, EnumString};
use unicode_xid::UnicodeXID;

use crate::{
    Alignment, BinaryOperator, Expr, FormText, Function, FunctionHeader, FunctionIndex, PrintFlags,
    Stmt, UnaryOperator, Variable, VariableDic,
};
use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag, take_while},
    character::complete::*,
    combinator::{cut, eof, map, map_res, opt, value, verify},
    error::{context, make_error, ContextError, Error, ErrorKind, ParseError},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult, Parser,
};

pub enum FormType {
    Percent,
    Brace,
    At,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FormStrType {
    Normal,
    /// no #
    FirstCond,
    /// no \\@
    SecondCond,
    /// no comma, blank
    Arg,
}

fn sp<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    take_while(move |c| " \t".contains(c))(i)
}

fn de_sp<'a, T, E: ParseError<&'a str>>(
    p: impl Parser<&'a str, T, E>,
) -> impl FnMut(&'a str) -> IResult<&'a str, T, E> {
    delimited(sp, p, sp)
}

fn ident<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    verify(
        take_while(move |c: char| c.is_xid_continue() || c == '_'),
        |s: &str| s.chars().next().map_or(false, |c| c.is_xid_start()),
    )(i)
}

fn parse_str<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    escaped(alphanumeric1, '\\', one_of("\"n\\"))(i)
}

fn parse_form_normal_str<'a>(
    ty: FormStrType,
) -> impl Fn(&'a str) -> IResult<&'a str, (String, Option<FormType>)> {
    move |mut i: &'a str| {
        let mut ret = String::new();

        let form_ty = loop {
            match take_while(move |c| !"\\\n%{#,".contains(c))(i) {
                Ok((left, plain)) => {
                    ret.push_str(plain);
                    i = left;

                    if let Some(left) = left.strip_prefix('%') {
                        i = left;
                        break Some(FormType::Percent);
                    } else if let Some(left) = left.strip_prefix('{') {
                        i = left;
                        break Some(FormType::Brace);
                    } else if let Some(left) = left.strip_prefix('#') {
                        if ty == FormStrType::FirstCond {
                            i = left;
                            break None;
                        }
                    } else if let Some(left) = left.strip_prefix('\\') {
                        if let Some(left) = left.strip_prefix('@') {
                            i = left;

                            if ty == FormStrType::SecondCond {
                                break None;
                            } else {
                                break Some(FormType::At);
                            }
                        } else {
                            let mut ch = left.chars();

                            // escape
                            match ch.next() {
                                Some(c) => ret.push(c),
                                // incomplete escape
                                None => {
                                    return Err(nom::Err::Error(nom::error::make_error(
                                        i,
                                        nom::error::ErrorKind::EscapedTransform,
                                    )))
                                }
                            };

                            i = ch.as_str();
                        }
                    }
                    match left.as_bytes().get(0).copied() {
                        Some(b'%') => break Some(FormType::Percent),
                        Some(b'{') => break Some(FormType::Brace),
                        Some(b'\n') | None => break None,
                        Some(b'\\') => {
                            match left.as_bytes().get(1).copied() {
                                Some(b'@') => {
                                    // \@
                                    break Some(FormType::Percent);
                                }
                                _ => {}
                            }
                        }
                        Some(_) => {}
                    }
                }
                Err(err) => return Err(err),
            }
        };

        if ty != FormStrType::Normal {
            while ret.chars().next_back() == Some(' ') {
                ret.pop();
            }
        }

        Ok((i, (ret, form_ty)))
    }
}

pub fn normal_form_str<'c>(
    ctx: &'c ParseContext<'c>,
) -> impl for<'a> FnMut(&'a str) -> IResult<&'a str, Expr> + 'c {
    move |i| form_str(FormStrType::Normal, ctx)(i)
}

pub fn form_str<'c, 'a>(
    ty: FormStrType,
    ctx: &'c ParseContext<'c>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Expr> + 'c {
    move |i: &'a str| {
        let normal_str = parse_form_normal_str(ty);
        let (mut i, (normal, mut ty)) = normal_str(i)?;

        let mut form = FormText::new(normal.into());

        loop {
            let (left, expr, padding, align) = match ty {
                Some(FormType::Percent) => {
                    let (i, ex) = np_expr(ctx)(i)?;
                    let (i, padding) = opt(preceded(preceded(sp, char(',')), np_expr(ctx)))(i)?;
                    let (i, align) = if padding.is_some() {
                        opt(preceded(preceded(sp, char(',')), alignment))(i)?
                    } else {
                        (i, None)
                    };
                    let (i, _) = preceded(sp, char('%'))(i)?;

                    (i, ex, padding, align)
                }
                Some(FormType::Brace) => {
                    let (i, ex) = expr(ctx)(i)?;
                    let (i, padding) = opt(preceded(preceded(sp, char(',')), expr(ctx)))(i)?;
                    let (i, align) = if padding.is_some() {
                        opt(preceded(preceded(sp, char(',')), alignment))(i)?
                    } else {
                        (i, None)
                    };
                    let (i, _) = preceded(sp, char('}'))(i)?;

                    (i, ex, padding, align)
                }
                Some(FormType::At) => {
                    let (i, cond) = bin_expr(ctx)(i)?;
                    let (i, _) = de_sp(tag("?"))(i)?;
                    let (i, if_true) = form_str(FormStrType::FirstCond, ctx)(i)?;
                    let (i, or_false) = preceded(sp, form_str(FormStrType::SecondCond, ctx))(i)?;

                    (i, Expr::cond(cond, if_true, or_false), None, None)
                }
                None => break,
            };

            i = left;

            let (left, (normal, next_ty)) = normal_str(i)?;
            i = left;
            ty = next_ty;

            form.push(expr, padding, align, normal.into());
        }

        Ok((i, Expr::FormText(form)))
    }
}

fn string<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, &'a str, E> {
    context("string", delimited(char('\"'), parse_str, char('\"')))(i)
}

fn paran_expr<'c, 'a>(
    ctx: &'c ParseContext<'c>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Expr> + 'c {
    move |i| {
        let is_arg = ctx.is_arg.get();
        ctx.is_arg.set(false);

        let (i, expr) = delimited(tag("("), expr(ctx), tag(")"))(i)?;

        ctx.is_arg.set(is_arg);

        Ok((i, expr))
    }
}

fn single_expr<'c, 'a>(
    ctx: &'c ParseContext<'c>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Expr> + 'c {
    move |i| {
        let (i, expr) = if ctx.is_arg.get() {
            de_sp(alt((
                map(string, Expr::str),
                map(i64, Expr::IntLit),
                map(variable_no_arg(ctx), Expr::Var),
                paran_expr(ctx),
            )))(i)?
        } else {
            de_sp(alt((
                map(string, Expr::str),
                map(i64, Expr::IntLit),
                map(variable(ctx), Expr::Var),
                paran_expr(ctx),
            )))(i)?
        };

        let (i, op) = opt(de_sp(alt((
            value(UnaryOperator::Not, tag("!")),
            value(UnaryOperator::BitNot, tag("~")),
        ))))(i)?;

        let expr = match op {
            Some(op) => Expr::unary(expr, op),
            None => expr,
        };

        Ok((i, expr))
    }
}

macro_rules! define_bin_expr {
        (
            $name:ident
            $(($mul_op:expr, $mul_tag:expr))?
        ) => {
            fn $name<'c>(ctx: &'c ParseContext<'c>) -> impl for<'a> FnMut(&'a str) -> IResult<&'a str, Expr> + 'c {
                fn merge_binop((lhs, op, rhs): (Expr, BinaryOperator, Expr)) -> Expr {
                    Expr::binary(lhs, op, rhs)
                }

                fn expr1<'c>(ctx: &'c ParseContext<'c>) -> impl for<'a> FnMut(&'a str) -> IResult<&'a str, Expr> + 'c {

                    move |i| {
                        let op = alt((
                            value(BinaryOperator::Mul, tag("*")),
                            value(BinaryOperator::Div, tag("/")),
                            $(
                                value($mul_op, tag($mul_tag)),
                            )?
                        ));
                        de_sp(
                            alt((
                                map(tuple((single_expr(ctx), op, expr1(ctx))), merge_binop),
                                single_expr(ctx),
                            )),
                        )(i)
                    }
                }

                fn expr2<'a, 'c>(ctx: &'c ParseContext<'c>) -> impl FnMut(&'a str) -> IResult<&'a str, Expr> + 'c {
                    move |i| {
                        let op = alt((
                            value(BinaryOperator::Add, tag("+")),
                            value(BinaryOperator::Sub, tag("-")),
                        ));
                        de_sp(alt((map(tuple((expr1(ctx), op, expr2(ctx))), merge_binop), expr1(ctx))))(i)
                    }
                }

                move |i| de_sp(expr2(ctx))(i)
            }
        };
    }

define_bin_expr!(bin_expr(BinaryOperator::Rem, "%"));
define_bin_expr!(np_bin_expr);

pub fn expr<'c, 'a>(
    ctx: &'c ParseContext<'c>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Expr> + 'c {
    move |i| {
        let cond = map(
            tuple((
                bin_expr(ctx),
                delimited(de_sp(tag("?")), bin_expr(ctx), de_sp(tag("#"))),
                bin_expr(ctx),
            )),
            |(cond, if_true, or_false)| Expr::cond(cond, if_true, or_false),
        );

        alt((cond, bin_expr(ctx)))(i)
    }
}

pub fn expr_list<'c, 'a>(
    ctx: &'c ParseContext<'c>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<Expr>> + 'c {
    move |i| separated_list0(de_sp(tag(",")), expr(ctx))(i)
}

pub fn call_arg_list<'c, 'a>(
    ctx: &'c ParseContext<'c>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<Expr>> + 'c {
    move |i| {
        preceded(
            sp,
            alt((
                delimited(char('('), expr_list(ctx), char(')')),
                preceded(char(','), expr_list(ctx)),
                value(Vec::new(), eof),
            )),
        )(i)
    }
}

pub fn np_expr<'c, 'a>(
    ctx: &'c ParseContext<'c>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Expr> + 'c {
    move |i| {
        let cond = map(
            tuple((
                np_bin_expr(ctx),
                delimited(de_sp(tag("?")), np_bin_expr(ctx), de_sp(tag("#"))),
                np_bin_expr(ctx),
            )),
            |(cond, if_true, or_false)| Expr::cond(cond, if_true, or_false),
        );

        alt((cond, np_bin_expr(ctx)))(i)
    }
}

fn cut_comment<'a>(i: &'a str) -> &'a str {
    if let Some((i, _)) = i.split_once(';') {
        i
    } else {
        i
    }
}

fn variable_no_arg<'c, 'a>(
    ctx: &'c ParseContext<'c>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Variable> + 'c {
    move |i| {
        let (i, name) = ident(i)?;
        let idx = ctx.var.get_var(name, ctx.current_func).unwrap();

        Ok((
            i,
            Variable {
                var_idx: idx,
                args: Vec::new(),
            },
        ))
    }
}

fn variable<'c, 'a>(
    ctx: &'c ParseContext<'c>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Variable> + 'c {
    move |i| {
        ctx.is_arg.set(true);
        let (i, name) = ident(i)?;
        let (i, args) = many0(preceded(de_sp(char(':')), expr(ctx)))(i)?;
        ctx.is_arg.set(false);
        let idx = ctx
            .var
            .get_var(name, ctx.current_func)
            .expect("Variable not found");

        Ok((i, Variable { var_idx: idx, args }))
    }
}

fn print_flags<'a>(i: &'a str) -> IResult<&'a str, PrintFlags> {
    let (i, line) = map(
        opt(alt((
            value(PrintFlags::WAIT | PrintFlags::NEWLINE, char('W')),
            value(PrintFlags::NEWLINE, char('L')),
        ))),
        |l| l.unwrap_or_default(),
    )(i)?;
    let (i, align) = map(
        opt(alt((
            value(PrintFlags::LEFT_ALIGN, tag("LC")),
            value(PrintFlags::RIGHT_ALIGN, char('C')),
        ))),
        |l| l.unwrap_or_default(),
    )(i)?;

    Ok((i, line | align))
}

fn print_line<'a>(i: &'a str) -> IResult<&'a str, Stmt> {
    map(
        preceded(
            tag("PRINT"),
            pair(print_flags, preceded(char(' '), take_while(|c| c != '\n'))),
        ),
        |(flag, s)| Stmt::PrintSingle(flag, Expr::str(s)),
    )(i)
}

fn call_line<'a, 'c>(
    ctx: &'c ParseContext<'c>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Stmt> + 'c {
    move |i| {
        map(
            preceded(terminated(tag("CALL"), sp), pair(ident, call_arg_list(ctx))),
            |(ident, args)| Stmt::Call {
                name: Expr::str(ident),
                args,
                jump: false,
                catch: None,
            },
        )(i)
    }
}

fn alignment<'a>(i: &'a str) -> IResult<&'a str, Alignment> {
    alt((
        value(Alignment::Left, tag("LEFT")),
        value(Alignment::Center, tag("CENTER")),
        value(Alignment::Right, tag("RIGHT")),
    ))(i)
}

fn align_line<'a>(i: &'a str) -> IResult<&'a str, Stmt> {
    map(
        tuple((sp, tag("ALIGNMENT"), sp, alignment, sp)),
        |(_, _, _, align, _)| Stmt::Alignment(align),
    )(i)
}

fn builtin_com_line<'a, 'c>(
    ctx: &'c ParseContext<'c>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Stmt> + 'c {
    move |i| {
        let (com, i) = i.split_once(' ').unwrap_or((i, ""));
        match com.parse::<BuiltinCommand>() {
            Ok(BuiltinCommand::ReuseLastLine) => {
                let (i, text) = i.split_once('\n').unwrap_or(("", i));
                let text = text.trim_end_matches('\r');
                Ok((
                    i,
                    Stmt::Command(BuiltinCommand::ReuseLastLine, vec![Expr::str(text)]),
                ))
            }
            Ok(com) => {
                let (i, args) = expr_list(ctx)(i)?;
                Ok((i, Stmt::Command(com, args)))
            }
            _ => Err(nom::Err::Error(make_error(i, ErrorKind::Tag))),
        }
    }
}

fn command_line<'a, 'c>(
    ctx: &'c ParseContext<'c>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Stmt> + 'c {
    move |i| {
        let i = i.trim_start();

        alt((
            print_line,
            call_line(ctx),
            align_line,
            builtin_com_line(ctx),
        ))(i)
    }
}

fn label_line<'a>(i: &'a str) -> IResult<&'a str, Stmt> {
    map(preceded(tag("$"), ident), |s| Stmt::Label(s.into()))(i)
}

fn assign_line<'a, 'c>(
    ctx: &'c ParseContext<'c>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Stmt> + 'c {
    move |i| {
        let (i, var) = variable(ctx)(i)?;
        let (i, _) = de_sp(char('='))(i)?;

        let (_, info) = ctx.var.resolve_var(var.var_idx).unwrap();

        if info.is_str {
            let (i, form) = normal_form_str(ctx)(i)?;
            Ok((i, Stmt::Assign(var, None, form)))
        } else {
            let (i, expr) = expr(ctx)(i)?;
            Ok((i, Stmt::Assign(var, None, expr)))
        }
    }
}

fn function_label<'a>(i: &'a str) -> IResult<&'a str, &'a str> {
    de_sp(preceded(char('@'), ident))(i)
}

pub fn stmt<'a, 'c>(
    ctx: &'c ParseContext<'c>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Stmt> + 'c {
    move |i| {
        let mut i = i.trim_start_matches(' ');

        if !i.starts_with("PRINTFORM") && i.starts_with("PRINT") || i.starts_with("REUSELASTLINE") {
            // don't strip comment
        } else {
            i = cut_comment(i);
        }

        alt((label_line, command_line(ctx), assign_line(ctx)))(i)
    }
}

pub fn body<'a, 'r, 'c>(
    ctx: &'r ParseContext<'c>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<Stmt>> + 'r {
    move |i| separated_list0(many1(newline), stmt(ctx))(i)
}

pub fn function<'a, 'c>(
    var: &'c VariableDic,
) -> impl FnMut(&'a str) -> IResult<&'a str, Function> + 'c {
    move |i| {
        let (i, label) = function_label(i)?;
        let ctx = ParseContext::new(var, var.insert_func(&label));
        let (i, body) = body(&ctx)(i)?;
        Ok((
            i,
            Function {
                idx: ctx.current_func,
                header: FunctionHeader {
                    ..Default::default()
                },
                body,
            },
        ))
    }
}

pub fn era_program<'a, 'c>(
    var: &'c VariableDic,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<Function>> + 'c {
    move |i| preceded(opt(tag("\u{feff}")), many0(function(var)))(i)
}

#[derive(Clone)]
pub struct ParseContext<'c> {
    var: &'c VariableDic,
    current_func: FunctionIndex,
    is_arg: Cell<bool>,
}

impl<'c> ParseContext<'c> {
    pub fn new(var: &'c VariableDic, func_idx: FunctionIndex) -> Self {
        Self {
            var,
            current_func: func_idx,
            is_arg: Cell::new(false),
        }
    }
}

#[derive(Clone, Copy, Debug, EnumString, Display, Serialize, Deserialize, PartialEq, Eq)]
#[strum(serialize_all = "UPPERCASE")]
#[serde(rename_all = "UPPERCASE")]
#[allow(non_camel_case_types)]
pub enum BuiltinCommand {
    Limit,
    Min,
    Max,

    ReuseLastLine,

    StrLenS,
    StrLenSU,
    SubStringU,

    Input,
    InputS,
    TInput,
    TInputS,
    Wait,
    WaitAnykey,

    Restart,
    Quit,
    Throw,

    SaveGlobal,
    LoadGlobal,

    DrawLine,
    CustomDrawLine,
    ClearLine,

    Split,
    Unicode,

    Reset_Stain,
    GetExpLv,
    GetPalamLv,

    ResetData,
    ChkData,

    AddDefChara,
    AddChara,
    DelChara,
    SwapChara,
    FindChara,

    SetColor,
    ResetColor,

    FontBold,
    FontItalic,
    FontRegular,

    Bar,

    SetBit,
    GetBit,
    ClearBit,
    Power,
}

#[cfg(test)]
mod tests {
    fn dummy_ctx<'c>(var: &'c VariableDic) -> ParseContext<'c> {
        let func = var.insert_func("TEST");
        ParseContext::new(var, func)
    }

    use super::*;

    #[test]
    fn program() {
        k9::snapshot!(
            era_program(&VariableDic::default())("@SYSTEM_TITLE\nPRINTL Hello, world!"),
            r#"
Ok(
    (
        "",
        [
            Function {
                idx: FunctionIndex(
                    1,
                ),
                header: FunctionHeader {
                    args: [],
                    infos: [],
                },
                body: [
                    PrintSingle(
                        NEWLINE,
                        StringLit(
                            "Hello, world!",
                        ),
                    ),
                ],
            },
        ],
    ),
)
"#
        );
    }

    #[test]
    fn assign() {
        let var = VariableDic::default();
        let ctx = &dummy_ctx(&var);
        k9::snapshot!(
            body(ctx)("A = 123"),
            r#"
Ok(
    (
        "",
        [
            Assign(
                Variable {
                    var_idx: Global(
                        GlobalIndex(
                            18,
                        ),
                    ),
                    args: [],
                },
                None,
                IntLit(
                    123,
                ),
            ),
        ],
    ),
)
"#
        );
        k9::snapshot!(
            body(ctx)("STR = 123"),
            r#"
Ok(
    (
        "",
        [
            Assign(
                Variable {
                    var_idx: Global(
                        GlobalIndex(
                            87,
                        ),
                    ),
                    args: [],
                },
                None,
                FormText(
                    123,
                ),
            ),
        ],
    ),
)
"#
        );
    }

    #[test]
    fn lines() {
        let var = VariableDic::default();
        k9::snapshot!(
            body(&dummy_ctx(&var))("CALL FOO, 2 + 3"),
            r#"
Ok(
    (
        "",
        [
            Call {
                name: StringLit(
                    "FOO",
                ),
                args: [
                    BinopExpr(
                        IntLit(
                            2,
                        ),
                        Add,
                        IntLit(
                            3,
                        ),
                    ),
                ],
                jump: false,
                catch: None,
            },
        ],
    ),
)
"#
        );

        k9::snapshot!(
            body(&dummy_ctx(&var))("LIMIT 1, 2, 3"),
            r#"
Ok(
    (
        "",
        [
            Command(
                Limit,
                [
                    IntLit(
                        1,
                    ),
                    IntLit(
                        2,
                    ),
                    IntLit(
                        3,
                    ),
                ],
            ),
        ],
    ),
)
"#
        );
    }

    #[test]
    fn expr_tests() {
        let var = VariableDic::default();
        let ctx = &dummy_ctx(&var);
        k9::snapshot!(
            expr(ctx)(" 1 + 3 * 3 "),
            r#"
Ok(
    (
        "",
        BinopExpr(
            IntLit(
                1,
            ),
            Add,
            BinopExpr(
                IntLit(
                    3,
                ),
                Mul,
                IntLit(
                    3,
                ),
            ),
        ),
    ),
)
"#
        );

        k9::snapshot!(
            expr(ctx)("1 + 2 ? 3 # 5432"),
            r#"
Ok(
    (
        "",
        CondExpr(
            BinopExpr(
                IntLit(
                    1,
                ),
                Add,
                IntLit(
                    2,
                ),
            ),
            IntLit(
                3,
            ),
            IntLit(
                5432,
            ),
        ),
    ),
)
"#
        );
    }

    #[test]
    fn form_str_test() {
        let var = VariableDic::default();
        let ctx = &dummy_ctx(&var);

        k9::snapshot!(
            normal_form_str(ctx)("%1235% \\@ 1 ? 23# 45\\@"),
            r#"
Ok(
    (
        "",
        FormText(
            {IntLit(1235)} {CondExpr(IntLit(1), FormText(23), FormText(45))},
        ),
    ),
)
"#
        );
    }
}
