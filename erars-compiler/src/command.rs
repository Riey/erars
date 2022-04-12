use serde::{Deserialize, Serialize};
use strum::{Display, EnumString};
use unicode_xid::UnicodeXID;

use crate::{
    BinaryOperator, Expr, FormText, Function, FunctionHeader, FunctionInfo,
    ParserError, ParserResult, PrintFlags, Stmt, UnaryOperator,
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct ExprParser<'s> {
    s: &'s str,
    /// `,` is not allowed
    ban_arg: bool,
    /// `%` is not allowed
    ban_percent: bool,
    /// '#' is not allowed
    ban_sharp: bool,
}

impl<'s> ExprParser<'s> {
    fn new(s: &'s str) -> Self {
        Self {
            s,
            ban_arg: false,
            ban_percent: false,
            ban_sharp: false,
        }
    }
}

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
                        None => break None,
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

pub fn normal_form_str<'a>(i: &'a str) -> IResult<&'a str, Expr> {
    form_str(FormStrType::Normal)(i)
}

pub fn arg_form_str<'a>(i: &'a str) -> IResult<&'a str, Expr> {
    form_str(FormStrType::Arg)(i)
}

pub fn form_str<'a>(ty: FormStrType) -> impl Fn(&'a str) -> IResult<&'a str, Expr> {
    move |i: &'a str| {
        let normal_str = parse_form_normal_str(ty);
        let (mut i, (normal, mut ty)) = normal_str(i)?;

        let mut form = FormText::new(normal.into());

        loop {
            let (left, expr) = match ty {
                Some(FormType::Percent) => terminated(np_expr, tag("%"))(i)?,
                Some(FormType::Brace) => terminated(expr, tag("}"))(i)?,
                Some(FormType::At) => {
                    let (i, cond) = bin_expr(i)?;
                    let (i, _) = de_sp(tag("?"))(i)?;
                    let (i, if_true) = form_str(FormStrType::FirstCond)(i)?;
                    let (i, or_false) = preceded(sp, form_str(FormStrType::SecondCond))(i)?;

                    (i, Expr::cond(cond, if_true, or_false))
                }
                None => break,
            };

            i = left;

            let (left, (normal, next_ty)) = normal_str(i)?;
            i = left;
            ty = next_ty;

            form.push(expr, None, None, normal.into());
        }

        Ok((i, Expr::FormText(form)))
    }
}

fn string<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, &'a str, E> {
    context("string", delimited(char('\"'), parse_str, char('\"')))(i)
}

fn single_expr<'a>(i: &'a str) -> IResult<&'a str, Expr> {
    let expr = de_sp(alt((
        map(string, Expr::str),
        map(i64, Expr::IntLit),
        delimited(tag("("), expr, tag(")")),
    )));

    let op = de_sp(alt((
        value(UnaryOperator::Not, tag("!")),
        value(UnaryOperator::BitNot, tag("~")),
    )));

    let mut expr = map(tuple((opt(op), expr)), |(op, expr)| match op {
        Some(op) => Expr::unary(expr, op),
        None => expr,
    });

    expr(i)
}

macro_rules! define_bin_expr {
        (
            $name:ident
            $(($mul_op:expr, $mul_tag:expr))?
        ) => {
            fn $name<'a>(i: &'a str) -> IResult<&'a str, Expr> {
                fn merge_binop((lhs, op, rhs): (Expr, BinaryOperator, Expr)) -> Expr {
                    Expr::binary(lhs, op, rhs)
                }

                fn expr1<'a>(i: &'a str) -> IResult<&'a str, Expr> {
                    let op = alt((
                        value(BinaryOperator::Mul, tag("*")),
                        value(BinaryOperator::Div, tag("/")),
                        $(
                            value($mul_op, tag($mul_tag)),
                        )?
                    ));

                    de_sp(
                        alt((
                            map(tuple((single_expr, op, expr1)), merge_binop),
                            single_expr,
                        )),
                    )(i)
                }

                fn expr2<'a>(i: &'a str) -> IResult<&'a str, Expr> {
                    let op = alt((
                        value(BinaryOperator::Add, tag("+")),
                        value(BinaryOperator::Sub, tag("-")),
                    ));

                    de_sp(alt((map(tuple((expr1, op, expr2)), merge_binop), expr1)))(i)
                }

                de_sp(expr2)(i)
            }
        };
    }

define_bin_expr!(bin_expr(BinaryOperator::Rem, "%"));
define_bin_expr!(np_bin_expr);

pub fn expr<'a>(i: &'a str) -> IResult<&'a str, Expr> {
    let cond = map(
        tuple((
            bin_expr,
            delimited(de_sp(tag("?")), bin_expr, de_sp(tag("#"))),
            bin_expr,
        )),
        |(cond, if_true, or_false)| Expr::cond(cond, if_true, or_false),
    );

    alt((cond, bin_expr))(i)
}

pub fn expr_list<'a>(i: &'a str) -> IResult<&'a str, Vec<Expr>> {
    separated_list0(de_sp(tag(",")), expr)(i)
}

pub fn call_arg_list<'a>(i: &'a str) -> IResult<&'a str, Vec<Expr>> {
    preceded(
        sp,
        alt((
            delimited(char('('), expr_list, char(')')),
            preceded(char(','), expr_list),
            value(Vec::new(), eof),
        )),
    )(i)
}

pub fn np_expr<'a>(i: &'a str) -> IResult<&'a str, Expr> {
    let cond = map(
        tuple((
            np_bin_expr,
            delimited(de_sp(tag("?")), np_bin_expr, de_sp(tag("#"))),
            np_bin_expr,
        )),
        |(cond, if_true, or_false)| Expr::cond(cond, if_true, or_false),
    );

    alt((cond, np_bin_expr))(i)
}

fn cut_comment<'a>(i: &'a str) -> &'a str {
    if let Some((i, _)) = i.split_once(';') {
        i
    } else {
        i
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

fn call_line<'a>(i: &'a str) -> IResult<&'a str, Stmt> {
    map(
        preceded(terminated(tag("CALL"), sp), pair(ident, call_arg_list)),
        |(ident, args)| Stmt::Call {
            name: Expr::str(ident),
            args,
            jump: false,
            catch: None,
        },
    )(i)
}

fn builtin_com_line<'a>(i: &'a str) -> IResult<&'a str, Stmt> {
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
            let (i, args) = expr_list(i)?;
            Ok((i, Stmt::Command(com, args)))
        }
        _ => Err(nom::Err::Error(make_error(i, ErrorKind::Tag))),
    }
}

fn command_line<'a>(i: &'a str) -> IResult<&'a str, Stmt> {
    let i = i.trim_start();

    alt((print_line, call_line, builtin_com_line))(i)
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

fn label_line<'a>(i: &'a str) -> IResult<&'a str, Stmt> {
    map(preceded(tag("$"), ident), |s| Stmt::Label(s.into()))(i)
}

fn function_label<'a>(i: &'a str) -> IResult<&'a str, String> {
    map(de_sp(preceded(char('@'), ident)), Into::into)(i)
}

pub fn stmt<'a>(i: &'a str) -> IResult<&'a str, Stmt> {
    let mut i = i.trim_start_matches(' ');

    if !i.starts_with("PRINTFORM") && i.starts_with("PRINT") || i.starts_with("REUSELASTLINE") {
        // don't strip comment
    } else {
        i = cut_comment(i);
    }

    alt((label_line, command_line))(i)
}

pub fn body<'a>(i: &'a str) -> IResult<&'a str, Vec<Stmt>> {
    separated_list0(many1(newline), stmt)(i)
}

pub fn era_program<'a>(i: &'a str) -> IResult<&'a str, Vec<Function>> {
    let functions = many0(map(tuple((function_label, body)), |(label, body)| {
        Function {
            header: FunctionHeader {
                name: label,
                ..Default::default()
            },
            body,
        }
    }));

    preceded(opt(tag("\u{feff}")), functions)(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn program() {
        k9::snapshot!(
            era_program("@SYSTEM_TITLE\nPRINTL Hello, world!"),
            r#"
Ok(
    (
        "",
        [
            Function {
                header: FunctionHeader {
                    name: "SYSTEM_TITLE",
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
    fn lines() {
        k9::snapshot!(
            body("CALL FOO, 2 + 3"),
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
            body("LIMIT 1, 2, 3"),
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
    fn tests() {
        k9::snapshot!(
            expr(" 1 + 3 * 3 "),
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
            expr("1 + 2 ? 3 # 5432"),
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
        k9::snapshot!(
            normal_form_str("%1235% \\@ 1 ? 23# 45\\@"),
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
