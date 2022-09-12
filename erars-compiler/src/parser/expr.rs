use std::borrow::Cow;

use super::ParserContext;
use erars_ast::{
    Alignment, BinaryOperator, Expr, FormText, LocalVariable, NotNan, SelectCaseCond, Stmt,
    UnaryOperator, Value, Variable, VariableInfo,
};
use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag, take_while, take_while1},
    character::complete::*,
    combinator::{eof, map, opt, success, value, verify},
    error::{context, ErrorKind, VerboseError},
    error_position,
    multi::{many0, separated_list0, separated_list1},
    number::complete::float,
    sequence::{delimited, pair, preceded, terminated, tuple},
    Parser,
};
use smol_str::SmolStr;
use unicode_xid::UnicodeXID;

type Error<'a> = nom::error::VerboseError<&'a str>;
type IResult<'a, T> = nom::IResult<&'a str, T, Error<'a>>;

fn sp<'a>(i: &'a str) -> IResult<'a, ()> {
    map(take_while(move |c| " \t\r".contains(c)), |_| ())(i)
}

// fn sp_nl<'a>(i: &'a str) -> IResult<'a, ()> {
//     map(take_while(move |c| " \t\r\n".contains(c)), |_| ())(i)
// }

fn char_sp<'a>(ch: char) -> impl FnMut(&'a str) -> IResult<'a, char> {
    delimited(sp, char(ch), sp)
}

fn de_char_sp<'a, T>(
    first: char,
    p: impl Parser<&'a str, T, Error<'a>>,
    last: char,
) -> impl FnMut(&'a str) -> IResult<'a, T> {
    delimited(char_sp(first), p, char_sp(last))
}

fn de_sp<'a, T>(p: impl Parser<&'a str, T, Error<'a>>) -> impl FnMut(&'a str) -> IResult<'a, T> {
    delimited(sp, p, sp)
}

fn is_ident(i: &str) -> bool {
    i.chars().all(|c| c.is_xid_continue() || c == '_')
}

pub fn ident<'a>(i: &'a str) -> IResult<'a, &'a str> {
    verify(
        take_while(move |c: char| c.is_xid_continue() || c == '_'),
        |s: &str| s.chars().next().map_or(false, |c| c.is_xid_start()),
    )(i)
}

fn ident_or_macro<'c, 'a>(ctx: &'c ParserContext, i: &'a str) -> IResult<'a, Cow<'a, str>> {
    let (i, ident) = ident(i)?;

    Ok((i, ctx.replace(ident)))
}

fn parse_str<'a>(i: &'a str) -> IResult<'a, &'a str> {
    escaped(is_not("\\\""), '\\', one_of("\"n\\"))(i)
}

fn alignment<'a>(i: &'a str) -> IResult<'a, Alignment> {
    alt((
        value(Alignment::Left, tag("LEFT")),
        value(Alignment::Center, tag("CENTER")),
        value(Alignment::Right, tag("RIGHT")),
    ))(i)
}

#[derive(Debug)]
pub enum FormType {
    Percent,
    Brace,
    At,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FormStrType {
    Normal,
    /// no #
    FirstCond,
    /// no \\@
    SecondCond,
    /// no comma, blank
    Arg,
    /// no "
    Str,
}

fn parse_form_normal_str<'a>(
    ty: FormStrType,
) -> impl Fn(&'a str) -> IResult<'a, (String, Option<FormType>)> {
    move |mut i: &'a str| {
        let mut ret = String::new();
        let mut ch = i.chars();

        let form_ty = loop {
            match ch.next() {
                Some('%') => {
                    i = ch.as_str();
                    break Some(FormType::Percent);
                }
                Some('{') => {
                    i = ch.as_str();
                    break Some(FormType::Brace);
                }
                Some('#') if ty == FormStrType::FirstCond => {
                    break None;
                }
                Some('\\') => match ch.next() {
                    Some('@') => {
                        if ty == FormStrType::SecondCond || ty == FormStrType::FirstCond {
                            break None;
                        } else {
                            i = ch.as_str();
                            break Some(FormType::At);
                        }
                    }
                    Some(escape) => {
                        i = ch.as_str();
                        ret.push(escape);
                    }
                    None => {
                        return Err(nom::Err::Error(nom::error::make_error(
                            i,
                            nom::error::ErrorKind::EscapedTransform,
                        )));
                    }
                },
                Some(',') if ty == FormStrType::Arg => {
                    break None;
                }
                Some('"') if ty == FormStrType::Str => {
                    break None;
                }
                Some(other) => {
                    i = ch.as_str();
                    ret.push(other);
                }
                None => {
                    break None;
                }
            }
        };

        if matches!(ty, FormStrType::FirstCond | FormStrType::SecondCond) {
            while ret.chars().next_back() == Some(' ') {
                ret.pop();
            }
        }

        Ok((i, (ret, form_ty)))
    }
}

pub fn normal_form_str<'c>(
    ctx: &'c ParserContext,
) -> impl for<'a> FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i| form_str(FormStrType::Normal, ctx)(i)
}

pub fn form_str<'c, 'a>(
    ty: FormStrType,
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i: &'a str| {
        let normal_str = parse_form_normal_str(ty);
        let (mut i, (normal, mut ty)) = normal_str(i)?;

        let mut form = FormText::new(normal.into());

        loop {
            let (left, expr, padding, align) = match ty {
                Some(FormType::Percent) => {
                    let ban_percent = ctx.ban_percent.get();
                    ctx.ban_percent.set(true);
                    let (i, ex) = expr(ctx)(i)?;
                    let (i, padding) = opt(preceded(char_sp(','), expr(ctx)))(i)?;
                    let (i, align) = if padding.is_some() {
                        opt(preceded(char_sp(','), alignment))(i)?
                    } else {
                        (i, None)
                    };
                    let (i, _) = preceded(sp, char('%'))(i)?;
                    ctx.ban_percent.set(ban_percent);

                    (i, ex, padding, align)
                }
                Some(FormType::Brace) => {
                    let (i, ex) = expr(ctx)(i)?;
                    let (i, padding) = opt(preceded(char_sp(','), expr(ctx)))(i)?;
                    let (i, align) = if padding.is_some() {
                        opt(preceded(char_sp(','), alignment))(i)?
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

                    if let Some(i) = i.strip_prefix('#') {
                        let (i, or_false) =
                            preceded(sp, form_str(FormStrType::SecondCond, ctx))(i)?;
                        let i = i.strip_prefix("\\@").unwrap();
                        (i, Expr::cond(cond, if_true, or_false), None, None)
                    } else if let Some(i) = i.strip_prefix("\\@") {
                        (i, Expr::cond(cond, if_true, Expr::str("")), None, None)
                    } else {
                        unreachable!()
                    }
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

fn string<'a>(i: &'a str) -> IResult<'a, &'a str> {
    context(
        "string",
        delimited(char('\"'), alt((parse_str, success(""))), char('\"')),
    )(i)
}

fn paran_expr<'c, 'a>(ctx: &'c ParserContext) -> impl FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i| {
        let is_arg = ctx.is_arg.get();
        let ban_percent = ctx.ban_percent.get();
        ctx.is_arg.set(false);
        ctx.ban_percent.set(false);

        let (i, expr) = de_char_sp('(', expr(ctx), ')')(i)?;

        ctx.is_arg.set(is_arg);
        ctx.ban_percent.set(ban_percent);

        Ok((i, expr))
    }
}

pub fn var_func_extern<'a>(i: &'a str) -> IResult<'a, Option<Box<str>>> {
    opt(map(preceded(char('@'), ident), |s| s.into()))(i)
}

fn ident_or_method_expr<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i| {
        let (i, ident) = ident_or_macro(ctx, i)?;
        let i = i.trim_start_matches(' ');

        if let Some(i) = i.strip_prefix('(') {
            let p = ctx.ban_percent.get();
            ctx.ban_percent.set(false);
            let (i, args) = terminated(separated_list0(char_sp(','), expr(ctx)), char_sp(')'))(i)?;
            ctx.ban_percent.set(p);
            Ok((i, Expr::Method(ident.into(), args)))
        } else {
            let (i, func_extern) = var_func_extern(i)?;
            match ident {
                Cow::Borrowed(ident) => {
                    let var = ident;
                    if !ctx.is_arg.get() {
                        let (i, args) = variable_arg(ctx, &var)(i)?;
                        Ok((
                            i,
                            Expr::Var(Variable {
                                var: var.into(),
                                func_extern,
                                args,
                            }),
                        ))
                    } else {
                        Ok((
                            i,
                            Expr::Var(Variable {
                                var: var.into(),
                                func_extern,
                                args: Vec::new(),
                            }),
                        ))
                    }
                }
                Cow::Owned(m) => {
                    if !ctx.is_arg.get() && is_ident(&m) {
                        let (i, args) = variable_arg(ctx, &m)(i)?;
                        Ok((
                            i,
                            Expr::Var(Variable {
                                var: m.into_boxed_str(),
                                func_extern,
                                args,
                            }),
                        ))
                    } else {
                        match expr(ctx)(&m) {
                            Ok((left, expr)) => {
                                if !left.is_empty() {
                                    log::error!("Macro must be complete form");
                                    Err(nom::Err::Failure(error_position!(i, ErrorKind::Eof)))
                                } else {
                                    Ok((i, expr))
                                }
                            }
                            Err(err) => Err(err.map(|e| VerboseError {
                                errors: e.errors.into_iter().map(|(_i, kind)| (i, kind)).collect(),
                            })),
                        }
                    }
                }
            }
        }
    }
}

fn single_expr<'c, 'a>(ctx: &'c ParserContext) -> impl FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i| {
        enum UnaryIncOp {
            Inc,
            Dec,
            Unary(UnaryOperator),
            None,
        }
        use UnaryIncOp::*;
        let i = i.trim_start_matches(' ');

        let (i, op) = if let Some(i) = i.strip_prefix("++") {
            (i, Inc)
        } else if let Some(i) = i.strip_prefix("--") {
            (i, Dec)
        } else if let Some(i) = i.strip_prefix('+') {
            (i, None)
        } else if let Some(i) = i.strip_prefix('!') {
            (i, Unary(UnaryOperator::Not))
        } else if let Some(i) = i.strip_prefix('~') {
            (i, Unary(UnaryOperator::Not))
        } else if let Some(i) = i.strip_prefix('-') {
            (i, Unary(UnaryOperator::Minus))
        } else {
            (i, None)
        };
        let i = i.trim_start_matches(' ');

        let (i, expr) = context(
            "single_expr",
            de_sp(alt((
                value(Expr::Int(i64::MAX), tag("__INT_MAX__")),
                value(Expr::Int(i64::MIN), tag("__INT_MIN__")),
                delimited(tag("@\""), form_str(FormStrType::Str, ctx), tag("\"")),
                map(string, |s| Expr::String(s.into())),
                map(preceded(tag("0x"), hex_digit1), |s| {
                    Expr::Int(i64::from_str_radix(s, 16).unwrap())
                }),
                map(preceded(tag("0o"), oct_digit1), |s| {
                    Expr::Int(i64::from_str_radix(s, 8).unwrap())
                }),
                map(
                    preceded(tag("0b"), take_while1(|c| matches!(c, '0' | '1'))),
                    |s| Expr::Int(i64::from_str_radix(s, 2).unwrap()),
                ),
                map(pair(terminated(i64, char('p')), i64), |(l, r)| {
                    Expr::Int(l ^ r)
                }),
                map(i64, Expr::Int),
                ident_or_method_expr(ctx),
                paran_expr(ctx),
            ))),
        )(i)?;
        let i = i.trim_start_matches(' ');

        let expr = match op {
            Unary(op) => Expr::unary(expr, op),
            Inc | Dec => {
                let is_inc = matches!(op, Inc);

                match expr {
                    Expr::Var(var) => Expr::IncOpExpr {
                        var,
                        is_pre: true,
                        is_inc,
                    },
                    _ => {
                        log::error!("증감연산자는 변수와 함께 써야합니다.");
                        return Err(nom::Err::Failure(error_position!(i, ErrorKind::Verify)));
                    }
                }
            }
            None => expr,
        };

        if ctx.is_arg.get() {
            return Ok((i, expr));
        }

        Ok(if let Some(i) = i.strip_prefix("++") {
            let expr = match expr {
                Expr::Var(var) => Expr::IncOpExpr {
                    var,
                    is_pre: false,
                    is_inc: true,
                },
                _ => {
                    return Err(nom::Err::Error(error_position!(i, ErrorKind::Verify)));
                }
            };
            (i, expr)
        } else if let Some(i) = i.strip_prefix("--") {
            let expr = match expr {
                Expr::Var(var) => Expr::IncOpExpr {
                    var,
                    is_pre: false,
                    is_inc: false,
                },
                _ => {
                    return Err(nom::Err::Error(error_position!(i, ErrorKind::Verify)));
                }
            };
            (i, expr)
        } else {
            (i, expr)
        })
    }
}

fn binop(i: &str) -> IResult<'_, BinaryOperator> {
    alt((
        value(BinaryOperator::Add, char('+')),
        value(BinaryOperator::Sub, char('-')),
        value(BinaryOperator::Mul, char('*')),
        value(BinaryOperator::Div, char('/')),
        value(BinaryOperator::Rem, char('%')),
        value(BinaryOperator::Lhs, tag("<<")),
        value(BinaryOperator::Rhs, tag(">>")),
        value(BinaryOperator::Xor, tag("^^")),
        value(BinaryOperator::Or, tag("||")),
        value(BinaryOperator::And, tag("&&")),
        value(BinaryOperator::BitXor, char('^')),
        value(BinaryOperator::BitOr, char('|')),
        value(BinaryOperator::BitAnd, char('&')),
        value(BinaryOperator::Equal, tag("==")),
        value(BinaryOperator::NotEqual, tag("!=")),
        value(BinaryOperator::GreaterOrEqual, tag(">=")),
        value(BinaryOperator::Greater, tag(">")),
        value(BinaryOperator::LessOrEqual, tag("<=")),
        value(BinaryOperator::Less, tag("<")),
    ))(i)
}

fn bin_expr<'c, 'a>(ctx: &'c ParserContext) -> impl FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i| {
        let (mut i, first) = single_expr(ctx)(i)?;
        let mut stack = Vec::with_capacity(8);

        loop {
            if ctx.is_arg.get() {
                if i.starts_with("++") || i.starts_with("--") {
                    break;
                }
            }

            let (new_i, op) = opt(binop)(i)?;

            if new_i.starts_with('=') {
                break;
            }

            match op {
                Some(BinaryOperator::Rem) if ctx.ban_percent.get() => break,
                Some(op) => {
                    let (new_i, expr) = single_expr(ctx)(new_i)?;
                    i = new_i;
                    stack.push((op, expr));
                }
                None => {
                    break;
                }
            }
        }

        Ok((i, calculate_binop_expr(first, &mut stack)))
    }
}

pub fn expr<'c, 'a>(ctx: &'c ParserContext) -> impl FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i| {
        let (i, expr) = bin_expr(ctx)(i)?;

        let (i, cond) = opt(pair(de_char_sp('?', bin_expr(ctx), '#'), bin_expr(ctx)))(i)?;

        let expr = match cond {
            Some((if_true, or_false)) => Expr::cond(expr, if_true, or_false),
            None => expr,
        };

        Ok((i, expr))
    }
}

pub fn expr_list<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Vec<Expr>> + 'c {
    move |i| separated_list0(char(','), de_sp(expr(ctx)))(i)
}

pub fn call_arg_list<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Vec<Expr>> + 'c {
    move |i| {
        preceded(
            sp,
            alt((
                de_char_sp('(', expr_list(ctx), ')'),
                preceded(char_sp(','), expr_list(ctx)),
                value(Vec::new(), eof),
            )),
        )(i)
    }
}

pub fn assign_op<'a>(i: &'a str) -> IResult<'a, Option<BinaryOperator>> {
    let op = alt((
        value(BinaryOperator::Add, char('+')),
        value(BinaryOperator::Sub, char('-')),
        value(BinaryOperator::Mul, char('*')),
        value(BinaryOperator::Div, char('/')),
        value(BinaryOperator::Rem, char('%')),
        value(BinaryOperator::Lhs, tag("<<")),
        value(BinaryOperator::Rhs, tag(">>")),
        value(BinaryOperator::BitXor, char('^')),
        value(BinaryOperator::BitOr, char('|')),
        value(BinaryOperator::BitAnd, char('&')),
    ));

    context("AssignOp", terminated(opt(op), char('=')))(i)
}

pub fn call_jump_line<'c, 'a>(
    ctx: &'c ParserContext,
    is_form: bool,
) -> impl FnMut(&'a str) -> IResult<'a, (Expr, Vec<Expr>)> + 'c {
    move |i| {
        let (i, name) = if is_form {
            form_arg_expr(ctx)(i)?
        } else {
            let (i, function) = ident(i)?;
            let function = ctx.replace(function);

            if !is_ident(function.as_ref()) {
                panic!("CALL/JUMP문은 식별자를 받아야합니다");
            }

            (i, Expr::String(function.into_owned().into_boxed_str()))
        };

        let (i, args) = call_arg_list(ctx)(i)?;

        Ok((i, (name, args)))
    }
}

fn case_cond<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, SelectCaseCond> + 'c {
    move |i| {
        alt((
            map(
                tuple((expr(ctx), de_sp(tag("TO")), expr(ctx))),
                |(l, _, r)| SelectCaseCond::To(l, r),
            ),
            map(
                preceded(de_sp(tag("IS")), pair(de_sp(binop), expr(ctx))),
                |(op, expr)| SelectCaseCond::Is(op, expr),
            ),
            map(expr(ctx), SelectCaseCond::Single),
        ))(i)
    }
}

pub fn case_line<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Vec<SelectCaseCond>> + 'c {
    move |i| separated_list1(char_sp(','), case_cond(ctx))(i)
}

pub fn for_line<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, (Variable, Expr, Expr, Expr)> + 'c {
    move |i| {
        let (i, mut exprs) = expr_list(ctx)(i)?;
        match exprs.len() {
            3 => {
                let end = exprs.pop().unwrap();
                let init = exprs.pop().unwrap();
                let var = exprs.pop().unwrap().into_var().unwrap();
                Ok((i, (var, init, end, Expr::int(1))))
            }
            4 => {
                let step = exprs.pop().unwrap();
                let end = exprs.pop().unwrap();
                let init = exprs.pop().unwrap();
                let var = exprs.pop().unwrap().into_var().unwrap();
                Ok((i, (var, init, end, step)))
            }
            other => {
                log::error!("FOR문은 인자로 3개나 4개를 가져야합니다: 받은 인자수 {other}개");
                Err(nom::Err::Failure(error_position!(i, ErrorKind::Verify)))
            }
        }
    }
}

pub fn times_line<'c, 'a>(ctx: &'c ParserContext) -> impl FnMut(&'a str) -> IResult<'a, Stmt> + 'c {
    move |i| {
        let (i, var) = variable(ctx)(i)?;
        let (i, times) = preceded(char_sp(','), float)(i)?;
        Ok((i, Stmt::Times(var, NotNan::new(times).unwrap())))
    }
}

pub fn assign_line<'c, 'a>(
    ctx: &'c ParserContext,
    ident: &'c str,
) -> impl FnMut(&'a str) -> IResult<'a, Stmt> + 'c {
    move |mut i| {
        let var_name = ctx.replace(ident);

        if !is_ident(&var_name) {
            log::error!("Expanded variable name in assign line is incorrect `{var_name}`");
            return Err(nom::Err::Failure(error_position!(i, ErrorKind::Verify)));
        }

        i = i.trim_end_matches(' ');

        let (i, func_extern) = var_func_extern(i)?;
        let (i, args) = variable_arg(ctx, &var_name)(i)?;

        let var = Variable {
            var: var_name.into(),
            func_extern,
            args,
        };

        let i = i.trim_start_matches(' ');

        if let Some(i) = i.strip_prefix("++") {
            Ok((
                i,
                Stmt::Assign(var, Some(BinaryOperator::Add), Expr::Int(1)),
            ))
        } else if let Some(i) = i.strip_prefix("--") {
            Ok((
                i,
                Stmt::Assign(var, Some(BinaryOperator::Sub), Expr::Int(1)),
            ))
        } else if ctx.is_str_var(&var.var) {
            let (i, op) = delimited(sp, assign_op, opt(char(' ')))(i)?;
            let (i, rhs) = normal_form_str(ctx)(i)?;

            Ok((i, Stmt::Assign(var, op, rhs)))
        } else {
            let (i, op) = de_sp(assign_op)(i)?;
            let (i, rhs) = expr(ctx)(i)?;

            Ok((i, Stmt::Assign(var, op, rhs)))
        }
    }
}

pub fn dim_line<'c, 'a>(
    ctx: &'c ParserContext,
    is_str: bool,
) -> impl FnMut(&'a str) -> IResult<'a, LocalVariable> + 'c {
    move |mut i| {
        let mut info = VariableInfo::default();
        info.is_str = is_str;

        let var = loop {
            let (i_, id) = ident(i)?;
            i = i_;
            match id {
                "CHARADATA" => {
                    info.is_chara = true;
                }
                other => break other,
            }
        };
        let (i, sizes) = separated_list0(char_sp(','), map(u32, |i| i as usize))(i)?;
        info.size = sizes;
        let (i, init) = opt(preceded(
            char_sp('='),
            separated_list0(char_sp(','), map(expr(ctx), |expr| const_eval(ctx, expr))),
        ))(i)?;
        info.init = init.unwrap_or_default();

        Ok((
            i,
            LocalVariable {
                var: var.into(),
                info,
            },
        ))
    }
}

fn const_eval(_ctx: &ParserContext, expr: Expr) -> Value {
    match expr {
        Expr::Int(i) => Value::Int(i),
        Expr::String(s) => Value::String(s.into_string()),
        _ => panic!("Can't evaulating expression"),
    }
}

pub fn form_arg_expr<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i| (de_sp(form_str(FormStrType::Arg, ctx)))(i)
}

fn function_arg_list<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Vec<(Variable, Option<Value>)>> + 'c {
    move |i| {
        separated_list0(
            char(','),
            de_sp(pair(
                variable(ctx),
                opt(preceded(
                    char_sp('='),
                    map(expr(ctx), |expr| const_eval(ctx, expr)),
                )),
            )),
        )(i)
    }
}

pub fn function_line<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, (&'a str, Vec<(Variable, Option<Value>)>)> + 'c {
    move |i| {
        pair(
            ident,
            preceded(
                sp,
                alt((
                    de_char_sp('(', function_arg_list(ctx), ')'),
                    preceded(char_sp(','), function_arg_list(ctx)),
                    value(Vec::new(), eof),
                )),
            ),
        )(i)
    }
}

fn variable_named_arg<'c, 'a>(
    ctx: &'c ParserContext,
    var: &'c str,
) -> impl FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i| {
        // alias
        let var = match var {
            "MAXBASE" | "UPBASE" | "DOWNBASE" | "LOSEBASE" => "BASE",
            "GOTJUEL" | "JUEL" | "UP" | "DOWN" => "PALAM",
            "ITEMSALES" | "ITEMPRICE" => "ITEM",
            "NOWEX" => "EX",
            _ => var,
        };

        let (i, ident) = ident(i)?;
        if let Some(v) = ctx
            .header
            .var_names
            .get(&(SmolStr::new_inline(var), SmolStr::new_inline(ident)))
        {
            Ok((i, Expr::int(*v)))
        } else {
            Err(nom::Err::Error(error_position!(i, ErrorKind::Verify)))
        }
    }
}

pub fn variable_arg<'c, 'a>(
    ctx: &'c ParserContext,
    var: &'c str,
) -> impl FnMut(&'a str) -> IResult<'a, Vec<Expr>> + 'c {
    move |i| {
        let is_arg = ctx.is_arg.get();
        ctx.is_arg.set(true);
        let (i, args) = many0(preceded(
            char_sp(':'),
            alt((variable_named_arg(ctx, var), single_expr(ctx))),
        ))(i)?;
        ctx.is_arg.set(is_arg);

        Ok((i, args))
    }
}

pub fn variable<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Variable> + 'c {
    move |i| {
        let (i, name) = de_sp(ident)(i)?;

        if !is_ident(&name) {
            panic!("Variable error");
        }

        let (i, func_extern) = var_func_extern(i)?;
        let (i, args) = variable_arg(ctx, name)(i)?;

        Ok((
            i,
            Variable {
                var: name.into(),
                func_extern,
                args,
            },
        ))
    }
}

fn calculate_binop_expr(first: Expr, stack: &mut Vec<(BinaryOperator, Expr)>) -> Expr {
    let mut expr_stack = Vec::with_capacity(16);
    let mut op_stack: Vec<BinaryOperator> = Vec::with_capacity(10);

    expr_stack.push(first);

    for (op, expr) in stack.drain(..) {
        loop {
            if op_stack.last().map_or(true, |o| o.priority() < op.priority()) {
                expr_stack.push(expr);
                op_stack.push(op);
                break;
            } else {
                let op = op_stack.pop().unwrap();
                let rhs = expr_stack.pop().unwrap();
                let lhs = expr_stack.pop().unwrap();
                expr_stack.push(Expr::binary(lhs, op, rhs));
            }
        }
    }

    for op in op_stack.into_iter().rev() {
        let rhs = expr_stack.pop().unwrap();
        let lhs = expr_stack.pop().unwrap();
        expr_stack.push(Expr::binary(lhs, op, rhs));
    }

    let ret = expr_stack.pop().unwrap();

    debug_assert!(expr_stack.is_empty());

    ret
}
