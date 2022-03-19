use std::ops::Range;

use crate::{
    ast::FormText, BinaryOperator, EventFlags, Expr, Function, FunctionHeader, FunctionInfo,
    ParserError, ParserResult, Stmt, Variable,
};
use bitflags::bitflags;
use serde::{Deserialize, Serialize};

macro_rules! try_option {
    ($e:expr) => {
        match $e {
            Ok(e) => e,
            Err(err) => return Some(Err(err)),
        }
    };
}

option_set::option_set! {
    pub struct PrintFlags: UpperSnake + u32 {
        const NEWLINE = 0x1;
        const WAIT = 0x2;
        const LEFT_ALIGN = 0x4;
        const RIGHT_ALIGN = 0x8;
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Alignment {
    Left,
    Center,
    Right,
}

impl Default for Alignment {
    fn default() -> Self {
        Alignment::Left
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FormStatus {
    /// \@ ~ ?
    FormCondExpr,
    /// In this case, '%' can't be used as rem operator
    FormIntExpr,
    FormStrExpr,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CondStatus {
    // ? ~ #
    CondFormer,
    // # ~ \@
    CondLater,
}

#[derive(Clone)]
pub struct Parser<'s> {
    text: &'s str,
    form_status: Option<FormStatus>,
    cond_status: Option<CondStatus>,
    begin_loc: usize,
}

impl<'s> Parser<'s> {
    pub fn new(text: &'s str) -> Self {
        Self {
            text,
            form_status: None,
            cond_status: None,
            begin_loc: text.as_ptr() as usize,
        }
    }

    fn is_str_var(&self, name: &str) -> bool {
        match name {
            "LOCALS" | "STR" => true,
            _ => false,
        }
    }

    fn current_loc(&self) -> usize {
        self.text.as_ptr() as usize - self.begin_loc
    }

    fn from_prev_loc_span(&self, prev_loc: usize) -> Range<usize> {
        prev_loc..self.current_loc()
    }

    fn current_loc_span(&self) -> Range<usize> {
        let loc = self.current_loc();
        loc..loc
    }

    fn read_until_quote(&mut self) -> &'s str {
        let slice = self.text.as_bytes();
        let pos = memchr::memchr(b'"', slice).unwrap_or(slice.len());
        let ret = unsafe { std::str::from_utf8_unchecked(slice.get_unchecked(..pos)) };
        self.consume(ret);
        ret
    }

    fn read_until_newline(&mut self) -> &'s str {
        let slice = self.text.as_bytes();
        let pos = memchr::memchr(b'\n', slice).unwrap_or(slice.len());
        let ret = unsafe { std::str::from_utf8_unchecked(slice.get_unchecked(..pos)) }
            .trim_end_matches('\r');
        self.consume(ret);
        ret
    }

    fn skip_ws(&mut self) {
        let mut bytes = self.text.as_bytes().iter();

        while let Some(b) = bytes.next() {
            match b {
                b' ' | b'\t' | b'\r' | b'\n' => {}
                b';' => {
                    let slice = bytes.as_slice();
                    let pos = memchr::memchr(b'\n', slice).unwrap_or(slice.len());
                    // let comment =
                    //     unsafe { std::str::from_utf8_unchecked(slice.get_unchecked(..pos)) };
                    // self.comment_handler.add_comment(self.location(), comment);
                    bytes = unsafe { slice.get_unchecked(pos..) }.iter();
                }
                _ => {
                    self.text = self
                        .text
                        .split_at(self.text.len() - bytes.as_slice().len() - 1)
                        .1;
                    return;
                }
            }
        }

        self.text = &self.text[self.text.len()..];
    }

    fn consume(&mut self, s: &str) {
        self.text = &self.text[s.len()..];
    }

    fn get_ident(&mut self) -> &'s str {
        let pos = self.text.find(is_not_ident_char).unwrap_or(self.text.len());
        let (ret, text) = self.text.split_at(pos);

        if ret
            .as_bytes()
            .first()
            .map_or(false, |&b| matches!(b, b'0'..=b'9'))
        {
            return "";
        }

        self.text = text;
        ret
    }

    fn get_symbol(&mut self) -> &'s str {
        let pos = self
            .text
            .find(is_not_symbol_char)
            .unwrap_or(self.text.len());
        let (ret, text) = self.text.split_at(pos);
        self.text = text;
        ret
    }

    fn try_get_ident(&mut self) -> Option<&'s str> {
        let ident = self.get_ident();

        if ident.is_empty() {
            None
        } else {
            Some(ident)
        }
    }

    fn try_get_symbol(&mut self) -> Option<&'s str> {
        let symbol = self.get_symbol();

        if symbol.is_empty() {
            None
        } else {
            Some(symbol)
        }
    }

    /// return false if current char is whitespace or EOS
    fn ensure_whitespace_or_empty(&self) -> bool {
        self.text
            .chars()
            .next()
            .map(|c| c.is_ascii_whitespace())
            .unwrap_or(true)
    }

    fn try_get_char(&mut self, prefix: char) -> bool {
        if let Some(text) = self.text.strip_prefix(prefix) {
            self.text = text;
            true
        } else {
            false
        }
    }

    #[allow(unused)]
    fn ensure_get_prefix(&mut self, prefix: &str) -> ParserResult<()> {
        if self.try_get_prefix(prefix) {
            Ok(())
        } else {
            Err((
                ParserError::MissingToken(prefix.to_string()),
                self.current_loc_span(),
            ))
        }
    }

    fn ensure_get_char(&mut self, prefix: char) -> ParserResult<()> {
        if self.try_get_char(prefix) {
            Ok(())
        } else {
            Err((
                ParserError::MissingToken(prefix.to_string()),
                self.current_loc_span(),
            ))
        }
    }

    fn skip_blank(&mut self) {
        self.try_get_char(' ');
    }

    fn try_get_prefix(&mut self, prefix: &str) -> bool {
        if let Some(text) = self.text.strip_prefix(prefix) {
            self.text = text;
            true
        } else {
            false
        }
    }

    fn read_print_flags(&mut self) -> ParserResult<PrintFlags> {
        let start = self.current_loc();

        let mut ret = PrintFlags::empty();

        if self.try_get_char('L') {
            ret.insert(PrintFlags::NEWLINE);
        } else if self.try_get_char('W') {
            ret.insert(PrintFlags::WAIT | PrintFlags::NEWLINE);
        }

        if self.ensure_whitespace_or_empty() {
            Ok(ret)
        } else {
            Err((
                ParserError::InvalidCode(format!("알수없는 PRINT 플래그입니다")),
                self.from_prev_loc_span(start),
            ))
        }
    }

    fn read_normal_text(&mut self) -> ParserResult<&'s str> {
        let mut chars = self.text.chars();
        let mut skip_count = 1;

        loop {
            if let Some(ch) = chars.next() {
                match ch {
                    '%' => {
                        self.form_status = Some(FormStatus::FormStrExpr);
                        break;
                    }
                    '{' => {
                        self.form_status = Some(FormStatus::FormIntExpr);
                        break;
                    }
                    '#' if self.cond_status == Some(CondStatus::CondFormer) => {
                        self.cond_status = Some(CondStatus::CondLater);
                        break;
                    }
                    '\\' => {
                        match chars.next() {
                            Some('@') => {
                                match self.cond_status {
                                    Some(CondStatus::CondLater) => {
                                        self.cond_status = None;
                                        skip_count = 3;
                                    }
                                    Some(_) => {
                                    return Err((ParserError::UnexpectedToken("\\@".into()), self.current_loc_span()));
                                    }
                                    None => {
                                    self.form_status = Some(FormStatus::FormCondExpr);
                                    skip_count = 2;
                                    }
                                }
                                break;
                            }
                            // TODO other escapes
                            Some(_) |
                            // TODO return error
                            None => break,
                        }
                    }
                    '\r' => {
                        skip_count += 1;
                        chars.next();
                        self.form_status = None;
                        break;
                    }
                    '\n' => {
                        self.form_status = None;
                        break;
                    }
                    _ => {}
                }
            } else {
                let ret = self.text;
                self.text = "";
                self.form_status = None;
                return Ok(ret);
            }
        }

        let mut ret = if self.cond_status == Some(CondStatus::CondLater) {
            let (ret, left) = self
                .text
                .split_at((self.text.len() - chars.as_str().len()).saturating_sub(1));
            self.text = left;
            self.skip_blank();
            ret
        } else {
            let (ret, left) = self.text.split_at(self.text.len() - chars.as_str().len());
            self.text = left;
            ret
        };

        ret = &ret[..ret.len() - skip_count];

        Ok(ret)
    }

    fn read_form_text(&mut self) -> ParserResult<FormText> {
        debug_assert!(self.form_status.is_none());

        let mut form_text = FormText::new(self.read_normal_text()?.into());

        loop {
            let expr = match self.form_status {
                None => break,
                Some(FormStatus::FormIntExpr) => {
                    let expr = self.next_expr()?;
                    self.skip_ws();
                    self.ensure_get_char('}')?;
                    expr
                }
                Some(FormStatus::FormStrExpr) => {
                    let expr = self.next_expr()?;
                    self.skip_ws();
                    self.ensure_get_char('%')?;
                    expr
                }
                Some(FormStatus::FormCondExpr) => {
                    let cond = self.next_expr()?;
                    self.skip_ws();
                    self.ensure_get_char('?')?;
                    self.skip_blank();
                    self.form_status = None;
                    self.cond_status = Some(CondStatus::CondFormer);
                    let if_true = self.read_form_text()?;
                    let or_false = self.read_form_text()?;
                    Expr::cond(cond, Expr::FormText(if_true), Expr::FormText(or_false))
                }
            };

            self.form_status = None;
            form_text.push(expr, self.read_normal_text()?.into());
        }

        Ok(form_text)
    }

    fn read_if_block(
        &mut self,
        else_ifs: &mut Vec<(Expr, Vec<Stmt>)>,
    ) -> ParserResult<Option<Vec<Stmt>>> {
        self.skip_ws();

        let mut body = Vec::new();
        let cond = self.next_expr()?;

        let else_body = loop {
            self.skip_ws();

            if self.try_get_prefix("ENDIF") {
                else_ifs.push((cond, body));
                break None;
            } else if self.try_get_prefix("ELSE") {
                if self.try_get_prefix("IF") {
                    // ELSEIF
                    else_ifs.push((cond, body));
                    break self.read_if_block(else_ifs)?;
                } else {
                    else_ifs.push((cond, body));
                    // ELSE
                    let mut else_body = Vec::new();
                    loop {
                        self.skip_ws();
                        if self.try_get_prefix("ENDIF") {
                            break;
                        }
                        else_body.push(self.next_stmt()?);
                    }
                    break Some(else_body);
                }
            }

            body.push(self.next_stmt()?);
        };

        Ok(else_body)
    }

    fn try_read_command(&mut self) -> Option<ParserResult<Stmt>> {
        if self.try_get_prefix("PRINTFORM") {
            let flags = try_option!(self.read_print_flags());
            self.skip_blank();

            let form_text = try_option!(self.read_form_text());

            Some(Ok(Stmt::PrintForm(flags, form_text)))
        } else if self.try_get_prefix("PRINT") {
            let flags = try_option!(self.read_print_flags());
            self.skip_blank();

            let text = self.read_until_newline();
            Some(Ok(Stmt::Print(flags, text.into())))
        } else if self.try_get_prefix("IF") {
            let mut else_ifs = Vec::new();
            let else_body = try_option!(self.read_if_block(&mut else_ifs));

            Some(Ok(Stmt::If(else_ifs, else_body)))
        } else if self.try_get_prefix("CALL") {
            self.skip_ws();

            let func = match self.try_get_ident() {
                Some(ident) => ident,
                None => {
                    return Some(Err((
                        ParserError::MissingToken(format!("Function label")),
                        self.current_loc_span(),
                    )))
                }
            };

            let args = if self.try_get_char(',') {
                try_option!(self.read_args())
            } else {
                Vec::new()
            };

            Some(Ok(Stmt::Call(func.into(), args)))
        } else {
            None
        }
    }

    fn read_args(&mut self) -> ParserResult<Vec<Expr>> {
        self.skip_ws();

        let mut ret = Vec::new();

        loop {
            ret.push(self.next_expr()?);

            self.skip_ws();

            if !self.try_get_char(',') {
                break;
            }
        }

        Ok(ret)
    }

    fn try_read_number(&mut self) -> Option<ParserResult<i64>> {
        let start = self.current_loc();
        let mut chars = self.text.chars();

        let (mut ret, minus) = match chars.next() {
            Some(n @ '0'..='9') => ((n as u32 - '0' as u32) as i64, false),
            Some('-') => {
                if let Some(n @ '0'..='9') = chars.next() {
                    ((n as u32 - '0' as u32) as i64, true)
                } else {
                    return None;
                }
            }
            _ => return None,
        };

        loop {
            match chars.next() {
                Some(n @ '0'..='9') => {
                    ret = ret * 10 + (n as u32 - '0' as u32) as i64;
                }
                Some(ch) => {
                    if is_ident_char(ch) {
                        return Some(Err((
                            ParserError::InvalidCode(format!("식별자 앞에 숫자를 쓸 수 없습니다.")),
                            self.from_prev_loc_span(start),
                        )));
                    }

                    self.text = self
                        .text
                        .split_at(self.text.len() - chars.as_str().len() - 1)
                        .1;

                    break;
                }
                None => {
                    self.text = "";
                    break;
                }
            }
        }

        Some(Ok(if minus { -ret } else { ret }))
    }

    fn read_term(&mut self) -> ParserResult<Expr> {
        self.skip_ws();

        if self.try_get_char('(') {
            let inner = self.next_expr()?;
            self.ensure_get_char(')')?;
            Ok(inner)
        } else if self.try_get_char('"') {
            let inner = self.read_until_quote();
            self.ensure_get_char('"')?;
            Ok(Expr::str(inner))
        } else if let Some(ident) = self.try_get_ident() {
            return Ok(Expr::var(ident, Vec::new()));
        } else if let Some(num) = self.try_read_number() {
            num.map(Expr::IntLit)
        } else {
            Err((
                ParserError::InvalidCode(format!("표현식이 와야합니다.")),
                self.current_loc_span(),
            ))
        }
    }

    fn next_var(&mut self, ident: &'s str) -> ParserResult<Variable> {
        let mut args = Vec::new();

        while self.try_get_char(':') {
            args.push(self.read_term()?);
        }

        Ok(Variable {
            name: ident.into(),
            args,
        })
    }

    fn next_expr(&mut self) -> ParserResult<Expr> {
        self.skip_ws();

        let mut term = if let Some(ident) = self.try_get_ident() {
            if self.try_get_char('(') {
                // method
                let args = self.read_args()?;
                self.ensure_get_char(')')?;
                Expr::Method(ident.into(), args)
            } else {
                // variable
                Expr::Var(self.next_var(ident)?)
            }
        } else {
            self.read_term()?
        };

        let mut operand_stack = Vec::new();

        self.skip_ws();

        loop {
            let backup = self.text;
            if let Some(symbol) = self.try_get_symbol() {
                if let Ok(binop) = symbol.parse::<BinaryOperator>() {
                    operand_stack.push((binop, self.read_term()?));
                    self.skip_ws();
                } else if symbol == "?" {
                    let cond = calculate_binop_expr(term, &mut operand_stack);
                    let if_true = self.next_expr()?;
                    self.skip_ws();
                    self.ensure_get_char('#')?;

                    let or_false = self.next_expr()?;
                    self.skip_ws();
                    term = Expr::cond(cond, if_true, or_false);
                } else {
                    self.text = backup;
                    break;
                }
            } else {
                break;
            }
        }

        Ok(calculate_binop_expr(term, &mut operand_stack))
    }

    fn next_stmt(&mut self) -> ParserResult<Stmt> {
        self.skip_ws();

        if let Some(com) = self.try_read_command() {
            com
        } else if let Some(ident) = self.try_get_ident() {
            // assign
            let var = self.next_var(ident)?;

            self.skip_ws();
            let symbol_start = self.current_loc();
            match self.try_get_symbol() {
                Some(symbol) => {
                    if self.is_str_var(ident) {
                        if symbol == "=" {
                            self.skip_blank();
                            return Ok(Stmt::Assign(
                                var,
                                None,
                                Expr::FormText(self.read_form_text()?),
                            ));
                        }
                    } else if let Some(left) = symbol.strip_suffix("=") {
                        let additional_op = left.parse().ok();
                        return Ok(Stmt::Assign(var, additional_op, self.next_expr()?));
                    }

                    Err((
                        ParserError::InvalidCode(format!("알수없는 연산자")),
                        self.from_prev_loc_span(symbol_start),
                    ))
                }
                None => Err((
                    ParserError::InvalidCode(format!("알수없는 코드")),
                    self.current_loc_span(),
                )),
            }
        } else if self.text.is_empty() {
            Err((ParserError::Eof, self.current_loc_span()))
        } else {
            Err((
                ParserError::InvalidCode(format!("알수없는 코드")),
                self.current_loc_span(),
            ))
        }
    }

    fn next_body(&mut self) -> ParserResult<Vec<Stmt>> {
        let mut ret = Vec::with_capacity(100);

        loop {
            match self.next_stmt() {
                Ok(stmt) => ret.push(stmt),
                Err((ParserError::Eof, _)) => break,
                Err(err) => return Err(err),
            }
        }

        Ok(ret)
    }

    fn next_function(&mut self) -> ParserResult<Function> {
        self.skip_ws();
        self.ensure_get_char('@')?;
        let label = self.try_get_ident().ok_or_else(|| {
            (
                ParserError::MissingToken("Function label".into()),
                self.current_loc_span(),
            )
        })?;

        self.skip_ws();

        if self.try_get_char('(') {
            todo!("paran argument");
        } else if self.try_get_char(',') {
            todo!("comma argument");
        }

        let mut infos = Vec::new();

        loop {
            self.skip_ws();
            if self.try_get_char('#') {
                let start = self.current_loc();
                let info = self.try_get_ident().ok_or_else(|| {
                    (
                        ParserError::MissingToken("Function info".into()),
                        self.current_loc_span(),
                    )
                })?;
                match info {
                    "PRI" => {
                        infos.push(FunctionInfo::EventFlag(EventFlags::Pre));
                    }
                    "LATER" => {
                        infos.push(FunctionInfo::EventFlag(EventFlags::Later));
                    }
                    "SINGLE" => {
                        infos.push(FunctionInfo::EventFlag(EventFlags::Single));
                    }
                    other => {
                        return Err((
                            ParserError::UnexpectedToken(other.into()),
                            self.from_prev_loc_span(start),
                        ))
                    }
                }
            } else {
                break;
            }
        }

        let body = self.next_body()?;

        Ok(Function {
            header: FunctionHeader {
                name: label.into(),
                infos,
            },
            body,
        })
    }

    fn next_program(&mut self) -> ParserResult<Vec<Function>> {
        let mut ret = Vec::new();

        loop {
            match self.next_function() {
                Ok(f) => ret.push(f),
                Err(err) => {
                    if !self.text.is_empty() {
                        return Err(err);
                    } else {
                        break;
                    }
                }
            }
        }

        Ok(ret)
    }
}

fn is_ident_char(c: char) -> bool {
    matches!(c, '_' | '0'..='9' | 'a'..='z' | 'A'..='Z' | 'ㄱ'..='ㅎ' | 'ㅏ'..='ㅣ' | '가'..='힣')
}

fn is_not_ident_char(c: char) -> bool {
    !is_ident_char(c)
}

fn is_symbol_char(c: char) -> bool {
    matches!(
        c,
        '!' | '@'
            | '#'
            | '$'
            | '%'
            | '^'
            | '&'
            | '*'
            | '('
            | ')'
            | '{'
            | '}'
            | '+'
            | '-'
            | '='
            | ','
            | '.'
            | '/'
            | '?'
            | ':'
            | '<'
            | '>'
    )
}

fn is_not_symbol_char(c: char) -> bool {
    !is_symbol_char(c)
}

fn calculate_binop_expr(first: Expr, stack: &mut Vec<(BinaryOperator, Expr)>) -> Expr {
    let mut expr_stack = Vec::with_capacity(16);
    let mut op_stack: Vec<BinaryOperator> = Vec::with_capacity(10);

    expr_stack.push(first);

    for (op, expr) in stack.drain(..) {
        loop {
            if op_stack
                .last()
                .map_or(true, |o| o.priority() < op.priority())
            {
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

pub fn parse_program(s: &str) -> ParserResult<Vec<Function>> {
    Parser::new(s).next_program()
}

pub fn parse_function(s: &str) -> ParserResult<Function> {
    Parser::new(s).next_function()
}

pub fn parse_expr(s: &str) -> ParserResult<Expr> {
    Parser::new(s).next_expr()
}

pub fn parse_body(s: &str) -> ParserResult<Vec<Stmt>> {
    Parser::new(s).next_body()
}

#[test]
fn calculate_test() {
    k9::snapshot!(
        calculate_binop_expr(
            Expr::int(1),
            &mut vec![
                (BinaryOperator::Add, Expr::int(2)),
                (BinaryOperator::Mul, Expr::int(3)),
            ]
        ),
        "
BinopExpr(
    IntLit(
        1,
    ),
    Add,
    BinopExpr(
        IntLit(
            2,
        ),
        Mul,
        IntLit(
            3,
        ),
    ),
)
"
    );

    // 1 * 3 + 2
    k9::snapshot!(
        calculate_binop_expr(
            Expr::int(1),
            &mut vec![
                (BinaryOperator::Mul, Expr::int(3)),
                (BinaryOperator::Add, Expr::int(2)),
            ]
        ),
        "
BinopExpr(
    BinopExpr(
        IntLit(
            1,
        ),
        Mul,
        IntLit(
            3,
        ),
    ),
    Add,
    IntLit(
        2,
    ),
)
"
    );
}
