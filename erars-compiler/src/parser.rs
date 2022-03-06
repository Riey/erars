use std::ops::Range;

use crate::{BinaryOperator, Expr, Function, ParserError, ParserResult, Stmt};
use serde::{Deserialize, Serialize};

bitflags::bitflags! {
    #[derive(Serialize, Deserialize)]
    pub struct PrintFlags: u32 {
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FormStatus {
    // \@ ~ ?
    FormCondExpr,
    // #
    FormSharpExpr,
    /// In this case, '%' can't be used as rem operator
    FormIntExpr,
    FormStrExpr,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CondStatus {
    // ? ~ #
    FormCondExpr1,
    // # ~ \@
    FormCondExpr2,
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

    fn read_until_newline(&mut self) -> &'s str {
        let slice = self.text.as_bytes();
        let pos = memchr::memchr(b'\n', slice).unwrap_or(slice.len());
        let ret = unsafe { std::str::from_utf8_unchecked(slice.get_unchecked(..pos)) };
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

        self.text = self
            .text
            .split_at(self.text.len() - bytes.as_slice().len())
            .0;
    }

    fn consume(&mut self, s: &str) {
        self.text = &self.text[s.len()..];
    }

    fn get_ident(&mut self) -> &'s str {
        let pos = self.text.find(is_not_ident_char).unwrap_or(self.text.len());
        let (ret, text) = self.text.split_at(pos);
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

    fn read_normal_text(&mut self) -> &'s str {
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
                    '#' if self.cond_status == Some(CondStatus::FormCondExpr1) => {
                        self.form_status = Some(FormStatus::FormSharpExpr);
                        self.cond_status = Some(CondStatus::FormCondExpr2);
                        break;
                    }
                    '\\' => {
                        match chars.next() {
                            Some('@') => {
                                if self.cond_status == Some(CondStatus::FormCondExpr2) {
                                    self.cond_status = None;
                                    skip_count = 3;
                                } else {
                                    self.form_status = Some(FormStatus::FormCondExpr);
                                    self.cond_status = Some(CondStatus::FormCondExpr1);
                                    skip_count = 2;
                                }
                                break;
                            }
                            // TODO other escapes
                            Some(_) |
                            // TODO return error
                            None => break,
                        }
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
                return ret;
            }
        }

        let mut ret = if self.cond_status == Some(CondStatus::FormCondExpr2) {
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

        ret
    }

    fn read_form_text(&mut self) -> ParserResult<(String, Vec<(Expr, String)>)> {
        let first = self.read_normal_text().into();

        let mut other = Vec::new();

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
                Some(FormStatus::FormCondExpr) | Some(FormStatus::FormSharpExpr) => {
                    todo!("print conditional")
                }
            };

            self.form_status = None;

            other.push((expr, self.read_normal_text().into()));
        }

        Ok((first, other))
    }

    fn try_read_command(&mut self) -> Option<ParserResult<Stmt>> {
        if self.try_get_prefix("PRINTFORM") {
            let flags = match self.read_print_flags() {
                Ok(f) => f,
                Err(err) => return Some(Err(err)),
            };
            self.skip_blank();

            Some(
                self.read_form_text()
                    .map(|(first, other)| Stmt::PrintForm(flags, first, other)),
            )
        } else if self.try_get_prefix("PRINT") {
            let flags = match self.read_print_flags() {
                Ok(f) => f,
                Err(err) => return Some(Err(err)),
            };
            self.skip_blank();

            let text = self.read_until_newline();
            Some(Ok(Stmt::Print(flags, text.into())))
        } else {
            None
        }
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

        if let Some(num) = self.try_read_number() {
            self.skip_ws();

            num.map(Expr::IntLit)
        } else {
            Err((
                ParserError::InvalidCode(format!("표현식이 와야합니다.")),
                self.current_loc_span(),
            ))
        }
    }

    fn next_expr(&mut self) -> ParserResult<Expr> {
        self.skip_ws();

        let mut term = self.read_term()?;

        self.skip_ws();

        loop {
            let backup = self.text;
            if let Some(symbol) = self.try_get_symbol() {
                if let Ok(binop) = symbol.parse::<BinaryOperator>() {
                    term = Expr::BinopExpr(Box::new(term), binop, Box::new(self.read_term()?));
                } else if symbol == "?" {
                    let if_true = self.next_expr()?;
                    self.skip_ws();
                    self.ensure_get_char('#')?;

                    let or_false = self.next_expr()?;
                    self.skip_ws();
                    term = Expr::CondExpr(Box::new(term), Box::new(if_true), Box::new(or_false));
                } else {
                    self.text = backup;
                    break;
                }
            } else {
                break;
            }
        }

        Ok(term)
    }

    fn next_stmt(&mut self) -> Option<ParserResult<Stmt>> {
        self.skip_ws();

        if let Some(com) = self.try_read_command() {
            Some(com)
        } else if let Some(_ident) = self.try_get_ident() {
            // assign
            self.skip_ws();
            match self.try_get_symbol() {
                Some(symbol) => todo!("{}", symbol),
                None => Some(Err((
                    ParserError::InvalidCode(format!("알수없는 코드")),
                    self.current_loc_span(),
                ))),
            }
        } else {
            None
        }
    }

    fn next_body(&mut self) -> ParserResult<Vec<Stmt>> {
        let mut ret = Vec::with_capacity(100);

        while let Some(stmt) = self.next_stmt() {
            ret.push(stmt?);
        }

        Ok(ret)
    }

    fn next_function(&mut self) -> ParserResult<Function> {
        todo!("Function")
    }

    fn next_program(&mut self) -> ParserResult<Vec<Function>> {
        todo!("Program")
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
    )
}

fn is_not_symbol_char(c: char) -> bool {
    !is_symbol_char(c)
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

#[cfg(test)]
mod tests {
    use super::*;
    use codespan_reporting::diagnostic::{Diagnostic, Label};
    use codespan_reporting::files::SimpleFiles;
    use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
    use codespan_reporting::term::Config;

    fn do_test<T>(f: fn(&str) -> ParserResult<T>, source: &str) -> T {
        let mut files = SimpleFiles::new();
        let file_id = files.add("test.erb", source);

        match f(source) {
            Ok(ret) => ret,
            Err((err, span)) => {
                let diagnostic = Diagnostic::error()
                    .with_code("E0001")
                    .with_message("Compile ERROR")
                    .with_labels(vec![
                        Label::primary(file_id, span).with_message(format!("{}", err))
                    ]);
                let writer = StandardStream::stderr(ColorChoice::Always);
                let config = Config::default();
                codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)
                    .unwrap();
                panic!("Test failed");
            }
        }
    }

    macro_rules! snapshot {
        ($func:expr, $code:literal $(,)? $( $expected:literal )?) => {
            k9::snapshot!(do_test($func, $code) $(, $expected)?);
        };
    }

    #[test]
    fn var_expr() {
        snapshot!(parse_expr, "COUNT:123");
    }

    #[test]
    fn var_empty_expr() {
        snapshot!(parse_expr, "COUNT");
    }

    #[test]
    fn var_var_expr() {
        snapshot!(parse_expr, "COUNT:A");
    }

    #[test]
    fn assign() {
        snapshot!(parse_body, "A:2 = 123");
    }

    #[test]
    fn paran_expr() {
        snapshot!(parse_expr, "1 + 2 ? 1 + 2 * 3 # (5+1) / 2");
    }

    #[test]
    fn cond_printform() {
        snapshot!(parse_body, "PRINTFORML \\@ 1 ? asdf2 # 3fe \\@");
    }

    #[test]
    fn plus() {
        snapshot!(
            parse_expr,
            "1 + 1",
            "
BinopExpr(
    IntLit(
        1,
    ),
    Add,
    IntLit(
        1,
    ),
)
"
        );
    }

    #[test]
    fn cond_expr() {
        snapshot!(
            parse_expr,
            "1 ? 2 # 3",
            "
CondExpr(
    IntLit(
        1,
    ),
    IntLit(
        2,
    ),
    IntLit(
        3,
    ),
)
"
        );
    }

    #[test]
    fn parse_simple_function() {
        snapshot!(
            parse_function,
            "@SYSTEM_TITLE\n#PRI\nPRINTL Hello, world!\n",
            r#"
Function {
    header: FunctionHeader {
        name: "SYSTEM_TITLE",
        infos: [
            EventFlag(
                Pre,
            ),
        ],
    },
    body: [
        Print(
            NEWLINE,
            "Hello, world!",
        ),
    ],
}
"#
        );
    }

    #[test]
    fn hello_world() {
        snapshot!(
            parse_body,
            "PRINTL Hello, world!",
            r#"
[
    Print(
        NEWLINE,
        "Hello, world!",
    ),
]
"#
        );
    }

    #[test]
    fn form_simple() {
        snapshot!(
            parse_body,
            "PRINTFORML 1 + 1 = {1 + 1}",
            r#"
[
    PrintForm(
        NEWLINE,
        "1 + 1 = ",
        [
            (
                BinopExpr(
                    IntLit(
                        1,
                    ),
                    Add,
                    IntLit(
                        1,
                    ),
                ),
                "",
            ),
        ],
    ),
]
"#
        );
    }
}