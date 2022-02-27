use crate::{Expr, Function, ParserError, ParserResult, Stmt};
use either::Either;
use serde::{Deserialize, Serialize};
use source_span::{DefaultMetrics, Position, Span};

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

const METRICS: DefaultMetrics = DefaultMetrics::with_tab_stop(4);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ParserStatus {
    Print,
    PrintForm,
    // \@ ~ ?
    PrintFormCondExpr,
    // #
    PrintFormSharpExpr,
    PrintFormIntExpr,
    PrintFormStrExpr,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CondStatus {
    // ? ~ #
    PrintFormCondExpr1,
    // # ~ \@
    PrintFormCondExpr2,
}

pub struct Parser<'s> {
    text: &'s str,
    status: Option<ParserStatus>,
    cond_status: Option<CondStatus>,
    span: Span,
}

impl<'s> Parser<'s> {
    pub fn new(text: &'s str) -> Self {
        Self {
            text,
            span: Span::default(),
            status: None,
            cond_status: None,
        }
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
                    self.span.clear();
                    return;
                }
            }

            self.span.push(*b as char, &METRICS);
        }

        self.span.clear();
        self.text = self
            .text
            .split_at(self.text.len() - bytes.as_slice().len())
            .0;
    }

    fn shift_position_char(&mut self, ch: char) {
        self.span.push(ch, &METRICS);
    }

    fn shift_position(&mut self, s: &str) {
        for c in s.chars() {
            self.span.push(c, &METRICS);
        }
    }

    fn consume(&mut self, s: &str) {
        self.shift_position(s);
        self.text = &self.text[s.len()..];
    }

    fn get_ident(&mut self) -> &'s str {
        let pos = self.text.find(is_not_ident_char).unwrap_or(self.text.len());
        let (ret, text) = self.text.split_at(pos);
        self.shift_position(ret);
        self.text = text;
        ret
    }

    fn get_symbol(&mut self) -> &'s str {
        let pos = self
            .text
            .find(is_not_symbol_char)
            .unwrap_or(self.text.len());
        let (ret, text) = self.text.split_at(pos);
        self.shift_position(ret);
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
            self.shift_position_char(prefix);
            self.text = text;
            true
        } else {
            false
        }
    }

    fn skip_blank(&mut self) {
        self.try_get_char(' ');
    }

    fn try_read_prefix(&mut self, prefix: &str) -> bool {
        if let Some(text) = self.text.strip_prefix(prefix) {
            self.shift_position(prefix);
            self.text = text;
            true
        } else {
            false
        }
    }

    fn read_print_flags(&mut self) -> ParserResult<PrintFlags> {
        let mut ret = PrintFlags::empty();

        if self.try_get_char('L') {
            ret.insert(PrintFlags::NEWLINE);
        } else if self.try_get_char('W') {
            ret.insert(PrintFlags::WAIT | PrintFlags::NEWLINE);
        }

        if self.ensure_whitespace_or_empty() {
            Ok(ret)
        } else {
            Err(ParserError::InvalidCode(
                format!("알수없는 PRINT 플래그입니다"),
                self.span,
            ))
        }
    }

    fn read_normal_text(&mut self) -> &'s str {
        let mut chars = self.text.chars();
        let mut skip_count = 1;

        loop {
            if let Some(ch) = chars.next() {
                self.shift_position_char(ch);
                match ch {
                    '%' => {
                        self.status = Some(ParserStatus::PrintFormStrExpr);
                        break;
                    }
                    '{' => {
                        self.status = Some(ParserStatus::PrintFormIntExpr);
                        break;
                    }
                    '#' if self.cond_status == Some(CondStatus::PrintFormCondExpr1) => {
                        self.status = Some(ParserStatus::PrintFormSharpExpr);
                        self.cond_status = Some(CondStatus::PrintFormCondExpr2);
                        break;
                    }
                    '\\' => {
                        match chars.next() {
                            Some('@') => {
                                if self.cond_status == Some(CondStatus::PrintFormCondExpr2) {
                                    self.cond_status = None;
                                    skip_count = 3;
                                } else {
                                    self.status = Some(ParserStatus::PrintFormCondExpr);
                                    self.cond_status = Some(CondStatus::PrintFormCondExpr1);
                                    skip_count = 2;
                                }
                                self.shift_position_char('@');
                                break;
                            }
                            // TODO other escapes
                            Some(_) |
                            // TODO return error
                            None => break,
                        }
                    }
                    '\n' => {
                        self.status = None;
                        break;
                    }
                    _ => {}
                }
            } else {
                let ret = self.text;
                self.text = "";
                self.status = None;
                return ret;
            }
        }

        let mut ret = if self.cond_status == Some(CondStatus::PrintFormCondExpr2) {
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

    fn try_read_command(&mut self) -> Option<ParserResult<Stmt>> {
        if self.try_read_prefix("PRINTFORM") {
            todo!()
            // let flags = self.read_print_flags();
            // self.skip_blank();

            // self.status = Some(ParserStatus::PrintForm);
            // Some(ret)
        } else if self.try_read_prefix("PRINT") {
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
        let mut chars = self.text.chars();

        let (mut ret, minus) = match chars.next() {
            Some(n @ '0'..='9') => {
                self.shift_position_char(n);
                ((n as u32 - '0' as u32) as i64, false)
            }
            Some('-') => {
                if let Some(n @ '0'..='9') = chars.next() {
                    self.shift_position_char('-');
                    self.shift_position_char(n);
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
                    self.shift_position_char(n);
                    ret = ret * 10 + (n as u32 - '0' as u32) as i64;
                }
                Some(ch) => {
                    if is_ident_char(ch) {
                        return Some(Err(ParserError::InvalidCode(
                            format!("식별자 앞에 숫자를 쓸 수 없습니다."),
                            self.span,
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

    fn next_expr(&mut self) -> ParserResult<Expr> {
        todo!("Expr")
    }

    fn next_stmt(&mut self) -> Option<ParserResult<Stmt>> {
        self.skip_ws();

        if let Some(com) = self.try_read_command() {
            Some(com)
        } else if let Some(ident) = self.try_get_ident() {
            // assign
            self.skip_ws();
            match self.try_get_symbol() {
                Some(symbol) => todo!("{}", symbol),
                None => Some(Err(ParserError::InvalidCode(
                    format!("잘못된 코드"),
                    self.span,
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

    #[test]
    fn var_expr() {
        k9::snapshot!(
            parse_expr("COUNT:123"),
            r#"
Var(
    "COUNT",
    [
        IntLit(
            123,
        ),
    ],
)
"#
        );
    }

    #[test]
    fn var_empty_expr() {
        k9::snapshot!(
            parse_expr("COUNT"),
            r#"
Var(
    "COUNT",
    [],
)
"#
        );
    }

    #[test]
    fn var_var_expr() {
        k9::snapshot!(
            parse_expr("COUNT:A"),
            r#"
Var(
    "COUNT",
    [
        Var(
            "A",
            [],
        ),
    ],
)
"#
        );
    }

    #[test]
    fn assign() {
        k9::snapshot!(
            parse_body("A:2 = 123"),
            r#"
[
    Assign(
        Var(
            "A",
            [
                IntLit(
                    2,
                ),
            ],
        ),
        IntLit(
            123,
        ),
    ),
]
"#
        );
    }

    #[test]
    fn paran_expr() {
        k9::snapshot!(
            parse_expr("1 + 2 ? 1 + 2 * 3 # (5+1) / 2"),
            "
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
    ),
    BinopExpr(
        BinopExpr(
            IntLit(
                5,
            ),
            Add,
            IntLit(
                1,
            ),
        ),
        Div,
        IntLit(
            2,
        ),
    ),
)
"
        );
    }

    #[test]
    fn cond_printform() {
        k9::snapshot!(
            parse_body("PRINTFORML \\@ 1 ? asdf2 # 3fe \\@"),
            r#"
[
    PrintForm(
        NEWLINE,
        "",
        [
            (
                CondExpr(
                    IntLit(
                        1,
                    ),
                    StringLit(
                        "asdf2",
                    ),
                    StringLit(
                        "3fe",
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

    #[test]
    fn cond_expr() {
        k9::snapshot!(
            parse_expr("1 ? 2 # 3"),
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
        k9::snapshot!(
            parse_function("@SYSTEM_TITLE\n#PRI\nPRINTL Hello, world!\n"),
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
        k9::snapshot!(
            parse_body("PRINTL Hello, world!").unwrap(),
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
        k9::snapshot!(
            parse_body("PRINTFORML 1 + 1 = {1 + 1}").unwrap(),
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
