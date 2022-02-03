use source_span::{DefaultMetrics, Position, Span};

use crate::{LexicalError, LexicalResult, PrintFlags, Source, Token};

type Spanned = ((Position, Position), Token, (Position, Position));

const METRICS: DefaultMetrics = DefaultMetrics::with_tab_stop(4);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum LexerStatus {
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

pub struct Lexer<'s> {
    text: &'s str,
    status: Option<LexerStatus>,
    cond_status: Option<CondStatus>,
    span: Span,
}

impl<'s> Lexer<'s> {
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

    fn spanned(&self, token: Token) -> Spanned {
        (
            (self.span.start(), self.span.last()),
            token,
            (self.span.last(), self.span.end()),
        )
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

    fn try_read_symbol(&mut self) -> Option<LexicalResult<Spanned>> {
        let symbol = self.try_get_symbol()?;

        let token = match symbol {
            "+" => Token::Plus,
            "-" => Token::Minus,
            "*" => Token::Star,
            "/" => Token::Slash,
            "(" => Token::OpenParan,
            ")" => Token::CloseParan,
            "{" => Token::OpenBrace,
            "}" => Token::CloseBrace,
            "@" => Token::At,
            "#" => Token::Sharp,
            "~" => Token::Tilde,
            ":" => Token::Colon,
            "." => Token::Period,
            "," => Token::Comma,
            "=" => Token::Assign,
            "%" => Token::Percent,
            ">" => Token::Gt,
            ">=" => Token::Ge,
            "<" => Token::Lt,
            "<=" => Token::Le,
            "==" => Token::Equal,
            "!=" => Token::NotEqual,
            "?" => Token::Question,
            "!" => Token::Exclamation,
            _ => return Some(Err(LexicalError::InvalidSymbol(self.span))),
        };

        Some(Ok(self.spanned(token)))
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

    fn read_print_flags(&mut self) -> PrintFlags {
        let mut ret = PrintFlags::empty();

        if self.try_get_char('L') {
            ret.insert(PrintFlags::NEWLINE);
        } else if self.try_get_char('W') {
            ret.insert(PrintFlags::WAIT | PrintFlags::NEWLINE);
        }

        ret
    }

    fn read_normal_text(&mut self) -> &'s str {
        let mut chars = self.text.chars();
        let mut skip_count = 1;

        loop {
            if let Some(ch) = chars.next() {
                self.shift_position_char(ch);
                match ch {
                    '%' => {
                        self.status = Some(LexerStatus::PrintFormStrExpr);
                        break;
                    }
                    '{' => {
                        self.status = Some(LexerStatus::PrintFormIntExpr);
                        break;
                    }
                    '#' if self.cond_status == Some(CondStatus::PrintFormCondExpr1) => {
                        self.status = Some(LexerStatus::PrintFormSharpExpr);
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
                                    self.status = Some(LexerStatus::PrintFormCondExpr);
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

    fn try_read_keyword(&mut self) -> Option<Spanned> {
        if self.try_read_prefix("PRINTFORM") {
            let flags = self.read_print_flags();
            let ret = self.spanned(Token::PrintForm(flags));
            self.skip_blank();

            self.status = Some(LexerStatus::PrintForm);
            Some(ret)
        } else if self.try_read_prefix("PRINT") {
            let flags = self.read_print_flags();
            let ret = self.spanned(Token::Print(flags));
            self.skip_blank();

            self.status = Some(LexerStatus::Print);
            Some(ret)
        } else {
            None
        }
    }

    fn try_read_number(&mut self) -> Option<LexicalResult<Spanned>> {
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
                        return Some(Err(LexicalError::InvalidNumber(self.span)));
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

        Some(Ok(self.spanned(Token::IntLit(if minus {
            -ret
        } else {
            ret
        }))))
    }

    fn next_token(&mut self) -> Option<LexicalResult<Spanned>> {
        self.skip_ws();

        if let Some(span) = self.try_read_number() {
            Some(span)
        } else if let Some(span) = self.try_read_keyword() {
            Some(Ok(span))
        } else if let Some(symbol) = self.try_read_symbol() {
            Some(symbol)
        } else if let Some(ident) = self.try_get_ident() {
            Some(Ok(self.spanned(Token::Ident(ident.into()))))
        } else {
            None
        }
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = LexicalResult<Spanned>;

    fn next(&mut self) -> Option<Self::Item> {
        self.span.clear();

        eprintln!("{} [{:?} @ {}]", self.text, self.status, self.span);

        if let Some(status) = self.status {
            match status {
                LexerStatus::Print => {
                    let text = self.read_until_newline();
                    self.status = None;
                    Some(Ok(self.spanned(Token::StringLit(text.into()))))
                }
                LexerStatus::PrintFormSharpExpr => {
                    assert!(self.try_get_char('#'));
                    let token = self.spanned(Token::Sharp);
                    self.skip_blank();
                    self.status = Some(LexerStatus::PrintForm);
                    Some(Ok(token))
                }
                LexerStatus::PrintForm => {
                    let text = self.read_normal_text();
                    Some(Ok(self.spanned(Token::StringLit(text.into()))))
                }
                LexerStatus::PrintFormStrExpr => {
                    if self.try_get_char('%') {
                        self.status = Some(LexerStatus::PrintForm);
                        self.next()
                    } else {
                        self.next_token()
                    }
                }
                LexerStatus::PrintFormIntExpr => {
                    if self.try_get_char('}') {
                        self.status = Some(LexerStatus::PrintForm);
                        self.next()
                    } else {
                        self.next_token()
                    }
                }
                LexerStatus::PrintFormCondExpr => {
                    self.skip_ws();
                    if self.try_get_char('?') {
                        self.skip_blank();
                        self.status = Some(LexerStatus::PrintForm);
                        self.cond_status = Some(CondStatus::PrintFormCondExpr1);
                        Some(Ok(self.spanned(Token::Question)))
                    } else {
                        eprintln!("no ?");
                        self.next_token()
                    }
                }
            }
        } else {
            self.next_token()
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    fn tokens(source: &str) -> Vec<Token> {
        Lexer::new(source)
            .map(|r| r.map(|s| s.1))
            .collect::<LexicalResult<Vec<Token>>>()
            .unwrap()
    }

    #[test]
    fn hello_world() {
        k9::snapshot!(
            tokens("PRINTL Hello, world!\n"),
            r#"
[
    Print(
        NEWLINE,
    ),
    StringLit(
        "Hello, world!",
    ),
]
"#
        );
    }

    #[test]
    fn hello_world_form() {
        k9::snapshot!(
            tokens("PRINTFORML Hello, world!\n"),
            r#"
[
    PrintForm(
        NEWLINE,
    ),
    StringLit(
        "Hello, world!",
    ),
]
"#
        );
    }

    #[test]
    fn int_lit() {
        k9::snapshot!(
            tokens("123 -123"),
            "
[
    IntLit(
        123,
    ),
    IntLit(
        -123,
    ),
]
"
        );
    }

    #[test]
    #[should_panic]
    fn invalid_symbol() {
        k9::snapshot!(tokens("123 +-+ 123"));
    }

    #[test]
    fn cond_expr() {
        k9::snapshot!(
            tokens("1 ? 2 # 3"),
            "
[
    IntLit(
        1,
    ),
    Question,
    IntLit(
        2,
    ),
    Sharp,
    IntLit(
        3,
    ),
]
"
        );
    }

    #[test]
    fn form_cond2() {
        k9::snapshot!(
            tokens("PRINTFORML \\@ 1 ? asdf2 # 3fe \\@"),
            r#"
[
    PrintForm(
        NEWLINE,
    ),
    StringLit(
        "",
    ),
    IntLit(
        1,
    ),
    Question,
    StringLit(
        "asdf2",
    ),
    Sharp,
    StringLit(
        "3fe",
    ),
    StringLit(
        "",
    ),
]
"#
        );
    }

    #[test]
    fn form_cond() {
        k9::snapshot!(
            tokens("PRINTFORML \\@ 1 ? ?? # !! \\@"),
            r#"
[
    PrintForm(
        NEWLINE,
    ),
    StringLit(
        "",
    ),
    IntLit(
        1,
    ),
    Question,
    StringLit(
        "??",
    ),
    Sharp,
    StringLit(
        "!!",
    ),
    StringLit(
        "",
    ),
]
"#
        );
    }

    #[test]
    fn form_simple() {
        k9::snapshot!(
            tokens("PRINTFORML 1 + 1 = {1 + 1}"),
            r#"
[
    PrintForm(
        NEWLINE,
    ),
    StringLit(
        "1 + 1 = ",
    ),
    IntLit(
        1,
    ),
    Plus,
    IntLit(
        1,
    ),
    StringLit(
        "",
    ),
]
"#
        );
    }
}
