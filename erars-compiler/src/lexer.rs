use source_span::{DefaultMetrics, Position, Span};

use crate::{LexicalError, LexicalResult, PrintFlags, Source, Token};

type Spanned = ((Position, Position), Token, (Position, Position));

const METRICS: DefaultMetrics = DefaultMetrics::with_tab_stop(4);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum LexerStatus {
    Print,
    PrintForm,
    PrintFormIntExpr,
    PrintFormStrExpr,
}

pub struct Lexer<'s> {
    text: &'s str,
    status: Option<LexerStatus>,
    span: Span,
}

impl<'s> Lexer<'s> {
    pub fn new(text: &'s str) -> Self {
        Self {
            text,
            span: Span::default(),
            status: None,
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
                    // TODO \@
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

        let (mut ret, left) = self.text.split_at(self.text.len() - chars.as_str().len());
        ret = &ret[..ret.len() - 1];
        self.text = left;
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

        // eprintln!("{} [{:?} @ {}]", self.text, self.status, self.span);
        if let Some(status) = self.status {
            match status {
                LexerStatus::Print => {
                    let text = self.read_until_newline();
                    self.status = None;
                    Some(Ok(self.spanned(Token::StringLit(text.into()))))
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
    use crate::PrintFlags;

    fn do_test(source: &str, expected: &[Token]) {
        let tokens = Lexer::new(source)
            .map(|r| r.map(|s| s.1))
            .collect::<LexicalResult<Vec<Token>>>()
            .unwrap();
        k9::assert_equal!(tokens, expected);
    }

    #[test]
    fn hello_world() {
        do_test(
            "PRINTL Hello, world!",
            &[
                Token::Print(PrintFlags::NEWLINE),
                Token::StringLit("Hello, world!".into()),
            ],
        );
        do_test(
            "PRINTL Hello, world!\n",
            &[
                Token::Print(PrintFlags::NEWLINE),
                Token::StringLit("Hello, world!".into()),
            ],
        );
    }

    #[test]
    fn hello_world_form() {
        do_test(
            "PRINTFORML Hello, world!",
            &[
                Token::PrintForm(PrintFlags::NEWLINE),
                Token::StringLit("Hello, world!".into()),
            ],
        );
        do_test(
            "PRINTFORML Hello, world!\n",
            &[
                Token::PrintForm(PrintFlags::NEWLINE),
                Token::StringLit("Hello, world!".into()),
            ],
        );
    }

    #[test]
    fn int_lit() {
        do_test("123 -123", &[Token::IntLit(123), Token::IntLit(-123)]);
    }

    #[test]
    #[should_panic]
    fn invalid_symbol() {
        do_test("123 +-+ 123", &[]);
    }

    #[test]
    fn cond_expr() {
        do_test(
            "1 ? 2 # 3",
            &[
                Token::IntLit(1),
                Token::Question,
                Token::IntLit(2),
                Token::Sharp,
                Token::IntLit(3),
            ],
        );
    }

    #[test]
    fn form_simple() {
        do_test(
            "PRINTFORML 1 + 1 = {1 + 1}",
            &[
                Token::PrintForm(PrintFlags::NEWLINE),
                Token::StringLit("1 + 1 = ".into()),
                Token::IntLit(1),
                Token::Plus,
                Token::IntLit(1),
                Token::StringLit("".into()),
            ],
        );
    }
}
