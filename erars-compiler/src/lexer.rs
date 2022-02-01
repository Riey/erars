use source_span::{DefaultMetrics, Position};

use crate::{LexicalError, LexicalResult, PrintFlags, Source, Token};

type Spanned = (Position, Token, Position);

const METRICS: DefaultMetrics = DefaultMetrics::with_tab_stop(4);

pub struct Lexer<'s> {
    text: &'s str,
    position: Position,
    next_buffer: Option<Spanned>,
}

impl<'s> Lexer<'s> {
    pub fn new(text: &'s str) -> Self {
        Self {
            text,
            position: Position::default(),
            next_buffer: None,
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
                b'#' => {
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

            self.position.shift(*b as char, &METRICS);
        }

        self.text = self
            .text
            .split_at(self.text.len() - bytes.as_slice().len())
            .0;
    }

    fn shift_position_char(&mut self, ch: char) {
        self.position.shift(ch, &METRICS);
    }

    fn shift_position(&mut self, s: &str) {
        for c in s.chars() {
            self.position.shift(c, &METRICS);
        }
    }

    fn consume(&mut self, s: &str) {
        self.shift_position(s);
        self.text = &self.text[s.len()..];
    }

    fn read_ident(&mut self) -> &'s str {
        let pos = self.text.find(is_not_ident_char).unwrap_or(self.text.len());
        let (ret, text) = self.text.split_at(pos);
        self.shift_position(ret);
        self.text = text;
        ret
    }

    fn try_read_ident(&mut self) -> Option<&'s str> {
        let ident = self.read_ident();

        if ident.is_empty() {
            None
        } else {
            Some(ident)
        }
    }

    fn try_read_char(&mut self, prefix: char) -> bool {
        if let Some(text) = self.text.strip_prefix(prefix) {
            self.shift_position_char(prefix);
            self.text = text;
            true
        } else {
            false
        }
    }

    fn skip_blank(&mut self) {
        self.try_read_char(' ');
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

        if self.try_read_char('L') {
            ret.insert(PrintFlags::NEWLINE);
        } else if self.try_read_char('W') {
            ret.insert(PrintFlags::WAIT | PrintFlags::NEWLINE);
        }

        ret
    }

    fn try_read_keyword(&mut self) -> Option<Token> {
        // if self.try_read_prefix("PRINTFORM") {
        //     let flags
        // }

        if self.try_read_prefix("PRINT") {
            let flags = self.read_print_flags();
            let print_end = self.position;
            self.skip_blank();
            let text_start = self.position;
            let text = self.read_until_newline();
            self.next_buffer = Some((text_start, Token::StringLit(text.into()), self.position));
            self.position = print_end;
            Some(Token::Print(flags))
        } else {
            None
        }
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = LexicalResult<Spanned>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_ws();

        let start = self.position;

        if let Some(next) = self.next_buffer.take() {
            self.position = next.2;
            return Some(Ok(next));
        }

        if let Some(keyword) = self.try_read_keyword() {
            return Some(Ok((start, keyword, self.position)));
        }

        None
    }
}

fn is_ident_char(c: char) -> bool {
    match c {
        '_' | '0'..='9' | 'a'..='z' | 'A'..='Z' | 'ㄱ'..='ㅎ' | 'ㅏ'..='ㅣ' | '가'..='힣' => {
            true
        }
        _ => false,
    }
}

fn is_not_ident_char(c: char) -> bool {
    !is_ident_char(c)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::PrintFlags;

    #[test]
    fn hello_world() {
        let source = "PRINTL Hello, world!";
        let tokens = Lexer::new(source)
            .map(|r| r.map(|s| s.1))
            .collect::<LexicalResult<Vec<Token>>>()
            .unwrap();

        k9::assert_equal!(
            tokens,
            [
                Token::Print(PrintFlags::NEWLINE),
                Token::StringLit("Hello, world!".into())
            ]
        );
    }
}
