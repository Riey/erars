use std::ops::Range;

use crate::{
    ast::FormText, BeginType, BinaryOperator, EventFlags, Expr, Function, FunctionHeader,
    FunctionInfo, ParserError, ParserResult, Stmt, Variable,
};
use bitflags::bitflags;
use ordered_float::NotNan;
use serde::{Deserialize, Serialize};
use strum::EnumString;

option_set::option_set! {
    pub struct PrintFlags: UpperSnake + u32 {
        const NEWLINE = 0x1;
        const WAIT = 0x2;
        const LEFT_ALIGN = 0x4;
        const RIGHT_ALIGN = 0x8;
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize, EnumString)]
pub enum Alignment {
    #[strum(to_string = "LEFT")]
    Left,
    #[strum(to_string = "CENTER")]
    Center,
    #[strum(to_string = "RIGHT")]
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

#[derive(Clone, Copy, Debug, Default)]
struct BanState {
    percent: bool,
    arg: bool,
}

#[derive(Clone)]
pub struct Parser<'s> {
    text: &'s str,
    form_status: Option<FormStatus>,
    cond_status: Option<CondStatus>,
    begin_loc: usize,
    ban_state: BanState,
}

impl<'s> Parser<'s> {
    pub fn new(text: &'s str) -> Self {
        let begin_loc = text.as_ptr() as usize;

        // BOM
        let text = text.trim_start_matches("\u{feff}");

        Self {
            text,
            form_status: None,
            cond_status: None,
            begin_loc,
            ban_state: BanState::default(),
        }
    }

    fn is_str_var(&self, name: &str) -> bool {
        match name {
            "LOCALS" | "STR" | "NAME" | "CALLNAME" | "NICKNAME" | "MASTERNAME" => true,
            _ => false,
        }
    }

    fn current_loc(&self) -> usize {
        match (self.text.as_ptr() as usize).checked_sub(self.begin_loc) {
            Some(n) => n,
            None => {
                unreachable!("Text pointer exceeded {}", self.text,)
            }
        }
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

    fn skip_ws_newline(&mut self) {
        loop {
            self.skip_ws();
            if !self.try_get_char('\n') {
                break;
            }
        }
    }

    fn skip_ws(&mut self) {
        let mut bytes = self.text.as_bytes().iter();

        while let Some(b) = bytes.next() {
            match b {
                b' ' | b'\t' | b'\r' => {}
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

    fn get_non_percent_symbol(&mut self) -> &'s str {
        let pos = self
            .text
            .find(is_not_non_percent_symbol_char)
            .unwrap_or(self.text.len());
        let (ret, text) = self.text.split_at(pos);
        self.text = text;
        ret
    }

    fn ensure_ident<S: Into<String>>(&mut self, name: impl FnOnce() -> S) -> ParserResult<&'s str> {
        self.try_get_ident().ok_or_else(|| {
            (
                ParserError::MissingToken(name().into()),
                self.current_loc_span(),
            )
        })
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

    fn try_get_non_percent_symbol(&mut self) -> Option<&'s str> {
        let symbol = self.get_non_percent_symbol();

        if symbol.is_empty() {
            None
        } else {
            Some(symbol)
        }
    }

    // /// return false if current char is whitespace or EOS
    // fn ensure_whitespace_or_empty(&self) -> bool {
    //     self.text
    //         .chars()
    //         .next()
    //         .map(|c| c.is_ascii_whitespace())
    //         .unwrap_or(true)
    // }

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

    /// Never `self.text = ""` it'll invalidate location infomations
    fn clear_text(&mut self) {
        self.text = &self.text[self.text.len()..];
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

    fn read_print_flags(&mut self, mut postfix: &str) -> ParserResult<PrintFlags> {
        let mut cursor = self.current_loc() - postfix.len();

        let mut ret = PrintFlags::empty();

        macro_rules! parse_flag {
            (
                $($ch:expr => $flag:expr,)+
            ) => {
                $(
                    if let Some(left) = postfix.strip_prefix($ch) {
                        ret |= $flag;
                        postfix = left;
                        cursor += $ch.len();
                    }
                )+
            };
        }

        parse_flag!(
            "L" => PrintFlags::NEWLINE,
            "W" => PrintFlags::NEWLINE | PrintFlags::WAIT,
            "LC" => PrintFlags::LEFT_ALIGN,
            "C" => PrintFlags::RIGHT_ALIGN,
        );

        if !postfix.is_empty() {
            return Err((
                ParserError::InvalidCode(format!("알수없는 PRINT 플래그입니다")),
                self.from_prev_loc_span(cursor),
            ));
        }

        Ok(ret)
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
                                        skip_count = 2;
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
                self.clear_text();
                self.form_status = None;
                return Ok(ret);
            }
        }

        let ret = if self.cond_status == Some(CondStatus::CondLater) {
            let (ret, left) = self.text.split_at(self.text.len() - chars.as_str().len());
            let ret = &ret[..ret.len() - skip_count];
            let ret = ret.strip_suffix(' ').unwrap_or(ret);
            self.text = left;
            self.skip_blank();
            ret
        } else {
            let (ret, left) = self.text.split_at(self.text.len() - chars.as_str().len());
            self.text = left;
            &ret[..ret.len() - skip_count]
        };

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
                    self.ban_state.percent = true;
                    let expr = self.next_expr()?;
                    self.ban_state.percent = false;
                    self.skip_ws();
                    self.ensure_get_char('%')?;
                    expr
                }
                Some(FormStatus::FormCondExpr) => {
                    let cond = self.read_term()?;
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
            self.skip_ws_newline();

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
                    let else_body = self.read_block_until("ENDIF")?;
                    break Some(else_body);
                }
            }

            body.push(self.next_stmt()?);
        };

        Ok(else_body)
    }

    fn try_process_command(&mut self, command: &str) -> ParserResult<Option<Stmt>> {
        if let Some(postfix) = command.strip_prefix("PRINT") {
            if let Some(postfix) = postfix.strip_prefix("FORM") {
                let flags = self.read_print_flags(postfix)?;
                self.skip_blank();
                return Ok(Some(Stmt::PrintForm(flags, self.read_form_text()?)));
            } else {
                let flags = self.read_print_flags(postfix)?;
                self.skip_blank();
                return Ok(Some(Stmt::Print(flags, self.read_until_newline().into())));
            }
        }

        match command {
            "IF" => {
                let mut else_ifs = Vec::new();
                let else_body = self.read_if_block(&mut else_ifs)?;

                Ok(Some(Stmt::If(else_ifs, else_body)))
            }
            "DO" => {
                let do_body = self.read_block_until("LOOP")?;
                let cond = self.next_expr()?;

                Ok(Some(Stmt::Do(cond, do_body)))
            }
            "TIMES" => {
                self.skip_ws();
                let var = self.ensure_get_var()?;
                self.skip_ws();
                self.ensure_get_char(',')?;

                let start = self.current_loc();
                let num = self
                    .read_until_newline()
                    .trim()
                    .parse::<f32>()
                    .map_err(|err| {
                        (
                            ParserError::InvalidCode(format!("Float parsing error: {}", err)),
                            self.from_prev_loc_span(start),
                        )
                    })?;

                Ok(Some(Stmt::Times(var, NotNan::new(num).unwrap())))
            }
            "GOTO" => {
                self.skip_ws();

                Ok(Some(Stmt::Goto(self.ensure_ident(|| "label name")?.into())))
            }
            "REUSELASTLINE" => {
                self.skip_blank();
                Ok(Some(Stmt::ReuseLastLine(self.read_until_newline().into())))
            }
            "SIF" => {
                let cond = self.next_expr()?;
                let body = self.next_stmt()?;
                Ok(Some(Stmt::Sif(cond, Box::new(body))))
            }
            "RETURN" => {
                self.skip_ws();
                Ok(Some(Stmt::Return(self.read_args()?)))
            }
            "RETURNF" => {
                self.skip_ws();
                Ok(Some(Stmt::ReturnF(self.next_expr()?)))
            }
            "BEGIN" => {
                self.skip_ws();
                let start = self.current_loc();
                let ty = self.get_ident().parse::<BeginType>().map_err(|_| {
                    (
                        ParserError::MissingToken(format!("Begin type")),
                        self.from_prev_loc_span(start),
                    )
                })?;
                Ok(Some(Stmt::Begin(ty)))
            }
            "CALL" | "CALLF" => {
                self.skip_ws();

                let func = self.ensure_ident(|| "Function label")?;

                let args = if self.try_get_char(',') {
                    self.read_args()?
                } else {
                    Vec::new()
                };

                Ok(Some(Stmt::Call(func.into(), args)))
            }
            "ALIGNMENT" => {
                self.skip_ws();
                let start = self.current_loc();
                let align = self.get_ident().parse::<Alignment>().map_err(|_| {
                    (
                        ParserError::MissingToken(format!("ALIGNMENT")),
                        self.from_prev_loc_span(start),
                    )
                })?;

                Ok(Some(Stmt::Alignment(align)))
            }
            "REPEAT" => {
                self.skip_ws();
                let end = self.next_expr()?;
                let body = self.read_block_until("REND")?;

                Ok(Some(Stmt::Repeat(end, body)))
            }
            "FOR" => {
                self.skip_ws();
                let var = self.ensure_get_var()?;
                self.skip_ws();
                self.ensure_get_char(',')?;
                let init = self.next_expr()?;
                self.ensure_get_char(',')?;
                let end = self.next_expr()?;
                let step = if self.try_get_char(',') {
                    self.next_expr()?
                } else {
                    Expr::IntLit(1)
                };

                let body = self.read_block_until("NEXT")?;

                Ok(Some(Stmt::For(var, init, end, step, body)))
            }
            "VARSET" => {
                self.skip_ws();
                let var = self.ensure_get_var()?;
                self.skip_ws();

                let args = if self.try_get_char(',') {
                    self.read_args()?
                } else {
                    Vec::new()
                };

                Ok(Some(Stmt::Varset(var, args)))
            }
            "CONTINUE" => Ok(Some(Stmt::Continue)),
            "BREAK" => Ok(Some(Stmt::Break)),
            "DRAWLINE" | "INPUT" | "INPUTS" | "RESETDATA" | "ADDDEFCHARA" | "WAIT"
            | "WAITANYKEY" | "RESTART" | "FONTITALIC" | "FONTBOLD" | "FONTREGULAR" => {
                Ok(Some(Stmt::Command(command.into(), Vec::new())))
            }
            "STRLENS" | "ADDCHARA" | "DELCHARA" | "CLEARLINE" => {
                Ok(Some(Stmt::Command(command.into(), self.read_args()?)))
            }
            _ => Ok(None),
        }
    }

    fn read_args(&mut self) -> ParserResult<Vec<Expr>> {
        self.skip_ws();

        let mut ret = Vec::new();

        if self.try_get_char('\n') {
            // empty
            return Ok(ret);
        }

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
                    self.clear_text();
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
            if self.try_get_char('(') {
                // method
                let b = self.ban_state;
                let args = self.read_args()?;
                self.ban_state = b;
                self.ensure_get_char(')')?;
                Ok(Expr::Method(ident.into(), args))
            } else {
                // variable

                // eliminate nested variable
                let var = if self.ban_state.arg {
                    Variable {
                        name: ident.into(),
                        args: Vec::new(),
                    }
                } else {
                    self.ban_state.arg = true;
                    let var = self.next_var(ident)?;
                    self.ban_state.arg = false;
                    var
                };
                Ok(Expr::Var(var))
            }
        } else if let Some(num) = self.try_read_number() {
            num.map(Expr::IntLit)
        } else {
            Err((
                ParserError::InvalidCode(format!("표현식이 와야합니다.")),
                self.current_loc_span(),
            ))
        }
    }

    fn ensure_get_var(&mut self) -> ParserResult<Variable> {
        self.ensure_ident(|| format!("Variable"))
            .and_then(|i| self.next_var(i))
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

        let mut term = self.read_term()?;

        let mut operand_stack = Vec::new();

        self.skip_ws();

        loop {
            let backup = self.text;
            let symbol = if self.ban_state.percent {
                self.try_get_non_percent_symbol()
            } else {
                self.try_get_symbol()
            };

            if let Some(symbol) = symbol {
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

    fn read_block_until(&mut self, end: &str) -> ParserResult<Vec<Stmt>> {
        let mut body = Vec::new();

        loop {
            self.skip_ws_newline();
            if self.try_get_prefix(end) {
                break;
            }

            body.push(self.next_stmt()?);
        }

        Ok(body)
    }

    fn next_stmt(&mut self) -> ParserResult<Stmt> {
        self.skip_ws_newline();

        let ident_start = self.current_loc();

        if let Some(ident) = self.try_get_ident() {
            let ident_end = self.current_loc();
            if let Some(com) = self.try_process_command(ident)? {
                return Ok(com);
            }

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
                        ParserError::InvalidCode(format!("식별자뒤에 알수없는 연산자가 왔습니다")),
                        self.from_prev_loc_span(symbol_start),
                    ))
                }
                None => Err((
                    ParserError::InvalidCode(format!("알수없는 식별자 {}", ident)),
                    ident_start..ident_end,
                )),
            }
        } else if self.try_get_char('$') {
            let label = self.ensure_ident(|| "GOTO label")?;
            Ok(Stmt::Label(label.into()))
        } else if self.text.is_empty() {
            Err((ParserError::Eof, self.from_prev_loc_span(ident_start)))
        } else {
            Err((
                ParserError::InvalidCode(format!("알수없는 코드")),
                self.from_prev_loc_span(ident_start),
            ))
        }
    }

    fn next_body(&mut self) -> ParserResult<Vec<Stmt>> {
        let mut ret = Vec::with_capacity(100);

        loop {
            match self.next_stmt() {
                Ok(stmt) => ret.push(stmt),
                Err((ParserError::Eof, _)) => break,
                Err(_) if self.text.starts_with('@') => break,
                Err(err) => return Err(err),
            }
        }

        Ok(ret)
    }

    fn read_function_args(&mut self) -> ParserResult<Vec<(Variable, Option<Expr>)>> {
        let mut args = Vec::new();

        loop {
            self.skip_ws();
            let ident = self.ensure_ident(|| "Argument ident")?;
            let var = self.next_var(ident)?;
            self.skip_ws();
            let default_arg = if self.try_get_char('=') {
                Some(self.next_expr()?)
            } else {
                None
            };
            args.push((var, default_arg));

            self.skip_ws();
            if !self.try_get_char(',') {
                break;
            }
        }

        Ok(args)
    }

    fn next_function(&mut self) -> ParserResult<Function> {
        self.skip_ws_newline();
        self.ensure_get_char('@')?;
        let label = self.ensure_ident(|| "Function label")?;

        self.skip_ws();

        let paran = self.try_get_char('(');
        let comma = self.try_get_char(',');
        let args = if paran || comma {
            self.read_function_args()?
        } else {
            Vec::new()
        };

        if paran {
            self.ensure_get_char(')')?;
        }

        let mut infos = Vec::new();

        loop {
            self.skip_ws_newline();
            if self.try_get_char('#') {
                let start = self.current_loc();
                let info = self.ensure_ident(|| "Function info")?;
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
                args,
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
                    self.skip_ws_newline();

                    if self.text.is_empty() || self.text.starts_with('@') {
                        break;
                    } else {
                        return Err(err);
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
            | '|'
    )
}

fn is_not_symbol_char(c: char) -> bool {
    !is_symbol_char(c)
}

fn is_not_non_percent_symbol_char(c: char) -> bool {
    c == '%' || !is_symbol_char(c)
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
