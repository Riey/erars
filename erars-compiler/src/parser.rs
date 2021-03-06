use std::ops::Range;

use crate::{
    ast::{FormText, LocalVariable, SelectCaseCond},
    BeginType, BinaryOperator, BuiltinCommand, EventFlags, Expr, Function, FunctionHeader,
    FunctionInfo, ParserError, ParserResult, Stmt, UnaryOperator, Variable, VariableIndex,
    VariableInfo, VariableInterner,
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

pub fn parse_program(s: &str, var: &mut VariableInterner) -> ParserResult<Vec<Function>> {
    Parser::new(s, var).next_program()
}

pub fn parse_function(s: &str, var: &mut VariableInterner) -> ParserResult<Function> {
    Parser::new(s, var).next_function()
}

pub fn parse_expr(s: &str, var: &mut VariableInterner) -> ParserResult<Expr> {
    Parser::new(s, var).next_expr()
}

pub fn parse_body(s: &str, var: &mut VariableInterner) -> ParserResult<Vec<Stmt>> {
    Parser::new(s, var).next_body()
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
    comma: bool,
}

pub struct Parser<'s, 'v> {
    text: &'s str,
    current_label: &'s str,
    var: &'v mut VariableInterner,
    form_status: Option<FormStatus>,
    cond_status: Option<CondStatus>,
    begin_loc: usize,
    ban_state: BanState,
}

impl<'s, 'v> Parser<'s, 'v> {
    pub fn new(text: &'s str, var: &'v mut VariableInterner) -> Self {
        let begin_loc = text.as_ptr() as usize;

        // BOM
        let text = text.trim_start_matches("\u{feff}");

        Self {
            text,
            current_label: "",
            var,
            form_status: None,
            cond_status: None,
            begin_loc,
            ban_state: BanState::default(),
        }
    }

    fn is_str_var(&self, name: &str) -> bool {
        matches!(
            name,
            "RESULTS"
                | "LOCALS"
                | "ARGS"
                | "STR"
                | "CSTR"
                | "TSTR"
                | "NAME"
                | "CALLNAME"
                | "NICKNAME"
                | "MASTERNAME"
                | "CUSTOMDRAWLINE"
        )
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

    fn get_op(&mut self) -> &'s str {
        let pos = self.text.find(is_not_op_char).unwrap_or(self.text.len());
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

    fn get_non_percent_op(&mut self) -> &'s str {
        let pos = self
            .text
            .find(is_not_non_percent_op_char)
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

    fn try_get_op(&mut self) -> Option<&'s str> {
        let op = self.get_op();

        if op.is_empty() {
            None
        } else {
            Some(op)
        }
    }

    fn try_get_non_percent_op(&mut self) -> Option<&'s str> {
        let op = self.get_non_percent_op();

        if op.is_empty() {
            None
        } else {
            Some(op)
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

    fn check_prefix(&self, prefix: &str) -> bool {
        self.text.starts_with(prefix)
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
                ParserError::InvalidCode(format!("???????????? PRINT ??????????????????")),
                self.from_prev_loc_span(cursor),
            ));
        }

        Ok(ret)
    }

    fn read_normal_text(&mut self) -> ParserResult<&'s str> {
        let mut chars = self.text.chars();
        let mut pad = 1;
        let mut strip_blank = false;

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
                    ',' if self.ban_state.comma => {
                        break;
                    }
                    '#' if self.cond_status == Some(CondStatus::CondFormer) => {
                        strip_blank = true;
                        self.cond_status = Some(CondStatus::CondLater);
                        break;
                    }
                    '\\' => {
                        match chars.next() {
                            Some('@') => {
                                pad = 2;
                                match self.cond_status {
                                    Some(CondStatus::CondLater) => {
                                        self.cond_status = None;
                                    }
                                    Some(_) => {
                                        return Err((ParserError::UnexpectedToken("\\@".into()), self.current_loc_span()));
                                    }
                                    None => {
                                        self.form_status = Some(FormStatus::FormCondExpr);
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

        let (ret, left) = self
            .text
            .split_at(self.text.len() - chars.as_str().len() - pad);
        self.text = left;

        Ok(if strip_blank {
            ret.strip_suffix(' ').unwrap_or(ret)
        } else {
            ret
        })
    }

    fn read_form_and_args(&mut self) -> ParserResult<(Expr, Vec<Expr>)> {
        self.ban_state.comma = true;
        let name = self.read_form_text()?;
        self.ban_state.comma = false;

        let args = self.read_comma_or_paran_args()?;
        Ok((Expr::FormText(name), args))
    }

    fn read_form_text(&mut self) -> ParserResult<FormText> {
        debug_assert!(self.form_status.is_none());

        let mut form_text = FormText::new(self.read_normal_text()?.into());

        loop {
            let mut padding = None;
            let mut align = None;

            let expr = match self.form_status {
                None => break,
                Some(FormStatus::FormIntExpr) => {
                    self.ensure_get_char('{')?;
                    let expr = self.next_expr()?;
                    self.skip_ws();

                    if !self.try_get_char('}') {
                        self.ensure_get_char(',')?;
                        padding = Some(self.next_expr()?);
                        self.skip_ws();
                        if !self.try_get_char('}') {
                            self.ensure_get_char(',')?;
                            self.skip_ws();
                            align = Some(self.read_align()?);
                            self.skip_ws();
                            self.ensure_get_char('}')?;
                        }
                    }

                    expr
                }
                Some(FormStatus::FormStrExpr) => {
                    self.ensure_get_char('%')?;
                    self.ban_state.percent = true;
                    let expr = self.next_expr()?;
                    self.skip_ws();

                    if !self.try_get_char('%') {
                        self.ensure_get_char(',')?;
                        padding = Some(self.next_expr()?);
                        self.skip_ws();
                        if !self.try_get_char('%') {
                            self.skip_ws();
                            self.ensure_get_char(',')?;
                            self.skip_ws();
                            align = Some(self.read_align()?);
                            self.skip_ws();
                            self.ensure_get_char('%')?;
                        }
                    }

                    self.ban_state.percent = false;

                    expr
                }
                Some(FormStatus::FormCondExpr) => {
                    self.ensure_get_prefix("\\@")?;
                    let cond = self.next_expr()?;
                    self.skip_ws();
                    self.ensure_get_char('?')?;
                    self.skip_blank();
                    self.form_status = None;
                    self.cond_status = Some(CondStatus::CondFormer);
                    let if_true = self.read_form_text()?;
                    self.ensure_get_char('#')?;
                    self.skip_blank();
                    let or_false = self.read_form_text()?;
                    self.ensure_get_prefix("\\@")?;
                    Expr::cond(cond, Expr::FormText(if_true), Expr::FormText(or_false))
                }
            };

            self.form_status = None;
            form_text.push(expr, padding, align, self.read_normal_text()?.into());
        }

        Ok(form_text)
    }

    fn read_if_block(
        &mut self,
        else_ifs: &mut Vec<(Expr, Vec<Stmt>)>,
    ) -> ParserResult<Option<Vec<Stmt>>> {
        self.skip_ws();

        let mut body = Vec::new();
        let cond = match self.next_expr() {
            Ok(c) => c,
            Err((ParserError::MissingExpr, _)) => {
                // TODO: print warnings
                Expr::IntLit(0)
            }
            Err(err) => return Err(err),
        };

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
            } else if let Some(postfix) = postfix.strip_prefix("S") {
                let flags = self.read_print_flags(postfix)?;
                self.skip_blank();
                return Ok(Some(Stmt::Print(flags, self.next_expr()?)));
            } else if let Some(postfix) = postfix.strip_prefix("V") {
                let flags = self.read_print_flags(postfix)?;
                self.skip_blank();
                let args = self.read_args('\n')?;
                return Ok(Some(Stmt::PrintList(flags, args)));
            } else {
                let flags = self.read_print_flags(postfix)?;
                self.skip_blank();
                return Ok(Some(Stmt::Print(
                    flags,
                    Expr::StringLit(self.read_until_newline().to_string()),
                )));
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
            "WHILE" => {
                let cond = self.next_expr()?;
                let while_body = self.read_block_until("WEND")?;

                Ok(Some(Stmt::While(cond, while_body)))
            }
            "TIMES" => {
                self.skip_ws();
                let var = self.ensure_get_var()?;
                self.skip_ws();
                self.ensure_get_char(',')?;
                self.skip_ws();

                let (num, left) = self
                    .text
                    .find(|c| !matches!(c, '0'..='9' | '.'))
                    .map(|pos| self.text.split_at(pos))
                    .unwrap_or(("", self.text));

                let num = num.parse::<f32>().map_err(|err| {
                    (
                        ParserError::MissingToken(format!("times factor: {}", err)),
                        self.current_loc_span(),
                    )
                })?;
                self.text = left;

                Ok(Some(Stmt::Times(var, NotNan::new(num).unwrap())))
            }
            "GOTO" | "GOTOFORM" | "TRYGOTO" | "TRYCGOTO" | "TRYGOTOFORM" | "TRYCGOTOFORM"
            | "JUMP" | "JUMPFORM" | "TRYJUMP" | "TRYCJUMP" | "TRYJUMPFORM" | "TRYCJUMPFORM"
            | "CALL" | "CALLFORM" | "TRYCALL" | "TRYCCALL" | "TRYCALLFORM" | "TRYCCALLFORM" => {
                let mut left = command;

                let catch = if let Some(l) = left.strip_prefix("TRY") {
                    if let Some(nl) = l.strip_prefix("C") {
                        // TRYCALL..
                        if nl.starts_with("ALL") {
                            left = l;
                            Some(false)
                        } else {
                            left = nl;
                            Some(true)
                        }
                    } else {
                        left = l;
                        Some(false)
                    }
                } else {
                    None
                };

                let (ty, left) = left.split_at(4);

                let (label, args) = if left == "FORM" {
                    self.skip_blank();
                    if ty == "GOTO" {
                        (Expr::FormText(self.read_form_text()?), Vec::new())
                    } else {
                        self.read_form_and_args()?
                    }
                } else {
                    debug_assert!(left.is_empty());
                    self.skip_ws();
                    if ty == "GOTO" {
                        (Expr::str(self.read_until_newline()), Vec::new())
                    } else {
                        let name = self.ensure_ident(|| "function label")?;
                        self.skip_ws();
                        let args = self.read_comma_or_paran_args()?;
                        (Expr::str(name), args)
                    }
                };

                let catch = match catch {
                    Some(true) => {
                        self.skip_ws_newline();
                        self.ensure_get_prefix("CATCH")?;
                        Some(self.read_block_until("ENDCATCH")?)
                    }
                    Some(false) => Some(Vec::new()),
                    None => None,
                };

                if ty == "GOTO" {
                    Ok(Some(Stmt::Goto { label, catch }))
                } else {
                    Ok(Some(Stmt::Call {
                        name: label,
                        args,
                        catch,
                        jump: ty == "JUMP",
                    }))
                }
            }
            "CALLF" => {
                self.skip_ws();

                let func = self.ensure_ident(|| "Function label")?;

                let args = self.read_comma_or_paran_args()?;

                Ok(Some(Stmt::Call {
                    name: Expr::str(func),
                    args,
                    jump: false,
                    catch: None,
                }))
            }
            "REUSELASTLINE" => {
                self.skip_blank();
                Ok(Some(Stmt::ReuseLastLine(self.read_until_newline().into())))
            }
            "THROW" => {
                self.skip_blank();
                Ok(Some(Stmt::Command(
                    BuiltinCommand::Throw,
                    vec![Expr::str(self.read_until_newline())],
                )))
            }
            "SELECTCASE" => {
                self.skip_ws();
                let cond = self.next_expr()?;

                self.skip_ws_newline();

                let mut cases = Vec::new();
                let mut case_else = None;

                loop {
                    let start = self.current_loc();

                    if self.try_get_prefix("ENDSELECT") {
                        break;
                    } else if self.try_get_prefix("CASEELSE") {
                        case_else = Some(self.read_block_until("ENDSELECT")?);
                        break;
                    } else if self.try_get_prefix("CASE") {
                        let mut current_case = Vec::new();
                        loop {
                            self.skip_ws();

                            if self.try_get_char('\n') {
                                break;
                            } else if self.try_get_prefix("IS") {
                                self.skip_ws();
                                let symbol_start = self.current_loc();
                                let symbol = self.get_symbol();
                                let op = symbol.parse::<BinaryOperator>().map_err(|_| {
                                    (
                                        ParserError::InvalidCode(format!("???????????? ???????????????.")),
                                        self.from_prev_loc_span(symbol_start),
                                    )
                                })?;
                                let expr = self.next_expr()?;
                                current_case.push(SelectCaseCond::Is(op, expr));
                            } else {
                                let first = self.next_expr()?;
                                if self.try_get_prefix("TO") {
                                    let second = self.next_expr()?;
                                    self.skip_ws();
                                    current_case.push(SelectCaseCond::To(first, second));
                                } else {
                                    current_case.push(SelectCaseCond::Single(first));
                                }
                            }

                            self.skip_ws();
                            self.try_get_char(',');
                        }

                        let body = self.read_block_check_many(&["CASE", "ENDSELECT"])?;

                        cases.push((current_case, body))
                    } else {
                        return Err((
                            ParserError::InvalidCode(format!("CASE ?????? CASEELSE??? ???????????????.")),
                            self.from_prev_loc_span(start),
                        ));
                    }
                }

                Ok(Some(Stmt::SelectCase(cond, cases, case_else)))
            }
            "SIF" => {
                let cond = self.next_expr()?;
                let body = self.next_stmt()?;
                Ok(Some(Stmt::Sif(cond, Box::new(body))))
            }
            "RETURN" => {
                self.skip_ws();
                Ok(Some(Stmt::Return(self.read_args('\n')?)))
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
            "CUSTOMDRAWLINE" => {
                self.skip_blank();
                Ok(Some(Stmt::Command(
                    BuiltinCommand::CustomDrawLine,
                    vec![Expr::str(self.read_until_newline())],
                )))
            }
            "ALIGNMENT" => {
                self.skip_ws();
                Ok(Some(Stmt::Alignment(self.read_align()?)))
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
                    self.read_args('\n')?
                } else {
                    Vec::new()
                };

                Ok(Some(Stmt::Varset(var, args)))
            }
            "CONTINUE" => Ok(Some(Stmt::Continue)),
            "BREAK" => Ok(Some(Stmt::Break)),
            other => {
                if let Ok(com) = other.parse::<BuiltinCommand>() {
                    Ok(Some(Stmt::Command(com, self.read_args('\n')?)))
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn read_comma_or_paran_args(&mut self) -> ParserResult<Vec<Expr>> {
        if self.try_get_char('(') {
            self.read_args(')')
        } else if self.try_get_char(',') {
            self.read_args('\n')
        } else {
            Ok(Vec::new())
        }
    }

    fn read_args(&mut self, end: char) -> ParserResult<Vec<Expr>> {
        self.skip_ws();

        let mut ret = Vec::new();

        if self.try_get_char(end) {
            // empty
            return Ok(ret);
        }

        loop {
            ret.push(self.next_expr()?);

            self.skip_ws();

            if !self.try_get_char(',') {
                if !self.text.is_empty() {
                    self.ensure_get_char(end)?;
                }
                break;
            }

            self.skip_ws();

            if self.try_get_char(end) {
                break;
            }
        }

        Ok(ret)
    }

    fn read_align(&mut self) -> ParserResult<Alignment> {
        let start = self.current_loc();
        self.get_ident().parse::<Alignment>().map_err(|_| {
            (
                ParserError::MissingToken(format!("ALIGNMENT")),
                self.from_prev_loc_span(start),
            )
        })
    }

    fn ensure_number(&mut self) -> ParserResult<i64> {
        self.try_read_number().ok_or_else(|| {
            (
                ParserError::MissingToken("????????? ???????????????.".into()),
                self.current_loc_span(),
            )
        })
    }

    fn try_read_number(&mut self) -> Option<i64> {
        let (left, minus) = if let Some(left) = self.text.strip_prefix("-") {
            (left, true)
        } else {
            (self.text.strip_prefix("+").unwrap_or(self.text), false)
        };

        let mut ret = 0;
        let mut consume = 0;

        if let Some(left) = left.strip_prefix("0x").or_else(|| left.strip_prefix("0X")) {
            consume += 2;
            let mut chars = left.chars();

            loop {
                match chars.next() {
                    Some(ch @ '0'..='9') => {
                        ret = ret * 16 + (ch as u32 - '-' as u32) as i64;
                        consume += 1;
                    }
                    Some(ch @ 'a'..='f') => {
                        ret = ret * 16 + (ch as u32 - 'a' as u32 + 10) as i64;
                        consume += 1;
                    }
                    Some(ch @ 'A'..='F') => {
                        ret = ret * 16 + (ch as u32 - 'A' as u32 + 10) as i64;
                        consume += 1;
                    }
                    _ => break,
                }
            }
        } else if let Some(left) = left.strip_prefix("0b") {
            consume += 2;
            let mut chars = left.chars();

            loop {
                match chars.next() {
                    Some('0') => {
                        ret *= 2;
                        consume += 1;
                    }
                    Some('1') => {
                        ret *= 2 + 1;
                        consume += 1;
                    }
                    _ => break,
                }
            }
        } else if matches!(left.as_bytes().get(..2), Some([b'0'..=b'9', b'p'])) {
            ret = (left.as_bytes()[0] as u32 - b'0' as u32) as i64;
            if ret >= 10 {
                return None;
            }

            let (num, c) = read_digit(&left[2..]);

            consume = c + 2;
            ret = ret.wrapping_shl(num.try_into().ok()?);
        } else if matches!(left.as_bytes()[0], b'0'..=b'9') {
            let (num, c) = read_digit(left);

            consume = c;
            ret = num;
        } else {
            return None;
        }

        self.text = &left[consume..];

        Some(if minus { ret.wrapping_neg() } else { ret })
    }

    fn read_term(&mut self) -> ParserResult<Expr> {
        self.skip_ws();

        let start_idx = self.current_loc();

        if self.try_get_char('(') {
            let b = self.ban_state.arg;
            self.ban_state.arg = false;
            let inner = self.next_expr()?;
            self.ban_state.arg = b;
            self.ensure_get_char(')')?;
            Ok(inner)
        } else if self.try_get_char('"') {
            let inner = self.read_until_quote();
            self.ensure_get_char('"')?;
            Ok(Expr::str(inner))
        } else if let Some(ident) = self.try_get_ident() {
            self.skip_ws();
            if self.try_get_char('(') {
                // method
                let b = self.ban_state.arg;
                self.ban_state.arg = false;
                let args = self.read_args(')')?;
                self.ban_state.arg = b;
                Ok(Expr::Method(ident.into(), args))
            } else {
                // variable
                let var_idx = self.get_var_idx(ident, self.from_prev_loc_span(start_idx))?;

                // eliminate nested variable
                let var = if self.ban_state.arg {
                    Variable {
                        var_idx,
                        args: Vec::new(),
                    }
                } else {
                    self.ban_state.arg = true;
                    let var = self.next_var(var_idx)?;
                    self.ban_state.arg = false;
                    var
                };
                Ok(Expr::Var(var))
            }
        } else if let Some(num) = self.try_read_number() {
            Ok(Expr::IntLit(num))
        } else if self.try_get_char('!') {
            Ok(Expr::UnaryopExpr(
                Box::new(self.read_term()?),
                UnaryOperator::Not,
            ))
        } else if self.try_get_char('-') {
            // dec
            if self.try_get_char('-') {
                self.skip_ws();
                let var = self.ensure_get_var()?;
                Ok(Expr::IncOpExpr {
                    var,
                    is_pre: true,
                    is_inc: false,
                })
            } else {
                Ok(Expr::UnaryopExpr(
                    Box::new(self.read_term()?),
                    UnaryOperator::Minus,
                ))
            }
        } else if self.try_get_char('+') {
            // inc
            if self.try_get_char('+') {
                self.skip_ws();
                let var = self.ensure_get_var()?;
                Ok(Expr::IncOpExpr {
                    var,
                    is_pre: true,
                    is_inc: true,
                })
            } else {
                // unary plus is no-op
                Ok(self.read_term()?)
            }
        } else {
            Err((ParserError::MissingExpr, self.from_prev_loc_span(start_idx)))
        }
    }

    fn ensure_get_var(&mut self) -> ParserResult<Variable> {
        let start = self.current_loc();
        let ident = self.ensure_ident(|| format!("Variable"))?;
        let idx = self.get_var_idx(ident, self.from_prev_loc_span(start))?;
        self.next_var(idx)
    }

    fn next_var(&mut self, var_idx: VariableIndex) -> ParserResult<Variable> {
        let mut args = Vec::new();

        while self.try_get_char(':') {
            args.push(self.read_term()?);
        }

        Ok(Variable { var_idx, args })
    }

    fn next_expr(&mut self) -> ParserResult<Expr> {
        self.skip_ws();

        let mut term = self.read_term()?;

        let mut operand_stack = Vec::new();

        self.skip_ws();

        loop {
            let backup = self.text;
            let op = if self.ban_state.percent {
                self.try_get_non_percent_op()
            } else {
                self.try_get_op()
            };

            if let Some(op) = op {
                if let Ok(binop) = op.parse::<BinaryOperator>() {
                    operand_stack.push((binop, self.read_term()?));
                    self.skip_ws();
                } else if op == "++" || op == "--" {
                    let var = match term {
                        Expr::Var(var) => var,
                        _ => {
                            return Err((
                                ParserError::InvalidCode(format!(
                                    "????????? {}????????? ????????? ???????????????.",
                                    op
                                )),
                                self.current_loc_span(),
                            ))
                        }
                    };
                    term = Expr::IncOpExpr {
                        var,
                        is_inc: op == "++",
                        is_pre: false,
                    };
                } else if op == "?" && self.form_status != Some(FormStatus::FormCondExpr) {
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

    fn read_block_check_many(&mut self, ends: &[&str]) -> ParserResult<Vec<Stmt>> {
        let mut body = Vec::new();

        loop {
            self.skip_ws_newline();
            for end in ends {
                if self.check_prefix(end) {
                    return Ok(body);
                }
            }

            body.push(self.next_stmt()?);
        }
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

    fn get_var_idx(&self, ident: &str, span: Range<usize>) -> ParserResult<VariableIndex> {
        self.var
            .get(ident)
            .ok_or_else(|| (ParserError::UnknownVariable(ident.into()), span))
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
            let var =
                self.next_var(self.get_var_idx(ident, self.from_prev_loc_span(ident_start))?)?;

            self.skip_ws();
            let symbol_start = self.current_loc();
            match self.try_get_symbol() {
                Some(symbol) => {
                    if let Some(left) = symbol.strip_suffix("=") {
                        let additional_op = left.parse().ok();

                        if self.is_str_var(ident) {
                            Ok(Stmt::Assign(
                                var,
                                additional_op,
                                Expr::FormText(self.read_form_text()?),
                            ))
                        } else {
                            Ok(Stmt::Assign(var, additional_op, self.next_expr()?))
                        }
                    } else if symbol == "++" || symbol == "--" {
                        // inc / dec expr can be a stmt
                        Ok(Stmt::Assign(
                            var,
                            Some(if symbol == "++" {
                                BinaryOperator::Add
                            } else {
                                BinaryOperator::Sub
                            }),
                            Expr::IntLit(1),
                        ))
                    } else {
                        Err((
                            ParserError::InvalidCode(format!(
                                "??????????????? ???????????? ???????????? ????????????"
                            )),
                            self.from_prev_loc_span(symbol_start),
                        ))
                    }
                }
                None => Err((
                    ParserError::InvalidCode(format!("???????????? ????????? {}", ident)),
                    ident_start..ident_end,
                )),
            }
        } else if self.try_get_prefix("++") {
            self.skip_ws();
            Ok(Stmt::Assign(
                self.ensure_get_var()?,
                Some(BinaryOperator::Add),
                Expr::IntLit(1),
            ))
        } else if self.try_get_prefix("--") {
            self.skip_ws();
            Ok(Stmt::Assign(
                self.ensure_get_var()?,
                Some(BinaryOperator::Sub),
                Expr::IntLit(1),
            ))
        } else if self.try_get_char('$') {
            let label = self.ensure_ident(|| "GOTO label")?;
            Ok(Stmt::Label(label.into()))
        } else if self.text.is_empty() {
            Err((ParserError::Eof, self.from_prev_loc_span(ident_start)))
        } else {
            Err((
                ParserError::InvalidCode(format!("???????????? ??????")),
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
            let ident_start = self.current_loc();
            let ident = self.ensure_ident(|| "Argument ident")?;
            let var =
                self.next_var(self.get_var_idx(ident, self.from_prev_loc_span(ident_start))?)?;
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

        self.current_label = label;

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
                    "DIM" | "DIMS" => {
                        self.skip_ws();
                        let name = self.ensure_ident(|| "DIM ident")?;
                        self.skip_ws();

                        let mut var_info = VariableInfo::default();

                        var_info.is_str = info == "DIMS";

                        if self.try_get_prefix("CHARADATA") {
                            var_info.is_chara = true;
                            self.skip_ws();
                        }

                        if self.try_get_prefix("SAVEDATA") {
                            // TODO
                            self.skip_ws();
                        }

                        while self.try_get_char(',') {
                            self.skip_ws();
                            var_info.size.push(self.ensure_number()? as usize);
                            self.skip_ws();
                        }

                        self.skip_ws();

                        let init = if self.try_get_char('=') {
                            self.read_args('\n')?
                        } else {
                            Vec::new()
                        };

                        infos.push(FunctionInfo::Dim(LocalVariable {
                            idx: self.var.get_or_intern(name),
                            init,
                            info: var_info,
                        }));
                    }
                    "LATER" => {
                        infos.push(FunctionInfo::EventFlag(EventFlags::Later));
                    }
                    "SINGLE" => {
                        infos.push(FunctionInfo::EventFlag(EventFlags::Single));
                    }
                    "FUNCTION" => {
                        infos.push(FunctionInfo::Function);
                    }
                    "FUNCTIONS" => {
                        infos.push(FunctionInfo::FunctionS);
                    }
                    "LOCALSIZE" => {
                        self.skip_ws();
                        let size = self.ensure_number()?;
                        infos.push(FunctionInfo::LocalSize(size as usize));
                    }
                    "LOCALSSIZE" => {
                        self.skip_ws();
                        let size = self.ensure_number()?;
                        infos.push(FunctionInfo::LocalSSize(size as usize));
                    }
                    other => {
                        return Err((
                            ParserError::UnknownFunctionHeader(other.into()),
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
    matches!(c, '_' | '0'..='9' | 'a'..='z' | 'A'..='Z' | '???'..='???' | '???'..='???' | '???'..='???')
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

fn is_not_non_percent_op_char(c: char) -> bool {
    c == '%' || !is_op_char(c)
}

fn is_op_char(c: char) -> bool {
    matches!(
        c,
        '!' | '=' | '%' | '^' | '&' | '*' | '+' | '-' | '/' | '?' | ':' | '<' | '>' | '|'
    )
}

fn is_not_op_char(c: char) -> bool {
    !is_op_char(c)
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

fn read_digit(bytes: &str) -> (i64, usize) {
    let mut ret = 0;
    let mut consume = 0;
    let mut chars = bytes.chars();

    loop {
        match chars.next() {
            Some(ch @ '0'..='9') => {
                consume += 1;
                ret = ret * 10 + (ch as u32 - '0' as u32) as i64;
            }
            _ => break,
        }
    }

    (ret, consume)
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
