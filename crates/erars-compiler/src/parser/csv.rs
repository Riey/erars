use erars_ast::{Interner, StrKey};
use erars_lexer::CsvToken;
use logos::Lexer;

use crate::ParserResult;

pub fn csv2_line_allow_other<'s>(
    lex: &mut Lexer<'s, CsvToken<'s>>,
) -> ParserResult<Option<(&'s str, &'s str)>> {
    match lex.next() {
        Some(CsvToken::Csv2((key, value))) => Ok(Some((key, value))),
        _ => Ok(None),
    }
}

pub fn csv2_line<'s>(
    lex: &mut Lexer<'s, CsvToken<'s>>,
) -> ParserResult<Option<(&'s str, &'s str)>> {
    match lex.next() {
        Some(CsvToken::Csv2((key, value))) => Ok(Some((key, value))),
        Some(_) => Err((format!("CSV 형식이 잘못됐습니다."), lex.span())),
        None => Ok(None),
    }
}

pub fn name_csv_line<'s>(
    interner: &Interner,
    lex: &mut Lexer<'s, CsvToken<'s>>,
) -> ParserResult<Option<(u32, StrKey)>> {
    match lex.next() {
        Some(CsvToken::Csv2((idx, name))) => {
            let idx = match idx.parse::<u32>() {
                Ok(idx) => idx,
                Err(err) => return Err((format!("숫자가 와야합니다. :{err}"), lex.span())),
            };

            Ok(Some((idx, interner.get_or_intern(name))))
        }
        Some(_) => Err((format!("CSV 형식이 잘못됐습니다."), lex.span())),
        None => Ok(None),
    }
}

pub fn chara_line<'s>(
    lex: &mut Lexer<'s, CsvToken<'s>>,
) -> ParserResult<Option<(&'s str, &'s str, &'s str)>> {
    match lex.next() {
        Some(CsvToken::Csv3(line)) => Ok(Some(line)),
        Some(CsvToken::Csv2((name, value))) => Ok(Some((name, value, ""))),
        Some(_) => Err((format!("CSV 형식이 잘못됐습니다."), lex.span())),
        None => Ok(None),
    }
}

pub fn name_item_line<'s>(
    interner: &Interner,
    lex: &mut Lexer<'s, CsvToken<'s>>,
) -> ParserResult<Option<(u32, StrKey, u32)>> {
    match lex.next() {
        Some(CsvToken::Csv3((idx, name, price))) => {
            let idx = match idx.parse::<u32>() {
                Ok(idx) => idx,
                Err(err) => return Err((format!("숫자가 와야합니다. :{err}"), lex.span())),
            };
            let price = match price.parse::<u32>() {
                Ok(idx) => idx,
                Err(err) => return Err((format!("숫자가 와야합니다. :{err}"), lex.span())),
            };

            Ok(Some((idx, interner.get_or_intern(name), price)))
        }
        Some(_) => Err((format!("CSV 형식이 잘못됐습니다."), lex.span())),
        None => Ok(None),
    }
}

pub fn variable_size_line<'s>(
    lex: &mut Lexer<'s, CsvToken<'s>>,
) -> ParserResult<Option<(String, Option<Vec<usize>>)>> {
    macro_rules! parse_size {
        ($size:expr) => {
            match $size.parse::<i64>().map(usize::try_from) {
                Ok(Err(_)) => None,
                Ok(Ok(size)) => Some(size),
                Err(err) => return Err((format!("숫자가 와야합니다. :{err}"), lex.span())),
            }
        };
    }

    match lex.next() {
        Some(CsvToken::Csv2((var, size))) => {
            let size = parse_size!(size);

            match size {
                Some(size) => Ok(Some((var.into(), Some(vec![size])))),
                None => Ok(Some((var.into(), None))),
            }
        }
        Some(CsvToken::Csv3((var, size1, size2))) => {
            let size1 = parse_size!(size1);
            let size2 = parse_size!(size2);

            match (size1, size2) {
                (Some(size1), Some(size2)) => Ok(Some((var.into(), Some(vec![size1, size2])))),
                _ => Ok(Some((var.into(), None))),
            }
        }
        Some(CsvToken::Csv4((var, size1, size2, size3))) => {
            let size1 = parse_size!(size1);
            let size2 = parse_size!(size2);
            let size3 = parse_size!(size3);

            match (size1, size2, size3) {
                (Some(size1), Some(size2), Some(size3)) => {
                    Ok(Some((var.into(), Some(vec![size1, size2, size3]))))
                }
                _ => Ok(Some((var.into(), None))),
            }
        }
        Some(_) => Err((format!("CSV 형식이 잘못됐습니다."), lex.span())),
        None => Ok(None),
    }
}
