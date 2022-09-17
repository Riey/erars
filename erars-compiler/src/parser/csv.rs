use erars_lexer::CsvToken;
use logos::Lexer;
use smol_str::SmolStr;

use crate::ParserResult;

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
    lex: &mut Lexer<'s, CsvToken<'s>>,
) -> ParserResult<Option<(u32, SmolStr)>> {
    match lex.next() {
        Some(CsvToken::Csv2((idx, name))) => {
            let idx = match idx.parse::<u32>() {
                Ok(idx) => idx,
                Err(err) => return Err((format!("숫자가 와야합니다. :{err}"), lex.span())),
            };

            Ok(Some((idx, name.into())))
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
    lex: &mut Lexer<'s, CsvToken<'s>>,
) -> ParserResult<Option<(u32, SmolStr, u32)>> {
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

            Ok(Some((idx, name.into(), price)))
        }
        Some(_) => Err((format!("CSV 형식이 잘못됐습니다."), lex.span())),
        None => Ok(None),
    }
}
