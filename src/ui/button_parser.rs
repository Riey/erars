use super::{ConsoleLinePart};
use erars_ast::Value;
use pest::Parser;

#[derive(pest_derive::Parser)]
#[grammar = "erb_button.pest"]
pub struct ErbButtonParser;

fn parse_value(s: &str) -> Value {
    if let Ok(n) = s.parse() {
        Value::Int(n)
    } else {
        Value::String(s.into())
    }
}

pub(super) fn parse_button(s: String, out: &mut Vec<ConsoleLinePart>) {
    match ErbButtonParser::parse(Rule::print_button_text, &s) {
        Ok(mut text) => {
            let mut parts = text.next().unwrap().into_inner();

            let plain = parts.next().unwrap();
            if !plain.as_str().is_empty() {
                out.push(ConsoleLinePart::Normal(plain.as_str().into()));
            }

            for button in parts {
                let mut button_parts = button.into_inner();
                let value_pair = button_parts.next().unwrap();
                let value = parse_value(value_pair.as_str());
                let text_pair = button_parts.next().unwrap();
                let text = format!("[{}]{}", value_pair.as_str(), text_pair.as_str());
                out.push(ConsoleLinePart::Button(value, text));
            }
        }
        Err(err) => {
            log::error!("Can't parse button text {}", err);
            out.push(ConsoleLinePart::Normal(s));
        }
    }
}
