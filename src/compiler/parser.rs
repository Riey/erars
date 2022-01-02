use once_cell::sync::Lazy;
use pest::prec_climber::{Assoc, Operator, PrecClimber};

#[derive(pest_derive::Parser)]
#[grammar = "erb.pest"]
pub struct ErbParser;

pub static PREC_CLIMBER: Lazy<PrecClimber<Rule>> = Lazy::new(|| {
    use Assoc::*;
    use Rule::*;
    PrecClimber::new(vec![
        Operator::new(add, Left) | Operator::new(sub, Left),
        Operator::new(mul, Left) | Operator::new(div, Left) | Operator::new(rem, Right),
    ])
});

#[cfg(test)]
mod tests {
    use super::{ErbParser, Rule};
    use pest::Parser;

    #[test]
    fn call() {
        ErbParser::parse(Rule::call_com, "CALL ident").unwrap();
    }

    macro_rules! make_ident_same_case {
        ($t:expr) => {
            let pair = ErbParser::parse(Rule::ident, $t).unwrap().next().unwrap();
            k9::assert_equal!(pair.as_str(), $t);
        };
    }

    #[test]
    fn ident() {
        make_ident_same_case!("adf1rvdsad_312");
    }

    #[test]
    fn underscore() {
        make_ident_same_case!("_adf1rvdsad_312");
        make_ident_same_case!("___");
    }

    #[test]
    fn hangul_ident() {
        make_ident_same_case!("함수이름");
        make_ident_same_case!("가나다");
    }
}
