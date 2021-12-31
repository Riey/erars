// const FONT: &[u8] = include_bytes!("../res/D2Coding-Ver1.3.2-20180524.ttc");

use erars::compiler::ProgramLine;
use hashbrown::HashMap;
use rayon::prelude::*;

fn main() -> anyhow::Result<()> {
    let infos = serde_yaml::from_str(include_str!("./variable.yaml"))?;

    let erbs = glob::glob_with(
        "ERB/**/*.ERB",
        glob::MatchOptions {
            case_sensitive: false,
            require_literal_leading_dot: true,
            require_literal_separator: true,
        },
    )
    .unwrap();

    let functions: HashMap<String, Vec<ProgramLine>> = erbs
        .par_bridge()
        .flat_map(|erb| {
            erb.map_err(anyhow::Error::from)
                .and_then(|erb| erars::compiler::compile(&std::fs::read_to_string(erb)?, &infos))
                .unwrap_or_default()
        })
        .collect::<HashMap<_, _>>();

    dbg!(functions);

    Ok(())
}
