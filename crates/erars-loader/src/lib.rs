use anyhow::Context;
use erars_reader::read_file;
use parking_lot::Mutex;
#[cfg(feature = "multithread")]
use rayon::prelude::*;
use std::{fs::File, io::BufWriter, path::Path, sync::Arc, time::Instant};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::{
        termcolor::{ColorChoice, StandardStream, WriteColor},
        Config,
    },
};
use erars_ast::{StrKey, VariableInfo};
use erars_compiler::{
    CompiledFunction, EraConfig, HeaderInfo, Lexer, ParserContext, Preprocessor, PP_REGEX,
};
use erars_lint::{check_function, ErarsFiles};
use erars_ui::VirtualConsole;
use erars_vm::{FunctionDic, SystemFunctions, TerminalVm, VmContext};
use hashbrown::HashMap;

pub fn save_script(vm: TerminalVm, ctx: VmContext, target_path: &str) -> anyhow::Result<()> {
    let mut out = BufWriter::new(File::create(Path::new(target_path).join("game.era"))?);
    erars_bytecode::write_to(&mut out, &vm.dic)?;
    let local_infos: HashMap<StrKey, Vec<(StrKey, &VariableInfo)>> =
        ctx.var.local_infos().collect();
    rmp_serde::encode::write(&mut out, &(&*ctx.header_info, local_infos)).unwrap();

    Ok(())
}

pub fn load_config(target_path: &str) -> EraConfig {
    log::info!("Load config");
    let config_path = Path::new(target_path).join("emuera.config");

    let config = if Path::new(&config_path).exists() {
        match read_file(config_path.as_ref()) {
            Ok(s) => EraConfig::from_text(&s).unwrap(),
            Err(err) => {
                log::error!("config file load error: {err}");
                EraConfig::default()
            }
        }
    } else {
        EraConfig::default()
    };

    log::info!("Config: {config:?}");

    config
}

/// SAFETY: Any reference to interner is not exist
pub unsafe fn load_script(
    target_path: &str,
    system: Box<dyn SystemFunctions>,
    config: EraConfig,
) -> anyhow::Result<(TerminalVm, VmContext, VirtualConsole)> {
    let start = Instant::now();

    log::info!("Load game script");

    let game_path = Path::new(target_path).join("game.era");
    let file = File::open(game_path).context("Open bytecode file")?;
    let file = memmap2::MmapOptions::new()
        .populate()
        .map(&file)
        .context("mmap bytecode file")?;
    let mut file_bytes = &*file;
    let dic = erars_bytecode::read_from(&mut file_bytes)?;

    log::info!("Load game data");
    let (header, local_infos): (HeaderInfo, HashMap<StrKey, Vec<(StrKey, VariableInfo)>>) =
        rmp_serde::decode::from_read(&mut file_bytes)?;
    let vconsole = VirtualConsole::new(config.printc_width, config.max_log);

    let elapsed = start.elapsed();
    log::info!("Load done! {}ms elapsed", elapsed.as_millis());

    let mut ctx = VmContext::new(
        Arc::new(header),
        Arc::new(config),
        system,
        Path::new(target_path).join("sav"),
    );

    for (key, vars) in local_infos {
        for var in vars {
            ctx.var.add_local_info(key, var.0, var.1);
        }
    }

    Ok((
        TerminalVm {
            dic,
            header: ctx.header_info.clone(),
        },
        ctx,
        vconsole,
    ))
}

#[allow(unused_assignments)]
pub fn run_script(
    target_path: &str,
    mut system: Box<dyn SystemFunctions>,
    config: EraConfig,
    error_to_stderr: bool,
) -> anyhow::Result<(TerminalVm, VmContext, VirtualConsole)> {
    erars_ast::init_interner();

    let interner = erars_ast::get_interner();

    let mut time = Instant::now();

    let config = Arc::new(config);
    let mut tx = VirtualConsole::new(config.printc_width, config.max_log);

    macro_rules! check_time {
        ($work:expr) => {
            check_time!($work, system);
        };

        ($work:expr, $system:expr) => {
            let m = time.elapsed().as_millis();
            time = Instant::now();

            log::info!("[{}]: {}ms", $work, m);
            tx.print_line(format!("[{}]: {}ms", $work, m));
            $system.redraw(&mut tx)?;
        };
    }

    let mut function_dic = FunctionDic::new();
    let header_info;
    let mut ctx: VmContext;

    {
        check_time!("Initialize");

        let var_infos: HashMap<_, VariableInfo> =
            serde_yaml::from_str(include_str!("./variable.yaml"))?;

        let csvs = glob::glob_with(
            &format!("{}/CSV/**/*.CSV", target_path),
            glob::MatchOptions {
                case_sensitive: false,
                require_literal_leading_dot: true,
                require_literal_separator: true,
            },
        )?;

        let erhs = glob::glob_with(
            &format!("{}/ERB/**/*.ERH", target_path),
            glob::MatchOptions {
                case_sensitive: false,
                require_literal_leading_dot: true,
                require_literal_separator: true,
            },
        )?;

        let erbs = glob::glob_with(
            &format!("{}/ERB/**/*.ERB", target_path),
            glob::MatchOptions {
                case_sensitive: false,
                require_literal_leading_dot: true,
                require_literal_separator: true,
            },
        )?;

        #[cfg(feature = "multithread")]
        let csvs = csvs.par_bridge();

        #[cfg(feature = "multithread")]
        let erbs = erbs.par_bridge();

        let files = Mutex::new(ErarsFiles::new());
        let diagnostics = Mutex::new(Vec::new());

        macro_rules! report_error {
            ($code:expr, $msg:expr, $path:expr, $source:expr, $err:expr, $span:expr) => {
                diagnostics.lock().push(
                    Diagnostic::error()
                        .with_code($code)
                        .with_message($msg)
                        .with_labels(vec![Label::primary(files.lock().add(interner.get_or_intern($path.display().to_string()), $source), $span).with_message($err)]),
                );
            };
        }

        let mut info = HeaderInfo {
            global_variables: var_infos,
            ..Default::default()
        };

        let mut csv_dic = csvs
            .filter_map(|csv| match csv {
                Ok(csv) => {
                    log::trace!("Load {}", csv.display());
                    let s = read_file(&csv).ok()?;

                    Some((
                        csv.file_stem().unwrap().to_str().unwrap().to_ascii_uppercase(),
                        (csv, s),
                    ))
                }
                Err(err) => {
                    log::error!("Failed to load csv file: {err}");
                    None
                }
            })
            .collect::<HashMap<_, _>>();

        let chara_csv_dic = csv_dic
            .drain_filter(|k, _v| k.starts_with("CHARA"))
            .collect::<HashMap<_, _>>();

        check_time!("Load CSV");

        for (k, (path, v)) in csv_dic.into_iter() {
            match k.as_str() {
                "ABL" | "MARK" | "BASE" | "CFLAG" | "EQUIP" | "TEQUIP" | "PALAM" | "EXP" | "EX"
                | "FLAG" | "TFLAG" | "TALENT" | "STAIN" | "SOURCE" | "TSTR" | "CSTR" | "STR"
                | "SAVESTR" | "GLOBAL" | "GLOBALS" | "TRAIN" | "TCVAR" => {
                    log::debug!("Merge {k}.CSV");
                    match info.merge_name_csv(&k, &v) {
                        Ok(()) => {}
                        Err((err, span)) => {
                            report_error!("E0000", "Parse name csv", path, v, err, span);
                        }
                    }
                }
                "GAMEBASE" => {
                    log::debug!("Merge GAMEBASE.CSV");

                    match info.merge_gamebase_csv(&v) {
                        Ok(()) => {}
                        Err((err, span)) => {
                            report_error!("E0000", "Parse gamebase csv", path, v, err, span);
                        }
                    }

                    log::info!("GAMEBASE: {:?}", info.gamebase);
                }
                "VARIABLESIZE" => {
                    log::debug!("Merge VARIABLESIZE.CSV");

                    match info.merge_variable_size_csv(&v) {
                        Ok(()) => {}
                        Err((err, span)) => {
                            report_error!("E0000", "Parse variablesize csv", path, v, err, span);
                        }
                    }
                }
                "_RENAME" => {
                    log::debug!("Merge _RENAME.CSV");
                    match info.merge_rename_csv(&v) {
                        Ok(()) => {}
                        Err((err, span)) => {
                            report_error!("E0000", "Parse _rename csv", path, v, err, span);
                        }
                    }
                }
                "_REPLACE" => {
                    log::debug!("Merge _REPLACE.CSV");
                    match info.merge_replace_csv(&v) {
                        Ok(()) => {}
                        Err((err, span)) => {
                            report_error!("E0000", "Parse _replace csv", path, v, err, span);
                        }
                    }
                    log::info!("Replace: {:?}", info.replace);
                }
                "ITEM" => {
                    log::debug!("Merge ITEM.CSV");
                    match info.merge_item_csv(&v) {
                        Ok(()) => {}
                        Err((err, span)) => {
                            report_error!("E0000", "Parse item csv", path, v, err, span);
                        }
                    }
                }
                other => {
                    log::warn!("Unknown csv name {other}");
                }
            }
        }

        check_time!("Merge CSV");

        for (k, (path, v)) in chara_csv_dic.into_iter() {
            log::debug!("Merge {k}.CSV");
            match info.merge_chara_csv(&v) {
                Ok(()) => {}
                Err((err, span)) => {
                    report_error!("E0000", "Parse character csv", path, v, err, span);
                }
            }
        }

        check_time!("Merge chara CSV");

        tx.print_line(info.replace.start_message.clone());

        for erh in erhs {
            let erh = erh.unwrap();
            let source = read_file(&erh).unwrap();
            log::debug!("Parse {}", erh.display());

            match info.merge_header(&source) {
                Ok(()) => (),
                Err((err, span)) => {
                    report_error!("E1000", "Parse erh", erh, source, err, span);
                }
            }
        }

        check_time!("Merge ERH");

        // log::trace!("Header: {info:#?}");

        header_info = Arc::new(info);

        let funcs = erbs
            // .into_iter()
            .flat_map(|erb| {
                let erb = erb.unwrap();
                let source = read_file(&erb).unwrap();
                let ctx =
                    ParserContext::new(header_info.clone(), StrKey::new(erb.to_str().unwrap()));

                log::debug!("Parse And Compile {}", erb.display());

                let program = ctx.parse_and_compile(
                    &mut Preprocessor::new(&PP_REGEX, source.as_str()),
                    &mut String::new(),
                );

                match program {
                    Ok(p) => p,
                    Err((err, span)) => {
                        report_error!("E2000", "Parse erb", erb, source, err, span);
                        Vec::new()
                    }
                }
            })
            .collect::<Vec<CompiledFunction>>();

        ctx = VmContext::new(
            header_info.clone(),
            config,
            system,
            Path::new(target_path).join("sav"),
        );

        for func in funcs {
            function_dic.insert_compiled_func(
                &mut ctx.var,
                &ctx.header_info.default_local_size,
                func,
            );
        }

        check_time!("Parse/Compile ERB", ctx.system);

        let mut diagnostics = diagnostics.into_inner();
        let mut files = files.into_inner();

        diagnostics.extend(check_function(&function_dic, &ctx.var, &mut files));

        check_time!("Check codes", ctx.system);

        if !diagnostics.is_empty() {
            let config = Config::default();
            let writer = StandardStream::stderr(ColorChoice::Always);
            let mut writer = writer.lock();
            for diagnostic in diagnostics.iter().take(40) {
                if error_to_stderr {
                    codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic)
                        .unwrap();
                }
                let mut writer = LogWriter::default();
                codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic).unwrap();
                std::io::Write::flush(&mut writer).unwrap();
            }

            if let Some(left) = diagnostics.len().checked_sub(20) {
                if error_to_stderr {
                    eprintln!("And {left} more errors...");
                    log::error!("And {left} more errors...");
                }
            }
        }

        check_time!("Report errors", ctx.system);
    }

    let vm = TerminalVm::new(function_dic, ctx.header_info.clone());

    Ok((vm, ctx, tx))
}

#[derive(Default)]
struct LogWriter(String);

impl std::io::Write for LogWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let s = std::str::from_utf8(buf).unwrap();
        self.0.push_str(s);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        for line in self.0.lines() {
            log::error!("{line}");
        }
        self.0.clear();
        Ok(())
    }
}

impl WriteColor for LogWriter {
    fn supports_color(&self) -> bool {
        false
    }

    fn set_color(
        &mut self,
        _spec: &codespan_reporting::term::termcolor::ColorSpec,
    ) -> std::io::Result<()> {
        Ok(())
    }

    fn reset(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
