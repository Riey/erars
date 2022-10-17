use erars_ui::{InputRequest, InputRequestType, VirtualConsole};
use erars_vm::{TerminalVm, VmContext, VmResult};
use std::io::{self, BufRead, BufWriter};

pub struct JsonFrontend {
    vconsole: VirtualConsole,
    from: usize,
}

impl JsonFrontend {
    pub fn new(vconsole: VirtualConsole) -> Self {
        Self { vconsole, from: 0 }
    }

    fn draw(
        &mut self,
        current_req: Option<&InputRequest>,
        mut out: impl io::Write,
    ) -> anyhow::Result<()> {
        if self.vconsole.need_rebuild {
            self.from = self.vconsole.top_index;
        }

        let ret = self.vconsole.make_serializable(current_req, self.from);

        serde_json::to_writer(&mut out, &ret)?;

        self.from += ret.lines.len();
        self.vconsole.need_rebuild = false;

        out.flush()?;

        Ok(())
    }

    pub fn run(&mut self, vm: &TerminalVm, ctx: &mut VmContext) -> anyhow::Result<()> {
        let mut input = String::with_capacity(64);
        let mut stdin = io::stdin().lock();
        let mut lock = BufWriter::new(io::stdout().lock());

        loop {
            match vm.run_state(&mut self.vconsole, ctx) {
                VmResult::Exit => break Ok(()),
                VmResult::Input { req, set_result } => {
                    self.draw(Some(&req), &mut lock)?;

                    loop {
                        let size = stdin.read_line(&mut input)?;

                        let s = input[..size].trim();

                        match req.ty {
                            InputRequestType::Int => match s.trim().parse::<i64>() {
                                Ok(i) => {
                                    log::info!("[stdio] <- {i}");
                                    if set_result {
                                        ctx.var.set_result(i);
                                    } else {
                                        ctx.push(i);
                                    }
                                    break;
                                }
                                Err(_) => {
                                    continue;
                                }
                            },
                            InputRequestType::Str => {
                                log::info!("[stdio] <- \"{s}\"");
                                if set_result {
                                    ctx.var.set_results(s.into());
                                } else {
                                    ctx.push(s);
                                }
                                break;
                            }
                            InputRequestType::AnyKey | InputRequestType::EnterKey => {
                                log::info!("[stdio] <- \"\"");
                                break;
                            }
                        }
                    }

                    input.clear();
                }
                VmResult::Redraw => {
                    self.draw(None, &mut lock)?;
                }
            }
        }
    }
}
