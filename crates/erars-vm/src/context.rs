use anyhow::{bail, Context, Result};
use std::fmt;
use std::path::PathBuf;
use std::sync::Arc;

use erars_ast::{EventType, ScriptPosition, StrKey, Value, VariableInfo};
use erars_ui::VirtualConsole;
use erars_compiler::{EraConfig, HeaderInfo};

use crate::variable::StrKeyLike;
use crate::{ArgVec, SystemFunctions, VariableStorage, VmVariable};

use super::UniformVariable;

/// Emuera's infinite-loop watchdog.
///
/// Emuera counts executed lines and, every 10,000 of them, compares wall clock
/// against `InfiniteLoopAlertTime` — "reading the time is itself expensive, so
/// once per 10,000 lines or so" is its own comment
/// (`GameProc/Process.ScriptProc.cs:20-24`).
///
/// The reset is on *input*, not on the `WAIT` family, whatever
/// `docs/research/2026-09-06-language-feature-work.md` §6.2 used to say.
/// `UpdateCheckInfiniteLoopState` (`GameProc/Process.cs:304-307`) has exactly
/// two callers: `DoScript` (`Process.cs:266-270`), which the console
/// re-enters after every answer (`GameView/EmueraConsole.cs:787`), and
/// `EmueraConsole.Await` (`GameView/EmueraConsole.cs:553`), reached only from
/// `AWAIT` (`GameProc/Function/Instraction.Child.cs:1668`).
/// `WAIT`/`FORCEWAIT`/`TWAIT` go through `WaitInput`, which does not reset —
/// they reset only transitively, by forcing an input round-trip. Wiring this
/// against "no `WAIT` for N ms" would fire where Emuera does not and stay
/// silent where it does.
///
/// The default is 5000 ms (`Config/ConfigData.cs:71`), so this is armed
/// unless a game asks for `0`.
///
/// DELIBERATE: on trigger Emuera opens a modal "無限ループの可能性がありま
/// す … 強制終了しますか?" dialog and aborts only if the user says yes
/// (`GameProc/Process.cs:331-344`). A headless build has nobody to ask, and
/// killing a long-but-correct script is far worse than a warning nobody
/// reads, so erars logs and always continues — Emuera's own "No" branch,
/// which resets the clock and the counter (`:341-343`).
#[derive(Debug)]
pub(crate) struct LoopAlert {
    /// `InfiniteLoopAlertTime` in milliseconds; 0 disables the watchdog.
    limit_ms: u32,
    /// Instructions still to run before the next clock read. erars has no
    /// line table at run time, so it counts instructions; the poll interval
    /// is Emuera's. Counting *down* keeps the per-instruction cost to one
    /// decrement and one branch, with the disabled key never re-reached
    /// after the first poll (`countdown` is then parked at `u32::MAX`).
    countdown: u32,
    start: std::time::Instant,
}

impl LoopAlert {
    const POLL: u32 = 10_000;

    pub fn new(limit_ms: u32) -> Self {
        Self {
            limit_ms,
            countdown: if limit_ms == 0 { u32::MAX } else { Self::POLL },
            start: std::time::Instant::now(),
        }
    }

    /// Emuera's `UpdateCheckInfiniteLoopState` (`Process.cs:304-307`).
    pub fn reset(&mut self) {
        if self.limit_ms != 0 {
            self.countdown = Self::POLL;
            self.start = std::time::Instant::now();
        }
    }

    /// One executed instruction. Returns the elapsed milliseconds of a
    /// triggered episode, at most once per episode: a trigger restarts the
    /// clock, exactly as Emuera's "No" branch does (`Process.cs:341-343`), so
    /// a tight loop warns once per `limit_ms` rather than once per poll.
    #[inline]
    pub fn tick(&mut self) -> Option<u128> {
        if self.countdown > 0 {
            self.countdown -= 1;
            return None;
        }
        self.poll()
    }

    #[inline(never)]
    fn poll(&mut self) -> Option<u128> {
        if self.limit_ms == 0 {
            self.countdown = u32::MAX;
            return None;
        }
        self.countdown = Self::POLL;
        let elapsed = self.start.elapsed().as_millis();
        if elapsed < self.limit_ms as u128 {
            return None;
        }
        self.start = std::time::Instant::now();
        Some(elapsed)
    }
}

pub struct VmContext {
    pub var: VariableStorage,
    pub header_info: Arc<HeaderInfo>,
    pub config: Arc<EraConfig>,
    pub system: Box<dyn SystemFunctions>,
    pub sav_dir: PathBuf,
    /// Emuera `Program.ContentDir` (`Program.cs:63`) — `<game>/resources`,
    /// the root a `GCREATEFROMFILE` relative path resolves against.
    pub content_dir: PathBuf,

    /// Off-screen bitmaps and sprites for the `G*` / `SPRITE*` commands.
    pub graphics: crate::GraphicsStore,

    /// For NOSKIP/ENDNOSKIP
    pub(crate) prev_skipdisp: Option<bool>,
    /// Set `true` during SAVEGAME
    pub(crate) put_form_enabled: bool,
    /// Emuera `EmueraConsole.isTimeout` — read by `ISTIMEOUT`, maintained in
    /// [`VmContext::input_redraw`].
    pub(crate) is_timeout: bool,

    /// Emuera's infinite-loop watchdog (`無限ループ警告までのミリ秒数`,
    /// `InfiniteLoopAlertTime`, default 5000 ms —
    /// `Config/ConfigData.cs:71`). See [`LoopAlert`].
    pub(crate) loop_alert: LoopAlert,

    /// Emuera `Program.DebugMode`, the `-Debug` command line argument
    /// (`Program.cs:219-220`). `@DEBUG` is gated on it
    /// (`GameView/EmueraConsole.cs:1367-1373`); a front end that exposes the
    /// debug console sets it with [`VmContext::with_debug_mode`].
    pub(crate) debug_mode: bool,
    /// `@REBOOT` asked for a fresh engine (`Forms/MainWindow.cs:810-811`).
    /// Read with [`VmContext::reboot_requested`] once the run has ended.
    reboot_requested: bool,

    /// Emuera `Process.isCTrain`: only a `CALLTRAIN` sequence — never
    /// `DOTRAIN` — may be abandoned by `STOPCALLTRAIN`.
    pub(crate) call_train_running: bool,
    /// Emuera `Process.ClearCommands`: `STOPCALLTRAIN` empties the queue of
    /// commands `CALLTRAIN` has not run yet.
    pub(crate) call_train_stopped: bool,

    pub(crate) lastload_version: u32,
    pub(crate) lastload_no: u32,
    pub(crate) lastload_text: String,

    /// Emuera's `static readonly short[] keytoggle`
    /// (`Creator.Method.cs:6709`): per virtual key, the toggle state the last
    /// `GETKEY`/`GETKEYTRIGGERED` call observed, stored as `(state & 1) + 1`
    /// so 0 means "never queried". Both methods share it, so a `GETKEY` call
    /// consumes the edge a later `GETKEYTRIGGERED` would have seen.
    key_toggle: [u8; erars_ui::InputState::KEYS],

    stack: Vec<LocalValue>,
    call_stack: Vec<Callstack>,
}

impl VmContext {
    pub fn new(
        header_info: Arc<HeaderInfo>,
        config: Arc<EraConfig>,
        system: Box<dyn SystemFunctions>,
        sav_dir: PathBuf,
        content_dir: PathBuf,
    ) -> Self {
        let mut ret = Self {
            var: VariableStorage::new(header_info.clone(), &header_info.global_variables),
            sav_dir,
            content_dir,
            graphics: crate::GraphicsStore::default(),
            system,
            header_info,
            stack: Vec::with_capacity(1024),
            call_stack: Vec::with_capacity(512),
            prev_skipdisp: None,
            put_form_enabled: false,
            is_timeout: false,
            loop_alert: LoopAlert::new(config.infinite_loop_alert_time),
            debug_mode: false,
            reboot_requested: false,
            call_train_running: false,
            call_train_stopped: false,
            lastload_no: 0,
            lastload_text: "".into(),
            lastload_version: 0,
            key_toggle: [0; erars_ui::InputState::KEYS],
            config,
        };

        ret.init_variable().unwrap();

        // `キャラクタ変数の引数を補完しない` (`Config/ConfigData.cs:114`,
        // default `false`) is consulted on every character-variable access, so
        // the storage keeps its own copy instead of reaching back for the
        // config. Set after `init_variable`, which is engine start-up rather
        // than script code and must not be subject to it.
        ret.var.no_target = ret.config.system_no_target;

        ret
    }

    /// Emuera's `-Debug` argument (`Program.cs:219-220`), which decides
    /// whether `@DEBUG` opens the debug window or refuses. Set by whichever
    /// front end exposes the debug console, after the game has loaded, so the
    /// compiled-bytecode path gets it too.
    pub fn set_debug_mode(&mut self, debug_mode: bool) {
        self.debug_mode = debug_mode;
    }

    /// Whether the run ended because `@REBOOT` asked for a fresh engine
    /// (`Forms/MainWindow.cs:807-812`). Only a front end that reloads the
    /// game can honour it; see §5.16 of
    /// `docs/research/2026-09-03-emuera-command-gap.md`.
    pub fn reboot_requested(&self) -> bool {
        self.reboot_requested
    }

    /// Show `tx`, publishing every bitmap changed since the last frame first.
    ///
    /// Every path that makes the console visible goes through this method or
    /// one of the `input_*` wrappers below. Nothing else *can*:
    /// [`SystemFunctions::redraw`] demands a [`graphics::Painted`], only
    /// `GraphicsStore::publish` mints one, and the token borrows the store, so
    /// the publish is provably the last thing that touches pixels before the
    /// frame leaves the VM thread. Without that, the renderer could draw new
    /// text against old pixels.
    pub fn redraw(&mut self, tx: &mut VirtualConsole) -> Result<()> {
        let painted = self.graphics.publish(&tx.images);
        self.system.redraw(tx, painted)
    }

    /// `GETKEY` and `GETKEYTRIGGERED`, which Emuera serves from one
    /// `GetKeyStateMethod` (`Creator.Method.cs:6710-6735`).
    ///
    /// The order of effects is the C#'s: an out-of-range code returns 0
    /// *without* touching the latch, and the latch is written on every
    /// in-range call whether the key is down or not. `GETKEYTRIGGERED` is
    /// therefore "down, and its toggle bit differs from the last observation",
    /// which is true on the first observation of a held key and again on every
    /// fresh press.
    ///
    /// Emuera returns 0 for both when the console is not active; in this fork
    /// `IsActive` is a constant `true` (`GameView/EmueraConsole.cs:276-277`),
    /// so that guard can never fire and is not reproduced.
    pub fn get_key(&mut self, keycode: i64, triggered: bool) -> Result<i64> {
        if !(0..=255).contains(&keycode) {
            return Ok(0);
        }
        let vk = keycode as u8;
        let state = self.system.input_state()?;
        let prev = self.key_toggle[vk as usize];
        let now = state.is_toggled(vk) as u8 + 1;
        self.key_toggle[vk as usize] = now;
        Ok(if triggered {
            (state.is_down(vk) && prev != now) as i64
        } else {
            state.is_down(vk) as i64
        })
    }

    /// [`SystemFunctions::input_redraw`] behind the image publish, and the
    /// one place a debug console command can be entered.
    ///
    /// A request that carries a deadline also drives `ISTIMEOUT`, exactly as
    /// Emuera's timer does: `presetTimer` clears the flag when a request with
    /// `Timelimit > 0` starts and `endTimer` raises it when the timer expires,
    /// so an untimed `INPUT` in between leaves the previous answer standing
    /// (`GameView/EmueraConsole.cs:575-580,671-673,737-738`).
    ///
    /// Emuera diverts an answer beginning with `@` to `doSystemCommand`
    /// *instead of* consuming the pending request, so the same prompt comes
    /// back once the command has run (`GameView/EmueraConsole.cs:1103-1110`).
    /// Its console can do that because it owns the input loop; erars' VM owns
    /// it, so the loop below is the same thing in the same place: classify the
    /// answer, run it, ask again. See [`crate::debug_console`].
    pub fn input_redraw(
        &mut self,
        tx: &mut VirtualConsole,
        req: erars_ui::InputRequest,
    ) -> Result<Option<Value>> {
        // Emuera resets the watchdog by re-entering `DoScript` after the
        // console hands an answer back (`Process.cs:266-270` from
        // `GameView/EmueraConsole.cs:787`); erars owns the input loop, so the
        // reset belongs here.
        self.loop_alert.reset();
        let expiry = req.timeout.as_ref().map(|t| (t.timeout, t.default_value.clone()));

        loop {
            let painted = self.graphics.publish(&tx.images);

            if expiry.is_some() {
                self.is_timeout = false;
            }

            let ret = self.system.input_redraw(tx, req.clone(), painted)?;

            if let Some((deadline, default_value)) = &expiry {
                // DELIBERATE: erars enforces the deadline in the frontend,
                // which answers with `Timeout::default_value` once it passes,
                // so the VM has no timer of its own to read. Both conditions
                // below hold for every genuine expiry and the only way to fake
                // them is to send the default value after the deadline has
                // already gone by — an answer indistinguishable from the
                // timeout in every other respect. See §5 of
                // `docs/research/2026-09-03-emuera-command-gap.md`.
                self.is_timeout = time::OffsetDateTime::now_utc().unix_timestamp_nanos()
                    >= *deadline
                    && ret.as_ref() == Some(default_value);
            }

            let Some(Value::String(answer)) = &ret else {
                return Ok(ret);
            };

            let line = crate::debug_console::classify(answer, req.is_one, self.config.ignore_case);

            if line == crate::debug_console::DebugLine::Answer {
                return Ok(ret);
            }

            if expiry.is_some() {
                // `if (timer.Enabled)` (`GameView/EmueraConsole.cs:1323-1329`):
                // no command runs while a `TINPUT` deadline is counting down,
                // and the request stays pending either way.
                crate::debug_console::refuse_for_timer(tx);
            } else {
                // `Print(command); PrintFlush(false);` (`:1336-1338`) — the
                // line is echoed before anything acts on it, and `@` alone is
                // the one case that prints nothing (`:1340-1341`).
                if line != crate::debug_console::DebugLine::Empty {
                    tx.print_line(answer.clone());
                }
                let quit = crate::debug_console::run(line, tx, self);
                if let Err(err) = quit {
                    if let Some(q) = err.downcast_ref::<crate::debug_console::DebugConsoleQuit>() {
                        self.reboot_requested = q.reboot;
                    }
                    return Err(err);
                }
            }
        }
    }

    /// [`SystemFunctions::input_int_redraw`] behind the image publish.
    pub fn input_int_redraw(&mut self, tx: &mut VirtualConsole) -> Result<i64> {
        self.loop_alert.reset();
        let painted = self.graphics.publish(&tx.images);
        self.system.input_int_redraw(tx, painted)
    }

    /// [`SystemFunctions::input_mouse_key`] behind the image publish.
    pub fn input_mouse_key(
        &mut self,
        tx: &mut VirtualConsole,
        req: erars_ui::InputRequest,
    ) -> Result<erars_ui::MouseKeyEvent> {
        self.loop_alert.reset();
        let painted = self.graphics.publish(&tx.images);
        self.system.input_mouse_key(tx, req, painted)
    }

    /// The game language's legacy encoding (`Language::encoding`) — the
    /// same encoding the console's `WidthTable` was built from.
    pub fn encoding(&self) -> &'static encoding_rs::Encoding {
        self.config.lang.encoding()
    }

    fn init_variable(&mut self) -> Result<()> {
        self.var.init(&self.header_info)?;

        Ok(())
    }

    pub fn var_mut(&mut self) -> &mut VariableStorage {
        &mut self.var
    }

    pub fn call_stack(&self) -> &[Callstack] {
        &self.call_stack
    }

    pub fn stack(&self) -> &[LocalValue] {
        &self.stack
    }

    pub fn update_position(&mut self, pos: ScriptPosition) {
        if let Some(last) = self.call_stack.last_mut() {
            last.script_position = pos;
        }
    }

    /// Resolve `func`/`var_name` to the variable it names, following a
    /// `#DIM REF` variable to its target.
    ///
    /// A reference's storage is one int holding `(name, func)` interned keys,
    /// which `REF` and reference call arguments write. Both halves of an
    /// unbound reference are zero, and `lasso` keys are non-zero, so a zero
    /// slot is Emuera's `trerror.EmptyRefVar` rather than a real target.
    pub fn make_var_ref(
        &mut self,
        func: impl StrKeyLike,
        var_name: impl StrKeyLike,
        idxs: ArgVec,
    ) -> Result<VariableRef> {
        let mut func_name = func.get_key(&self.var);
        let mut var_name = var_name.get_key(&self.var);

        if let Ok((info, var)) = self.var.get_local_var(func_name, var_name) {
            if info.is_ref {
                let packed = match var {
                    UniformVariable::Normal(VmVariable::Int(v)) => v[0],
                    _ => bail!("참조 변수 {var_name}의 저장소가 정수가 아닙니다"),
                };

                if packed == 0 {
                    bail!("참조 변수 {var_name}는 아무것도 참조하고 있지 않습니다");
                }

                let (name, func): (u32, u32) = unsafe { std::mem::transmute(packed) };
                var_name = StrKey::from_u32(name);
                func_name = StrKey::from_u32(func);
            }
        }

        Ok(VariableRef {
            name: var_name,
            func_name,
            idxs,
        })
    }

    pub fn reduce_local_value(&mut self, value: LocalValue) -> Result<Value> {
        match value {
            LocalValue::Value(v) => Ok(v),
            LocalValue::InternedStr(s) => Ok(self.var.resolve_key(s).into()),
            LocalValue::VarRef(r) => self.read_var_ref(&r),
            LocalValue::Omitted => bail!("Omitted argument used as a value"),
        }
    }

    pub fn read_var_ref(&mut self, var_ref: &VariableRef) -> Result<Value> {
        let (_, var, idx) = self.resolve_var_ref(var_ref)?;
        // log::info!("Read {} -> {:?}", var_ref.name, var.get(idx)?);

        var.get(idx)
    }

    pub fn ref_int_var_ref(&mut self, var_ref: &VariableRef) -> Result<&mut i64> {
        let (_, var, idx) = self.resolve_var_ref(var_ref)?;

        var.as_int()?
            .get_mut(idx as usize)
            .ok_or_else(|| anyhow::anyhow!("Variable {:?} out of index", var_ref.name))
    }

    pub fn set_var_ref(&mut self, var_ref: &VariableRef, value: Value) -> Result<()> {
        let (info, var, idx) = self.resolve_var_ref(var_ref)?;
        if info.is_const {
            bail!(
                "variable {} can't be modified because it's CONST",
                var_ref.name
            );
        }
        var.set(idx, value)?;
        Ok(())
    }

    /// Emuera's comma-separated bulk array-literal assignment
    /// (`erars_ast::Stmt::ArrayAssign`): `values[0]` goes to `var_ref`'s own
    /// index, `values[1]` to the next element, and so on, filling
    /// consecutively. Per `docs/research/emuera-wiki/exetc.md`'s "Batch
    /// Assignment to Array Variables" (`DA:0:0 to DA:0:99 is not assigned to
    /// DA:1:0, and an out-of-array reference error occurs`), the fill is
    /// bounded by the *last* declared dimension's size, not the variable's
    /// total flat storage size — spilling into the next outer index is an
    /// error, never a silent wraparound. Earlier elements in `values` are
    /// already written by the time a later one is found out of range
    /// (a plain sequential fill, not validated upfront), matching a naive
    /// imperative loop.
    pub fn set_var_ref_seq(&mut self, var_ref: &VariableRef, values: Vec<Value>) -> Result<()> {
        if values.is_empty() {
            return Ok(());
        }

        let last_dim_size = {
            let (info, _var) = self.var.get_maybe_local_var(var_ref.func_name, var_ref.name)?;
            info.size.last().copied().unwrap_or(1)
        };

        let start = var_ref.idxs.last().copied().unwrap_or(0);
        let leading_len = var_ref.idxs.len().saturating_sub(1);

        for (i, value) in values.into_iter().enumerate() {
            let cur = start + i as u32;
            if cur >= last_dim_size {
                bail!(
                    "배치 대입 인덱스가 배열 범위를 벗어났습니다: {} index {} >= size {}",
                    var_ref.name,
                    cur,
                    last_dim_size
                );
            }

            let mut idxs: ArgVec = var_ref.idxs[..leading_len].iter().copied().collect();
            idxs.push(cur);

            let sub_ref = VariableRef {
                name: var_ref.name,
                func_name: var_ref.func_name,
                idxs,
            };

            self.set_var_ref(&sub_ref, value)?;
        }

        Ok(())
    }

    pub fn resolve_var_ref<'c>(
        &'c mut self,
        r: &VariableRef,
    ) -> Result<(&'c mut VariableInfo, &'c mut VmVariable, u32)> {
        self.var.index_maybe_local_var(r.func_name, r.name, &r.idxs)
    }

    pub fn resolve_var_ref_raw<'c>(
        &'c mut self,
        r: &VariableRef,
    ) -> Result<(&'c mut VariableInfo, &'c mut UniformVariable, ArgVec)> {
        let (info, var) = self.var.get_maybe_local_var(r.func_name, r.name)?;

        Ok((info, var, r.idxs))
    }

    pub fn new_func(&mut self, func_name: FunctionIdentifier, file_path: StrKey) {
        self.call_stack.push(Callstack {
            func_name,
            file_path,
            script_position: ScriptPosition::default(),
            stack_base: self.stack.len(),
        });
    }

    pub fn end_func(&mut self, func_name: FunctionIdentifier) {
        self.pop_call_stack();

        if let FunctionIdentifier::Normal(name) = func_name {
            self.var.clear_dynamic_vars(name);
        }
    }

    pub fn pop_call_stack(&mut self) -> Option<Callstack> {
        self.call_stack.pop()
    }

    pub fn clear_call_stack(&mut self) {
        self.call_stack.clear();
    }

    pub fn return_func(&mut self) -> Result<impl Iterator<Item = Value>> {
        let count = self.current_stack_count();
        Ok(self.take_value_list(count as u32)?.into_iter())
    }

    pub fn current_stack_count(&self) -> usize {
        self.stack.len() - self.call_stack.last().map_or(0, |s| s.stack_base)
    }

    pub fn take_arg_list(&mut self, var_name: Option<StrKey>, count: u32) -> Result<ArgVec> {
        self.take_value_list(count)?
            .into_iter()
            .map(|value| match value {
                Value::Int(i) => u32::try_from(i).context("Index convert error"),
                Value::String(str) => match var_name
                    .map(|s| self.var.resolve_key(s))
                    .map(erars_ast::var_name_alias)
                    .map(|s| self.var.interner().get_or_intern(s))
                    .and_then(|var_name| {
                        self.header_info.var_names.get(&var_name.get_key(&self.var))
                    })
                    .and_then(|names| names.get(&str.get_key(&self.var)))
                {
                    Some(value) => Ok(*value),
                    None => anyhow::bail!("Can't index variable with String"),
                },
            })
            .collect()
    }

    pub fn take_value_list(&mut self, count: u32) -> Result<Vec<Value>> {
        let mut ret = Vec::new();

        for arg in self.stack.drain(self.stack.len() - count as usize..) {
            match arg {
                LocalValue::Value(v) => ret.push(v),
                LocalValue::VarRef(var) => {
                    let var = self.var.index_maybe_local_var(var.func_name, var.name, &var.idxs)?;
                    ret.push(var.1.get(var.2)?);
                }
                LocalValue::Omitted => bail!("Omitted argument used as a value"),
                LocalValue::InternedStr(s) => {
                    ret.push(self.var.resolve_key(s).into());
                }
            }
        }

        Ok(ret)
    }

    pub fn take_list(&mut self, count: u32) -> impl Iterator<Item = LocalValue> + '_ {
        self.stack.drain(self.stack.len() - count as usize..)
    }

    pub fn dup(&mut self) {
        let last = self.stack.last().unwrap().clone();
        self.stack.push(last);
    }

    pub fn dup_prev(&mut self) {
        let prev = self.stack[self.stack.len() - 2].clone();
        self.stack.push(prev);
    }

    pub fn push_var_ref(
        &mut self,
        name: StrKey,
        func_name: StrKey,
        idxs: ArgVec,
    ) -> Result<()> {
        let var_ref = self.make_var_ref(func_name, name, idxs)?;
        self.stack.push(LocalValue::VarRef(var_ref));
        Ok(())
    }

    pub fn push_strkey(&mut self, key: StrKey) {
        self.stack.push(LocalValue::InternedStr(key));
    }

    pub fn push(&mut self, value: impl Into<Value>) {
        self.stack.push(LocalValue::Value(value.into()));
    }

    pub fn push_omitted(&mut self) {
        self.stack.push(LocalValue::Omitted);
    }

    pub fn pop(&mut self) -> Result<LocalValue> {
        if let Some(last_stack) = self.call_stack.last() {
            if last_stack.stack_base >= self.stack.len() {
                bail!("다른 함수의 스택을 침범했습니다. 이전 함수 콜스택: {last_stack:?} 현재 스택 길이: {}", self.stack.len());
            }
        }

        // if this failed, it must be compiler error
        match self.stack.pop() {
            Some(v) => Ok(v),
            None => bail!("Stack is empty"),
        }
    }

    pub fn pop_value(&mut self) -> Result<Value> {
        match self.pop()? {
            LocalValue::Value(v) => Ok(v),
            LocalValue::VarRef(var_ref) => self.read_var_ref(&var_ref),
            LocalValue::InternedStr(s) => Ok(Value::String(self.var.resolve_key(s).into())),
            LocalValue::Omitted => bail!("Omitted argument used as a value"),
        }
    }

    pub fn pop_var_ref(&mut self) -> Result<VariableRef> {
        self.pop()?.try_into()
    }

    pub fn pop_str(&mut self) -> Result<String> {
        self.pop_value().and_then(|v| v.try_into_str())
    }

    /// A key off the stack, as an *identity*: the name of a function, a
    /// variable or a label.
    ///
    /// `LoadStr` may have pushed an ERB literal, whose key is a slot in the
    /// literal store and unique to nothing, so it is traded for the interner's
    /// answer here. The compiler already does that for a name it can see
    /// (`Compiler::push_name_expr`); what reaches this is a name computed at
    /// run time.
    pub fn pop_strkey(&mut self) -> Result<StrKey> {
        let value = match self.pop()? {
            LocalValue::InternedStr(s) => return Ok(s.to_global()),
            LocalValue::Value(v) => v,
            LocalValue::VarRef(var_ref) => self.read_var_ref(&var_ref)?,
            LocalValue::Omitted => bail!("Omitted argument used as a value"),
        };

        match value {
            Value::Int(_) => bail!("Value is not Str"),
            Value::String(s) => Ok(self.var.interner().get_or_intern(s)),
        }
    }

    pub fn pop_int(&mut self) -> Result<i64> {
        self.pop_value().and_then(|v| v.try_into_int())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FunctionIdentifier {
    Normal(StrKey),
    Event(EventType),
}

impl fmt::Display for FunctionIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionIdentifier::Normal(s) => s.fmt(f),
            FunctionIdentifier::Event(ev) => ev.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Callstack {
    pub func_name: FunctionIdentifier,
    pub file_path: StrKey,
    pub script_position: ScriptPosition,
    pub stack_base: usize,
}

#[derive(Clone, Copy)]
pub struct VariableRef {
    pub name: StrKey,
    pub func_name: StrKey,
    pub idxs: ArgVec,
}

impl fmt::Debug for VariableRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{:?}", self.name, self.func_name)?;

        for idx in self.idxs.iter() {
            write!(f, ":{}", idx)?;
        }

        Ok(())
    }
}

impl fmt::Display for VariableRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.name)?;

        for idx in self.idxs.iter() {
            write!(f, ":{}", idx)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub enum LocalValue {
    Value(Value),
    InternedStr(StrKey),
    VarRef(VariableRef),
    /// A positional argument the call left empty, for a parameter that has no
    /// default value and is not `ARG`/`ARGS`/a private variable.
    ///
    /// Emuera's `Def[i]` is `null` for exactly that shape
    /// (`GameProc/ErbLoader.cs:580-590`: only `ARG`, `ARGS` and private
    /// variables get the implicit `0`/`""`), and its call binder then either
    /// refuses the call or leaves the callee's variable untouched depending on
    /// `CompatiFuncArgOptional` (`GameProc/Process.CalledFunction.cs:191-198`,
    /// with `UserDefinedFunctionArgument.SetTransporter`'s
    /// `if (Arguments[i] == null) continue;` at `:36-37` performing the
    /// "untouched" half). Only `TerminalVm::call_internal` ever reads it.
    Omitted,
}

impl<T> From<T> for LocalValue
where
    Value: From<T>,
{
    fn from(v: T) -> Self {
        Self::Value(Value::from(v))
    }
}

impl From<VariableRef> for LocalValue {
    fn from(r: VariableRef) -> Self {
        Self::VarRef(r)
    }
}

impl TryFrom<LocalValue> for VariableRef {
    type Error = anyhow::Error;

    fn try_from(value: LocalValue) -> Result<VariableRef, Self::Error> {
        match value {
            LocalValue::VarRef(v) => Ok(v),
            _ => bail!("LocalValue type is not VariableRef"),
        }
    }
}
