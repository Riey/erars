use anyhow::{anyhow, Result};
use derivative::Derivative;
use enum_map::EnumMap;
use erars_ast::get_interner;
use erars_ast::InlineValue;
use erars_ast::Interner;
use erars_ast::StrKey;
use erars_compiler::DefaultLocalVarSize;
use hashbrown::HashMap;

use erars_ast::{Event, EventFlags, EventType, Expr, FunctionInfo, VariableInfo};
use erars_compiler::{CompiledFunction, Instruction};
use itertools::Itertools;

use crate::variable::KnownVariableNames;
use crate::ArgVec;
use crate::VariableStorage;

// Tag(8), Data(8)
static_assertions::assert_eq_size!(InlineValue, (u64, i64));

// StrKey(4), ArgVec(4 * 4), Option<InlineValue>(8 * 2)
static_assertions::assert_eq_size!(FunctionArgDef, [u32; 10]);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct FunctionArgDef(pub StrKey, pub ArgVec, pub Option<InlineValue>);

// StrKey(4), u32(4)
static_assertions::assert_eq_size!(FunctionGotoLabel, [u32; 2]);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct FunctionGotoLabel(pub StrKey, pub u32);

#[derive(Clone, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct FunctionBody {
    pub file_path: StrKey,
    pub is_function: bool,
    pub is_functions: bool,
    pub goto_labels: Box<[FunctionGotoLabel]>,
    pub args: Box<[FunctionArgDef]>,
    pub body: Box<[Instruction]>,
}

impl FunctionBody {
    pub fn goto_labels(&self) -> &[FunctionGotoLabel] {
        &self.goto_labels
    }

    pub fn args(&self) -> &[FunctionArgDef] {
        &self.args
    }

    pub fn body(&self) -> &[Instruction] {
        &self.body
    }

    pub fn file_path(&self) -> StrKey {
        self.file_path
    }

    pub fn is_function(&self) -> bool {
        self.is_function
    }

    pub fn is_functions(&self) -> bool {
        self.is_functions
    }
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct EventCollection {
    pub single: bool,
    /// A `#ONLY` body is registered, and it is `events[0]`. Emuera's group 0
    /// (`GameProc/LabelDictionary.cs:99-100`, `:112`).
    pub only: bool,
    pub empty_count: usize,
    pub events: Vec<FunctionBody>,
}

impl EventCollection {
    /// The bodies this event actually runs, in order.
    ///
    /// `#ONLY` ends the event the moment its body returns
    /// (`GameProc/Process.State.cs:399-400`: `called.IsOnly` → `FinishEvent()`),
    /// so a collection with one only ever runs that one — every later body is
    /// registered (and linted) but unreachable, which is exactly what Emuera's
    /// `AlreadyDeclaredOnly` warns about.
    pub fn iter(&self) -> impl Iterator<Item = &'_ FunctionBody> {
        self.events.iter().take(match self.only {
            true => 1,
            false => self.events.len(),
        })
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq)]
pub struct FunctionDic {
    #[derivative(Debug = "ignore", PartialEq = "ignore")]
    pub interner: &'static Interner,
    pub normal: HashMap<StrKey, FunctionBody>,
    pub event: EnumMap<EventType, EventCollection>,
    /// `イベント関数のCALLを許可する` — when on, every event function also gets
    /// a normal-function entry so `CALL` can reach it
    /// (`GameProc/LabelDictionary.cs:83-84`). A load-time decision, like
    /// `--debug`: it is baked into the dictionary `--save` writes, so a
    /// dictionary read back from `game.era` needs no flag.
    #[derivative(Debug = "ignore", PartialEq = "ignore")]
    pub compati_call_event: bool,
}

impl FunctionDic {
    pub fn new() -> Self {
        Self {
            interner: get_interner(),
            normal: HashMap::new(),
            event: EnumMap::default(),
            compati_call_event: false,
        }
    }

    pub fn insert_compiled_func(
        &mut self,
        var_dic: &mut VariableStorage,
        default_var_size: &DefaultLocalVarSize,
        func: CompiledFunction,
    ) {
        let mut body = FunctionBody {
            body: func.body,
            args: func
                .header
                .args
                .into_iter()
                .map(|(var, default_value)| {
                    FunctionArgDef(
                        var.var,
                        var.args
                            .into_iter()
                            .map(|v| {
                                if let Expr::Int(i) = v {
                                    i as u32
                                } else {
                                    panic!(
                                        "Variable index must be constant, @{} in {} has {v:?}",
                                        func.header.name, func.header.file_path
                                    )
                                }
                            })
                            .collect(),
                        default_value,
                    )
                })
                .collect_vec()
                .into_boxed_slice(),
            goto_labels: func
                .goto_labels
                .into_iter()
                .map(|(k, pos)| FunctionGotoLabel(k, pos))
                .collect_vec()
                .into_boxed_slice(),
            file_path: func.header.file_path,
            is_function: false,
            is_functions: false,
        };

        let mut flags = EventFlags::None;
        let mut local_size = default_var_size.default_local_size;
        let mut locals_size = default_var_size.default_locals_size;
        let mut local_entries: Vec<(StrKey, VariableInfo)> = Vec::new();

        for info in func.header.infos {
            match info {
                // Already folded, positive and in range: the parser rejects
                // everything else with Emuera's own diagnostic
                // (`erars-compiler/src/parser.rs`, `#LOCALSIZE` arm of
                // `push_info`), where a failure to fold here would have
                // silently replaced the `!VariableSize.csv` default with
                // nothing.
                FunctionInfo::LocalSize(size) => {
                    local_size = Some(size);
                }
                FunctionInfo::LocalSSize(size) => {
                    locals_size = Some(size);
                }
                FunctionInfo::EventFlag(f) => {
                    flags = f;
                }
                // `#FUNCTION` and `#FUNCTIONS` on one function is a parse-time
                // diagnostic (`AlreadyDeclaredSharpFunction(s)`,
                // `GameProc/LogicalLineParser.cs:161-165`) and the second
                // directive is dropped there, so only one of these can be set.
                FunctionInfo::Function => {
                    body.is_function = true;
                }
                FunctionInfo::FunctionS => {
                    body.is_functions = true;
                }
                FunctionInfo::Dim(local) => {
                    local_entries.push((local.var, local.info));
                }
            }
        }

        // builtin locals

        let local = var_dic.known_key(KnownVariableNames::Local);
        let locals = var_dic.known_key(KnownVariableNames::LocalS);
        let arg = var_dic.known_key(KnownVariableNames::Arg);
        let args = var_dic.known_key(KnownVariableNames::ArgS);

        if let Some(local_size) = local_size {
            local_entries.push((
                local,
                VariableInfo {
                    size: vec![local_size],
                    ..Default::default()
                },
            ));
        }

        if let Some(locals_size) = locals_size {
            local_entries.push((
                locals,
                VariableInfo {
                    is_str: true,
                    size: vec![locals_size],
                    ..Default::default()
                },
            ));
        }

        if let Some(arg_size) = default_var_size.default_arg_size {
            local_entries.push((
                arg,
                VariableInfo {
                    size: vec![arg_size],
                    ..Default::default()
                },
            ));
        }

        if let Some(args_size) = default_var_size.default_args_size {
            local_entries.push((
                args,
                VariableInfo {
                    is_str: true,
                    size: vec![args_size],
                    ..Default::default()
                },
            ));
        }

        var_dic.insert_local_table(func.header.name, local_entries);

        if let Ok(ty) = func.header.name.resolve().parse::<EventType>() {
            // With `イベント関数のCALLを許可する` on, Emuera also files the
            // event function under its own name in the *non*-event dictionary,
            // and only the first body defined gets that slot — `#PRI`,
            // `#LATER` and `#SINGLE` are ignored for the `CALL` path
            // (`GameProc/LabelDictionary.cs:82-84`, eramaker behaviour). ERBs
            // are loaded in sorted filename order, so "first defined" is
            // "first inserted": keep whichever entry is already there.
            if self.compati_call_event {
                self.normal.entry(func.header.name).or_insert_with(|| body.clone());
            }
            self.insert_event(Event { ty, flags }, body);
        } else {
            self.insert_func(func.header.name, body);
        }
    }

    pub fn insert_func(&mut self, name: StrKey, body: FunctionBody) {
        self.normal.insert(name, body);
    }

    pub fn insert_event(&mut self, event: Event, body: FunctionBody) {
        let collection = &mut self.event[event.ty];
        // `events[0]` belongs to `#ONLY` once one is registered; nothing may
        // displace it, because it is the only body that runs.
        let base = collection.only as usize;
        match event.flags {
            EventFlags::Only => {
                if collection.only {
                    // 「このイベント関数"@{0}"にはすでに#ONLYが宣言されています
                    // （この関数は実行されません）」
                    // (`_Library/EvilMask/Lang.cs:856`). Emuera warns at
                    // level 1 and registers the body anyway.
                    log::warn!(
                        "이벤트 함수 \"@{}\"에는 이미 #ONLY 선언이 있습니다(이 함수는 실행되지 않습니다): {}",
                        <&str>::from(event.ty),
                        body.file_path.resolve(),
                    );
                    collection.events.push(body);
                } else {
                    collection.events.insert(0, body);
                    collection.only = true;
                }
            }
            EventFlags::Single => {
                collection.events.truncate(base);
                collection.events.push(body);
                collection.single = true;
            }
            EventFlags::Later => {
                if !collection.single {
                    collection.events.push(body);
                }
            }
            EventFlags::Pre => {
                if !collection.single {
                    collection.events.insert(base + collection.empty_count, body);
                }
            }
            EventFlags::None => {
                if !collection.single {
                    collection.events.insert(base, body);
                    collection.empty_count += 1;
                }
            }
        }
    }

    pub fn get_event(&self, ty: EventType) -> &EventCollection {
        &self.event[ty]
    }

    pub fn get_func_opt(&self, name: StrKey) -> Option<&FunctionBody> {
        self.normal.get(&name)
    }

    pub fn get_func(&self, name: StrKey) -> Result<&FunctionBody> {
        self.get_func_opt(name)
            .ok_or_else(|| anyhow!("Function {} is not exists", name.resolve()))
    }
}
