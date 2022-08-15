use anyhow::{anyhow, Result};
use arrayvec::ArrayVec;
use enum_map::EnumMap;
use hashbrown::HashMap;
use smol_str::SmolStr;

use erars_ast::{Event, EventFlags, EventType, Expr, FunctionInfo, Value, VariableInfo};
use erars_compiler::{CompiledFunction, Instruction};
use serde::{Deserialize, Serialize};

#[derive(Clone, Default, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct FunctionBody {
    file_path: SmolStr,
    body: Vec<Instruction>,
    goto_labels: HashMap<SmolStr, u32>,
    args: Vec<(Box<str>, Option<Value>, ArrayVec<usize, 4>)>,
    local_vars: HashMap<SmolStr, VariableInfo>,
}

impl FunctionBody {
    pub fn push_arg(
        &mut self,
        var: Box<str>,
        default_value: Option<Value>,
        indices: ArrayVec<usize, 4>,
    ) {
        self.args.push((var, default_value, indices));
    }

    pub fn push_local(&mut self, var: SmolStr, info: VariableInfo) {
        self.local_vars.insert(var, info);
    }

    pub fn goto_labels(&self) -> &HashMap<SmolStr, u32> {
        &self.goto_labels
    }

    pub fn args(&self) -> &[(Box<str>, Option<Value>, ArrayVec<usize, 4>)] {
        &self.args
    }

    pub fn local_vars(&self) -> &HashMap<SmolStr, VariableInfo> {
        &self.local_vars
    }

    pub fn body(&self) -> &[Instruction] {
        &self.body
    }

    pub fn file_path(&self) -> &SmolStr {
        &self.file_path
    }
}

#[derive(Clone, Default, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct EventCollection {
    single: Option<FunctionBody>,
    pre: Vec<FunctionBody>,
    empty: Vec<FunctionBody>,
    later: Vec<FunctionBody>,
}

impl EventCollection {
    pub fn run(&self, mut f: impl FnMut(&FunctionBody) -> Result<()>) -> Result<()> {
        if let Some(single) = self.single.as_ref() {
            f(single)?;
        } else {
            for pre in self.pre.iter() {
                f(pre)?;
            }

            for empty in self.empty.iter() {
                f(empty)?;
            }

            for later in self.later.iter() {
                f(later)?;
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct FunctionDic {
    normal: HashMap<String, FunctionBody>,
    event: EnumMap<EventType, EventCollection>,
}

impl FunctionDic {
    pub fn new() -> Self {
        Self {
            normal: HashMap::new(),
            event: EnumMap::default(),
        }
    }

    pub fn insert_compiled_func(&mut self, func: CompiledFunction) {
        let mut body = FunctionBody {
            body: func.body,
            goto_labels: func.goto_labels,
            ..Default::default()
        };

        let header = func.header;

        body.file_path = header.file_path;

        for (var, default_value) in header.args {
            body.push_arg(
                var.var,
                default_value.map(|v| match v {
                    Expr::Int(i) => i.into(),
                    Expr::String(s) => s.into_string().into(),
                    _ => panic!("default arg must be constant"),
                }),
                var.args
                    .into_iter()
                    .map(|v| {
                        if let Expr::Int(i) = v {
                            i as usize
                        } else {
                            panic!("Variable index must be constant")
                        }
                    })
                    .collect(),
            );
        }

        let mut flags = EventFlags::None;
        let mut local_size = 1000;
        let mut locals_size = 100;

        for info in header.infos {
            match info {
                FunctionInfo::LocalSize(size) => {
                    local_size = size;
                }
                FunctionInfo::LocalSSize(size) => {
                    locals_size = size;
                }
                FunctionInfo::EventFlag(f) => {
                    flags = f;
                }
                FunctionInfo::Function | FunctionInfo::FunctionS => {}
                FunctionInfo::Dim(local) => {
                    body.push_local(local.var, local.info);
                }
            }
        }

        // builtin locals

        static LOCAL: SmolStr = SmolStr::new_inline("LOCAL");
        static LOCALS: SmolStr = SmolStr::new_inline("LOCALS");
        static ARG: SmolStr = SmolStr::new_inline("ARG");
        static ARGS: SmolStr = SmolStr::new_inline("ARGS");

        body.push_local(
            LOCAL.clone(),
            VariableInfo {
                size: vec![local_size],
                ..Default::default()
            },
        );

        body.push_local(
            LOCALS.clone(),
            VariableInfo {
                is_str: true,
                size: vec![locals_size],
                ..Default::default()
            },
        );

        body.push_local(
            ARG.clone(),
            VariableInfo {
                size: vec![1000],
                ..Default::default()
            },
        );

        body.push_local(
            ARGS.clone(),
            VariableInfo {
                is_str: true,
                size: vec![100],
                ..Default::default()
            },
        );

        if let Ok(ty) = header.name.parse::<EventType>() {
            self.insert_event(Event { ty, flags }, body);
        } else {
            self.insert_func(header.name, body);
        }
    }

    pub fn insert_func(&mut self, name: String, body: FunctionBody) {
        self.normal.insert(name, body);
    }

    pub fn insert_event(&mut self, event: Event, body: FunctionBody) {
        let mut collection = &mut self.event[event.ty];
        match event.flags {
            EventFlags::Later => collection.later.push(body),
            EventFlags::Pre => collection.pre.push(body),
            EventFlags::None => collection.empty.push(body),
            EventFlags::Single => collection.single = Some(body),
        }
    }

    pub fn get_event(&self, ty: EventType) -> &EventCollection {
        &self.event[ty]
    }

    pub fn get_func(&self, name: &str) -> Result<&FunctionBody> {
        self.normal
            .get(name)
            .ok_or_else(|| anyhow!("Function {} is not exists", name))
    }
}
