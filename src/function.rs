use anyhow::{anyhow, Result};
use arrayvec::ArrayVec;
use enum_map::EnumMap;
use hashbrown::HashMap;
use smol_str::SmolStr;

use erars_ast::{Event, EventFlags, EventType, Expr, FunctionInfo, Value, VariableInfo};
use erars_compiler::{CompiledFunction, Instruction};
use serde::{Deserialize, Serialize};

use crate::vm::VariableStorage;
use crate::vm::Workflow;

#[derive(Clone, Default, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct FunctionBody {
    file_path: SmolStr,
    body: Box<[Instruction]>,
    goto_labels: HashMap<SmolStr, u32>,
    args: Vec<(Box<str>, Option<Value>, ArrayVec<usize, 4>)>,
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

    pub fn goto_labels(&self) -> &HashMap<SmolStr, u32> {
        &self.goto_labels
    }

    pub fn args(&self) -> &[(Box<str>, Option<Value>, ArrayVec<usize, 4>)] {
        &self.args
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
    pub fn run(&self, mut f: impl FnMut(&FunctionBody) -> Result<Workflow>) -> Result<Workflow> {
        if let Some(single) = self.single.as_ref() {
            return f(single);
        } else {
            for pre in self.pre.iter() {
                if f(pre)? == Workflow::Exit {
                    return Ok(Workflow::Exit);
                }
            }

            for empty in self.empty.iter() {
                if f(empty)? == Workflow::Exit {
                    return Ok(Workflow::Exit);
                }
            }

            for later in self.later.iter() {
                if f(later)? == Workflow::Exit {
                    return Ok(Workflow::Exit);
                }
            }
        }

        Ok(Workflow::Return)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct FunctionDic {
    normal: HashMap<SmolStr, FunctionBody>,
    event: EnumMap<EventType, EventCollection>,
}

impl FunctionDic {
    pub fn new() -> Self {
        Self {
            normal: HashMap::new(),
            event: EnumMap::default(),
        }
    }

    pub fn insert_compiled_func(&mut self, var_dic: &mut VariableStorage, func: CompiledFunction) {
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
                default_value,
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
                    var_dic.add_local_info(header.name.clone(), local.var, local.info);
                }
            }
        }

        // builtin locals

        static LOCAL: SmolStr = SmolStr::new_inline("LOCAL");
        static LOCALS: SmolStr = SmolStr::new_inline("LOCALS");
        static ARG: SmolStr = SmolStr::new_inline("ARG");
        static ARGS: SmolStr = SmolStr::new_inline("ARGS");

        var_dic.add_local_info(
            header.name.clone(),
            LOCAL.clone(),
            VariableInfo {
                size: vec![local_size],
                ..Default::default()
            },
        );

        var_dic.add_local_info(
            header.name.clone(),
            LOCALS.clone(),
            VariableInfo {
                is_str: true,
                size: vec![locals_size],
                ..Default::default()
            },
        );

        var_dic.add_local_info(
            header.name.clone(),
            ARG.clone(),
            VariableInfo {
                size: vec![1000],
                ..Default::default()
            },
        );

        var_dic.add_local_info(
            header.name.clone(),
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

    pub fn insert_func(&mut self, name: SmolStr, body: FunctionBody) {
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

    pub fn get_func_opt(&self, name: &str) -> Option<&FunctionBody> {
        self.normal.get(name)
    }

    pub fn get_func(&self, name: &str) -> Result<&FunctionBody> {
        self.get_func_opt(name)
            .ok_or_else(|| anyhow!("Function {} is not exists", name))
    }
}
