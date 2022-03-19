use anyhow::{anyhow, Result};
use arrayvec::ArrayVec;
use enum_map::EnumMap;
use hashbrown::HashMap;

use crate::value::Value;
use erars_compiler::{CompiledFunction, Event, EventFlags, EventType, FunctionInfo, Instruction};
use serde::{Deserialize, Serialize};

#[derive(Clone, Default, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct FunctionBody {
    local_size: usize,
    locals_size: usize,
    default_arg: Option<Value>,
    body: Vec<Instruction>,
    args: Vec<(String, ArrayVec<usize, 4>)>,
}

impl FunctionBody {
    pub fn new(local_size: usize, locals_size: usize, body: Vec<Instruction>) -> Self {
        Self {
            local_size,
            locals_size,
            body,
            default_arg: None,
            args: Vec::new(),
        }
    }

    pub fn push_arg(&mut self, name: impl Into<String>, indices: ArrayVec<usize, 4>) {
        self.args.push((name.into(), indices));
    }

    pub fn set_default_arg(&mut self, value: Value) {
        self.default_arg = Some(value);
    }

    pub fn args(&self) -> &[(String, ArrayVec<usize, 4>)] {
        &self.args
    }

    pub fn local_size(&self) -> usize {
        self.local_size
    }

    pub fn locals_size(&self) -> usize {
        self.locals_size
    }

    pub fn body(&self) -> &[Instruction] {
        &self.body
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
        let body = FunctionBody {
            body: func.body,
            ..Default::default()
        };
        let header = func.header;
        let mut flags = EventFlags::None;

        for info in header.infos {
            match info {
                FunctionInfo::EventFlag(f) => {
                    flags = f;
                }
            }
        }

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
