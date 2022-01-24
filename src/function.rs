use anyhow::{anyhow, Result};
use enum_map::EnumMap;
use hashbrown::HashMap;

use crate::{
    event::{Event, EventFlags, EventType},
    instruction::Instruction,
};
use serde::{Deserialize, Serialize};

type FunctionBody = Vec<Instruction>;

#[derive(Clone, Default, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct EventCollection {
    single: Option<FunctionBody>,
    pre: Vec<FunctionBody>,
    empty: Vec<FunctionBody>,
    later: Vec<FunctionBody>,
}

impl EventCollection {
    pub fn run(&self, mut f: impl FnMut(&[Instruction]) -> Result<()>) -> Result<()> {
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

    pub fn get_func(&self, name: &str) -> Result<&[Instruction]> {
        self.normal
            .get(name)
            .map(|b| &**b)
            .ok_or_else(|| anyhow!("Function {} is not exists", name))
    }
}
