use anyhow::{anyhow, Result};
use arrayvec::ArrayVec;
use enum_map::EnumMap;
use hashbrown::HashMap;
use smartstring::{LazyCompact, SmartString};

use erars_compiler::{
    CompiledFunction, Event, EventFlags, EventType, Expr, FunctionIndex, Instruction, Value,
    VariableDic, VariableIndex,
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Default, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct FunctionBody {
    idx: FunctionIndex,
    body: Vec<Instruction>,
    goto_labels: HashMap<SmartString<LazyCompact>, u32>,
    args: Vec<(VariableIndex, Option<Value>, ArrayVec<usize, 4>)>,
}

impl FunctionBody {
    #[inline]
    pub fn push_arg(
        &mut self,
        var_idx: VariableIndex,
        default_value: Option<Value>,
        indices: ArrayVec<usize, 4>,
    ) {
        self.args.push((var_idx, default_value, indices));
    }

    #[inline]
    pub fn idx(&self) -> FunctionIndex {
        self.idx
    }

    #[inline]
    pub fn goto_labels(&self) -> &HashMap<SmartString<LazyCompact>, u32> {
        &self.goto_labels
    }

    #[inline]
    pub fn args(&self) -> &[(VariableIndex, Option<Value>, ArrayVec<usize, 4>)] {
        &self.args
    }

    #[inline]
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
    normal: HashMap<FunctionIndex, FunctionBody>,
    event: EnumMap<EventType, EventCollection>,
}

impl FunctionDic {
    pub fn new() -> Self {
        Self {
            normal: HashMap::new(),
            event: EnumMap::default(),
        }
    }

    pub fn insert_compiled_func(
        &mut self,
        variable_interner: &VariableDic,
        func: CompiledFunction,
    ) {
        let mut body = FunctionBody {
            body: func.body,
            goto_labels: func.goto_labels,
            ..Default::default()
        };

        let header = func.header;

        for (var, default_value) in header.args {
            body.push_arg(
                var.var_idx,
                default_value.map(|v| match v {
                    Expr::IntLit(i) => i.into(),
                    Expr::StringLit(s) => s.into(),
                    _ => panic!("default arg must be constant"),
                }),
                var.args
                    .into_iter()
                    .map(|v| {
                        if let Expr::IntLit(i) = v {
                            i as usize
                        } else {
                            panic!("Variable index must be constant")
                        }
                    })
                    .collect(),
            );
        }

        let name = variable_interner.resolve_func(func.idx).unwrap();
        if let Ok(ty) = name.parse::<EventType>() {
            self.insert_event(
                Event {
                    ty,
                    flags: header.event_flags,
                },
                body,
            );
        } else {
            self.insert_func(func.idx, body);
        }
    }

    pub fn insert_func(&mut self, idx: FunctionIndex, body: FunctionBody) {
        self.normal.insert(idx, body);
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

    pub fn get_func(&self, idx: FunctionIndex) -> Result<&FunctionBody> {
        self.normal
            .get(&idx)
            .ok_or_else(|| anyhow!("Function {} is not exists", idx))
    }
}
