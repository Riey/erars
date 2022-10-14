use anyhow::{anyhow, Result};
use arrayvec::ArrayVec;
use enum_map::EnumMap;
use erars_ast::Interner;
use erars_ast::StrKey;
use erars_ast::GLOBAL_INTERNER;
use erars_compiler::DefaultLocalVarSize;
use hashbrown::HashMap;
use smol_str::SmolStr;

use erars_ast::{Event, EventFlags, EventType, Expr, FunctionInfo, Value, VariableInfo};
use erars_compiler::{CompiledFunction, Instruction};

use crate::variable::KnownVariableNames;
use crate::VariableStorage;
use crate::Workflow;

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct FunctionBody {
    pub file_path: SmolStr,
    pub is_function: bool,
    pub is_functions: bool,
    pub goto_labels: HashMap<StrKey, u32>,
    pub args: Vec<(StrKey, Option<Value>, ArrayVec<usize, 4>)>,
    pub body: Box<[Instruction]>,
}

impl FunctionBody {
    pub fn push_arg(
        &mut self,
        var: StrKey,
        default_value: Option<Value>,
        indices: ArrayVec<usize, 4>,
    ) {
        self.args.push((var, default_value, indices));
    }

    pub fn goto_labels(&self) -> &HashMap<StrKey, u32> {
        &self.goto_labels
    }

    pub fn args(&self) -> &[(StrKey, Option<Value>, ArrayVec<usize, 4>)] {
        &self.args
    }

    pub fn body(&self) -> &[Instruction] {
        &self.body
    }

    pub fn file_path(&self) -> &SmolStr {
        &self.file_path
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
    pub single: Option<FunctionBody>,
    pub events: Vec<FunctionBody>,
    pub empty_count: usize,
}

impl EventCollection {
    pub fn run(
        &self,
        from: usize,
        mut f: impl FnMut(&FunctionBody, usize) -> Result<Workflow>,
    ) -> Result<Workflow> {
        if let Some(single) = self.single.as_ref() {
            if from == 0 {
                return f(single, 0);
            }
        } else {
            for (i, pre) in self.events[from..].iter().enumerate() {
                match f(pre, i)? {
                    Workflow::Return => {}
                    other => return Ok(other),
                }
            }
        }

        Ok(Workflow::Return)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionDic {
    pub interner: &'static Interner,
    pub normal: HashMap<StrKey, FunctionBody>,
    pub event: EnumMap<EventType, EventCollection>,
}

impl FunctionDic {
    pub fn new() -> Self {
        Self {
            interner: &*GLOBAL_INTERNER,
            normal: HashMap::new(),
            event: EnumMap::default(),
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
        let mut local_size = default_var_size.default_local_size;
        let mut locals_size = default_var_size.default_locals_size;

        for info in header.infos {
            match info {
                FunctionInfo::LocalSize(size) => {
                    local_size = Some(size);
                }
                FunctionInfo::LocalSSize(size) => {
                    locals_size = Some(size);
                }
                FunctionInfo::EventFlag(f) => {
                    flags = f;
                }
                FunctionInfo::Function => {
                    body.is_function = true;
                    assert!(!body.is_functions);
                }
                FunctionInfo::FunctionS => {
                    body.is_functions = true;
                    assert!(!body.is_function);
                }
                FunctionInfo::Dim(local) => {
                    var_dic.add_local_info(header.name, local.var, local.info);
                }
            }
        }

        // builtin locals

        let local = var_dic.known_key(KnownVariableNames::Local);
        let locals = var_dic.known_key(KnownVariableNames::LocalS);
        let arg = var_dic.known_key(KnownVariableNames::Arg);
        let args = var_dic.known_key(KnownVariableNames::ArgS);

        if let Some(local_size) = local_size {
            var_dic.add_local_info(
                header.name.clone(),
                local,
                VariableInfo {
                    size: vec![local_size],
                    ..Default::default()
                },
            );
        }

        if let Some(locals_size) = locals_size {
            var_dic.add_local_info(
                header.name.clone(),
                locals,
                VariableInfo {
                    is_str: true,
                    size: vec![locals_size],
                    ..Default::default()
                },
            );
        }

        if let Some(arg_size) = default_var_size.default_arg_size {
            var_dic.add_local_info(
                header.name.clone(),
                arg,
                VariableInfo {
                    size: vec![arg_size],
                    ..Default::default()
                },
            );
        }

        if let Some(args_size) = default_var_size.default_args_size {
            var_dic.add_local_info(
                header.name.clone(),
                args,
                VariableInfo {
                    is_str: true,
                    size: vec![args_size],
                    ..Default::default()
                },
            );
        }

        if let Ok(ty) = self.interner.resolve(&header.name).parse::<EventType>() {
            self.insert_event(Event { ty, flags }, body);
        } else {
            self.insert_func(header.name, body);
        }
    }

    pub fn insert_func(&mut self, name: StrKey, body: FunctionBody) {
        self.normal.insert(name, body);
    }

    pub fn insert_event(&mut self, event: Event, body: FunctionBody) {
        let mut collection = &mut self.event[event.ty];
        match event.flags {
            EventFlags::Single => collection.single = Some(body),
            EventFlags::Later => {
                collection.events.push(body);
            }
            EventFlags::Pre => {
                collection.events.insert(collection.empty_count, body);
            }
            EventFlags::None => {
                collection.events.insert(0, body);
                collection.empty_count += 1;
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
            .ok_or_else(|| anyhow!("Function {} is not exists", self.interner.resolve(&name)))
    }
}
