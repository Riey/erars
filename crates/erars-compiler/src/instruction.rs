use std::fmt::Debug;

use erars_ast::{
    Alignment, BeginType, BinaryOperator, BuiltinCommand, BuiltinMethod, BuiltinVariable,
    EventType, NotNan, PrintFlags, ScriptPosition, StrKey, UnaryOperator,
};
use paste::paste;
use std::mem::transmute;

macro_rules! define_instruction {
    (@empty $(($name:ident, $ty:ident),)*) => {
        paste! {
            impl Instruction {
                $(
                    #[inline]
                    pub const fn $name() -> Self {
                        Self {
                            ty: InstructionType::$ty,
                            data: 0,
                        }
                    }

                    #[inline]
                    pub const fn [<is_ $name>](self) -> bool {
                        match self.ty {
                            InstructionType::$ty => true,
                            _ => false,
                        }
                    }
                )*
            }
        }
    };
    ($data_ty:ty, $(($name:ident, $ty:ident),)*) => {
        static_assertions::assert_impl_all!($data_ty: Copy);
        static_assertions::assert_eq_size!($data_ty, u32);
        static_assertions::assert_eq_align!($data_ty, u32);

        paste! {
            impl Instruction {
                $(
                    #[inline]
                    pub const fn $name(n: $data_ty) -> Self {
                        Self {
                            ty: InstructionType::$ty,
                            data: unsafe { transmute(n) },
                        }
                    }

                    #[inline]
                    pub const fn [<is_ $name>](self) -> bool {
                        match self.ty {
                            InstructionType::$ty => true,
                            _ => false,
                        }
                    }

                    #[inline]
                    pub const fn [<as_ $name>](self) -> Option<$data_ty> {
                        if self.[<is_ $name>]() {
                            Some(
                                unsafe { transmute(self.data) }
                            )
                        } else {
                            None
                        }
                    }
                )*
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u32)]
enum InstructionType {
    Nop = 0,
    Pop = 1,
    ReportPosition = 2,
    Duplicate = 3,
    DuplicatePrev = 4,
    LoadInt = 5,
    LoadIntSuffix = 6,
    LoadStr = 9,
    EvalFormString = 10,
    PadStr = 11,
    ConcatString = 12,

    LoadVarRef = 20,
    LoadExternVarRef = 21,
    LoadCountVarRef = 22,
    ReadVar = 23,
    StoreVar = 24,
    StoreResult = 25,

    Call = 30,
    TryCall = 31,
    Jump = 32,
    TryJump = 33,
    Begin = 34,
    CallEvent = 35,
    GotoLabel = 36,
    TryGotoLabel = 37,
    Goto = 38,
    GotoIfNot = 39,
    GotoIf = 40,

    Print = 50,
    ReuseLastLine = 51,
    BuiltinVar = 52,
    BuiltinCommand = 53,
    BuiltinMethod = 54,
    BinaryOperator = 55,
    UnaryOperator = 56,
    SetAlignment = 57,
    Times = 58,
}

static_assertions::assert_eq_size!(Instruction, (u32, u32));
static_assertions::assert_eq_align!(Instruction, (u32, u32));

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Instruction {
    ty: InstructionType,
    data: u32,
}

impl Instruction {
    #[inline]
    pub const fn raw_data(self) -> u32 {
        self.data
    }
}

define_instruction! {
    @empty
    (nop, Nop),
    (pop, Pop),
    (duplicate, Duplicate),
    (duplicate_prev, DuplicatePrev),
    (eval_form_string, EvalFormString),
    (load_count_var_ref, LoadCountVarRef),
    (read_var, ReadVar),
    (store_var, StoreVar),
    (store_result, StoreResult),
    (reuse_lastline, ReuseLastLine),
    (goto_label, GotoLabel),
    (try_goto_label, TryGotoLabel),
}

define_instruction! {
    i32,
    (load_int, LoadInt),
    (load_int_suffix, LoadIntSuffix),
}

define_instruction! {
    u32,
    (load_var_ref, LoadVarRef),
    (load_extern_varref, LoadExternVarRef),
    (call, Call),
    (try_call, TryCall),
    (jump, Jump),
    (try_jump, TryJump),
    (concat_string, ConcatString),
    (goto, Goto),
    (goto_if_not, GotoIfNot),
    (goto_if, GotoIf),
}

define_instruction! {
    StrKey,
    (load_str, LoadStr),
}

define_instruction! {
    PrintFlags,
    (print, Print),
}

define_instruction! {
    EventType,
    (call_event, CallEvent),
}

define_instruction! {
    BeginType,
    (begin, Begin),
}

define_instruction! {
    BuiltinVariable,
    (builtin_var, BuiltinVar),
}

define_instruction! {
    BuiltinCommand,
    (builtin_command, BuiltinCommand),
}

define_instruction! {
    BuiltinMethod,
    (builtin_method, BuiltinMethod),
}

define_instruction! {
    BinaryOperator,
    (binop, BinaryOperator),
}

define_instruction! {
    UnaryOperator,
    (unaryop, UnaryOperator),
}

define_instruction! {
    Alignment,
    (set_aligment, SetAlignment),
    (pad_str, PadStr),
}

define_instruction! {
    NotNan<f32>,
    (times, Times),
}

define_instruction! {
    ScriptPosition,
    (report_position, ReportPosition),
}
