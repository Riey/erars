use std::fmt;

use erars_ast::{
    Alignment, BeginType, BinaryOperator, BuiltinCommand, BuiltinMethod, BuiltinVariable,
    EventType, NotNan, PrintFlags, ScriptPosition, StrKey, UnaryOperator,
};
use paste::paste;
use std::mem::transmute;

macro_rules! define_instruction {
    (
        @empty $(($empty_name:ident, $empty_ty:ident),)*
        $(@$data_ty:ty, $(($name:ident, $ty:ident),)*)*
    ) => {
        $(
            static_assertions::assert_impl_all!($data_ty: Copy);
            static_assertions::assert_eq_size!($data_ty, u32);
        )*
        paste! {
            impl Instruction {
                $(
                    #[inline]
                    pub const fn $empty_name() -> Self {
                        Self {
                            ty: InstructionType::$empty_ty,
                            data: [0; 4],
                        }
                    }

                    #[inline]
                    pub const fn [<is_ $empty_name>](self) -> bool {
                        match self.ty {
                            InstructionType::$empty_ty => true,
                            _ => false,
                        }
                    }
                )*
                $(
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
                )*
            }

            impl fmt::Debug for Instruction {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    match self.ty {
                        $(
                            InstructionType::$empty_ty => self.ty.fmt(f),
                        )*
                        $(
                            $(
                                InstructionType::$ty => write!(f, "{}({:?})", self.ty, unsafe { transmute::<_, $data_ty>(self.data) }),
                            )*
                        )*
                    }
                }
            }

            impl fmt::Display for Instruction {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    fmt::Debug::fmt(self, f)
                }
            }
        }
    };
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, strum::Display)]
#[repr(u8)]
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
    Times = 13,

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
    PrintButton = 51,
    ReuseLastLine = 52,
    BuiltinVar = 53,
    BuiltinCommand = 54,
    BuiltinMethod = 55,
    BinaryOperator = 56,
    UnaryOperator = 57,
    SetAlignment = 58,

    LoadDefaultArgument = 100,

    Debug = 255,
}

static_assertions::assert_eq_size!(Instruction, [u8; 5]);
static_assertions::assert_eq_align!(Instruction, u8);

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Instruction {
    ty: InstructionType,
    data: [u8; 4],
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

    @i32,
    (load_int, LoadInt),
    (load_int_suffix, LoadIntSuffix),

    @u32,
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
    (load_default_argument, LoadDefaultArgument),

    @StrKey,
    (load_str, LoadStr),
    (debug, Debug),

    @PrintFlags,
    (print, Print),
    (print_button, PrintButton),

    @EventType,
    (call_event, CallEvent),

    @BeginType,
    (begin, Begin),

    @BuiltinVariable,
    (builtin_var, BuiltinVar),

    @BuiltinCommand,
    (builtin_command, BuiltinCommand),

    @BuiltinMethod,
    (builtin_method, BuiltinMethod),

    @BinaryOperator,
    (binop, BinaryOperator),

    @UnaryOperator,
    (unaryop, UnaryOperator),

    @Alignment,
    (set_aligment, SetAlignment),
    (pad_str, PadStr),

    @NotNan<f32>,
    (times, Times),

    @ScriptPosition,
    (report_position, ReportPosition),
}
