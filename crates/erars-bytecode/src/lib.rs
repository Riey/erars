use rkyv::{ser::Serializer, Archive, with::With};
use erars_vm::FunctionDic;

use erars_ast::{get_interner, update_interner};

struct FunctionDicWrapper<'s> {
    dic: &'s FunctionDic,
}

pub fn write_to<S: Serializer>(mut out: S, dic: &FunctionDic) {
    out.

}
