use std::io::Write;

use rkyv::ser::Serializer;
use erars_vm::FunctionDic;

pub fn write_to<S: Serializer>(mut out: S, dic: FunctionDic) {
    
}
