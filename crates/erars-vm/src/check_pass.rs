use crate::{FunctionBody, FunctionDic, VariableStorage};

use erars_ast::StrKey;
#[cfg(feature = "multithread")]
use rayon::prelude::*;

fn check_function_exist_inner(fn_name: StrKey, func: &FunctionBody, dic: &FunctionDic) {
    let mut current_line = 1;
    for (i, inst) in func.body().iter().enumerate() {
        if let Some(pos) = inst.as_report_position() {
            current_line = pos.line;
        } else if inst.is_call() {
            // can only check with literal str
            if let Some(name) = func.body()[i - 1].as_load_str() {
                if !dic.normal.contains_key(&name) {
                    log::error!("Find CALL `{name}` in @{fn_name}:{current_line} at file {file_path} but {name} not exists.", file_path = func.file_path);
                }
            }
        }
    }
}

fn check_variable_exist_inner(fn_name: StrKey, func: &FunctionBody, var: &VariableStorage) {
    let mut current_line = 1;
    for (i, inst) in func.body().iter().enumerate() {
        if let Some(pos) = inst.as_report_position() {
            current_line = pos.line;
        } else if inst.is_load_var_ref() {
            // can only check with literal str
            if let Some(name) = func.body()[i - 1].as_load_str() {
                if !var.check_var_exists(fn_name, name) {
                    // dbg!(&func.body()[i-4..i+1]);
                    log::error!("Find LOADVAR `{name}` in @{fn_name}:{current_line} at file {file_path} but {name} not exists.", file_path = func.file_path);
                }
            }
        } else if inst.is_load_extern_varref() {
            // can only check with literal str
            if let Some(name) = func.body()[i - 2].as_load_str() {
                if let Some(ex_fn_name) = func.body()[i - 1].as_load_str() {
                    if !var.check_var_exists(ex_fn_name, name) {
                        // dbg!(&func.body()[i-4..i+1]);
                        log::error!("Find LOADEXVAR `{ex_fn_name}@{name}` in @{fn_name}:{current_line} at file {file_path} but {name} not exists.", file_path = func.file_path);
                    }
                }
            }
        }
    }
}

pub fn check_function(dic: &FunctionDic, var: &VariableStorage) {
    #[cfg(feature = "multithread")]
    let normal = dic.normal.par_iter();
    #[cfg(not(feature = "multithread"))]
    let normal = dic.normal.iter();

    normal.for_each(|(fn_name, func)| {
        check_function_exist_inner(*fn_name, func, dic);
        check_variable_exist_inner(*fn_name, func, var);
    });

    dic.event.iter().for_each(|(k, v)| {
        let fn_name = var.interner().get_or_intern(<&str>::from(k));
        v.events.iter().for_each(|func| {
            check_function_exist_inner(fn_name, func, dic);
            check_variable_exist_inner(fn_name, func, var);
        });
    });
}
