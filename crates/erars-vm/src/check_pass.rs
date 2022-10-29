use crate::{FunctionBody, FunctionDic};

#[cfg(feature = "multithread")]
use rayon::prelude::*;

fn check_function_exist_inner(
    fn_name: &dyn std::fmt::Display,
    dic: &FunctionDic,
    func: &FunctionBody,
) {
    for (i, inst) in func.body().iter().enumerate() {
        if inst.is_call() {
            // can only check with literal str
            if let Some(name) = func.body()[i - 1].as_load_str() {
                if !dic.normal.contains_key(&name) {
                    log::error!("Find CALL `{name}` in @{fn_name} at file {file_path} but {name} not exists.", file_path = func.file_path);
                }
            }
        }
    }
}

pub fn check_function_exist(dic: &FunctionDic) {
    #[cfg(feature = "multithread")]
    let normal = dic.normal.par_iter();
    #[cfg(not(feature = "multithread"))]
    let normal = dic.normal.iter();

    normal.for_each(|(fn_name, func)| {
        check_function_exist_inner(fn_name, dic, func);
    });

    dic.event.iter().for_each(|(k, v)| {
        v.events.iter().for_each(|func| {
            check_function_exist_inner(&k, dic, func);
        });
    });
}
