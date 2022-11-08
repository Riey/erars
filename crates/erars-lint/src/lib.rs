use codespan_reporting::{
    diagnostic::Label,
    files::{self, Files as _},
};
use erars_vm::{FunctionBody, FunctionDic, VariableStorage};

use erars_ast::StrKey;
use hashbrown::HashMap;
use parking_lot::Mutex;
#[cfg(feature = "multithread")]
use rayon::prelude::*;

type Diagnostic = codespan_reporting::diagnostic::Diagnostic<StrKey>;

pub struct ErarsFiles {
    files: HashMap<StrKey, files::SimpleFile<StrKey, String>>,
}

impl ErarsFiles {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
        }
    }

    fn add_from_path(&mut self, path: StrKey) {
        self.files.entry(path).or_insert_with(|| {
            files::SimpleFile::new(
                path,
                erars_reader::read_file(std::path::Path::new(path.resolve())).unwrap(),
            )
        });
    }

    pub fn add(&mut self, id: StrKey, source: String) -> StrKey {
        self.files.insert(id, files::SimpleFile::new(id, source));
        id
    }

    pub fn get(&self, id: StrKey) -> Result<&files::SimpleFile<StrKey, String>, files::Error> {
        self.files.get(&id).ok_or(files::Error::FileMissing)
    }
}

impl<'a> files::Files<'a> for ErarsFiles {
    type FileId = StrKey;

    type Name = StrKey;

    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, files::Error> {
        Ok(id)
    }

    fn source(
        &'a self,
        id: Self::FileId,
    ) -> Result<Self::Source, codespan_reporting::files::Error> {
        Ok(self.get(id)?.source())
    }

    fn line_index(
        &'a self,
        id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        Ok(self.get(id)?.line_index((), byte_index)?)
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, codespan_reporting::files::Error> {
        Ok(self.get(id)?.line_range((), line_index)?)
    }
}

// TODO: const assign pass

fn check_function_exist_inner(
    fn_name: StrKey,
    func: &FunctionBody,
    dic: &FunctionDic,
    files: &Mutex<&mut ErarsFiles>,
    diagnostics: &Mutex<Vec<Diagnostic>>,
) {
    let mut current_line = 0;
    for (i, inst) in func.body().iter().enumerate() {
        if let Some(pos) = inst.as_report_position() {
            current_line = pos.line;
        } else if inst.is_call() {
            // can only check with literal str
            if let Some(name) = func.body()[i - 1].as_load_str() {
                if !dic.normal.contains_key(&name) {
                    let mut files = files.lock();
                    let file_id = func.file_path();
                    files.add_from_path(file_id);
                    let diagnostic = Diagnostic::error()
                        .with_code("E1000")
                        .with_notes(vec![format!("In function @{fn_name}")])
                        .with_message(format!("Find CALL `{name}` but @{name} is not exists."))
                        .with_labels(vec![Label::primary(
                            file_id,
                            files.line_range(file_id, current_line as usize).unwrap(),
                        )]);
                    diagnostics.lock().push(diagnostic);
                }
            }
        }
    }
}

fn check_variable_exist_inner(
    fn_name: StrKey,
    func: &FunctionBody,
    var: &VariableStorage,
    files: &Mutex<&mut ErarsFiles>,
    diagnostics: &Mutex<Vec<Diagnostic>>,
) {
    let mut current_line = 1;
    for (i, inst) in func.body().iter().enumerate() {
        if let Some(pos) = inst.as_report_position() {
            current_line = pos.line;
        } else {
            let (current_fn_name, name) = if inst.is_load_var_ref() {
                // can only check with literal str
                let Some(name) = func.body()[i - 1].as_load_str() else {
                    continue;
                };
                (fn_name, name)
            } else if inst.is_load_extern_varref() {
                // can only check with literal str
                let Some(name) = func.body()[i - 2].as_load_str() else {
                    continue;
                };
                let Some(ex_fn_name) = func.body()[i - 1].as_load_str() else {
                    continue;
                };
                (ex_fn_name, name)
            } else {
                continue;
            };

            if !var.check_var_exists(current_fn_name, name) {
                let msg = if current_fn_name == fn_name {
                    // skip fn_name if current function's local var
                    format!("Find LOADVAR `{name}` but `{name}` not exists.")
                } else {
                    format!("Find LOADEXVAR `{fn_name}@{name}` but `{fn_name}@{name}` not exists.")
                };

                let mut files = files.lock();
                let file_id = func.file_path();
                files.add_from_path(file_id);
                let mut diagnostic = Diagnostic::error()
                    .with_code("E1001")
                    .with_notes(vec![format!("In function @{fn_name}")])
                    .with_message(msg)
                    .with_labels(vec![Label::primary(
                        file_id,
                        files.line_range(file_id, current_line as usize).unwrap(),
                    )]);

                let name = name.resolve();
                if name.chars().any(|c| c.is_ascii_lowercase()) {
                    let upper = name.to_ascii_uppercase();

                    if var.check_var_exists(fn_name, var.interner().get_or_intern(&upper)) {
                        diagnostic.notes.push(format!("Did you mean `{upper}`?"));
                    }
                }

                diagnostics.lock().push(diagnostic);
            }
        }
    }
}

pub fn check_function(
    dic: &FunctionDic,
    var: &VariableStorage,
    files: &mut ErarsFiles,
) -> Vec<Diagnostic> {
    #[cfg(feature = "multithread")]
    let normal = dic.normal.par_iter();
    #[cfg(not(feature = "multithread"))]
    let normal = dic.normal.iter();

    let files = Mutex::new(files);
    let diagnostic = Mutex::new(Vec::new());

    normal.for_each(|(fn_name, func)| {
        check_function_exist_inner(*fn_name, func, dic, &files, &diagnostic);
        check_variable_exist_inner(*fn_name, func, var, &files, &diagnostic);
    });

    dic.event.iter().for_each(|(k, v)| {
        let fn_name = var.interner().get_or_intern(<&str>::from(k));
        v.events.iter().for_each(|func| {
            check_function_exist_inner(fn_name, func, dic, &files, &diagnostic);
            check_variable_exist_inner(fn_name, func, var, &files, &diagnostic);
        });
    });

    diagnostic.into_inner()
}
