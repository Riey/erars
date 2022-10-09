use std::sync::{atomic::AtomicBool, Arc};

use erars_ui::{ConsoleResult, ConsoleSender, InputRequest, InputRequestType, Timeout};
use pyo3::exceptions::PyException;
use pyo3::prelude::*;
use pyo3::types::PyAny;

use crate::{TerminalVm, VmContext, Workflow};
use erars_ast::{BeginType, Value};

#[repr(transparent)]
pub struct ValueWrapper(Value);

fn value_cast(wrapper: &[ValueWrapper]) -> &[Value] {
    // SAFETY: ValueWrapper must be repr(transparent)
    unsafe { std::mem::transmute(wrapper) }
}

pub fn value_to_wrapper_slice(slice: &[Value]) -> &[ValueWrapper] {
    // SAFETY: ValueWrapper must be repr(transparent)
    unsafe { std::mem::transmute(slice) }
}

pub struct InputRequestTypeWrapper(InputRequestType);

#[pyclass]
pub struct VmProxy {
    pub vm: TerminalVm,
    pub tx: ConsoleSender,
    pub ctx: VmContext,
    pub quited: Arc<AtomicBool>,
}

impl VmProxy {
    fn wait_internal(
        &mut self,
        ty: InputRequestType,
        is_one: bool,
        timeout: Option<Timeout>,
    ) -> PyResult<ValueWrapper> {
        let gen = self.tx.input_gen();
        match self.tx.input(InputRequest {
            generation: gen,
            ty,
            is_one,
            timeout,
        }) {
            ConsoleResult::Value(v) => Ok(v.into()),
            ConsoleResult::Quit => {
                self.quited.store(true, std::sync::atomic::Ordering::SeqCst);
                Err(PyErr::new::<pyo3::exceptions::PyConnectionError, _>(
                    "Connection exited",
                ))
            }
        }
    }
}

#[pymethods]
impl VmProxy {
    pub fn print(&mut self, s: String) {
        self.tx.print(s);
    }
    pub fn newline(&mut self) {
        self.tx.new_line();
    }
    pub fn println(&mut self, s: String) {
        self.tx.print_line(s);
    }

    pub fn printw(&mut self, s: String) -> PyResult<()> {
        self.println(s);
        self.wait_internal(InputRequestType::AnyKey, false, None)?;
        Ok(())
    }

    pub fn twait(
        &mut self,
        ty: InputRequestTypeWrapper,
        default_value: ValueWrapper,
        is_one: bool,
        timeout_ms: u32,
        show_timer: bool,
        timeout_msg: Option<String>,
    ) -> PyResult<ValueWrapper> {
        self.wait_internal(
            ty.0,
            is_one,
            Some(Timeout {
                timeout: (time::OffsetDateTime::now_utc()
                    + time::Duration::milliseconds(timeout_ms as _))
                .unix_timestamp_nanos(),
                default_value: default_value.0,
                timeout_msg,
                show_timer,
            }),
        )
    }

    pub fn wait(&mut self, ty: InputRequestTypeWrapper, is_one: bool) -> PyResult<ValueWrapper> {
        self.wait_internal(ty.0, is_one, None)
    }

    pub fn call(&mut self, name: &str, args: Vec<ValueWrapper>) -> PyResult<()> {
        self.vm
            .call(name, value_cast(&args), &mut self.tx, &mut self.ctx)
            .map_err(|err| {
                log::error!("ERROR: {err}");
                pyo3::exceptions::PyException::new_err("Call error")
            })
            .and_then(check_exit(&self.quited))
    }

    pub fn begin(&mut self, begin_ty: u32) -> PyResult<()> {
        self.vm
            .begin(
                match begin_ty {
                    0 => BeginType::Title,
                    1 => BeginType::First,
                    2 => BeginType::Shop,
                    3 => BeginType::Train,
                    4 => BeginType::TurnEnd,
                    5 => BeginType::AfterTrain,
                    _ => BeginType::AblUp,
                },
                &mut self.tx,
                &mut self.ctx,
            )
            .map_err(|err| {
                log::error!("ERROR: {err}");
                pyo3::exceptions::PyException::new_err("Begin error")
            })
            .and_then(check_exit(&self.quited))
    }

    pub fn get_var(&mut self, name: &str, args: Vec<usize>) -> PyResult<ValueWrapper> {
        let (_, var, idx) = self
            .ctx
            .var
            .index_var(name, &args)
            .map_err(|err| PyException::new_err(err.to_string()))?;
        var.get(idx).map(ValueWrapper).map_err(|err| {
            log::error!("ERROR: {err}");
            pyo3::exceptions::PyException::new_err("Variable error")
        })
    }

    pub fn set_var(&mut self, name: &str, value: ValueWrapper, args: Vec<usize>) -> PyResult<()> {
        let (_, var, idx) = self
            .ctx
            .var
            .index_var(name, &args)
            .map_err(|err| PyException::new_err(err.to_string()))?;
        var.set(idx, value.0).map_err(|err| {
            log::error!("ERROR: {err}");
            pyo3::exceptions::PyException::new_err("Variable error")
        })
    }

    pub fn begin_type(&mut self) -> i32 {
        self.ctx.begin.map_or(-1, |ty| ty as i32)
    }
}

#[pymodule]
#[pyo3(name = "erars_internal")]
pub fn module(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_class::<VmProxy>()?;
    Ok(())
}

fn check_exit(exited: &Arc<AtomicBool>) -> impl Fn(Workflow) -> PyResult<()> + '_ {
    |workflow| match workflow {
        Workflow::Exit => {
            exited.store(true, std::sync::atomic::Ordering::SeqCst);
            Err(pyo3::exceptions::PyException::new_err("Program exited"))
        }
        Workflow::Return => Ok(()),
    }
}

mod impls {
    use super::*;

    impl<'source> FromPyObject<'source> for Workflow {
        fn extract(ob: &'source PyAny) -> PyResult<Self> {
            if bool::extract(ob)? {
                Ok(Workflow::Exit)
            } else {
                Ok(Workflow::Return)
            }
        }
    }

    impl IntoPy<PyObject> for Workflow {
        fn into_py(self, py: Python<'_>) -> PyObject {
            matches!(self, Workflow::Exit).into_py(py)
        }
    }

    impl From<Value> for ValueWrapper {
        fn from(v: Value) -> Self {
            Self(v)
        }
    }

    impl<'a> IntoPy<PyObject> for &'a ValueWrapper {
        fn into_py(self, py: Python<'_>) -> PyObject {
            match &self.0 {
                Value::Int(i) => i.into_py(py),
                Value::String(s) => s.into_py(py),
            }
        }
    }

    impl IntoPy<PyObject> for ValueWrapper {
        fn into_py(self, py: Python<'_>) -> PyObject {
            match self.0 {
                Value::Int(i) => i.into_py(py),
                Value::String(s) => s.into_py(py),
            }
        }
    }

    impl<'source> FromPyObject<'source> for ValueWrapper {
        fn extract(ob: &'source PyAny) -> PyResult<Self> {
            if let Ok(s) = String::extract(ob) {
                Ok(Self(Value::String(s)))
            } else {
                Ok(Self(Value::Int(i64::extract(ob)?)))
            }
        }
    }

    impl IntoPy<PyObject> for InputRequestTypeWrapper {
        fn into_py(self, py: Python<'_>) -> PyObject {
            (self.0 as u32).into_py(py)
        }
    }

    impl<'source> FromPyObject<'source> for InputRequestTypeWrapper {
        fn extract(ob: &'source PyAny) -> PyResult<Self> {
            Ok(Self(match u32::extract(ob)? {
                3 => InputRequestType::Str,
                2 => InputRequestType::Int,
                1 => InputRequestType::EnterKey,
                0 => InputRequestType::AnyKey,
                _ => {
                    return Err(PyErr::new::<pyo3::exceptions::PyException, _>(
                        "Invalid request type",
                    ))
                }
            }))
        }
    }
}
