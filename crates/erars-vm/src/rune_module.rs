use std::sync::{atomic::AtomicBool, Arc};

use erars_ui::{ConsoleResult, ConsoleSender, InputRequest, InputRequestType, Timeout};
use pyo3::prelude::*;
use pyo3::types::PyAny;

use crate::{TerminalVm, VmContext, Workflow};
use erars_ast::Value;

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

pub struct ValueWrapper(Value);
pub struct InputRequestTypeWrapper(InputRequestType);

impl From<Value> for ValueWrapper {
    fn from(v: Value) -> Self {
        Self(v)
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
}

#[pymodule]
#[pyo3(name = "erars_internal")]
pub fn module(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_class::<VmProxy>()?;
    Ok(())
}
