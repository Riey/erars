use anyhow::Context;
use anyhow::Result;
use flate2::read;
use flate2::write;
use std::path::Path;

use erars_vm::{SerializableGlobalVariableStorage, SerializableVariableStorage};
