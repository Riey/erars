from pyclbr import Function
import erars;
from erars import *;

from foo import foo;

def export_functions() -> dict[str, Function]:
    return {"FOO": foo}

def entry_point(vm):
    erars.VM = vm
    begin(BEGIN_TITLE)
