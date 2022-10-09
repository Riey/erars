global VM

def pl(s: str):
    '''
    PRINTLINE
    '''
    VM.println(s)
printl = pl

def nl():
    '''
    NEWLINE
    '''
    VM.newline()
newline = nl

def pw(s: str):
    '''
    PRINTW
    '''
    VM.printw(s)
printw = pw

BEGIN_TITLE = 0
BEGIN_FIRST = 1
BEGIN_SHOP = 2

INPUT_STR = 3
INPUT_INT = 2
INPUT_ENTER = 1
INPUT_ANYKEY = 0

def var(name: str, *args) -> int | str:
    return VM.get_var(name, args)

def set_var(name: str, value: int | str, *args):
    VM.set_var(name, value, args)

def call(name: str, *args):
    VM.call(name, args)

def begin(ty: int):
    VM.begin(ty)

def era_inputs() -> str:
    VM.wait(INPUT_STR, False)

def era_input() -> int:
    VM.wait(INPUT_INT, False)
