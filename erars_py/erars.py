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

def var(name: str, *args) -> int | str:
    return VM.var(name, args)

def call(name: str, *args) -> int | str:
    VM.call(name, args)
    return 0

def begin(ty: int):
    if ty == BEGIN_TITLE:
        call("SYSTEM_TITLE")
    else:
        raise "Unknown begin type"
    return
