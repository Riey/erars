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

def call(name: str, *args) -> int | str:
    VM.call(name, args)
    return 0
