from erars import *;

def system_title():
    pl("[0] 힘세고 강한 시작")
    pl("[1] 불러오기")
    while(True):
        ret = era_input()
        if ret == 0:
            begin(BEGIN_FIRST)
