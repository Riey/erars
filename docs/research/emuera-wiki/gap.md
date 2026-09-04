# Emuera-wiki ⇄ erars gap analysis (machine-checked)

Mirror fetched 2026-09-03. erars source read from the working tree (uncommitted). erars enum variants are CamelCase; all comparisons are case-insensitive against the UPPERCASE wiki names (`strum(serialize_all = "UPPERCASE")` on the erars enums).

Update command (re-run): `python3 _extract.py` fetches; `python3 _gap.py` regenerates index.md + gap.md.

> **Audit correction, 2026-09-03 (image-output workstream).** The reading at
> §(a) and §(b1) below that `GCREATEFROMFILE`, `SETANIMETIMER`,
> `SPRITEANIMECREATE` and `SPRITEANIMEADDFRAME` are "in-expression only, not
> line-head" is **wrong**, and so is the inference that the line-head form is a
> gap. Both engines accept any method name at line head:
> Emuera merges the whole `FunctionMethodCreator.GetMethodList()` table into
> the instruction dictionary behind `methodInstruction`
> (`GameProc/Function/FunctionIdentifier.cs:428-436`), which runs
> `METHOD_Instruction` and stores the result in RESULT/RESULTS
> (`GameProc/Function/Instraction.Child.cs:480-493`); erars mirrors it with the
> `BuiltinMethod` fallback at the end of `next_line`
> (`crates/erars-lexer/src/lib.rs:571-579`, whose comment already cites
> `FunctionIdentifier.cs:428-436`) → `EraLine::MethodLine` →
> `Stmt::Method` (`crates/erars-compiler/src/parser.rs:1830-1834`,
> `crates/erars-compiler/src/compiler.rs:789-795`). All four therefore work at
> line head today; the corpus uses `SETANIMETIMER` /`SPRITEANIME*` in exactly
> that form (`eramegaten_p_kr/Data/ERB/BATTLE.ERB:566`,
> `.../画像処理/主人マップアイコン.ERB:51`). A name-set diff cannot see this
> fallback, so treat every "line-head" verdict in §(a) as "has its own
> `InstructionCode`/`BuiltinCommand` variant", not "callable at line head".

### (a) Instructions — excom+eramaerb wiki vs erars line-head command surface

erars line-head surface = `InstructionCode` (300) ∪ `BuiltinCommand` (116), matched underscore-insensitively. Wiki set = excom `####` headings (260) ∪ eramaerb-documented eramaker basics (17) ∪ excom-prose control flow (20). PRINT-family variants are dropped from the per-name diff because erars handles the whole family through the parser's `PrintType` (Plain/Form/FormS/S/V/Data), not enum variants.

**in wiki, not in erars** (21):
- CBGCLEAR
- CBGCLEARBUTTON
- CBGREMOVEBMAP
- CBGREMOVERANGE
- CBGSETBMAPG
- CBGSETBUTTONSPRITE
- CBGSETG
- CBGSETSPRITE
- CHARATU
- CSVJULE
- GCREATEFROMFILE
- GETKEY
- GETKEYTRIGGERED
- GFILLRECTANGLE
- ISACTIVE
- MOUSEX
- MOUSEY
- SETANIMETIMER
- SPRITEANIMEADDFRAME
- SPRITEANIMECREATE
- SPRITEGETCOLOR

Interpretation of in-wiki-not-erars: `GCREATEFROMFILE`, `SETANIMETIMER`, `SPRITEANIMECREATE`, `SPRITEANIMEADDFRAME` exist as erars `BuiltinMethod`s (in-expression only, not line-head); `CSVJULE` is the wiki's own spelling of `CSVJUEL` (normalises to the same token, so not a real gap). The genuinely-unimplemented line-head commands are the `CBG*` sprite-family, `CHARATU`, `GETKEY`, `GETKEYTRIGGERED`, `GFILLRECTANGLE`, `ISACTIVE`, `MOUSEX`, `MOUSEY`, `SPRITEGETCOLOR` — mostly 1.8xx fork additions.

**in erars, not in wiki** (28) — *flagged: candidate erars-only/fork/changed-spelling; do NOT delete:*
- ALLSAMES
- ARRAYMOVE
- CHKVARDATA
- CONVERT
- CSVEX
- CSVJUEL
- GETCONFIG
- GETCONFIGS
- GETTIMES
- GROUPMATCH
- HTMLPRINTISLAND
- HTMLPRINTISLANDCLEAR
- LOADVAR
- LOG
- LOG10
- MATCHALL
- MESSKIP
- NOSAMES
- PRINTBUTTONC
- PRINTBUTTONLC
- PRINTPLAINFORM
- RAND
- REF
- REFBYNAME
- SAVEVAR
- STRJOIN
- VARI
- VARS

Interpretation of in-erars-not-wiki: `PRINTBUTTONC`, `PRINTBUTTONLC`, `PRINTPLAINFORM` are erars' expansion of the wiki's mask headings `PRINTBUTTON(|C|LC)` and `PRINTPLAIN(|FORM)` (not real gaps). `ALLSAMES`, `GROUPMATCH`, `MESSKIP`, `CONVERT`, `STRJOIN`, `GETCONFIG`, `GETCONFIGS`, `LOG`, `LOG10`, `RAND`, `GETTIMES` are documented on the wiki's exmeth page as in-expression functions (the wiki documents them as functions, not commands; erars has both command and method forms). Genuine erars/fork-only or changed-spelling candidates: `HTML_PRINT_ISLAND(_CLEAR)`, `MATCHALL`, `VARI`, `VARS`, `CHKVARDATA`, `CSVEX`, `REF`, `REFBYNAME`, `SAVEVAR`, `LOADVAR`, `ARRAYMOVE`, `CSVJUEL` (wiki spells it `CSVJULE`).

**name matches** (272):
ABS, ADDCHARA, ADDCOPYCHARA, ADDDEFCHARA, ADDSPCHARA, ADDVOIDCHARA, ALIGNMENT, ARRAYCOPY, ARRAYMSORT, ARRAYREMOVE, ARRAYSHIFT, ARRAYSORT, ASSERT, AWAIT, BAR, BARL, BARSTR, BEGIN, BREAK, CALL, CALLEVENT, CALLF, CALLFORM, CALLFORMF, CALLTRAIN, CASE, CASEELSE, CATCH, CHKCHARADATA, CHKDATA, CHKFONT, CLEARBIT, CLEARLINE, CLEARTEXTBOX, CONTINUE, COPYCHARA, CSVABL, CSVBASE, CSVCALLNAME, CSVCFLAG, CSVCSTR, CSVEQUIP, CSVEXP, CSVMARK, CSVMASTERNAME, CSVNAME, CSVNICKNAME, CSVRELATION, CSVTALENT, CUPCHECK, CURRENTALIGN, CURRENTREDRAW, CUSTOMDRAWLINE, CVARSET, DATA, DATAFORM, DATALIST, DEBUGCLEAR, DEBUGPRINT, DEBUGPRINTFORM, DEBUGPRINTFORML, DEBUGPRINTL, DELALLCHARA, DELCHARA, DELDATA, DO, DOTRAIN, DRAWLINE, DRAWLINEFORM, DUMPRAND, ELSE, ELSEIF, ENCODETOUNI, ENDCATCH, ENDDATA, ENDFUNC, ENDIF, ENDLIST, ENDNOSKIP, ENDSELECT, ESCAPE, EXISTCSV, FINDCHARA, FINDELEMENT, FINDLASTCHARA, FINDLASTELEMENT, FIND_CHARADATA, FONTBOLD, FONTITALIC, FONTREGULAR, FONTSTYLE, FOR, FORCEKANA, FORCEWAIT, FUNC, GCLEAR, GCREATE, GCREATED, GDISPOSE, GDRAWG, GDRAWGWITHMASK, GDRAWSPRITE, GETBGCOLOR, GETBIT, GETCHARA, GETCOLOR, GETDEFBGCOLOR, GETDEFCOLOR, GETEXPLV, GETFOCUSCOLOR, GETFONT, GETMILLISECOND, GETNUM, GETPALAMLV, GETSECOND, GETSTYLE, GETTIME, GGETCOLOR, GHEIGHT, GLOAD, GOTO, GOTOFORM, GSAVE, GSETBRUSH, GSETCOLOR, GSETFONT, GSETPEN, GWIDTH, HTML_PRINT, HTML_TAGSPLIT, IF, INITRAND, INPUT, INPUTMOUSEKEY, INPUTS, INRANGE, INVERTBIT, ISNUMERIC, ISSKIP, JUMP, JUMPFORM, LIMIT, LINEISEMPTY, LOADCHARA, LOADDATA, LOADGAME, LOADGLOBAL, LOADTEXT, LOOP, MAX, MIN, MONEYSTR, MOUSESKIP, NEXT, NOSKIP, ONEINPUT, ONEINPUTS, OUTPUTLOG, PICKUPCHARA, POWER, PRINTBUTTON, PRINTCPERLINE, PRINTPLAIN, PRINT_ABL, PRINT_EXP, PRINT_IMG, PRINT_ITEM, PRINT_MARK, PRINT_PALAM, PRINT_RECT, PRINT_SHOPITEM, PRINT_SPACE, PRINT_TALENT, PUTFORM, QUIT, RANDOMIZE, REDRAW, REND, REPEAT, REPLACE, RESETBGCOLOR, RESETCOLOR, RESETDATA, RESETGLOBAL, RESET_STAIN, RESTART, RETURN, RETURNF, RETURNFORM, REUSELASTLINE, SAVECHARA, SAVEDATA, SAVEGAME, SAVEGLOBAL, SAVENOS, SAVETEXT, SELECTCASE, SETBGCOLOR, SETBGCOLORBYNAME, SETBIT, SETCOLOR, SETCOLORBYNAME, SETFONT, SIF, SIGN, SKIPDISP, SORTCHARA, SPLIT, SPRITECREATE, SPRITECREATED, SPRITEDISPOSE, SPRITEHEIGHT, SPRITEMOVE, SPRITEPOSX, SPRITEPOSY, SPRITESETPOS, SPRITEWIDTH, SQRT, STOPCALLTRAIN, STRCOUNT, STRDATA, STRFIND, STRFINDU, STRLEN, STRLENFORM, STRLENFORMU, STRLENS, STRLENSU, STRLENU, SUBSTRING, SUBSTRINGU, SWAP, SWAPCHARA, THROW, TIMES, TINPUT, TINPUTS, TOFULL, TOHALF, TOINT, TOLOWER, TONEINPUT, TONEINPUTS, TOOLTIP_SETCOLOR, TOOLTIP_SETDELAY, TOOLTIP_SETDURATION, TOSTR, TOUPPER, TRYCALL, TRYCALLFORM, TRYCALLLIST, TRYCCALL, TRYCCALLFORM, TRYCGOTO, TRYCGOTOFORM, TRYCJUMP, TRYCJUMPFORM, TRYGOTO, TRYGOTOFORM, TRYGOTOLIST, TRYJUMP, TRYJUMPFORM, TRYJUMPLIST, TWAIT, UNICODE, UPCHECK, VARSET, VARSIZE, WAIT, WAITANYKEY, WEND, WHILE


### (b) In-expression functions — exmeth vs `BuiltinMethod`

Extraction: (wiki) `grep -E '^##### ' exmeth.md` → `<type> NAME(args)` name (n=152); (erars) `BuiltinMethod` enum variants via their strum-serialized UPPERCASE tokens (n=131), matched underscore-insensitively.

**in wiki, not in erars** (28):
- CBGCLEAR
- CBGCLEARBUTTON
- CBGREMOVEBMAP
- CBGREMOVERANGE
- CBGSETBMAPG
- CBGSETBUTTONSPRITE
- CBGSETG
- CBGSETSPRITE
- CBRT
- CHARATU
- COLOR_FROMNAME
- COLOR_FROMRGB
- EXPONENT
- GETKEY
- GETKEYTRIGGERED
- GETLINESTR
- GETSPCHARA
- GFILLRECTANGLE
- HTML_ESCAPE
- HTML_GETPRINTEDSTR
- HTML_POPPRINTINGSTR
- HTML_TOPLAINTEXT
- ISACTIVE
- MOUSEX
- MOUSEY
- PRINTCLENGTH
- SPRITEGETCOLOR
- STRFORM

**in erars, not in wiki** (7) — *flagged: candidate erars-only/fork; do NOT delete:*
- ARRAYMSORT
- CHKVARDATA
- CSVEX
- GDRAWTEXT
- MOUSESKIP
- SETANIMETIMER
- STRJOIN

**name matches** (124):
ABS, ALLSAMES, BARSTR, CHKCHARADATA, CHKDATA, CHKFONT, CLIENTHEIGHT, CLIENTWIDTH, CMATCH, CONVERT, CSVABL, CSVBASE, CSVCALLNAME, CSVCFLAG, CSVCSTR, CSVEQUIP, CSVEXP, CSVJUEL, CSVMARK, CSVMASTERNAME, CSVNAME, CSVNICKNAME, CSVRELATION, CSVTALENT, CURRENTALIGN, CURRENTREDRAW, ENCODETOUNI, ESCAPE, EXISTCSV, FINDCHARA, FINDELEMENT, FINDLASTCHARA, FINDLASTELEMENT, FIND_CHARADATA, GCLEAR, GCREATE, GCREATED, GCREATEFROMFILE, GDISPOSE, GDRAWG, GDRAWGWITHMASK, GDRAWSPRITE, GETBGCOLOR, GETBIT, GETCHARA, GETCOLOR, GETCONFIG, GETCONFIGS, GETDEFBGCOLOR, GETDEFCOLOR, GETEXPLV, GETFOCUSCOLOR, GETFONT, GETMILLISECOND, GETNUM, GETPALAMLV, GETSECOND, GETSTYLE, GETTIME, GETTIMES, GGETCOLOR, GHEIGHT, GLOAD, GROUPMATCH, GSAVE, GSETBRUSH, GSETCOLOR, GSETFONT, GSETPEN, GWIDTH, INRANGE, ISNUMERIC, ISSKIP, LIMIT, LINEISEMPTY, LOADTEXT, LOG, LOG10, MATCH, MAX, MAXARRAY, MAXCARRAY, MESSKIP, MIN, MINARRAY, MINCARRAY, MONEYSTR, NOSAMES, POWER, PRINTCPERLINE, RAND, REPLACE, SAVENOS, SAVETEXT, SIGN, SPRITEANIMEADDFRAME, SPRITEANIMECREATE, SPRITECREATE, SPRITECREATED, SPRITEDISPOSE, SPRITEHEIGHT, SPRITEMOVE, SPRITEPOSX, SPRITEPOSY, SPRITESETPOS, SPRITEWIDTH, SQRT, STRCOUNT, STRFIND, STRFINDU, STRLENS, STRLENSU, SUBSTRING, SUBSTRINGU, SUMARRAY, SUMCARRAY, TOFULL, TOHALF, TOINT, TOLOWER, TOSTR, TOUPPER, UNICODE, VARSIZE

Note: `FIND_CHARADATA` is erars' strum-serialized token for `BuiltinMethod::FindCharaData` and matches the wiki heading. `GCREATEFROMFILE`, `SETANIMETIMER`, `SPRITEANIMECREATE`, `SPRITEANIMEADDFRAME`, `GDRAWTEXT`, `CLIENTWIDTH`, `CLIENTHEIGHT` are erars `BuiltinMethod`s whose wiki counterparts (exmeth) are missing or spelled differently — see needs-human-check.


### (b2) exmeth in-expression functions cross-checked vs BuiltinCommand (informational)

**in wiki, not in erars** (147):
- ABS
- ALLSAMES
- BARSTR
- CBGCLEAR
- CBGCLEARBUTTON
- CBGREMOVEBMAP
- CBGREMOVERANGE
- CBGSETBMAPG
- CBGSETBUTTONSPRITE
- CBGSETG
- CBGSETSPRITE
- CBRT
- CHARATU
- CHKCHARADATA
- CHKDATA
- CHKFONT
- CLIENTHEIGHT
- CLIENTWIDTH
- CMATCH
- COLOR_FROMNAME
- COLOR_FROMRGB
- CONVERT
- CSVABL
- CSVBASE
- CSVCALLNAME
- CSVCFLAG
- CSVCSTR
- CSVEQUIP
- CSVEXP
- CSVJUEL
- CSVMARK
- CSVMASTERNAME
- CSVNAME
- CSVNICKNAME
- CSVRELATION
- CSVTALENT
- CURRENTALIGN
- CURRENTREDRAW
- ESCAPE
- EXISTCSV
- EXPONENT
- FINDCHARA
- FINDELEMENT
- FINDLASTCHARA
- FINDLASTELEMENT
- FIND_CHARADATA
- GCLEAR
- GCREATE
- GCREATED
- GCREATEFROMFILE
- GDISPOSE
- GDRAWG
- GDRAWGWITHMASK
- GDRAWSPRITE
- GETBGCOLOR
- GETBIT
- GETCHARA
- GETCOLOR
- GETCONFIG
- GETCONFIGS
- GETDEFBGCOLOR
- GETDEFCOLOR
- GETEXPLV
- GETFOCUSCOLOR
- GETFONT
- GETKEY
- GETKEYTRIGGERED
- GETLINESTR
- GETMILLISECOND
- GETNUM
- GETPALAMLV
- GETSECOND
- GETSPCHARA
- GETSTYLE
- GETTIMES
- GFILLRECTANGLE
- GGETCOLOR
- GHEIGHT
- GLOAD
- GROUPMATCH
- GSAVE
- GSETBRUSH
- GSETCOLOR
- GSETFONT
- GSETPEN
- GWIDTH
- HTML_ESCAPE
- HTML_GETPRINTEDSTR
- HTML_POPPRINTINGSTR
- HTML_TOPLAINTEXT
- INRANGE
- ISACTIVE
- ISNUMERIC
- ISSKIP
- LIMIT
- LINEISEMPTY
- LOADTEXT
- LOG
- LOG10
- MATCH
- MAX
- MAXARRAY
- MAXCARRAY
- MESSKIP
- MIN
- MINARRAY
- MINCARRAY
- MONEYSTR
- MOUSEX
- MOUSEY
- NOSAMES
- PRINTCLENGTH
- RAND
- REPLACE
- SAVETEXT
- SIGN
- SPRITEANIMEADDFRAME
- SPRITEANIMECREATE
- SPRITECREATE
- SPRITECREATED
- SPRITEDISPOSE
- SPRITEGETCOLOR
- SPRITEHEIGHT
- SPRITEMOVE
- SPRITEPOSX
- SPRITEPOSY
- SPRITESETPOS
- SPRITEWIDTH
- SQRT
- STRCOUNT
- STRFIND
- STRFINDU
- STRFORM
- STRLENS
- STRLENSU
- SUBSTRING
- SUBSTRINGU
- SUMARRAY
- SUMCARRAY
- TOFULL
- TOHALF
- TOINT
- TOLOWER
- TOSTR
- TOUPPER
- UNICODE
- VARSIZE

**in erars, not in wiki** (111) — *flagged: candidate erars-only/fork extension; do NOT delete:*
- ADDCHARA
- ADDCOPYCHARA
- ADDDEFCHARA
- ADDSPCHARA
- ADDVOIDCHARA
- ARRAYCOPY
- ARRAYMOVE
- ARRAYMSORT
- ARRAYREMOVE
- ARRAYSHIFT
- ARRAYSORT
- ASSERT
- AWAIT
- BAR
- BARL
- CALLTRAIN
- CLEARBIT
- CLEARLINE
- CLEARTEXTBOX
- COPYCHARA
- CUPCHECK
- CUSTOMDRAWLINE
- CVARSET
- DEBUGCLEAR
- DELALLCHARA
- DELCHARA
- DELDATA
- DOTRAIN
- DRAWLINE
- DUMPRAND
- ENDNOSKIP
- FONTBOLD
- FONTITALIC
- FONTREGULAR
- FONTSTYLE
- FORCEKANA
- FORCEWAIT
- HTMLPRINT
- HTMLPRINTISLAND
- HTMLPRINTISLANDCLEAR
- HTMLTAGSPLIT
- INITRAND
- INPUT
- INPUTMOUSEKEY
- INPUTS
- INVERTBIT
- LOADCHARA
- LOADDATA
- LOADGAME
- LOADGLOBAL
- LOADVAR
- MATCHALL
- NOSKIP
- ONEINPUT
- ONEINPUTS
- OUTPUTLOG
- PICKUPCHARA
- PRINTABL
- PRINTEXP
- PRINTIMG
- PRINTITEM
- PRINTMARK
- PRINTPALAM
- PRINTRECT
- PRINTSHOPITEM
- PRINTSPACE
- PRINTTALENT
- PUTFORM
- QUIT
- RANDOMIZE
- REDRAW
- REF
- REFBYNAME
- RESETBGCOLOR
- RESETCOLOR
- RESETDATA
- RESETGLOBAL
- RESETSTAIN
- RESTART
- RETURN
- RETURNF
- SAVECHARA
- SAVEDATA
- SAVEGAME
- SAVEGLOBAL
- SAVEVAR
- SETBGCOLOR
- SETBGCOLORBYNAME
- SETBIT
- SETCOLOR
- SETCOLORBYNAME
- SETFONT
- SKIPDISP
- SORTCHARA
- SPLIT
- STOPCALLTRAIN
- SWAP
- SWAPCHARA
- THROW
- TINPUT
- TINPUTS
- TONEINPUT
- TONEINPUTS
- TOOLTIPSETCOLOR
- TOOLTIPSETDELAY
- TOOLTIPSETDURATION
- TWAIT
- UPCHECK
- VARSET
- WAIT
- WAITANYKEY

**name matches** (5):
ENCODETOUNI, GETTIME, POWER, PRINTCPERLINE, SAVENOS

**counts — functions_vs_command:** in-wiki-not-erars `147`, in-erars-not-wiki `111`, name-match `5`


### (c) Variables/constants — exvar+eramavar vs erars variables

**Variable-resolution union used for this diff (erars):** `variable.yaml` (globals) +
`KnownVariableNames` (`crates/erars-vm/src/variable.rs:1591`) + `BuiltinVariable`
(read-only system pseudo-vars, `crates/erars-ast/src/variable.rs`) + function-scoped
`ARG/ARGS/LOCAL/LOCALS` (parser) + name-CSV tables (`merge_name_csv`,
`crates/erars-loader/src/lib.rs:261`). The earlier draft extracted from `variable.yaml`
**only**, which wrongly reported 27 gaps; `BuiltinVariable` carries most of the read-only
system vars.

**in wiki, not in erars — GENUINE (6, empirically confirmed)**: reading each via a real
executed `PRINTFORM {X}` yields runtime `Variable X is not exists` (full parse→compile→
FunctionDic→`TerminalVm::start` path):
- CDFLAGNAME1
- CDFLAGNAME2
- GAMEBASE_GAMECODE — *naming gap:* erars exposes the same value as **GAMEBASE_CODE**
  (`BuiltinVariable::GamebaseCode`); Emuera/wiki name is `GAMEBASE_GAMECODE`
  (`VariableCode.cs:258`). Runtime probe: `GAMEBASE_CODE` RUNTIME-OK, `GAMEBASE_GAMECODE` missing.
- ISTIMEOUT
- MONEYLABEL
- TFLAGNAME

**in wiki, flagged missing by yaml-only extraction but PRESENT via other resolution paths**
(runtime `PRINTFORM {X}` returns a value, no error):
- `CHARANUM`, `LINECOUNT`, `RAND`, `DRAWLINESTR` — `BuiltinVariable::{CharaNum,LineCount,Rand,DrawLineStr}` (`erars-ast/src/variable.rs`), evaluated `executor.rs:382-408`.
- `GAMEBASE_AUTHOR/_INFO/_YEAR/_TITLE/_VERSION/_ALLOWVERSION/_DEFAULTCHARA/_NOITEM` — `BuiltinVariable::Gamebase*`, backed by the parsed `gamebase` struct.
- `LASTLOAD_VERSION/_NO/_TEXT` — `BuiltinVariable::LastLoad*`, `executor.rs:392-394`.
- `ARG/ARGS/LOCAL/LOCALS` — function-scoped identifiers (parser + `KnownVariableNames`).
- `GAMEBASE` and `LASTLOAD_` (bare prefixes) in the old list were extraction junk, not real variables — dropped.

**in erars, not in wiki** (25) — *flagged: candidate erars-only/fork extension; do NOT delete:*
- B
- C
- D
- E
- F
- G
- H
- I
- J
- K
- L
- M
- N
- O
- P
- Q
- R
- S
- T
- U
- V
- W
- X
- Y
- Z

**name matches** (96):
A, ABL, ABLNAME, ASSI, ASSIPLAY, BASE, BASENAME, BOUGHT, CALLNAME, CDFLAG, CDOWN, CFLAG, CFLAGNAME, COUNT, CSTR, CSTRNAME, CUP, DA, DAY, DB, DC, DD, DE, DITEMTYPE, DOWN, DOWNBASE, EJAC, EQUIP, EQUIPNAME, EX, EXNAME, EXP, EXPLV, EXPNAME, FLAG, FLAGNAME, GLOBAL, GLOBALNAME, GLOBALS, GLOBALSNAME, GOTJUEL, ISASSI, ITEM, ITEMNAME, ITEMPRICE, ITEMSALES, JUEL, LOSEBASE, MARK, MARKNAME, MASTER, MASTERNAME, MAXBASE, MONEY, NAME, NEXTCOM, NICKNAME, NO, NOITEM, NOWEX, PALAM, PALAMLV, PALAMNAME, PBAND, PLAYER, PREVCOM, RANDDATA, RELATION, RESULT, RESULTS, SAVEDATA_TEXT, SAVESTR, SAVESTRNAME, SELECTCOM, SOURCE, SOURCENAME, STAIN, STAINNAME, STR, STRNAME, TA, TALENT, TALENTNAME, TARGET, TB, TCVAR, TCVARNAME, TEQUIP, TEQUIPNAME, TFLAG, TIME, TRAINNAME, TSTR, TSTRNAME, UP, WINDOW_TITLE

**counts — variables (revised 2026-09-03):** in-wiki-not-erars `27` (yaml-only) → **`6` genuine after unioning all resolution sources + runtime-confirming** (CDFLAGNAME1, CDFLAGNAME2, GAMEBASE_GAMECODE, ISTIMEOUT, MONEYLABEL, TFLAGNAME); the other 21 are present via `BuiltinVariable`/function-scope/name-CSV. in-erars-not-wiki `25` (NOTE: single-letter A–Z entries are an extraction artifact of a category column, not real variables — re-derive before trusting), name-match `96`.


### (c2) eramaker-era variables — eramavar vs erars

**in wiki, not in erars** (2):
- CHARANUM
- RAND

**in erars, not in wiki** (66) — *flagged: candidate erars-only/fork extension; do NOT delete:*
- B
- BASENAME
- C
- CDFLAG
- CDOWN
- CFLAGNAME
- CSTR
- CSTRNAME
- CUP
- D
- DA
- DB
- DC
- DD
- DE
- DITEMTYPE
- DOWNBASE
- E
- EQUIP
- EQUIPNAME
- EXNAME
- F
- FLAGNAME
- G
- GLOBAL
- GLOBALNAME
- GLOBALS
- GLOBALSNAME
- H
- I
- ITEMPRICE
- J
- K
- L
- M
- MASTERNAME
- N
- NEXTCOM
- NICKNAME
- O
- P
- Q
- R
- RANDDATA
- S
- SAVEDATA_TEXT
- SAVESTRNAME
- SOURCENAME
- STAINNAME
- STRNAME
- T
- TA
- TB
- TCVAR
- TCVARNAME
- TEQUIPNAME
- TRAINNAME
- TSTR
- TSTRNAME
- U
- V
- W
- WINDOW_TITLE
- X
- Y
- Z

**name matches** (55):
A, ABL, ABLNAME, ASSI, ASSIPLAY, BASE, BOUGHT, CALLNAME, CFLAG, COUNT, DAY, DOWN, EJAC, EX, EXP, EXPLV, EXPNAME, FLAG, GOTJUEL, ISASSI, ITEM, ITEMNAME, ITEMSALES, JUEL, LOSEBASE, MARK, MARKNAME, MASTER, MAXBASE, MONEY, NAME, NO, NOITEM, NOWEX, PALAM, PALAMLV, PALAMNAME, PBAND, PLAYER, PREVCOM, RELATION, RESULT, RESULTS, SAVESTR, SELECTCOM, SOURCE, STAIN, STR, TALENT, TALENTNAME, TARGET, TEQUIP, TFLAG, TIME, UP

**counts — variables_eramavar:** in-wiki-not-erars `2`, in-erars-not-wiki `66`, name-match `55`


### (d1) `#…` directives — exfunc vs SharpCode

**in wiki, not in erars** (1):
- ONLY

**in erars, not in wiki** (3) — *flagged: candidate erars-only/fork extension; do NOT delete:*
- LATER
- PRI
- SINGLE

**name matches** (7):
DEFINE, DIM, DIMS, FUNCTION, FUNCTIONS, LOCALSIZE, LOCALSSIZE

**counts — sharp:** in-wiki-not-erars `1`, in-erars-not-wiki `3`, name-match `7`

Note: erars `SharpCode` also has `PRI`, `LATER`, `SINGLE` (event attributes) that the exfunc page does not document; exfunc documents `#ONLY` which is not in `SharpCode`.

### (d2) Bracket directives — exfunc vs erars `SquareCode`/preprocessor

Wiki lists: [SKIPSTART], [SKIPEND], [IF XXX], [ELSEIF XXX], [ELSE], [ENDIF], [IF_DEBUG], [IF_NDEBUG], [ENDIF]

erars `SquareCode` (crates/erars-lexer/src/square.rs) handles `SKIPSTART`, `IF`, `IF_DEBUG`; the lexer additionally tracks `SKIPEND`, `ELSEIF`, `ELSE`, `ENDIF`, `IF_NDEBUG` delimiters (lib.rs preprocessor). Implied-not-handled candidate: `['[IF_NDEBUG]']`.

### (e) Config keys — config vs EraConfigKey

**in wiki, not in erars** (61):
- CALLNAMEが空文字列の時にNAMEを代入する
- ERAMAKER互換性に関する警告を表示する
- FORM中の三連記号を展開しない
- ONEINPUT系命令でマウスによる2文字以上の入力を許可する
- VER1739以前の非ボタン折り返しを再現する
- _RENAME.CSVを利用する
- _REPLACE.CSVを利用する
- イベント関数のCALLを許可する
- ウィンドウの高さを可変にする
- ウィンドウ位置X
- ウィンドウ位置Y
- オートセーブを行なう
- キーボードマクロを使用する
- コマンドライン引数
- サブディレクトリを検索する
- システム関数が上書きされたとき警告を表示する
- システム関数の上書きを許可する
- スクロールの行数
- セーブデータをSAVフォルダ内に作成する
- セーブデータをUTF-8で保存する
- セーブデータをバイナリ形式で保存する
- デバッグウィンドウ位置X
- デバッグウィンドウ位置Y
- デバッグウインドウを最前面に表示する
- デバッグウインドウ位置を指定する
- デバッグウインドウ幅
- デバッグウインドウ高さ
- デバッグコマンドを使用する
- フレーム毎秒
- ボタンの途中で行を折りかえさない
- マウスを使用する
- メニューを使用する
- ユーザー関数の全ての引数の省略を許可する
- ユーザー関数の引数に自動的にTOSTRを補完する
- ロード時にレポートを表示する
- ロード時に引数を解析する
- 使用するセーブデータ数
- 全角スペースをホワイトスペースに含める
- 同名の非イベント関数が複数定義されたとき警告する
- 呼び出されなかった関数を無視する
- 多重起動を許可する
- 大文字小文字の違いを無視する
- 履歴文字色
- 擬似変数RANDの仕様をERAMAKERに合わせる
- 無限ループ警告までのミリ秒数
- 表示する最低警告レベル
- 解釈不能な行があっても実行する
- 読み込み順をファイル名順にソートする
- 起動時にウィンドウを最大化する
- 起動時にデバッグウインドウを表示する
- 起動時のウィンドウの位置を固定する
- 関数が呼び出されなかった警告の扱い
- 関数が見つからない警告の扱い
- 関数・属性については大文字小文字を無視しない
- 関連づけるテキストエディタ
- （DRAWLINEを常に新しい行で行う）
- （イメージバッファを使用する）
- （ロード時にFORM文字列を解析する）
- （指定したファイル中の警告を無視する）
- （描画にGDI+を用いる）
- （最大スキップフレーム数）

**in erars, not in wiki** (8) — *flagged: candidate erars-only/fork extension; do NOT delete:*
- CHINESE_HANS
- CHINESE_HANT
- GRAPHICS
- JAPANESE
- KOREAN
- TEXTRENDERER
- WINAPI
- 表示するセーブデータ数

**name matches** (14):
PRINTCの文字数, PRINTCを並べる数, SPキャラを使用する, ウィンドウ幅, ウィンドウ高さ, フォントサイズ, フォント名, 一行の高さ, 内部で使用する東アジア言語, 履歴ログの行数, 描画インターフェース, 文字色, 背景色, 選択中文字色

**counts — config:** in-wiki-not-erars `61`, in-erars-not-wiki `8`, name-match `14`

Extraction (wiki): `grep -E '^##### ' config.md` (Japanese key names, incl. obsolete items); (erars): `EraConfigKey` `#[strum(to_string=…)]` Japanese names in parser.rs. Many wiki keys are UI/host concerns erars does not need (e.g. window position, fonts it renders itself); flagged as in-wiki-not-erars. exconfig adds no new keys.

### (f) Debug commands — debugcom vs erars

**in wiki, not in erars** (5):
- CONFIG
- DEBUG
- EXIT
- OUTPUT
- REBOOT

**in erars, not in wiki** (0) — *flagged: candidate erars-only/fork extension; do NOT delete:*
_none_

**name matches** (0):


**counts — debug:** in-wiki-not-erars `5`, in-erars-not-wiki `0`, name-match `0`

erars has no interactive debug-console host implemented in the crates (`@REBOOT/@OUTPUT/@EXIT/@CONFIG/@DEBUG` are console-UI concerns); comparison left to **needs human/expert check**.

### (g) CSV files — eramacsv/replace/resources vs erars CSV handling

**eramaker CSV files (wiki eramacsv):** Abl.csv, CharaXX.csv, Exp.csv, GameBase.csv, Item.csv, Mark.csv, Palam.csv, Str.csv, Talent.csv, Train.csv

**erars CSV readers (parser.rs `merge_*_csv`):**
- `GameBase.csv` keys (`merge_gamebase_csv`): コード, バージョン, バージョン違い認める, 最初からいるキャラ, アイテムなし, 作者, 追加情報, 製作年, タイトル, ウィンドウタイトル
- `Chara*.csv` keys (`merge_chara_csv`): NO, NAME, MASTERNAME, CALLNAME, NICKNAME, ISASSI, TALENT, BASE, MARK, ABL, EXP, RELATION, EQUIP, JUEL, CFLAG, 番号, 名前, 主人の呼び方, 呼び名, あだ名, 助手, CSTR, 素質, 基礎, 刻印, 能力, 経験, 相性, 装着物, 珠, フラグ
- `STR.csv`/`ABL.csv`/`TALENT.csv`/… number→name CSVs are read generically (`merge_str_csv`).

**Differences (wiki vs erars):**
- eramacsv documents eran-maker-era CSVs (Palam/Abl/Talent/Mark/Exp/Train/Item/Str/CharaXX) and their columns. erars reads these plus the Emuera-only ones (flag/cflag/tcvar/stain/…).

### (g2) GameBase.csv keys — eramacsv vs erars

**in wiki, not in erars** (0):
_none_

**in erars, not in wiki** (1) — *flagged: candidate erars-only/fork extension; do NOT delete:*
- ウィンドウタイトル

**name matches** (9):
アイテムなし, コード, タイトル, バージョン, バージョン違い認める, 作者, 最初からいるキャラ, 製作年, 追加情報

**counts — gamebase:** in-wiki-not-erars `0`, in-erars-not-wiki `1`, name-match `9`


### (g3) Chara*.csv keys — exvar/exetc vs erars

**in wiki, not in erars** (0):
_none_

**in erars, not in wiki** (15) — *flagged: candidate erars-only/fork extension; do NOT delete:*
- あだ名
- フラグ
- 主人の呼び方
- 刻印
- 助手
- 名前
- 呼び名
- 基礎
- 珠
- 番号
- 相性
- 素質
- 経験
- 能力
- 装着物

**name matches** (16):
ABL, BASE, CALLNAME, CFLAG, CSTR, EQUIP, EXP, ISASSI, JUEL, MARK, MASTERNAME, NAME, NICKNAME, NO, RELATION, TALENT

**counts — chara_csv:** in-wiki-not-erars `0`, in-erars-not-wiki `15`, name-match `16`


## Arity / type disagreement (name matches, stated signature differs)

**Caveat:** erars does not encode static arity — the parser matches arbitrary-length `expr_list`/`normal_form_str` argument lists (`normal_command!`/`normal_method!` macros, parser.rs) and validates arity at runtime. So a mechanical arity diff against erars source is not possible; only exmeth's C-like signatures vs well-documented cases are checked. Deep arity verification for every command is left to **needs human/expert check** (run e.g. `cargo test` in `erars-vm` with the era* game's ERB to confirm).

Machine-observed discrepancies (wiki signature vs erars):

- **CSVJULE** (excom heading `CSVJULE <数式>, <数式>`) is the wiki's spelling; erars uses **CSVJUEL** (`BuiltinMethod::CsvJuel`), matching exmeth/exvar. Name-match only case-insensitively on `CSVJUEL`.
- **CSVRELATION** — excom states `CSVRELATION <数式>, <数式>, <数式>` (3 args) but exmeth states `int CSVRELATION(int no, int index)` / `CSVRELATION(int no, int index, int flag = 0)` (2–3 args). Discrepancy is between the wiki's own two pages, not erars.
- exmeth lists **GETSPCHARA** (not in InstructionCode; erars has no `GETSPCHARA` command) and **SAVETEXT/LOADTEXT** only as methods (erars `BuiltinMethod::SaveText/LoadText` exist).
- exmeth **EncodeToUni** return differs from erars `BuiltinMethod::EncodeToUni` (command is `EncodeToUni`; comment notes 'different from command').

## Needs human/expert check

- Arity of every command/function — erars stores no static arity (runtime-validated); verify by running the shipped era* games.
- erars `#ONLY` support — wiki (exfunc) documents `#ONLY`; erars `SharpCode` has no `ONLY` variant (only PRI/LATER/SINGLE). Confirm whether `#ONLY` is handled elsewhere.
- [IF_NDEBUG] — wiki documents it; erars SquareCode has only SKIPSTART/IF/IF_DEBUG. Confirm the lexer/compiler ignores or errors on `[IF_NDEBUG]`.
- Debug console (`@REBOOT/@OUTPUT/@EXIT/@CONFIG/@DEBUG`) — a host/UI concern; not in the crates.
- Variable type categorization (int/str/chara/const/savedata) for the wiki list — the exvar tables are authoritative where present; headings-only vars need manual typing.
- Config keys listed as in-wiki-not-erars: many are window/font/UI/console options emuera needs but a headless VM may intentionally ignore — decide which are real gaps.
- The 241-name `BuiltinMethod+BuiltinCommand` union vs exmeth/excom sets — several erars variants (e.g. `GCreateFromFile`, `GDrawText`, `SpriteAnimeCreate`, `SetAnimeTimer`, `ClientWidth/Height`, `HtmlPrintIsland`, `MatchAll`) are documented as `.NET版`/`1.8xx` fork additions; confirm against the wiki's exhtml/resources pages which erars extends.
