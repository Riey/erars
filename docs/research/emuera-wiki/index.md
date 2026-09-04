# Emuera wiki — mirrored name lists

Source: https://wiki.eragames.rip/ mirrored into this directory on 2026-09-03 (`_fetch.py`). Names are extracted mechanically from the `.md` mirrors (`_extract.py`); Japanese text is kept verbatim.

## (a) Instructions (命令)

From **excom** (`#### NAME … [ edit ]` headings, n=260):

- `PRINT(|V|S|FORM|FORMS)(|K|D)(|L|W)` — excom
- `PRINTSINGLE(|V|S|FORM|FORMS)(|K|D)` — excom
- `PRINTDATA(|K|D)(|L|W)` — excom
- `PRINTBUTTON(|C|LC) <文字列式>, <数式 or 文字列式>` — excom
- `PRINTPLAIN(|FORM)` — excom
- `CUSTOMDRAWLINE <文字列>` — excom
- `DRAWLINEFORM <FORM文字列>` — excom
- `REUSELASTLINE <書式付文字列>` — excom
- `CLEARLINE <消す行数>` — excom
- `PRINT_IMG <文字列式>` — excom
- `PRINT_RECT <数式>` — excom
- `PRINT_SPACE <数式>` — excom
- `SETCOLOR <R>, <G>, <B>` — excom
- `RESETCOLOR` — excom
- `SETBGCOLOR <R>, <G>, <B>` — excom
- `RESETBGCOLOR` — excom
- `SETCOLORBYNAME <文字列>` — excom
- `SETBGCOLORBYNAME <文字列>` — excom
- `GETCOLOR` — excom
- `GETDEFCOLOR` — excom
- `GETBGCOLOR` — excom
- `GETDEFBGCOLOR` — excom
- `GETFOCUSCOLOR` — excom
- `FONTBOLD` — excom
- `FONTITALIC` — excom
- `FONTREGULAR` — excom
- `FONTSTYLE <数式>` — excom
- `GETSTYLE` — excom
- `CHKFONT <文字列式>` — excom
- `SETFONT <文字列式>` — excom
- `GETFONT` — excom
- `FORCEKANA <数式>` — excom
- `ALIGNMENT <キーワード>` — excom
- `CURRENTALIGN` — excom
- `REDRAW <数式>` — excom
- `CURRENTREDRAW` — excom
- `PRINTCPERLINE` — excom
- `LINEISEMPTY` — excom
- `BARSTR <変数>, <最大値>, <長さ>` — excom
- `MONEYSTR <数値>{, <書式指定子>}` — excom
- `SKIPDISP <数値>` — excom
- `NOSKIP` — excom
- `ENDNOSKIP` — excom
- `ISSKIP` — excom
- `MOUSESKIP` — excom
- `TOUPPER <文字列式>` — excom
- `TOLOWER <文字列式>` — excom
- `TOHALF <文字列式>` — excom
- `TOFULL <文字列式>` — excom
- `TOSTR <数式>, <書式指定子>` — excom
- `ISNUMERIC <文字列式>` — excom
- `TOINT <文字列式>` — excom
- `STRLEN <文字列>` — excom
- `STRLENS <文字列式>` — excom
- `STRLENFORM <書式付文字列>` — excom
- `STRLENU <文字列>` — excom
- `STRLENSU <文字列式>` — excom
- `STRLENFORMU <書式付文字列>` — excom
- `SUBSTRING <文字列式>, <数式>, <数式>` — excom
- `SUBSTRINGU <文字列式>, <数式>, <数式>` — excom
- `CHARATU <文字列式>, <文字位置>` — excom
- `STRFIND <文字列式>, <文字列式>(, <数式>)` — excom
- `STRFINDU <検索対象>, <検索する文字列>{, <開始インデックス>}` — excom
- `STRCOUNT <検索対象文字列>, <検索文字列>` — excom
- `SPLIT <文字列式>, <文字列式>, <文字列変数>` — excom
- `REPLACE <置換対象文字列>, <置換対象パターン>, <置換後の文字列>` — excom
- `ESCAPE <文字列>` — excom
- `UNICODE <数式>` — excom
- `ENCODETOUNI <対象文字列(FORM型文字列)>` — excom
- `POWER <変数>, <数式>, <数式>` — excom
- `ABS <数式>` — excom
- `SIGN <数式>` — excom
- `SQRT <数式>` — excom
- `GETBIT <数式>, <数式>` — excom
- `MAX <数式>(, <数式>...)` — excom
- `MIN <数式>(, <数式>...)` — excom
- `LIMIT <数式>, <数式>, <数式>` — excom
- `INRANGE <数式>, <数式>, <数式>` — excom
- `SETBIT <数値型変数>, <数式>{, <数式>,...}` — excom
- `CLEARBIT <数値型変数>, <数式>{, <数式>,...}` — excom
- `INVERTBIT <数値型変数>, <数式>{, <数式>,...}` — excom
- `ADDCHARA <数式>(, <数式>, <数式>, ...)` — excom
- `DELCHARA <数式>(, <数式>, <数式>, ...)` — excom
- `SWAPCHARA <数式>, <数式>` — excom
- `SORTCHARA <キャラクタ変数> {, <FORWARDorBACK>}` — excom
- `GETCHARA <キャラ番号(NO:XXXの方)>` — excom
- `ADDDEFCHARA` — excom
- `ADDVOIDCHARA` — excom
- `DELALLCHARA` — excom
- `PICKUPCHARA <対象キャラ>(, <対象キャラ>, ....)` — excom
- `EXISTCSV <数式>` — excom
- `FINDCHARA <キャラクタ変数>, <式>(, <数式>, <数式>)` — excom
- `FINDLASTCHARA <キャラクタ変数>, <式>(, <数式>, <数式>)` — excom
- `COPYCHARA <数式>, <数式>` — excom
- `ADDCOPYCHARA <数式>` — excom
- `VARSIZE <変数名>` — excom
- `RESETDATA` — excom
- `RESETGLOBAL` — excom
- `RESET_STAIN <数式>` — excom
- `SWAP <変数1>, <変数2>` — excom
- `CSVNAME <数式>` — excom
- `CSVCALLNAME <数式>` — excom
- `CSVNICKNAME <数式>` — excom
- `CSVMASTERNAME <数式>` — excom
- `CSVBASE <数式>, <数式>` — excom
- `CSVCSTR <数式>, <数式>` — excom
- `CSVABL <数式>, <数式>` — excom
- `CSVTALENT <数式>, <数式>` — excom
- `CSVMARK <数式>, <数式>` — excom
- `CSVEXP <数式>, <数式>` — excom
- `CSVRELATION <数式>, <数式>, <数式>` — excom
- `CSVJULE <数式>, <数式>` — excom
- `CSVEQUIP <数式>, <数式>` — excom
- `CSVCFLAG <数式>, <数式>,` — excom
- `GETNUM <変数名>, <文字列式>` — excom
- `GETPALAMLV <数式>, <判定するLVの上限>` — excom
- `GETEXPLV <数式>, <判定するLVの上限>` — excom
- `FINDELEMENT <一次元配列変数>, <検索対象(変数と同型)>, <検索初位置>, <検索終位置>, <厳密一致かのフラグ>` — excom
- `FINDLASTELEMENT <一次元配列変数>, <検索対象(変数と同型)>, <検索初位置>, <検索終位置>, <厳密一致かのフラグ>` — excom
- `VARSET <変数名>{, <数式 or 文字列式>, <配列範囲初値>, <配列範囲終値+1>}` — excom
- `CVARSET <キャラクタ変数>{, <数式>, <式>, <キャラクタ範囲初値>, <キャラクタ範囲終値+1>}` — excom
- `ARRAYSHIFT <対象変数>, <ずらす数>, <ずらしてできた空白領域の初期値>{, <ずらす配列範囲の初値>, <ずらす配列要素の範囲の数>}` — excom
- `ARRAYREMOVE <対象変数>, <消す範囲初値>, <消す要素数>` — excom
- `ARRAYSORT <対象変数>{, <ソート方式(FORWARD or BACK)>, <開始インデックス>, <対象要素数>}` — excom
- `ARRAYCOPY <コピー元変数名>, <コピー先変数名>` — excom
- `ARRAYMSORT array1{, array2...}` — excom
- `CUPCHECK <登録キャラクター番号>` — excom
- `SAVEDATA <数式>, <文字列式>` — excom
- `LOADDATA <数式>` — excom
- `DELDATA <数式>` — excom
- `CHKDATA <数式>` — excom
- `SAVENOS <数値変数>` — excom
- `SAVEGLOBAL` — excom
- `LOADGLOBAL` — excom
- `OUTPUTLOG` — excom
- `SAVECHARA str filename, str memo, int charano{, int charano2, ...}` — excom
- `LOADCHARA str filename` — excom
- `CHKCHARADATA str filename` — excom
- `FIND_CHARADATA str filename` — excom
- `SAVETEXT str text, int fileNo{, int force_savdir, int force_UTF8}` — excom
- `LOADTEXT int fileNo{, int force_savdir, int force_UTF8}` — excom
- `GETTIME` — excom
- `GETMILLISECOND` — excom
- `GETSECOND` — excom
- `FORCEWAIT` — excom
- `INPUT {<数値>}` — excom
- `INPUTS {<文字列>}` — excom
- `TINPUT <数値>, <数値>{, <数値>, <文字列>}` — excom
- `TINPUTS <数値>, <文字列式>{, <数値>, <文字列>}` — excom
- `TWAIT <数値>, <数値>` — excom
- `ONEINPUT {<数値>}` — excom
- `ONEINPUTS {<文字列>}` — excom
- `TONEINPUT <数値>, <数値>{, <数値>, <文字列>}` — excom
- `TONEINPUTS <数値>, <文字列式>{, <数値>, <文字列>}` — excom
- `WAITANYKEY` — excom
- `INPUTMOUSEKEY {int time}` — excom
- `FOR <NameOfVariable>, <Number>, <Number>{, <Number>}` — excom
- `NEXT` — excom
- `WHILE <Formula>` — excom
- `WEND` — excom
- `DO` — excom
- `LOOP <FORMULA>` — excom
- `SELECTCASE <VARIABLE>` — excom
- `CASE <CASE CONDITION>(, <CASE CONDITION>, <CASE CONDITION> ……)` — excom
- `CASEELSE` — excom
- `ENDSELECT` — excom
- `RANDOMIZE <数式>` — excom
- `DUMPRAND` — excom
- `INITRAND` — excom
- `BEGIN <キーワード>` — excom
- `CALLTRAIN <コマンド数>` — excom
- `DOTRAIN <数式>` — excom
- `THROW <FORM構文>` — excom
- `TRYJUMP <文字列> (, 引数1, 引数2……)` — excom
- `TRYCALL <文字列> (, 引数1, 引数2……)` — excom
- `TRYGOTO <文字列>` — excom
- `JUMPFORM <書式付文字列> (, 引数1, 引数2……)` — excom
- `CALLFORM <書式付文字列> (, 引数1, 引数2……)` — excom
- `GOTOFORM <書式付文字列>` — excom
- `TRYJUMPFORM <書式付文字列> (, 引数1, 引数2……)` — excom
- `TRYCALLFORM <書式付文字列> (, 引数1, 引数2……)` — excom
- `TRYGOTOFORM <書式付文字列>` — excom
- `CALLF <文字列> (, 引数1, 引数2……)` — excom
- `CALLFORMF <書式付文字列> (, 引数1, 引数2……)` — excom
- `CALLEVENT <文字列>` — excom
- `TRYCJUMP <文字列> (, 引数1, 引数2……)` — excom
- `TRYCCALL <文字列> (, 引数1, 引数2……)` — excom
- `TRYCGOTO <文字列>` — excom
- `TRYCJUMPFORM <書式付文字列> (, 引数1, 引数2……)` — excom
- `TRYCCALLFORM <書式付文字列> (, 引数1, 引数2……)` — excom
- `TRYCGOTOFORM <書式付文字列>` — excom
- `CATCH` — excom
- `ENDCATCH` — excom
- `TRYCALLLIST` — excom
- `TRYJUMPLIST` — excom
- `TRYGOTOLIST` — excom
- `FUNC <文字列> (, 引数1, 引数2……)` — excom
- `ENDFUNC` — excom
- `RETURN <数式>(, <数式>, <数式>, ...)` — excom
- `RETURNFORM <書式付文字列>(, <書式付文字列>, <書式付文字列>, ...)` — excom
- `RETURNF <式>` — excom
- `DEBUGPRINT <文字列>` — excom
- `DEBUGPRINTL <文字列>` — excom
- `DEBUGPRINTFORM <書式付文字列>` — excom
- `DEBUGPRINTFORML <書式付文字列>` — excom
- `DEBUGCLEAR` — excom
- `ASSERT <数式>` — excom
- `TOOLTIP_SETCOLOR <数式>, <数式>` — excom
- `TOOLTIP_SETDELAY <数式>` — excom
- `TOOLTIP_SETDURATION int msDuration` — excom
- `HTML_PRINT <文字列式>` — excom
- `HTML_TAGSPLIT <文字列式>(, <数値変数>, <文字列変数>)` — excom
- `AWAIT {int time}` — excom
- `GETKEY int vkey` — excom
- `GETKEYTRIGGERED int vkey` — excom
- `MOUSEX` — excom
- `MOUSEY` — excom
- `ISACTIVE` — excom
- `GCREATE int ID, int width, int height` — excom
- `GCREATEFROMFILE int ID, str filepath` — excom
- `GDISPOSE int ID` — excom
- `GCLEAR int ID, int cARGB` — excom
- `GFILLRECTANGLE int ID, int x, int y, int width, int height` — excom
- `GDRAWG int destID, int srcID, int destX, int destY, int destWidth, int destHeight, int srcX, int srcY, int srcWidth, int srcHeight` — excom
- `GDRAWGWITHMASK int destID, int srcID, int maskID, int destX, int destY` — excom
- `GDRAWSPRITE int ID, str sprName` — excom
- `GSETCOLOR int ID, int cARGB, int x, int y` — excom
- `GSETBRUSH int ID, int cARGB` — excom
- `GSETFONT int ID, str fontName, int fontSize` — excom
- `GSETPEN int ID, int cARGB, int penWidth` — excom
- `GCREATED int ID` — excom
- `GWIDTH int ID` — excom
- `GHEIGHT int ID` — excom
- `GGETCOLOR int ID, int x, int y` — excom
- `GSAVE int ID, int fileNo` — excom
- `GLOAD int ID, int fileNo` — excom
- `SPRITECREATE str spriteName, int gID` — excom
- `SPRITEANIMECREATE str spriteName, int width, int height` — excom
- `SPRITEANIMEADDFRAME string spriteName, int gID, int x, int y, int width, int height, int offsetx, int offsety, int delay` — excom
- `SPRITEDISPOSE string spriteName` — excom
- `SPRITEGETCOLOR string spriteName, int x, int y` — excom
- `SPRITECREATED str spriteName` — excom
- `SPRITEWIDTH str spriteName` — excom
- `SPRITEHEIGHT str spriteName` — excom
- `SPRITEPOSX str spriteName` — excom
- `SPRITEPOSY str spriteName` — excom
- `SPRITESETPOS str spriteName, int posx, int posy` — excom
- `SPRITEMOVE str spriteName, int movex, int movey` — excom
- `CBGSETG int ID, int x, int y, int zdepth` — excom
- `CBGSETSPRITE str spriteName, int x, int y, int zdepth` — excom
- `CBGCLEAR` — excom
- `CBGCLEARBUTTON` — excom
- `CBGREMOVERANGE int zmin, int zmax` — excom
- `CBGREMOVEBMAP` — excom
- `CBGSETBMAPG int ID` — excom
- `CBGSETBUTTONSPRITE int button, str spriteName, str spriteNameB, int x, int y,int zdepth` — excom
- `SETANIMETIMER int time` — excom
- `CLEARTEXTBOX` — excom
- `STRDATA` — excom
- `STOPCALLTRAIN` — excom

From **eramaerb** (eramaker-basic subset mentioned in the tutorial's code fences, n=41; all are eramaker-era commands, a strict subset of excom):

- `ABL` — eramaerb
- `ADDCHARA` — eramaerb
- `AIUAIUAIUAIUAIUAIUAIUAIUAIUAIU` — eramaerb
- `BARL` — eramaerb
- `BREAK` — eramaerb
- `CALL` — eramaerb
- `CONTINUE` — eramaerb
- `DAY` — eramaerb
- `DELCHARA` — eramaerb
- `DRAWLINE` — eramaerb
- `ELSE` — eramaerb
- `ELSEIF` — eramaerb
- `ENDIF` — eramaerb
- `EXP` — eramaerb
- `FLAG` — eramaerb
- `GOTO` — eramaerb
- `INPUT` — eramaerb
- `INPUTS` — eramaerb
- `JUMP` — eramaerb
- `MONEY` — eramaerb
- `NAME` — eramaerb
- `PRINT` — eramaerb
- `PRINTFORML` — eramaerb
- `PRINTFORMSW` — eramaerb
- `PRINTFORMW` — eramaerb
- `PRINTL` — eramaerb
- `PRINTS` — eramaerb
- `PRINTSL` — eramaerb
- `PRINTV` — eramaerb
- `PRINTVL` — eramaerb
- `PRINTW` — eramaerb
- `QUIT` — eramaerb
- `REND` — eramaerb
- `REPEAT` — eramaerb
- `RESTART` — eramaerb
- `RETURN` — eramaerb
- `SIF` — eramaerb
- `STR` — eramaerb
- `TIME` — eramaerb
- `TIMES` — eramaerb
- `WAIT` — eramaerb

## (b) In-expression functions (式中関数)

From **exmeth** (`##### <type> NAME(args) [ edit ]`, n=152):

- `int GETCHARA(int no)` — exmeth
- `int GETSPCHARA(int no)` — exmeth
- `int FINDCHARA(var key, ? value, int start = 0, int end = ※)` — exmeth
- `int FINDLASTCHARA(var key, ? value, int start = 0, int end = ※)` — exmeth
- `str CSVNAME(int no)` — exmeth
- `str CSVCALLNAME(int no)` — exmeth
- `str CSVNICKNAME(int no)` — exmeth
- `str CSVMASTERNAME(int no)` — exmeth
- `str CSVCSTR(int no, int index)` — exmeth
- `int CSVBASE(int no, int index)` — exmeth
- `int CSVABL(int no, int index)` — exmeth
- `int CSVTALENT(int no, int index)` — exmeth
- `int CSVMARK(int no, int index)` — exmeth
- `int CSVEXP(int no, int index)` — exmeth
- `int CSVRELATION(int no, int index)` — exmeth
- `int CSVJUEL(int no, int index)` — exmeth
- `int CSVEQUIP(int no, int index)` — exmeth
- `int CSVCFLAG(int no, int index)` — exmeth
- `int EXISTCSV(int no)` — exmeth
- `int GETNUM(var key, str name)` — exmeth
- `int STRLENS(str s)` — exmeth
- `int STRLENSU(str s)` — exmeth
- `str SUBSTRING(str s, int start = 0, int length = -1)` — exmeth
- `str SUBSTRINGU(str s, int start = 0, int length = -1)` — exmeth
- `str CHARATU(str s, int position = 0)` — exmeth
- `int STRFIND(str str, str find, int start = 0)` — exmeth
- `int STRFINDU(str str, str find, int start = 0)` — exmeth
- `int STRCOUNT(str input, str match)` — exmeth
- `str UNICODE(int value)` — exmeth
- `int ENCODETOUNI(str value, int position = 0)` — exmeth
- `str REPLACE(str source, str match, str newvalue)` — exmeth
- `str ESCAPE(str value)` — exmeth
- `int VARSIZE(str name, int dim = 0)` — exmeth
- `int GETTIME()` — exmeth
- `str GETTIMES()` — exmeth
- `int GETMILLISECOND()` — exmeth
- `int GETSECOND()` — exmeth
- `int CHKFONT(str fontname)` — exmeth
- `int POWER(int x, int y)` — exmeth
- `int RAND(int min = 0, int max)` — exmeth
- `int ABS(int n)` — exmeth
- `int SIGN(int n)` — exmeth
- `int MAX(int n, int m...)` — exmeth
- `int MIN(int n, int m...)` — exmeth
- `int LIMIT(int value, int min, int max)` — exmeth
- `int INRANGE(int value, int min, int max)` — exmeth
- `int SQRT(int n)` — exmeth
- `int GETBIT(int n, int m)` — exmeth
- `int CBRT(int value)` — exmeth
- `int LOG(int value)` — exmeth
- `int LOG10(int value)` — exmeth
- `int EXPONENT(int value)` — exmeth
- `str GETFONT()` — exmeth
- `int GETCOLOR()` — exmeth
- `int GETDEFCOLOR()` — exmeth
- `int GETBGCOLOR()` — exmeth
- `int GETDEFBGCOLOR()` — exmeth
- `int GETFOCUSCOLOR()` — exmeth
- `int GETSTYLE()` — exmeth
- `str CURRENTALIGN()` — exmeth
- `int CURRENTREDRAW()` — exmeth
- `str TOSTR(int value, str format = "")` — exmeth
- `int GETPALAMLV(int value, int maxLV)` — exmeth
- `int GETEXPLV(int value, int maxLV)` — exmeth
- `str TOUPPER (str value)` — exmeth
- `str TOLOWER (str value)` — exmeth
- `str TOHALF (str value)` — exmeth
- `str TOFULL (str value)` — exmeth
- `int SUMARRAY(var array, int start = 0, int end = ※)` — exmeth
- `int MATCH(var array, ? value, int start = 0, int end = ※)` — exmeth
- `int MAXARRAY(var array, int start = 0, int end = ※)` — exmeth
- `int MINARRAY(var array, int start = 0, int end = ※)` — exmeth
- `int SUMCARRAY(var carray, int start = 0, int end = CHARANUM)` — exmeth
- `int CMATCH(var carray, ? value, int start = 0, int end = CHARANUM)` — exmeth
- `int MAXCARRAY(var carray, int start = 0, int end = CHARANUM)` — exmeth
- `int MINCARRAY(var carray, int start = 0, int end = CHARANUM)` — exmeth
- `int ISNUMERIC(str value)` — exmeth
- `int TOINT(str value)` — exmeth
- `int CHKDATA(int value)` — exmeth
- `int CHKCHARADATA(str filename)` — exmeth
- `int FIND_CHARADATA(str filename)` — exmeth
- `int SAVENOS()` — exmeth
- `int PRINTCPERLINE()` — exmeth
- `int LINEISEMPTY()` — exmeth
- `int GROUPMATCH(? key, ? value1, ? value2...)` — exmeth
- `int NOSAMES(? value1, ? value2...)` — exmeth
- `int ALLSAMES(? value1, ? value2...)` — exmeth
- `int ISSKIP()` — exmeth
- `int MESSKIP()` — exmeth
- `str CONVERT(int value, ※)` — exmeth
- `str MONEYSTR(int value, str format = "")` — exmeth
- `int FINDELEMENT (var array, ? value, int start = 0, int end = ※, int flag)` — exmeth
- `int FINDLASTELEMENT (var array, ? value, int start = 0, int end = ※, int flag)` — exmeth
- `str BARSTR(int value, int max, int length)` — exmeth
- `int COLOR_FROMNAME(str colorname)` — exmeth
- `int COLOR_FROMRGB(int r, int g, int b)` — exmeth
- `str GETLINESTR(str letter)` — exmeth
- `int PRINTCLENGTH()` — exmeth
- `str STRFORM(str value)` — exmeth
- `int GETCONFIG(str value)` — exmeth
- `str GETCONFIGS(str value)` — exmeth
- `str HTML_POPPRINTINGSTR()` — exmeth
- `str HTML_GETPRINTEDSTR(int lineNo)` — exmeth
- `str HTML_ESCAPE(str value)` — exmeth
- `str HTML_TOPLAINTEXT(str value)` — exmeth
- `int SAVETEXT(str text, int fileNo{, int force_savdir, int force_UTF8})` — exmeth
- `str LOADTEXT(int fileNo{, int force_savdir, int force_UTF8})` — exmeth
- `int GETKEY(int vkey)` — exmeth
- `int GETKEYTRIGGERED(int vkey)` — exmeth
- `int CLIENTWIDTH ()` — exmeth
- `int CLIENTHEIGHT ()` — exmeth
- `int MOUSEX()` — exmeth
- `int MOUSEY()` — exmeth
- `int ISACTIVE()` — exmeth
- `int SPRITECREATED(str spriteName)` — exmeth
- `int SPRITEWIDTH(str spriteName)` — exmeth
- `int SPRITEHEIGHT(str spriteName)` — exmeth
- `int SPRITEPOSX(str spriteName)` — exmeth
- `int SPRITEPOSY(str spriteName)` — exmeth
- `int SPRITESETPOS(str spriteName, int posx, int posy)` — exmeth
- `int SPRITEMOVE(str spriteName, int movex, int movey)` — exmeth
- `int GCREATE(int ID, int width, int height)` — exmeth
- `int GCREATEFROMFILE(int ID, str filepath)` — exmeth
- `int GDISPOSE(int ID)` — exmeth
- `int GCLEAR(int ID, int cARGB)` — exmeth
- `int GFILLRECTANGLE(int ID, int x, int y, int width, int height)` — exmeth
- `int GDRAWG(int destID, int srcID, int destX, int destY, int destWidth, int destHeight, int srcX, int srcY, int srcWidth, int srcHeight)` — exmeth
- `int GDRAWGWITHMASK(int destID, int srcID, int maskID, int destX, int destY)` — exmeth
- `int GDRAWSPRITE(int ID, str sprName)` — exmeth
- `int GSETCOLOR(int ID, int cARGB, int x, int y)` — exmeth
- `int GSETBRUSH(int ID, int cARGB)` — exmeth
- `int GSETFONT(int ID, str fontName, int fontSize)` — exmeth
- `int GSETPEN(int ID, int cARGB, int penWidth)` — exmeth
- `int GCREATED(int ID)` — exmeth
- `int GWIDTH(int ID)` — exmeth
- `int GHEIGHT(int ID)` — exmeth
- `int GGETCOLOR(int ID, int x, int y)` — exmeth
- `int GSAVE(int ID, int fileNo)` — exmeth
- `int GLOAD(int ID, int fileNo)` — exmeth
- `int SPRITECREATE(str spriteName, int gID)` — exmeth
- `int SPRITEANIMECREATE(str spriteName, int width, int height)` — exmeth
- `int SPRITEANIMEADDFRAME(string spriteName, int gID, int x, int y, int width, int height, int offsetx, int offsety, int delay)` — exmeth
- `int SPRITEDISPOSE(string spriteName)` — exmeth
- `int SPRITEGETCOLOR(string spriteName, int x, int y)` — exmeth
- `int CBGSETG(int ID, int x, int y, int zdepth)` — exmeth
- `int CBGSETSPRITE(str spriteName, int x, int y, int zdepth)` — exmeth
- `int CBGCLEAR()` — exmeth
- `int CBGCLEARBUTTON()` — exmeth
- `int CBGREMOVERANGE(int zmin, int zmax)` — exmeth
- `int CBGREMOVEBMAP()` — exmeth
- `int CBGSETBMAPG(int ID)` — exmeth
- `int CBGSETBUTTONSPRITE(int button, str spriteName, str spriteNameB, int x, int y,int zdepth)` — exmeth

## (c) Variables and constants

From **exvar** (`##### NAME` headings + the two spec tables, n=122). Type is from exvar's tables (整数=int, 文字列=str, 数値=numeric); `[const]`/`[savedata]` derive from the セーブ/禁止 columns where present.

| name | type / source |
|---|---|
| ABL | 数値 · array=キャラ＋ー次元 · save=○ — exvar |
| ABLNAME | 文字列 · array=一次元 · save=× — exvar |
| ARG | 整数 · array=一次元 · save=× — exvar |
| ARGS | 文字列 · array=一次元 · save=× — exvar |
| ASSI | 整数 · array=一次元 · save=○ — exvar |
| ASSIPLAY | 整数 · array=一次元 · save=○ — exvar |
| BASE | 数値 · array=キャラ＋ー次元 · save=○ — exvar |
| BASENAME | 文字列 · array=一次元 · save=× — exvar |
| BOUGHT | 整数 · array=一次元 · save=○ — exvar |
| CALLNAME | 文字列 · array=キャラ＋無次元 · save=○ — exvar |
| CDFLAG | 整数 · array=キャラ＋二次元 · save=○ — exvar |
| CDFLAGNAME1 | 文字列 · array=一次元 · save=× — exvar |
| CDFLAGNAME2 | 文字列 · array=一次元 · save=× — exvar |
| CDOWN | 整数 · array=キャラ＋ー次元 · save=○ — exvar |
| CFLAG | 数値 · array=キャラ＋ー次元 · save=○ — exvar |
| CFLAGNAME | 文字列 · array=一次元 · save=× — exvar |
| CHARANUM | 整数 · array=__無次元__ · save=__×__ — exvar |
| COUNT | 整数 · array=一次元 · save=○ — exvar |
| CSTR | 文字列 · array=キャラ＋ー次元 · save=○ — exvar |
| CSTRNAME | 文字列 · array=一次元 · save=× — exvar |
| CUP | 整数 · array=キャラ＋ー次元 · save=○ — exvar |
| DA | 整数 · array=二次元 · save=○ — exvar |
| DAY | 整数 · array=一次元 · save=○ — exvar |
| DB | 整数 · array=二次元 · save=○ — exvar |
| DC | 整数 · array=二次元 · save=○ — exvar |
| DD | 整数 · array=二次元 · save=○ — exvar |
| DE | 整数 · array=二次元 · save=○ — exvar |
| DITEMTYPE | 整数 · array=二次元 · save=○ — exvar |
| DOWN | 整数 · array=一次元 · save=○ — exvar |
| DOWNBASE | 整数 · array=キャラ＋ー次元 · save=○ — exvar |
| DRAWLINESTR | 文字列 · array=無次元 · save=× — exvar |
| EJAC | 整数 · array=一次元 · save=○ — exvar |
| EQUIP | 数値 · array=キャラ＋ー次元 · save=○ — exvar |
| EQUIPNAME | 文字列 · array=一次元 · save=× — exvar |
| EX | 数値 · array=キャラ＋ー次元 · save=○ — exvar |
| EXNAME | 文字列 · array=一次元 · save=× — exvar |
| EXP | 数値 · array=キャラ＋ー次元 · save=○ — exvar |
| EXPLV | 整数 · array=一次元 · save=○ — exvar |
| EXPNAME | 文字列 · array=一次元 · save=× — exvar |
| FLAG | 整数 · array=一次元 · save=○ — exvar |
| FLAGNAME | 文字列 · array=一次元 · save=× — exvar |
| GAMEBASE_ALLOWVERSION | 整数 · array=無次元 · save=× — exvar |
| GAMEBASE_AUTHOR | 文字列 · array=無次元 · save=× — exvar |
| GAMEBASE_DEFAULTCHARA | 整数 · array=無次元 · save=× — exvar |
| GAMEBASE_GAMECODE | 整数 · array=無次元 · save=× — exvar |
| GAMEBASE_INFO | 文字列 · array=無次元 · save=× — exvar |
| GAMEBASE_NOITEM | 整数 · array=無次元 · save=× — exvar |
| GAMEBASE_TITLE | 文字列 · array=無次元 · save=× — exvar |
| GAMEBASE_VERSION | 整数 · array=無次元 · save=× — exvar |
| GAMEBASE_YEAR | 文字列 · array=無次元 · save=× — exvar |
| GLOBAL | 整数 · array=一次元 · save=※ — exvar |
| GLOBALNAME | 文字列 · array=一次元 · save=× — exvar |
| GLOBALS | 文字列 · array=一次元 · save=※ — exvar |
| GLOBALSNAME | 文字列 · array=一次元 · save=× — exvar |
| GOTJUEL | 数値 · array=キャラ＋ー次元 · save=○ — exvar |
| ISASSI | 数値 · array=キャラ＋無次元 · save=○ — exvar |
| ISTIMEOUT | 整数 · array=無次元 · save=× — exvar |
| ITEM | 整数 · array=一次元 · save=○ — exvar |
| ITEMNAME | 文字列 · array=一次元 · save=× — exvar |
| ITEMPRICE | 整数 · array=一次元 · save=× — exvar |
| ITEMSALES | 整数 · array=一次元 · save=○ — exvar |
| JUEL | 数値 · array=キャラ＋ー次元 · save=○ — exvar |
| LASTLOAD_ | (heading only) — exvar |
| LASTLOAD_NO | 整数 · array=無次元 · save=× — exvar |
| LASTLOAD_TEXT | 文字列 · array=無次元 · save=× — exvar |
| LASTLOAD_VERSION | 整数 · array=無次元 · save=× — exvar |
| LINECOUNT | 整数 · array=無次元 · save=× — exvar |
| LOCAL | 整数 · array=一次元 · save=× — exvar |
| LOCALS | 文字列 · array=一次元 · save=× — exvar |
| LOSEBASE | 整数 · array=一次元 · save=○ — exvar |
| MARK | 数値 · array=キャラ＋ー次元 · save=○ — exvar |
| MARKNAME | 文字列 · array=一次元 · save=× — exvar |
| MASTER | 整数 · array=一次元 · save=○ — exvar |
| MASTERNAME | 文字列 · array=キャラ＋無次元 · save=○ — exvar |
| MAXBASE | 数値 · array=キャラ＋ー次元 · save=○ — exvar |
| MONEY | 整数 · array=一次元 · save=○ — exvar |
| MONEYLABEL | 文字列 · array=無次元 · save=× — exvar |
| NAME | 文字列 · array=キャラ＋無次元 · save=○ — exvar |
| NEXTCOM | 整数 · array=一次元 · save=○ — exvar |
| NICKNAME | 文字列 · array=キャラ＋無次元 · save=○ — exvar |
| NO | 数値 · array=キャラ＋無次元 · save=○ — exvar |
| NOITEM | 整数 · array=一次元 · save=○ — exvar |
| NOWEX | 数値 · array=キャラ＋ー次元 · save=○ — exvar |
| PALAM | 数値 · array=キャラ＋ー次元 · save=○ — exvar |
| PALAMLV | 整数 · array=一次元 · save=○ — exvar |
| PALAMNAME | 文字列 · array=一次元 · save=× — exvar |
| PBAND | 整数 · array=一次元 · save=○ — exvar |
| PLAYER | 整数 · array=一次元 · save=○ — exvar |
| PREVCOM | 整数 · array=一次元 · save=○ — exvar |
| RAND | 整数 · array=__無次元__ · save=__×__ — exvar |
| RANDDATA | 整数 · array=一次元 · save=○ — exvar |
| RELATION | 数値 · array=キャラ＋ー次元 · save=○ — exvar |
| RESULT | 整数 · array=一次元 · save=○ — exvar |
| RESULTS | 文字列 · array=一次元 · save=× — exvar |
| SAVEDATA_TEXT | 文字列 · array=無次元 · save=※ — exvar |
| SAVESTR | 文字列 · array=一次元 · save=○ — exvar |
| SAVESTRNAME | 文字列 · array=一次元 · save=× — exvar |
| SELECTCOM | 整数 · array=一次元 · save=○ — exvar |
| SOURCE | 数値 · array=キャラ＋ー次元 · save=○ — exvar |
| SOURCENAME | 文字列 · array=一次元 · save=× — exvar |
| STAIN | 数値 · array=キャラ＋ー次元 · save=○ — exvar |
| STAINNAME | 文字列 · array=一次元 · save=× — exvar |
| STR | 文字列 · array=一次元 · save=× — exvar |
| STRNAME | 文字列 · array=一次元 · save=× — exvar |
| TA | 整数 · array=三次元 · save=○ — exvar |
| TALENT | 数値 · array=キャラ＋ー次元 · save=○ — exvar |
| TALENTNAME | 文字列 · array=一次元 · save=× — exvar |
| TARGET | 整数 · array=一次元 · save=○ — exvar |
| TB | 整数 · array=三次元 · save=○ — exvar |
| TCVAR | 整数 · array=キャラ＋ー次元 · save=○ — exvar |
| TCVARNAME | 文字列 · array=一次元 · save=× — exvar |
| TEQUIP | 数値 · array=キャラ＋ー次元 · save=○ — exvar |
| TEQUIPNAME | 文字列 · array=一次元 · save=× — exvar |
| TFLAG | 整数 · array=一次元 · save=○ — exvar |
| TFLAGNAME | 文字列 · array=一次元 · save=× — exvar |
| TIME | 整数 · array=一次元 · save=○ — exvar |
| TRAINNAME | 文字列 · array=一次元 · save=× — exvar |
| TSTR | 文字列 · array=一次元 · save=× — exvar |
| TSTRNAME | 文字列 · array=一次元 · save=× — exvar |
| UP | 整数 · array=一次元 · save=○ — exvar |
| WINDOW_TITLE | 文字列 · array=無次元 · save=× — exvar |
| gamebase | (heading only) — exvar |

From **eramavar** (eramaker-era list, n=57): A, COUNT, RESULT, RESULTS, DAY, TIME, MONEY, MASTER, TARGET, ASSI, PLAYER, CHARANUM, ASSIPLAY, SELECTCOM, PREVCOM, LOSEBASE, UP, DOWN, PALAMLV, EXPLV, EJAC, FLAG, TFLAG, NO, BASE, MAXBASE, ABL, TALENT, EXP, MARK, RELATION, JUEL, CFLAG, ISASSI, NAME, CALLNAME, TEQUIP, PALAM, STAIN, EX, SOURCE, NOWEX, GOTJUEL, ITEM, ITEMSALES, BOUGHT, NOITEM, PBAND, ABLNAME, TALENTNAME, EXPNAME, MARKNAME, PALAMNAME, ITEMNAME, STR, SAVESTR, RAND

## (d) Preprocessor directives

### `#…` directives (exfunc / ERH)

- `#ONLY` — exfunc
- `#FUNCTION` — exfunc
- `#FUNCTIONS` — exfunc
- `#LOCALSIZE <定数式>` — exfunc
- `#LOCALSSIZE <定数式>` — exfunc
- `#DIM` — exfunc
- `#DIMS` — exfunc
- `#DEFINE` — exfunc

### Bracket directives (exfunc) — quoted verbatim:

- `[SKIPSTART]` — exfunc
- `[SKIPEND]` — exfunc
- `[IF XXX]` — exfunc
- `[ELSEIF XXX]` — exfunc
- `[ELSE]` — exfunc
- `[ENDIF]` — exfunc
- `[IF_DEBUG]` — exfunc
- `[IF_NDEBUG]` — exfunc
- `[ENDIF]` — exfunc

> Bracket-family semantics (verbatim from exfunc): `[SKIPSTART]`…`[SKIPEND]` lines are not executed; `[IF XXX]`/`[ELSEIF XXX]`/`[ELSE]`/`[ENDIF]` branch on whether macro XXX is #DEFINEd; `[IF_DEBUG]`…`[ENDIF]` runs only in debug mode; `[IF_NDEBUG]` is its inverse.

## (e) Config keys

From **config** (`##### <name>` headings, n=75; includes obsolete/removed items; note erars implements only a subset):

- マウスを使用する — config
- メニューを使用する — config
- デバッグコマンドを使用する — config
- 多重起動を許可する — config
- キーボードマクロを使用する — config
- オートセーブを行なう — config
- セーブデータをsavフォルダ内に作成する — config
- 履歴ログの行数 — config
- 無限ループ警告までのミリ秒数 — config
- 使用するセーブデータ数 — config
- 関連づけるテキストエディタ — config
- コマンドライン引数 — config
- 描画インターフェース — config
- （イメージバッファを使用する） — config
- （描画にGDI+を用いる） — config
- フレーム毎秒 — config
- （最大スキップフレーム数） — config
- PRINTCを並べる数 — config
- PRINTCの文字数 — config
- ボタンの途中で行を折りかえさない — config
- ウィンドウ幅 — config
- ウィンドウ高さ — config
- ウィンドウの高さを可変にする — config
- 起動時にウィンドウを最大化する — config
- 起動時のウィンドウの位置を固定する — config
- ウィンドウ位置X — config
- ウィンドウ位置Y — config
- スクロールの行数 — config
- 背景色 — config
- 文字色 — config
- 選択中文字色 — config
- 履歴文字色 — config
- フォント名 — config
- フォントサイズ — config
- 一行の高さ — config
- 大文字小文字の違いを無視する — config
- _Rename.csvを利用する — config
- _Replace.csvを利用する — config
- サブディレクトリを検索する — config
- 読み込み順をファイル名順にソートする — config
- システム関数の上書きを許可する — config
- システム関数が上書きされたとき警告を表示する — config
- 同名の非イベント関数が複数定義されたとき警告する — config
- 全角スペースをホワイトスペースに含める — config
- 内部で使用する東アジア言語 — config
- FORM中の三連記号を展開しない — config
- セーブデータをバイナリ形式で保存する — config
- セーブデータをUTF-8で保存する — config
- ONEINPUT系命令でマウスによる2文字以上の入力を許可する — config
- 解釈不能な行があっても実行する — config
- CALLNAMEが空文字列の時にNAMEを代入する — config
- 擬似変数RANDの仕様をeramakerに合わせる — config
- 関数・属性については大文字小文字を無視しない — config
- イベント関数のCALLを許可する — config
- ver1739以前の非ボタン折り返しを再現する — config
- （DRAWLINEを常に新しい行で行う） — config
- ユーザー関数の全ての引数の省略を許可する — config
- ユーザー関数の引数に自動的にTOSTRを補完する — config
- SPキャラを使用する — config
- eramaker互換性に関する警告を表示する — config
- ロード時にレポートを表示する — config
- ロード時に引数を解析する — config
- 表示する最低警告レベル — config
- （ロード時にFORM文字列を解析する） — config
- 呼び出されなかった関数を無視する — config
- 関数が見つからない警告の扱い — config
- 関数が呼び出されなかった警告の扱い — config
- （指定したファイル中の警告を無視する） — config
- 起動時にデバッグウインドウを表示する — config
- デバッグウインドウを最前面に表示する — config
- デバッグウインドウ幅 — config
- デバッグウインドウ高さ — config
- デバッグウインドウ位置を指定する — config
- デバッグウィンドウ位置X — config
- デバッグウィンドウ位置Y — config

**exconfig** defines no new keys — it only describes `_fixed.config` / `_default.config`, which force/seed the same key space as emuera.config (config above).

## (f) Debug commands (debugcom)

- `@CONFIG` — debugcom
- `@DEBUG` — debugcom
- `@EXIT` — debugcom
- `@OUTPUT` — debugcom
- `@REBOOT` — debugcom

> debugcom: besides the five above, *any* normal ERB instruction/expression is accepted as a debug command (minus flow-control/input ones). Case sensitivity follows the 'Ignore capitalization' config.

## (g) CSV files & column layouts

### eramaker-era CSV files (eramacsv, n=10):

- Abl.csv
- CharaXX.csv
- Exp.csv
- GameBase.csv
- Item.csv
- Mark.csv
- Palam.csv
- Str.csv
- Talent.csv
- Train.csv

Column layouts (eramacsv):
- **GameBase.csv**: コード, バージョン, タイトル, 作者, 製作年, 追加情報, 最初からいるキャラ, アイテムなし, バージョン違い認める
- **Palam.csv**: パラメータ番号, パラメータ名
- **Abl.csv**: 能力番号, 能力名
- **Talent.csv**: 素質番号, 素質名
- **Mark.csv**: 刻印番号, 刻印名
- **Exp.csv**: 経験番号, 経験名
- **Train.csv**: コマンド番号, コマンド名
- **Item.csv**: アイテム番号, アイテム名, 値段
- **Str.csv**: 文字列番号, 文字列
- **CharaXX.csv**: 番号, 名前, 呼び名, 基礎, 能力, 素質, 経験, 相性, 助手, フラグ

### `_replace.csv` settings (replace), n=16:

- お金の単位
- 単位の位置
- 起動時簡略表示
- 販売アイテム数
- DRAWLINE文字
- BAR文字1
- BAR文字2
- システムメニュー0
- システムメニュー1
- COM_ABLE初期値
- 汚れの初期値
- 時間切れ表示
- EXPLVの初期値
- PALAMLVの初期値
- PBANDの初期値
- RELATIONの初期値

### resources (image resource CSV format)

- Sprite: `リソース名, 元ファイル名, x, y, width, height, posx, posy`
- Animated sprite: header `リソース名, ANIME, width, height` + one frame line `リソース名, 元ファイル名, x, y, width, height, offsetx, offsety, delay` per frame

### CSV-backed array variables & their CSV sources (exetc entry)

CSV-backed array variables named in exetc: ABL, BASE, CDFLAG, CDOWN, CFLAG, CSTR, CUP, DOWN, DOWNBASE, EQUIP, EX, EXP, FLAG, GLOBAL, GLOBALS, GOTJUEL, ITEM, ITEMPRICE, ITEMSALES, JUEL, LOSEBASE, MARK, MAXBASE, NOWEX, PALAM, RELATION, SAVESTR, SOURCE, STAIN, STR, TALENT, TCVAR, TEQUIP, TFLAG, TSTR, UP
