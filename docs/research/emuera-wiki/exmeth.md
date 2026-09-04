# URL: https://wiki.eragames.rip/index.php/Emuera/exmeth
# fetch date: 2026-09-03

Contents 1 式中で使える関数 1.1 凡例 1.2 組み込み関数 1.2.1 int GETCHARA(int no) 1.2.2 int GETCHARA(int no, int flag = 0) 1.2.3 int GETSPCHARA(int no) 1.2.4 int FINDCHARA(var key, ? value, int start = 0, int end = ※) 1.2.5 int FINDLASTCHARA(var key, ? value, int start = 0, int end = ※) 1.2.6 str CSVNAME(int no) 1.2.7 str CSVCALLNAME(int no) 1.2.8 str CSVNICKNAME(int no) 1.2.9 str CSVMASTERNAME(int no) 1.2.10 str CSVCSTR(int no, int index) 1.2.11 int CSVBASE(int no, int index) 1.2.12 int CSVABL(int no, int index) 1.2.13 int CSVTALENT(int no, int index) 1.2.14 int CSVMARK(int no, int index) 1.2.15 int CSVEXP(int no, int index) 1.2.16 int CSVRELATION(int no, int index) 1.2.17 int CSVJUEL(int no, int index) 1.2.18 int CSVEQUIP(int no, int index) 1.2.19 int CSVCFLAG(int no, int index) 1.2.20 int EXISTCSV(int no) 1.2.21 str CSVNAME(int no, int flag = 0) 1.2.22 str CSVCALLNAME(int no, int flag = 0) 1.2.23 str CSVNICKNAME(int no, int flag = 0) 1.2.24 str CSVMASTERNAME(int no, int flag = 0) 1.2.25 str CSVCSTR(int no, int index, int flag = 0) 1.2.26 int CSVBASE(int no, int index, int flag = 0) 1.2.27 int CSVABL(int no, int index, int flag = 0) 1.2.28 int CSVTALENT(int no, int index, int flag = 0) 1.2.29 int CSVMARK(int no, int index, int flag = 0) 1.2.30 int CSVEXP(int no, int index, int flag = 0) 1.2.31 int CSVRELATION(int no, int index, int flag = 0) 1.2.32 int CSVJUEL(int no, int index, int flag = 0) 1.2.33 int CSVEQUIP(int no, int index, int flag = 0) 1.2.34 int CSVCFLAG(int no, int index, int flag = 0) 1.2.35 int EXISTCSV(int no, int flag = 0) 1.2.36 int GETNUM(var key, str name) 1.2.37 int STRLENS(str s) 1.2.38 int STRLENSU(str s) 1.2.39 str SUBSTRING(str s, int start = 0, int length = -1) 1.2.40 str SUBSTRINGU(str s, int start = 0, int length = -1) 1.2.41 str CHARATU(str s, int position = 0) 1.2.42 int STRFIND(str str, str find, int start = 0) 1.2.43 int STRFINDU(str str, str find, int start = 0) 1.2.44 int STRCOUNT(str input, str match) 1.2.45 str UNICODE(int value) 1.2.46 int ENCODETOUNI(str value, int position = 0) 1.2.47 str REPLACE(str source, str match, str newvalue) 1.2.48 str ESCAPE(str value) 1.2.49 int VARSIZE(str name, int dim = 0) 1.2.50 int GETTIME() 1.2.51 str GETTIMES() 1.2.52 int GETMILLISECOND() 1.2.53 int GETSECOND() 1.2.54 int CHKFONT(str fontname) 1.2.55 int POWER(int x, int y) 1.2.56 int RAND(int min = 0, int max) 1.2.57 int ABS(int n) 1.2.58 int SIGN(int n) 1.2.59 int MAX(int n, int m...) 1.2.60 int MIN(int n, int m...) 1.2.61 int LIMIT(int value, int min, int max) 1.2.62 int INRANGE(int value, int min, int max) 1.2.63 int SQRT(int n) 1.2.64 int GETBIT(int n, int m) 1.2.65 int CBRT(int value) 1.2.66 int LOG(int value) 1.2.67 int LOG10(int value) 1.2.68 int EXPONENT(int value) 1.2.69 str GETFONT() 1.2.70 int GETCOLOR() 1.2.71 int GETDEFCOLOR() 1.2.72 int GETBGCOLOR() 1.2.73 int GETDEFBGCOLOR() 1.2.74 int GETFOCUSCOLOR() 1.2.75 int GETSTYLE() 1.2.76 str CURRENTALIGN() 1.2.77 int CURRENTREDRAW() 1.2.78 str TOSTR(int value, str format = "") 1.2.79 int GETPALAMLV(int value, int maxLV) 1.2.80 int GETEXPLV(int value, int maxLV) 1.2.81 str TOUPPER (str value) 1.2.82 str TOLOWER (str value) 1.2.83 str TOHALF (str value) 1.2.84 str TOFULL (str value) 1.2.85 int SUMARRAY(var array, int start = 0, int end = ※) 1.2.86 int MATCH(var array, ? value, int start = 0, int end = ※) 1.2.87 int MAXARRAY(var array, int start = 0, int end = ※) 1.2.88 int MINARRAY(var array, int start = 0, int end = ※) 1.2.89 int SUMCARRAY(var carray, int start = 0, int end = CHARANUM) 1.2.90 int CMATCH(var carray, ? value, int start = 0, int end = CHARANUM) 1.2.91 int MAXCARRAY(var carray, int start = 0, int end = CHARANUM) 1.2.92 int MINCARRAY(var carray, int start = 0, int end = CHARANUM) 1.2.93 int ISNUMERIC(str value) 1.2.94 int TOINT(str value) 1.2.95 int CHKDATA(int value) 1.2.96 int CHKCHARADATA(str filename) 1.2.97 int FIND_CHARADATA(str filename) 1.2.98 int SAVENOS() 1.2.99 int PRINTCPERLINE() 1.2.100 int LINEISEMPTY() 1.2.101 int GROUPMATCH(? key, ? value1, ? value2...) 1.2.102 int NOSAMES(? value1, ? value2...) 1.2.103 int ALLSAMES(? value1, ? value2...) 1.2.104 int ISSKIP() 1.2.105 (int MOUSESKIP) 1.2.106 int MESSKIP() 1.2.107 str CONVERT(int value, ※) 1.2.108 str MONEYSTR(int value, str format = "") 1.2.109 int FINDELEMENT (var array, ? value, int start = 0, int end = ※, int flag) 1.2.110 int FINDLASTELEMENT (var array, ? value, int start = 0, int end = ※, int flag) 1.2.111 str BARSTR(int value, int max, int length) 1.2.112 int COLOR_FROMNAME(str colorname) 1.2.113 int COLOR_FROMRGB(int r, int g, int b) 1.2.114 INRANGEARRAY 1.2.115 INRANGECARRAY 1.2.116 str GETLINESTR(str letter) 1.2.117 int PRINTCLENGTH() 1.2.118 str STRFORM(str value) 1.2.119 int GETCONFIG(str value) 1.2.120 str GETCONFIGS(str value) 1.2.121 str HTML_POPPRINTINGSTR() 1.2.122 str HTML_GETPRINTEDSTR(int lineNo) 1.2.123 str HTML_ESCAPE(str value) 1.2.124 str HTML_TOPLAINTEXT(str value) 1.2.125 int SAVETEXT(str text, int fileNo{, int force_savdir, int force_UTF8}) 1.2.126 str LOADTEXT(int fileNo{, int force_savdir, int force_UTF8}) 1.2.127 int GETKEY(int vkey) 1.2.128 int GETKEYTRIGGERED(int vkey) 1.2.129 int CLIENTWIDTH () 1.2.130 int CLIENTHEIGHT () 1.2.131 int MOUSEX() 1.2.132 int MOUSEY() 1.2.133 int ISACTIVE() 1.2.134 int SPRITECREATED(str spriteName) 1.2.135 int SPRITEWIDTH(str spriteName) 1.2.136 int SPRITEHEIGHT(str spriteName) 1.2.137 int SPRITEPOSX(str spriteName) 1.2.138 int SPRITEPOSY(str spriteName) 1.2.139 int SPRITESETPOS(str spriteName, int posx, int posy) 1.2.140 int SPRITEMOVE(str spriteName, int movex, int movey) 1.2.141 int GCREATE(int ID, int width, int height) 1.2.142 int GCREATEFROMFILE(int ID, str filepath) 1.2.143 int GDISPOSE(int ID) 1.2.144 int GCLEAR(int ID, int cARGB) 1.2.145 int GFILLRECTANGLE(int ID, int x, int y, int width, int height) 1.2.146 int GDRAWG(int destID, int srcID, int destX, int destY, int destWidth, int destHeight, int srcX, int srcY, int srcWidth, int srcHeight) 1.2.147 int GDRAWG(int destID, int srcID, int destX, int destY, int destWidth, int destHeight, int srcX, int srcY, int srcWidth, int srcHeight, var cm) 1.2.148 int GDRAWGWITHMASK(int destID, int srcID, int maskID, int destX, int destY) 1.2.149 int GDRAWSPRITE(int ID, str sprName) 1.2.150 int GDRAWSPRITE(int ID, str sprName, int destX, int destY) 1.2.151 int GDRAWSPRITE(int ID, str sprName, int destX, int destY, int destWidth, int destHeight) 1.2.152 int GDRAWSPRITE(int ID, str sprName, int destX, int destY, int destWidth, int destHeight, var cm) 1.2.153 int GSETCOLOR(int ID, int cARGB, int x, int y) 1.2.154 int GSETBRUSH(int ID, int cARGB) 1.2.155 int GSETFONT(int ID, str fontName, int fontSize) 1.2.156 int GSETPEN(int ID, int cARGB, int penWidth) 1.2.157 int GCREATED(int ID) 1.2.158 int GWIDTH(int ID) 1.2.159 int GHEIGHT(int ID) 1.2.160 int GGETCOLOR(int ID, int x, int y) 1.2.161 int GSAVE(int ID, int fileNo) 1.2.162 int GLOAD(int ID, int fileNo) 1.2.163 int SPRITECREATE(str spriteName, int gID) 1.2.164 int SPRITECREATE(str spriteName, int gID, int x, int y, int width, int height) 1.2.165 int SPRITEANIMECREATE(str spriteName, int width, int height) 1.2.166 int SPRITEANIMEADDFRAME(string spriteName, int gID, int x, int y, int width, int height, int offsetx, int offsety, int delay) 1.2.167 int SPRITEDISPOSE(string spriteName) 1.2.168 int SPRITEGETCOLOR(string spriteName, int x, int y) 1.2.169 int CBGSETG(int ID, int x, int y, int zdepth) 1.2.170 int CBGSETSPRITE(str spriteName, int x, int y, int zdepth) 1.2.171 int CBGCLEAR() 1.2.172 int CBGCLEARBUTTON() 1.2.173 int CBGREMOVERANGE(int zmin, int zmax) 1.2.174 int CBGREMOVEBMAP() 1.2.175 int CBGSETBMAPG(int ID) 1.2.176 int CBGSETBUTTONSPRITE(int button, str spriteName, str spriteNameB, int x, int y,int zdepth) 1.2.177 int CBGSETBUTTONSPRITE(int button, str spriteName, str spriteNameB, int x, int y,int zdepth, str tooltipmes)

### 式中で使える関数 [ edit ]

このページで解説する文法は必須ではありません。 
 ERBで出来る全てのことは「式中で使える関数」を使わなくとも（スクリプトが多少長くなるだけで）実行できます。 
 いままでの文法に不便を感じないのであれば「式中で使える関数」を使用する必要はありません。
「式中で使える関数」はEmuera ver 1.712で追加された新しい文法です。 
これは多くのプログラミング言語で単に「関数」と呼ばれるものです。 
eramaker basicでは@～～で定義しCALLで呼び出すものを「関数」と呼んでいるため、Emueraでは新しい「関数」を「式中で使える関数」と呼びます。 
以下、誤解がない場合には「式中で使える関数」を単に関数と呼びます。 
 
また、他のページでは「式中で使える関数」を略して「式中関数」と呼ぶことがあります。 
しかし、他のプログラミング言語における式中関数（匿名関数）やインライン関数とは無関係です。 
組み込み関数ではない、ERB中で定義して使う「式中で使える関数」については[UserMeth ユーザー定義の式中関数]を参照してください。
「式中で使える関数」は以下のように使います。
```
	A = ABS(A)
	IF STRLENS(STR:0) > A
		LOCALS:0 = %SUBSTRING(STR:0, A, 1)%
	ENDIF
```
これは、 
Aの絶対値を求めてAに代入し、 
STR:0の文字列長がAよりも大きいならば、 
LOCALS:0にSTR:0のA番目の文字を代入する、 
というスクリプトです。
上記のスクリプトは以下のように「式中で使える関数」を使わない形で置き換えることができます。
```
	ABS A
	A = RESULT
	STRLENS STR:0
	IF RESULT > A
		SUBSTRING STR:0, A, 1
		LOCALS:0 = %RESULTS:0%
	ENDIF
```
RESULTとRESULTSに中間の値が代入されることを除けば、上と全く同じ動作をします。

#### 凡例 [ edit ]

以下の解説における記号の説明です。 
例えば、
```
	int STRLENS(str s)
	str SUBSTRING(str s, int start = 0, int length = -1)
```
などのうち、最初のintやstrは戻り値の型を表します。 
intであれば整数型、strであれば文字列型です。 
以下のスクリプトは1行目は正常に動作しますが、2行目はエラーになります。
```
	A = STRLENS("abc")
	A = SUBSTRING("abc", 0, 1)
```
SUBSTRINGの戻り値はstr、つまり文字列型なので、整数型の変数Aに代入することはできません。
文字列型を返す関数は代入こそできないものの、その他の点では文字列変数と同じように扱うことができます。
```
	STR = %SUBSTRING("abc", 0, 1)%
```
次のSTRLENSやSUBSTRINGが関数の名前になります。
()の中の文字、"str s"などは引数を表しています。 
引数が複数ある場合は,(カンマ)で区切られます。STRLENSには1つ、SUBSTRINGには3つの引数があります。
引数の最初の単語は引数の型です。 
STRLENSの引数は文字列型(str)です。 
SUBSTRINGは第1引数が文字列型(str)、第2引数と第3引数が整数型(int)です。
その次のstrやstart、lengthといった単語は引数の名前です。 
引数の名前は解説文の中で使うための便宜上の名前であり、特に意識する必要はありません。
引数の名前の後ろの = 0 などはその引数が省略可能であることと、省略した場合のディフォルト値を表します。 
以下のスクリプトはどの行も全て同じ意味になります。
```
	STR = SUBSTRING(RESULTS)
	STR = SUBSTRING(RESULTS, 0)
	STR = SUBSTRING(RESULTS, , -1)
	STR = SUBSTRING(RESULTS, 0, -1)
```
引数の省略の際に最後の引数ではなく途中の引数を省略する場合はSUBSTRING(RESULTS, , -1)のようにどの引数を省略したかがわかるように,を入れる必要があります。 
ただし、どの引数を省略されたかが明らかな場合は不要です。 
以下のスクリプトはどの行も全て同じ意味になります。
```
	;int RAND(int min = 0, int max)
	A = RAND(100)
	A = RAND( , 100)
	A = RAND(0, 100)
```
そのほか、
```
	int GETTIME()
```
は引数が無いことを表しています。この場合でもGETTIME()のように()は必須です（変数と区別するためです）
```
	int FINDCHARA(var key, ? value, int start = 0)
```
のうち、varは変数型を表します。ここにはTALENTなど変数を渡します。 
?は複数の型を受け入れることを示します。 
FINDCHARAでは第1引数に指定した変数によって第2引数に渡すべき型が変わります。
```
	int MAX(int n, int m...)
```
は引数がいくつでも良いことを表しています。
```
	M = MAX(A, B, C, D, E, F, G)
```
はA～Gの中で一番大きい数字をMに代入します。

#### 組み込み関数 [ edit ]

基本的に同名の命令と同じ動作をしますが、RESULTやRESULTSへの代入は行われません。 
また、一部には同名の命令と仕様が違うものがあります。
組み込み関数ではない、ERB中で定義して使う「式中で使える関数」については[UserMeth ユーザー定義の式中関数]を参照してください。

##### int GETCHARA(int no) [ edit ]

現在所有しているキャラの中にそのキャラがいるかを判定し、いればリスト上での位置（登録番号）を、いなければ-1を返します。 
リスト全体から特定のキャラの有無を確認したい場合に使えます。

##### int GETCHARA(int no, int flag = 0) [ edit ]

##### int GETSPCHARA(int no) [ edit ]

現在所有しているキャラの中にそのキャラがいるかを判定し、いればリスト上での位置（登録番号）を、いなければ-1を返します。 
リスト全体から特定のキャラの有無を確認したい場合に使えます。 
GETCHARAはCFLAG:0が0のキャラを、GETSPCHARAはCFLAG:0が0でないキャラを検索します。 
1734fよりGETCHARAについては第2引数に0以外を入れることにより非SP・SPキャラ両方を検索するようにできます。 
この場合において、非SP・SPキャラが両方ともいる場合には非SPキャラの位置を返します。

##### int FINDCHARA(var key, ? value, int start = 0, int end = ※) [ edit ]

##### int FINDLASTCHARA(var key, ? value, int start = 0, int end = ※) [ edit ]

キャラクタ変数keyとその値valueを指定し、変数がその値であるキャラクタの登録番号を返します。 
見つからなかった場合は-1を返します。 
複数見つかった場合、FINDCHARAは最初にヒットしたキャラを、 
FINDLASTCHARAは最後にヒットしたキャラを結果とします。 
start以上end未満の登録番号のキャラを検索範囲とします。 
endを省略した場合、start以降のすべてキャラが検索範囲です。 
ただし、検索範囲がキャラクタ数の範囲を超える場合はエラーとなります。
```
	X = -1
	WHILE 1
		X = FINDCHARA(CFLAG:10, 123, X + 1)
		SIF X < 0
			BREAK
		PRINTFORML %NAME:X%
	WEND
```
上記のスクリプトはCFLAG:10が123であるキャラクタの名前を全て表示します。 
キャラクタ変数はNOのような非配列変数でも、NAMEのような文字列変数でもかまいません。 
変数に文字列型変数を指定した場合、第2引数valueも文字列型でなければなりません。

##### str CSVNAME(int no) [ edit ]

##### str CSVCALLNAME(int no) [ edit ]

##### str CSVNICKNAME(int no) [ edit ]

##### str CSVMASTERNAME(int no) [ edit ]

CSVで定義されたNAME、CALLNAME、NICKNAME、MASTERNAMEを直接呼び出す関数です。 
第一引数noがキャラ番号(NOの方)です。

##### str CSVCSTR(int no, int index) [ edit ]

##### int CSVBASE(int no, int index) [ edit ]

##### int CSVABL(int no, int index) [ edit ]

##### int CSVTALENT(int no, int index) [ edit ]

##### int CSVMARK(int no, int index) [ edit ]

##### int CSVEXP(int no, int index) [ edit ]

##### int CSVRELATION(int no, int index) [ edit ]

##### int CSVJUEL(int no, int index) [ edit ]

##### int CSVEQUIP(int no, int index) [ edit ]

##### int CSVCFLAG(int no, int index) [ edit ]

CSVで定義された値を直接呼び出す関数です。 
第一引数noがキャラ番号、第二引数indexが各変数のインデックスです。 
CSVCSTRの戻り値は文字列型、その他は整数型です。

##### int EXISTCSV(int no) [ edit ]

対応するキャラが定義されているかどうかをチェックします。 
定義されていれば1、されていなければ0を返します。 
ADDCHARA noがエラーにならずに実行できるかどうかを調べることができます。

##### str CSVNAME(int no, int flag = 0) [ edit ]

##### str CSVCALLNAME(int no, int flag = 0) [ edit ]

##### str CSVNICKNAME(int no, int flag = 0) [ edit ]

##### str CSVMASTERNAME(int no, int flag = 0) [ edit ]

CSVで定義されたNAME、CALLNAME、NICKNAME、MASTERNAMEを直接呼び出す関数です。 
第一引数noがキャラ番号(NOの方)、第二引数flagがSPキャラフラグ(省略した場合は0、非SPキャラ)です。 
flag == 0のとき、通常キャラのNAME(CALLNAME、NICKNAME、MASTERNAME)を取得します。 
flag != 0以外ならSPキャラのNAME(CALLNAME、NICKNAME、MASTERNAME)を取得します。

##### str CSVCSTR(int no, int index, int flag = 0) [ edit ]

##### int CSVBASE(int no, int index, int flag = 0) [ edit ]

##### int CSVABL(int no, int index, int flag = 0) [ edit ]

##### int CSVTALENT(int no, int index, int flag = 0) [ edit ]

##### int CSVMARK(int no, int index, int flag = 0) [ edit ]

##### int CSVEXP(int no, int index, int flag = 0) [ edit ]

##### int CSVRELATION(int no, int index, int flag = 0) [ edit ]

##### int CSVJUEL(int no, int index, int flag = 0) [ edit ]

##### int CSVEQUIP(int no, int index, int flag = 0) [ edit ]

##### int CSVCFLAG(int no, int index, int flag = 0) [ edit ]

CSVで定義された値を直接呼び出す関数です。 
第一引数noがキャラ番号、第二引数indexが各変数のインデックス、第三引数flagがSPキャラフラグ(省略した場合は0、非SPキャラ)です。 
CSVCSTRの戻り値は文字列型、その他は整数型です。

##### int EXISTCSV(int no, int flag = 0) [ edit ]

対応するキャラが定義されているかどうかをチェックします。 
定義されていれば1、されていなければ0を返します。 
第一引数noがキャラ番号(NOの方)、第二引数flagがSPキャラフラグ(省略した場合は0、非SPキャラ)です。 
ADDCHARA no(またはADDSPCHARA no)がエラーにならずに実行できるかどうかを調べることができます。

##### int GETNUM(var key, str name) [ edit ]

各csvで定義されている名称からその数値を取得します。 
例えば、abl.csvで"2,技巧"が定義されていれば、GETNUM(ABL, "技巧")は2を返します。 
定義されていない場合は-1を返します。 
csvと変数の対応は[exetc Emueraの拡張文法#一般]のページの「文字列による配列変数の要素の指定」に準じます。

##### int STRLENS(str s) [ edit ]

引数strに指定された文字列の長さを測定し、返します。 
長さはSHIFT-JISでのバイト数です。つまり全角文字を2文字と数えます。

##### int STRLENSU(str s) [ edit ]

引数strに指定された文字列の長さを測定し、返します。 
長さは単純に文字の数です。つまり全角文字も1文字と数えます。

##### str SUBSTRING(str s, int start = 0, int length = -1) [ edit ]

sに指定された文字列の部分文字列を返します。 
startが0でない場合、指定された位置以降の文字列を返します。 
lengthが負の値でない場合、指定された長さの文字列を返します。 
いずれもSHIFT-JISでのバイト数です。つまり全角文字を2文字と数えます。

##### str SUBSTRINGU(str s, int start = 0, int length = -1) [ edit ]

SUBSTRINGのUnicode版です。違いは全角文字も1文字と数えることです。

##### str CHARATU(str s, int position = 0) [ edit ]

以下、私家改造版更新履歴より、
```
文字列中の任意の文字数目の文字を取得する式中関数CHARATU追加
　書式：CHARATU(<参照文字列>, [取得文字位置])
　内容：文字列の指定した位置の文字を取得する
　　　　処理系はユニコードとなります
```

##### int STRFIND(str str, str find, int start = 0) [ edit ]

同名の命令とほぼ同じです。 
strの中からfindの文字列を探し、見つかった場所の0から始まるインデックスを返します。 
インデックスは全角文字を2文字と数えます。
見つからなかった場合、-1を返します。 
startが指定された場合、startで指定されたインデックスから検索を開始します。 
startが指定されても、戻り値はstartからのインデックスではなく、全体のインデックスであることに注意してください。 
例えば以下のような結果になります。
```
	PRINTFORML {STRFIND("abcabc", "b")}
	PRINTFORML {STRFIND("abcabc", "b", 2)}
;結果
1
4
```

##### int STRFINDU(str str, str find, int start = 0) [ edit ]

以下、私家改造版更新履歴より、
```
STRFINDのユニコード版STRFINDU実装
　書式：STRFINDU(<検索対象>, <検索する文字列>{, <開始インデックス>})
　内容：STRFINDのユニコード版、返り値の文字の位置と開始インデックスがユニコードでのカウントになっている
```

##### int STRCOUNT(str input, str match) [ edit ]

以下、私家改造版更新履歴より、
```
文字列中の指定部分文字列の数を取得する命令STRCOUNT追加
　書式：STRCOUNT <検索対象文字列>, <検索文字列>
        STRCOUNT(<検索対象文字列>, <検索文字列>)
　　返り値、ヒットした数
　　ちなみに内部的には正規表現処理なので、検索文字列の書式はC#の正規表現の仕様に準じる
```

##### str UNICODE(int value) [ edit ]

引数の値に対応したunicodeの文字を返す命令です。 
例えば、以下のスクリプトは白抜きのハートマークを10個表示します。 
ただし、この関数ではサロゲートペアを扱うことはできません。 
また、フォントが対応していなければ表示できません。
```
UNICODE 0x2661
PRINTFORMW %UNICODE(0x2661) * 10%
```
なお、EmueraのUnicode対応は完全ではないことに注意してください。 
例えばEmueraはサロゲートペアを使用した場合、正確な動作は保証できません。

##### int ENCODETOUNI(str value, int position = 0) [ edit ]

以下、私家改造版更新履歴より、
```
ENCODETOUNIの式中関数版を追加
　書式：ENCODETOUNI(<参照文字列>{, [対象文字位置]})
　内容：文字列の対象位置の文字をユニコードのコード値に変換して返す
　　　　位置は省略可でその場合は0（先頭）を対象とします
```

##### str REPLACE(str source, str match, str newvalue) [ edit ]

以下、私家改造版更新履歴より、
```
文字列置換命令REPLACE実装
　書式：REPLACE(<置換対象文字列>, <置換対象パターン>, <置換後の文字列>)
　内容：置換対象文字列を置換対象パターンで検索し、ヒットしたら置換後の文字列で置き換える
　　内部処理は思いっきり正規表現です。第２引数はC#の正規表現の仕様に準じて動作します。
　　そのため、()や[]、$、/.*+等の正規表現で用いられる記号はエスケープ必須となります
```

##### str ESCAPE(str value) [ edit ]

以下、私家改造版更新履歴より、
```
文字列を正規表現用にエスケープする式中関数ESCAPEを追加
　書式：ESCAPE([文字列])
　内容：文字列が正規表現中で平文となるように、引数中の文字列の正規表現メタ文字をエスケープして返す
```

##### int VARSIZE(str name, int dim = 0) [ edit ]

同名の命令と少し挙動が違います。 
nameで指定した変数の配列のサイズ（要素数）を返します。 
同名の命令と異なり、
```
	X = VARSIZE("FLAG")
```
のような形で文字列として指定する必要があります。 
また、DITEMTYPEやTAなどの多次元配列変数のサイズを取得する場合、第2引数により次元を指定する必要があります（一番左の要素から順に、0, 1, 2となる）。 
多次元配列変数の要素数を一度に取得する場合についてはVARSIZE命令の方を用いてください。

##### int GETTIME() [ edit ]

同名の命令がRETURNに代入する値と同じです。 
現在の日時・時刻を数値で返します。 
現在日時が2009年3月28日13時5分23秒678ミリ秒であれば、"20090328130523678"を返します。

##### str GETTIMES() [ edit ]

同名の命令がRETURNSに代入する値と同じです。 
現在の日時・時刻を文字列で返します。 
現在日時が2009年3月28日13時5分23秒678ミリ秒であれば、"2009年03月28日 13:05:23"を返します。

##### int GETMILLISECOND() [ edit ]

西暦0001年1月1日からの経過時間をミリ秒単位で返します。 
GETTIME()の結果と異なり単純な加減算ができるので経過時間などを調べるには適しています。

##### int GETSECOND() [ edit ]

西暦0001年1月1日からの経過時間を秒単位で返します。
GETTIME()の結果と異なり単純な加減算ができるので経過時間などを調べるには適しています。

##### int CHKFONT(str fontname) [ edit ]

引数の名前のフォントがインストールされていれば1を、されていなければ0を返します。 
SETFONTの前に呼んでください。

##### int POWER(int x, int y) [ edit ]

同名の命令とほぼ同じです。 
xのy乗を返します。

##### int RAND(int min = 0, int max) [ edit ]

同名の変数とほぼ同じです。 
RAND(X)はRAND:Xと全く同じ動作をします。 
乱数生成器は全く同じで、RANDOMIZEやINITRANDによって乱数を制御できます。 
RAND関数では2つの引数を指定することができ、2つ指定された場合は1つ目の引数が乱数の最小値として使われます。 
この関数は0～18446744073709551615(2の64乗-1)の間で生成した乱数をmax-minで割った余りにminを加えて返します。 
したがって、maxはminより大きい（同じではだめ）値である必要があります。 
max-minが符号付64ビット整数(9223372036854775807)の最大値を超える場合にエラーになります。 
また、max-minが非常に大きい場合（1京くらい？）、余りを求める方法による偏りが無視できなくなります。

##### int ABS(int n) [ edit ]

指定された数値の絶対値を返します。

##### int SIGN(int n) [ edit ]

指定された数値の符号を返します。 
負の値なら-1、0なら0、正の値なら1を返します。

##### int MAX(int n, int m...) [ edit ]

指定された数値のうち、最大の数値を返します。 
引数は1つ以上、いくつでも指定できます。

##### int MIN(int n, int m...) [ edit ]

指定された数値のうち、最小の数値を返します。 
引数は1つ以上、いくつでも指定できます。

##### int LIMIT(int value, int min, int max) [ edit ]

valueの値を返します。ただし、valueがminより小さいならminを、maxより大きいならmaxを返します。 
例えばAに X - Y を代入したいが、代入後の値が0以上100以下であってほしい場合、通常は以下のように書きます。
```
A = X - Y
SIF A < 0
	A = 0
SIF A > 100
	A = 100
```
LIMIT関数を使うとこれを一行にまとめることができます。
```
A = LIMIT(X - Y, 0, 100)
```

##### int INRANGE(int value, int min, int max) [ edit ]

valueの値がmin以上max以下であれば1を、valueがminより小さい、またはmaxより大きいなら0を返します。

##### int SQRT(int n) [ edit ]

指定された数値の平方根を返します。 
端数は切り捨てます。

##### int GETBIT(int n, int m) [ edit ]

指定された数値nのm番目のビットを返します。 
mに指定できる値は0～63までで、範囲外の数値を指定するとエラーになります。 
mが定数の場合、例えば5であれば、
```
	A = GETBIT(X, 5)
	B = (X & 1p5) != 0
```
AとBは同じ結果になります。

##### int CBRT(int value) [ edit ]

##### int LOG(int value) [ edit ]

##### int LOG10(int value) [ edit ]

##### int EXPONENT(int value) [ edit ]

以下、私家改造版更新履歴より
```
数学関数を式中関数として追加
　　CBRT（三重根）
　　LOG（自然関数）
　　LOG10(常用対数）
　　EXPONENT（指数関数）
　　書式は全部：関数名(引数)
```
Emueraでは小数を扱うことができないため、使用の際には工夫を必要とします。

##### str GETFONT() [ edit ]

現在使用しているフォントの名前を取得します。 
これはSETFONT命令で指定した名前と同じです。 
SETFONT命令が行われていない時はemuera.configで指定されている標準のフォントの名前を取得します。

##### int GETCOLOR() [ edit ]

現在使用している文字色コードを取得します。 
戻り値は16進数で0xRRGGBBとなります。 
例えばオレンジ色(R,G,B) = (255, 128, 0)であれば0xFF8000(10進数で16744448)を返します。 
色と数字の対応はweb colorについて解説しているWebサイトを参考にするとよいでしょう。 
1.731での変更により、SETCOLOR命令もSETCOLOR 0xFF8000のような形式で指定できるようになりました。

##### int GETDEFCOLOR() [ edit ]

コンフィグで指定している文字色コードを取得します。 
基本的な仕様はGETCOLORと同様です。

##### int GETBGCOLOR() [ edit ]

現在使用している背景色コードを取得します。 
基本的な仕様はGETCOLORと同様です。

##### int GETDEFBGCOLOR() [ edit ]

コンフィグで指定している背景色コードを取得します。 
基本的な仕様はGETCOLORと同様です。

##### int GETFOCUSCOLOR() [ edit ]

コンフィグで指定している選択中文字色コードを取得します。 
基本的な仕様はGETCOLORと同様です。

##### int GETSTYLE() [ edit ]

現在のフォントのスタイル（太字、斜線など）を取得します。 
これはSETSTYLE命令で指定した値と同じです。 
SETSTYLE命令が行われていない時は0を返します。

##### str CURRENTALIGN() [ edit ]

現在のALIGNMENTを取得します。 
返値はALIGNMENT命令で指定した文字列と同じです（大文字）。 
ALIGNMENT命令が行われていない時は初期値のLEFTを返します。

##### int CURRENTREDRAW() [ edit ]

現在のREDRAW状態を取得します。 
返値は通常時は1、REDRAW命令を用いて描画を抑制した時は0になります。

##### str TOSTR(int value, str format = "") [ edit ]

数値を文字列に変換する命令です。 
変換したい数字を第一引数に、変換の書式を文字列で第二引数に指定します。 
第二引数は省略できますが、省略した場合はPRINTFORMの{}内などと同じように単に文字列になります。 
この関数は内部でC#のInt64.!ToString()関数を呼んでおり、C#と同じ書式指定ができます。
```
A = 438765
PRINTSL TOSTR(A)
PRINTSL TOSTR(A, "x")
PRINTSL TOSTR(A, "X")
PRINTSL TOSTR(A, "D8")
PRINTSL TOSTR(A, "X8")
PRINTSL TOSTR(A, "00000000")
PRINTSL TOSTR(A, "########")
PRINTSL TOSTR(A, "#,###")
PRINTSL TOSTR(A, "0000万0000")

;結果
438765　//標準
6b1ed　//"x" 16進数(小文字)
6B1ED　//"X" 16進数(大文字)
00438765　//"D8" 10進数 + 8桁
0006B1ED　//"X8" 16進数 + 8桁
00438765　//"00000000" 10進数 + 8桁
438765　//"########" 10進数
438,765　//"#,###" 3桁ごとに","
0043万8765　//"0000万0000" 8桁 + 4桁目に"万"
```
第二引数が適切でない場合、エラーになります。 
これ以上の詳細はC#の数値書式指定文字列について解説しているWebサイトを参考にして下さい。

##### int GETPALAMLV(int value, int maxLV) [ edit ]

##### int GETEXPLV(int value, int maxLV) [ edit ]

第1引数valueで指定した値とPALAMLV又はEXPLVの値を比較し、LVを返します。 
第2引数は調査する最大のLVを表します。 
"RESULT = GETPALAMLV(A, B)"の結果は
```
	RESULT = -1
	REPEAT B
		IF A < PARAMLV:(COUNT + 1)
			RESULT = COUNT
			BREAK
		ENDIF
	REND
	IF RESULT == -1
		RESULT = B
	ENDIF
```
と同じです。

##### str TOUPPER (str value) [ edit ]

##### str TOLOWER (str value) [ edit ]

##### str TOHALF (str value) [ edit ]

##### str TOFULL (str value) [ edit ]

引数の文字列に特定の変換をかけた結果を返す関数です。 
TOUPPERはアルファベットを大文字にした結果を返し、TOLOWERは小文字化した結果を返します。 
TOHALFは全角文字を半角にしますが、対応する半角文字がない全角文字はそのままです。 
TOFULLは半角文字を全角にします。

##### int SUMARRAY(var array, int start = 0, int end = ※) [ edit ]

配列の値の総和を返す関数です。 
arrayに総和を求めたい1次元数値型配列を指定し、start以上end未満の要素の範囲を合計します。 
endを省略した場合、配列の最後までを合計します。 
RESULT = SUMARRAY(X, A, B)の結果は
```
	RESULT = 0
	FOR COUNT, A, B
		RESULT += X:COUNT
	REND
```
と同等です。 
加算される値はX:(B - 1)まででX:Bが加算されないことに注意してください。 
arrayに指定できるものは数値型1次元配列変数のみで文字列変数や多次元配列は指定できません。 
arrayにCFLAGなどのキャラクタ配列を指定した場合、指定されたキャラについてのみ合計します。

##### int MATCH(var array, ? value, int start = 0, int end = ※) [ edit ]

arrayで指定した配列変数の中にvalueと一致する要素がいくつあるかを返す関数です。 
valueはarrayと同じ型である必要があります。 
arrayに検索対象となる1次元配列を指定し、start以上end未満の要素の範囲で検索します。 
endを省略した場合、配列の最後までを対象とします。 
RESULT = MATCH(X, Y, A, B)の結果は
```
	RESULT = 0
	FOR COUNT, A, B
		IF X:COUNT == Y
			RESULT += 1
		ENDIF
	REND
```
と同等です。（X、Yの代わりに文字列配列と文字列式を指定しても動作します） 
arrayに指定できるものは1次元配列変数のみで多次元配列は指定できません。 
arrayにCFLAGなどのキャラクタ配列を指定した場合、指定されたキャラについてのみ数え上げます。

##### int MAXARRAY(var array, int start = 0, int end = ※) [ edit ]

##### int MINARRAY(var array, int start = 0, int end = ※) [ edit ]

配列の値の最大又は最小値を返す関数です。 
arrayに検索対象となる1次元配列を指定し、start以上end未満の要素の範囲で検索します。 
endを省略した場合、配列の最後までを対象とします。 
RESULT = MAXARRAY(X, A, B)の結果は
```
	RESULT = X:A
	FOR COUNT, A, B
		IF X:COUNT > RESULT
			RESULT = X:COUNT
		ENDIF
	REND
```
と同等です。 
arrayに指定できるものは数値型1次元配列変数のみで文字列変数や多次元配列は指定できません。 
arrayにCFLAGなどのキャラクタ配列を指定した場合、指定されたキャラについてのみ検索します。

##### int SUMCARRAY(var carray, int start = 0, int end = CHARANUM) [ edit ]

##### int CMATCH(var carray, ? value, int start = 0, int end = CHARANUM) [ edit ]

##### int MAXCARRAY(var carray, int start = 0, int end = CHARANUM) [ edit ]

##### int MINCARRAY(var carray, int start = 0, int end = CHARANUM) [ edit ]

キャラクタ間を走査するSUMARRAY、MATCH、MAXARRAY、MINARRAYのバリエーションです。 
arrayはキャラクタ配列変数でなければなりません。 
start, endはキャラクタ登録番号で指定します。 
例えば"RESULT = SUMCARRAY(CFLAG:2, A, B)"は以下のようも書けます。 
（BはCHARANUMより小さくなければなりません）
```
	RESULT = 0
	FOR COUNT, A, B
		RESULT += CFLAG:COUNT:2
	REND
```

##### int ISNUMERIC(str value) [ edit ]

文字列を数値としてパース可能か（TOINTで値を取れるか）を判断します。 
引数を数値として解釈できる場合、1を返します。それ以外の場合、0を返します。

##### int TOINT(str value) [ edit ]

引数文字列を数値化して返します。ただし、半角数字で構成された文字列のみ数値化できます。 
引数を数値として解釈できない場合、0を返します。全角数字の場合も同様です。

##### int CHKDATA(int value) [ edit ]

引数で示される番号のファイルのデータの情報を返し、またRESULT:0,RESULTS:0に代入します。
返り値は以下の値をとります。0の場合のみそのファイルをロードすることができます。
```
* 0 - このファイルはロード可能です。
* 1 - 指定されたファイルは存在しません。 
* 2 - ゲームのコードが違います。(gamebase.csvの"コード"の値が違うデータ)
* 3 - バージョンが違います。(gamebase.csvの"バージョン"の値が異なり、許容されるバージョンでもないデータ) 
* 4 - 上記以外の問題があるファイルです。 
```
返り値が0のとき、RESULTS:0にはセーブデータのコメント（@SAVEINFOのPUTFORMで入力した文字列、またはSAVEDATAの第２引数）が代入されます。 
返り値が0以外のとき、RESULTS:0には"セーブデータのバーションが異なります"などのエラーメッセージが代入されます。 
また、CHKDATAを呼んだタイミングがRESULT:0に値を代入する途中でない場合（RESULT:0 = CHKDATA(LOCAL)など）、 
RESULT:0にはセーブデータのタイムスタンプ（2009年3月28日13時5分23秒678ミリ秒であれば、RESULT = 20090328130523678）が代入されます。

##### int CHKCHARADATA(str filename) [ edit ]

datフォルダ内の"chara_*.dat"で示されるファイル名のデータの情報を返します。 
戻り値はロード可能な場合は0、何らかの理由により不可能な場合は非0です。 
また、ロード可能な場合はRESULTS:0にセーブデータのメモを代入し、不可能な場合はその理由をRESULTS:0に代入します。

##### int FIND_CHARADATA(str filename) [ edit ]

LOADCHARAの対象となり得るファイルをdatフォルダの中から探索しファイル名(chara_*.datの*の部分)をRESULTSに代入します。 
戻り値はヒット数（発見されたファイル数）です。 
引数はchara_*.datの*の部分を指定できます。 
例えばFIND_CHARADATA("*あなた*")であれば、chara_*あなた*.datを探し、chara_001あなた.datやchara_あなたABC.datがヒットします。 
引数を省略した場合、"*"を指定したことになりchara_*.datを探します。 
なお、chara_.dat(*が空文字列)はLOADCHARAで指定できないのでヒットしません。 
ヒット数がRESULTSの要素数を超えた場合はエラーにはなりませんが超えた分のファイル名は代入されません。

##### int SAVENOS() [ edit ]

コンフィグ"表示するセーブデータ数"で指定された数を取得します。標準は20です。

##### int PRINTCPERLINE() [ edit ]

コンフィグ"PRINTCを並べる数"で指定された数を取得します。標準は3です。

##### int LINEISEMPTY() [ edit ]

現在PRINTしている行が空行かどうかを判断する式中関数です。 
この関数を呼び出した時点で「PRINTL 」を行おうとした場合、 
その結果がただの空行になる場合には1が、そうでない場合は0が返ります。 
PRINTC系で条件に応じたボタンを順次書き連ねる場合に、最後にこの関数を使用することで、 
表示されるボタンがあるかないかを判別し、なければ専用の表示をするといったことが可能です

##### int GROUPMATCH(? key, ? value1, ? value2...) [ edit ]

?はすべて同じ型であること 
以下、私家改造版更新履歴より、
```
変数の値の複数からの一致確認命令GROUPMATCH実装
　書式：GROUPMATCH(<値1>, <値2>，{<値3>,...})
　内容：値2から値N（任意の数）までの値に値1と同じ値のものが何個あるかを返す、ない場合は当然0
```

##### int NOSAMES(? value1, ? value2...) [ edit ]

?はすべて同じ型であること 
以下、私家改造版更新履歴より、
```
指定した値群が全て異なる値かをチェックする命令NOSAMES実装
　書式：NOSAMES(<値1>, <値2>{, <値3>, ...})
　内容：引数に渡した値が「全て異なる」ものであれば1を返す、そうでなければ0を返す
```

##### int ALLSAMES(? value1, ? value2...) [ edit ]

?はすべて同じ型であること 
以下、私家改造版更新履歴より、
```
指定した値群が全て同じ値かをチェックする命令ALLSAMES実装
　書式：ALLSAMES(<値1>, <値2>{, <値3>, ...})
　内容：引数に渡した値が「全て同じ」であれば1を返す、そうでなければ0を返す
```

##### int ISSKIP() [ edit ]

SKIPDISPのフラグが0以外（PRINT等の出力を無視）なら1を、そうでなければ0を返します

##### (int MOUSESKIP) [ edit ]

この関数はEmuera1.810でMESSKIPに統合されました。 
MESSKIPを使用してください。 
この関数はかつて以下のような処理をしていました。
```
右クリックが押されてWAITスキップの状態になっているなら1を、そうでなければ0を返します
マクロ処理時のスキップ中は0を返します。
マクロ処理のスキップと右クリックが競合した場合はマクロを優先し0を返します
```
現在ではEscキーによるスキップと右クリックによるスキップを区別せずに1を返します。

##### int MESSKIP() [ edit ]

WAITスキップの状態になっているなら1を、そうでなければ0を返します

##### str CONVERT(int value, ※) [ edit ]

※には2,8,10,16のみが入ります。 
第一引数を2,8,10,または16進数で表現した文字列を返す

##### str MONEYSTR(int value, str format = "") [ edit ]

以下、私家改造版更新履歴より、
```
式中関数MONEYSTRの引数を拡張
　書式：MONEYSTR([数値]{, [書式指定子]})
　内容：引数で与えられた数値に対して、設定されたお金の単位を付けた文字列を返す
　　　　単位の前置・後置も自動的に処理します
　　　　第2引数はTOSTR()関数と同様の数値の文字列化における変換書式指定子となります
```

##### int FINDELEMENT (var array, ? value, int start = 0, int end = ※, int flag) [ edit ]

##### int FINDLASTELEMENT (var array, ? value, int start = 0, int end = ※, int flag) [ edit ]

配列中の特定範囲から特定の要素の位置を取得する関数です。 
valueはarrayと同じ型である必要があります。 
start、endで指定された配列要素の検索範囲にvalueで指定された内容と同じ要素があれば、その位置を返します。 
endを省略した場合、配列の最後までを対象とします。 
複数ある場合はFINDELEMENTは最初にヒットしたものを、 
FINDLASTELEMENTは最後にヒットしたものを返します。ヒットしない場合は-1を返します。 
検索対象が文字列型の場合はREPLACEと同様に正規表現を使えます。 
flagは検索対象が文字列型の場合のみ有効で、0であれば文字列の一部が一致でもOKとし、 
0以外であれば文字列と完全に一致した場合のみOKとします。
arrayに指定できるものは1次元配列変数のみで多次元配列は指定できません。 
arrayにCFLAGなどのキャラクタ配列を指定した場合、指定されたキャラについてのみ数え上げます。

##### str BARSTR(int value, int max, int length) [ edit ]

与えられた引数と同一の引数のBAR命令で表示される文字列と同じ物を取得します。

##### int COLOR_FROMNAME(str colorname) [ edit ]

与えられた引数を色名として判定し0xRRGGBB形式の数値として返します。 
該当する色名が存在しない場合、-1を返します。

##### int COLOR_FROMRGB(int r, int g, int b) [ edit ]

与えられたR,G,B値を0xRRGGBB形式の数値として返します。 
引数が0-255の範囲外の場合、エラーになります。

##### INRANGEARRAY [ edit ]

##### INRANGECARRAY [ edit ]

以下、私家改造版更新履歴より、
```
　○配列内の指定した範囲内の数値を持つ要素数を取得するINRANGEARRAY、INRANGECARRAY追加
　　　書式：INRANGEARRAY([対象一次元数値配列変数], [範囲最小値]、[範囲最大値+1]{, [配列位置初値], [配列位置終値+1]})
　　　内容：最小値 <= 値 < 最大値をとる値を持つ配列中の要素の数を返す

　　　書式：INRANGECARRAY([対象数値型キャラクタ配列変数], [範囲最小値]、[範囲最大値+1]{, [探索キャラ位置初値], [探索キャラ配列位置終値+1]})
　　　内容：キャラクタ配列変数について指定した配列をキャラクタで回して、要素、最小値 <= 値 < 最大値をとる値を持つ要素数を取得する
```

##### str GETLINESTR(str letter) [ edit ]

以下、私家改造版更新履歴より、
```
○CUSTOMDRAWLINE、DRAWLINEFORMの表示文字列を取得する式中関数GETLINESTR追加
　　　書式：GETLINESTR(<文字列>)
　　　内容：引数文字列をCUSTOMDRAWLINE、DRAWLINEFORMに渡した時に表示される文字列を返す
　☆注意・必ず読んでください☆
　　　この変数及び式中関数が返す文字列の長さが「1行に表示できる文字列長」に対応することは『全く保証されません』
　　　そう思い込んで処理を書いてうまく動かなくてもそれはEmueraのバグではありませんし、この件について当方は一切の責任を負いません
```

##### int PRINTCLENGTH() [ edit ]

以下、私家改造版更新履歴より、
```
　○PRINTCの文字数を取得するPRINTCLENGTH追加
　　　書式：PRINTCLENGTH()
　　　内容：コンフィグのPRINTCの文字数の値を返す
```

##### str STRFORM(str value) [ edit ]

与えられた文字列をPRINTFORMなどと同様の書式付文字列とみなし、展開後の文字列を返します。

##### int GETCONFIG(str value) [ edit ]

##### str GETCONFIGS(str value) [ edit ]

コンフィグ及びreplace.csvの設定項目を整数または文字列で取得します。 
取得できる項目は以下の通りです。
```
	GETCONFIG("オートセーブを行なう")
	GETCONFIG("単位の位置")
	GETCONFIG("ウィンドウ幅")
	GETCONFIG("PRINTCを並べる数")
	GETCONFIG("PRINTCの文字数")
	GETCONFIG("フォントサイズ")
	GETCONFIG("一行の高さ")
	GETCONFIG("表示するセーブデータ数")
	GETCONFIG("販売アイテム数")
	GETCONFIG("COM_ABLE初期値")
	GETCONFIG("文字色")
	GETCONFIG("背景色")
	GETCONFIG("選択中文字色")
	GETCONFIG("履歴文字色")
	GETCONFIG("PBANDの初期値")
	GETCONFIG("RELATIONの初期値")

	GETCONFIGS("フォント名")
	GETCONFIGS("お金の単位")
	GETCONFIGS("起動時簡略表示")
	GETCONFIGS("DRAWLINE文字")
	GETCONFIGS("システムメニュー0")
	GETCONFIGS("システムメニュー1")
	GETCONFIGS("時間切れ表示")
	GETCONFIGS("BAR文字1")
	GETCONFIGS("BAR文字2")
	GETCONFIGS("描画インターフェース")
```

##### str HTML_POPPRINTINGSTR() [ edit ]

現在PRINT中で改行待ちの文字列バッファをHtml形式で取得し、バッファを空にします 
pタグはつかないのでALIGNMENT命令によるalignは反映されません。

##### str HTML_GETPRINTEDSTR(int lineNo) [ edit ]

表示済みのラインのうちlineNoで指定した行の内容をhtml形式の文字列として取得します。 
行の数え方はLINECOUNTやCLEARLINE命令と同じです。

##### str HTML_ESCAPE(str value) [ edit ]

対象の文字列をHtml向けにエスケープ（文字参照に変換）します。 
アンエスケープにはHTML_TOPLAINTEXT関数を使用します。

##### str HTML_TOPLAINTEXT(str value) [ edit ]

対象のhtml文字列をプレーンテキストに変換します。 
具体的には、文字列からhtmlタグを削除し文字参照を展開します。

##### int SAVETEXT(str text, int fileNo{, int force_savdir, int force_UTF8}) [ edit ]

textで指定したテキストを、ファイル名textXX.txt（例えばfileNoが2ならtext02.txt）に保存します。 
この命令はテキストにヘッダーなどを付け加えたり変更したりすることなく文字列そのままを保存します。 
この命令は通常はオプション設定の影響を受け、savフォルダ内に作成されたり、UTF-8で保存されます。 
第3引数に非0を指定した場合、オプションを無視して強制的にsavフォルダ内に保存します。savフォルダは必要に応じて作成されます。 
第4引数に非0を指定した場合、オプションを無視して強制的にUTF-8エンコードで保存します。 
成功した場合非0が、失敗した場合0を返します。 
短い時間中に同一ファイルに書き込むことを繰り返した場合、ウイルス対策ソフト等の影響で書き込みに失敗する可能性がありますので成否のチェックは重要です。

##### str LOADTEXT(int fileNo{, int force_savdir, int force_UTF8}) [ edit ]

LOADTEXT命令版はtextXX.savを読み取りその結果を返します。 
第2引数に非0を指定した場合、オプションによらずsavフォルダ内のファイルを探します。 
第3引数に非0を指定した場合、UTF-8エンコードで保存されているものとして読み取ります。 
失敗した場合、空文字列を返します

##### int GETKEY(int vkey) [ edit ]

キーボード及びマウスボタンの状態を返します。 
引数で指定されるキーが押されていれば1、押されていなければ0を返します。 
この関数はEmueraのウインドウがアクティブのときのみ1を返し、アクティブ状態でなければキー状態にかかわらず0を返します。 
キーコードの数値と実際のキーの対応に関してはマイクロソフト社が提供するMSDNのGetKeyState()の項を参照してください。

##### int GETKEYTRIGGERED(int vkey) [ edit ]

GETKEYと同様にキーボード及びマウスボタンの状態を返します。 
GETKEYは現在押されているかどうかを取得するのに対し、GETKEYTRIGGEREDは押された直後のみ1を返します。 
すなわち継続して押し続けている場合はGETKEYは1を返しますが、GETKEYTRIGGERDは最初のみ1を返し以降は0を返します。 
この関数はEmueraのウインドウがアクティブのときのみ1を返し、アクティブ状態でなければキー状態にかかわらず0を返します。 
キーコードの数値と実際のキーの対応に関してはマイクロソフト社が提供するMSDNのGetKeyState()の項を参照してください。

##### int CLIENTWIDTH () [ edit ]

##### int CLIENTHEIGHT () [ edit ]

クライアント領域（ウィンドウの描画領域）の現在の幅または高さを取得します。 
この数値にはウィンドウ枠やメニューバー、スクロールバー、テキスト入力領域の幅や高さを含みません。 
CLIENTHEIGHTはユーザーによってゲーム中に変更される可能性があることに注意して下さい。

##### int MOUSEX() [ edit ]

##### int MOUSEY() [ edit ]

マウスカーソルの現在のX座標またはY座標を取得します。 
座標はクライアント領域の左下位置を(0,0)とする相対位置であり、右方向がx軸の正、下方向がy軸の正です。 
カーソルがクライアント領域内にある場合MOUSEYは負の値を返すことに注意してください。 
クライアント領域の広さはCLIENTWIDTH、CLIENTHEIGHT関数によって取得できます。 
（クライアント領域左上を基準とするY座標が必要なら、MOUSEY()+CLIENTHEIGHT()によって取得できます） 
この関数はEmueraのウインドウがアクティブでなくても、また、マウスカーソルがウインドウ外であっても正常に動作します。

##### int ISACTIVE() [ edit ]

Emueraのウインドウの状態を返します。 
アクティブであれば1、アクティブでなければ0を返します。

##### int SPRITECREATED(str spriteName) [ edit ]

指定した名称のスプライトが作成済みであるなら1を、未作成又は廃棄済みであるなら0を返します。

##### int SPRITEWIDTH(str spriteName) [ edit ]

##### int SPRITEHEIGHT(str spriteName) [ edit ]

指定した名称のスプライトの幅または高さを取得します。 
スプライトが未作成又は廃棄済みであるなら0を返します。

##### int SPRITEPOSX(str spriteName) [ edit ]

##### int SPRITEPOSY(str spriteName) [ edit ]

指定した名称のスプライトの相対位置のX、Yを取得します。 
スプライトが未作成又は廃棄済みであるなら0を返します。 
相対位置のX、Yが0なのか未作成又は廃棄済であるのかの区別には別途SPRITECREATEDを呼び出してください。

##### int SPRITESETPOS(str spriteName, int posx, int posy) [ edit ]

指定した名称のスプライトの相対位置のX、Yを設定します。 
成功した場合は非0を、指定したスプライトが未作成又は廃棄済み等の理由で失敗した場合には0を返します。

##### int SPRITEMOVE(str spriteName, int movex, int movey) [ edit ]

指定した名称のスプライトの相対位置のX、Yに指定した値を加算します。 
成功した場合は非0を、指定したスプライトが未作成又は廃棄済み等の理由で失敗した場合には0を返します。

##### int GCREATE(int ID, int width, int height) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
指定したサイズで指定したIDのGraphicsを作成します。 
GraphicsのIDは0以上の整数、width、heightは1以上8192以下の間の整数値でなければなりません。 
引数がこの範囲を外れるとエラーです。 
作成に成功した場合、非0を返します。 
指定したIDのGraphicsが既に作成されている場合、0を返します。 
Graphicsを作り直す場合はGDISPOSE命令で既存のGraphicsを廃棄してください。

##### int GCREATEFROMFILE(int ID, str filepath) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
resourcesフォルダ内の画像ファイルを相対パスで指定し、その画像を開いてGraphicsを作成します。 
resourcesフォルダ内のcsvファイルでリソースを宣言した場合と異なり、画像ファイルはロックされません。 
作成に成功した場合、非0を返します。 
指定したIDのGraphicsが既に作成されている場合はGraphicsの作成に失敗し、この命令は何もせずに0を返して終了します。 
ファイルが存在しない、画像として認識できない、ファイルのサイズが大きすぎる、などで失敗した場合も0を返します。

##### int GDISPOSE(int ID) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
指定したIDのGraphicsを廃棄します。 
廃棄に成功した場合、非0を返します。 
指定したIDのGraphicsが未作成（廃棄済の場合を含む）の場合、0を返します。

##### int GCLEAR(int ID, int cARGB) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
指定したIDのGraphicsの全域を指定した色で置き換えます。 
処理に成功した場合、非0を返します。 
ID又は色指定が不適切な場合はエラーになります。 
画像処理系の命令での色指定はRGBではなくアルファ値（不透明度）を含むARGB形式であることに注意して下さい。 
ARGB型は16進数で0xAARRGGBBで表されます。

##### int GFILLRECTANGLE(int ID, int x, int y, int width, int height) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
指定したIDのGraphicsに(x, y, width, height)で指定した四角形を描画します。 
処理に成功した場合、非0を返します。 
描画の色はGSETBRUSH命令によってあらかじめ指定しなければEmueraコンフィグの文字色で描画されます。

##### int GDRAWG(int destID, int srcID, int destX, int destY, int destWidth, int destHeight, int srcX, int srcY, int srcWidth, int srcHeight) [ edit ]

##### int GDRAWG(int destID, int srcID, int destX, int destY, int destWidth, int destHeight, int srcX, int srcY, int srcWidth, int srcHeight, var cm) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
指定したdestIDのGraphicsに、指定したsrcIDのGraphicsを描画します。 
描画先destのGraphicsの位置及びサイズを4つの整数で、描画元srcのGraphicsの位置及びサイズを4つの整数で指定します。 
オプションとして、5x5以上の二次元数値配列をcmとして指定することでカラーマトリクスを適用して描画します。 
colorMatrixは全要素を1/256にして.Net Frameworkの!ColorMatrixクラスに渡されます。つまり、対角がすべて256である5x5行列が単位行列になります。 
処理に成功した場合、非0を返します。 
描画先Graphics又は描画元Graphicsのいずれかが未作成であるなどの場合、0を返します。 
描画先と描画元Graphicsは同一でも実行可能です。

##### int GDRAWGWITHMASK(int destID, int srcID, int maskID, int destX, int destY) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
指定したdestIDのGraphicsに、srcIDのGraphicsを、maskIDのGraphicsをマスクとして適用して描画します。 
出力先destIDのGraphics内の描画位置をdestX, destYで指定します。 
処理に成功した場合、非0を返します。 
srcIDとmaskIDの幅及び高さが完全に一致すること、描画領域がdestIDからはみ出さないことが成功の条件です。 
マスクとして適用して描画するとは、具体的には、マスク画像の青の値を不透明度としてソース画像に適用して描画することです。 
例えば、マスク画像が完全に白一色（つまり全部の場所で青色がMAX）の場合、ソース画像はマスクなしの場合と同様にそのまま描画されます。 
マスク画像が完全に黒一色（つまり全部の場所で青色が0）の場合、ソース画像は完全透明として扱われ何も起きません。 
この命令の処理はGPUではなく、CPUがシングルスレッドで解決します。速度には期待しないでください。

##### int GDRAWSPRITE(int ID, str sprName) [ edit ]

##### int GDRAWSPRITE(int ID, str sprName, int destX, int destY) [ edit ]

##### int GDRAWSPRITE(int ID, str sprName, int destX, int destY, int destWidth, int destHeight) [ edit ]

##### int GDRAWSPRITE(int ID, str sprName, int destX, int destY, int destWidth, int destHeight, var cm) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
指定したIDのGraphicsに指定したsprNameのSpriteを描画します。 
オプションとして、Graphics内部の位置をdestX,destYで指定することでその位置にSpriteを描画します。 
また、描画幅及び高さをdestWidth,destHeightで指定することでそのサイズに拡大又は縮小してSpriteを描画します。 
さらに、cmに5x5の行列を指定することでカラーマトリクスを適用して描画します。 
なおSpriteのサイズはSPRITEWIDTH(str imgName), SPRITEHEIGHT(str imgName)関数で取得できます。 
処理に成功した場合、非0を返します。 
 
アニメーションスプライトを指定した場合、実行時のフレーム1つが描画されます。

##### int GSETCOLOR(int ID, int cARGB, int x, int y) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
指定したIDのGraphicsの指定位置のピクセルを指定した色に置き換えます。 
処理に成功した場合、非0を返します。 
この命令はそれほど高速ではありません。 
GGETCOLOR命令と合わせて大きな画像の全体を書き換える試みは実用的な時間内には終わりません。 
画像処理系の命令での色指定はRGBではなくアルファ値（不透明度）を含むARGB形式であることに注意して下さい。 
ARGB型は16進数で0xAARRGGBBで表されます。

##### int GSETBRUSH(int ID, int cARGB) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
指定したIDのGraphicsに指定した色のブラシを設定します。 
指定したブラシはGDISPOSE命令でGraphicsが破棄されるまで記憶されます。 
処理に成功した場合、非0を返します。 
ここで設定した色のブラシがGFILLRECTANGLE命令で使用されます。 
画像処理系の命令での色指定はRGBではなくアルファ値（不透明度）を含むARGB形式であることに注意して下さい。 
ARGB型は16進数で0xAARRGGBBで表されます。

##### int GSETFONT(int ID, str fontName, int fontSize) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
指定したIDのGraphicsに指定した名前およびサイズのフォントを設定します。 
指定したフォントはGDISPOSE命令でGraphicsが破棄されるまで記憶されます。 
処理に成功した場合、非0を返します。 
設定したフォントを利用する命令・関数はバージョン1.824現在には存在しません。

##### int GSETPEN(int ID, int cARGB, int penWidth) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
指定したIDのGraphicsに指定した色及び幅のペンを設定します。 
指定したペンはGDISPOSE命令でGraphicsが破棄されるまで記憶されます。 
処理に成功した場合、非0を返します。 
画像処理系の命令での色指定はRGBではなくアルファ値（不透明度）を含むARGB形式であることに注意して下さい。 
ARGB型は16進数で0xAARRGGBBで表されます。 
設定したペンを利用する命令・関数はバージョン1.824現在には存在しません。

##### int GCREATED(int ID) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
指定したIDのGraphicsが作成済みであるなら1を、未作成（廃棄済を含む）なら0を取得します。

##### int GWIDTH(int ID) [ edit ]

##### int GHEIGHT(int ID) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
指定したIDのGraphicsの幅または高さを取得します。 
Graphicsが未作成（廃棄済を含む）なら0を返します。

##### int GGETCOLOR(int ID, int x, int y) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
指定したIDのGraphicsの指定位置の色を0xAARRGGBB形式の整数値で取得します。 
Graphicsが未作成又は廃棄済み、あるいはx,yが画像外の位置であるなら-1を返します。 
 
この命令のみ、失敗した場合に0ではなく-1を返すことに注意してください。 
黒色かつ完全透明の位置の色を取得した場合に、この命令は0を返します。

##### int GSAVE(int ID, int fileNo) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
指定したIDのGraphicsの画像を、fileNoの番号を付けたファイル名でpng形式で出力し保存します。 
処理に成功した場合、非0を返します。

##### int GLOAD(int ID, int fileNo) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
fileNoの番号を付けたファイル名の画像を開き、Graphicsを作成します。 
動作としてはCREATEFORMFILE命令とほぼ同じですがresoucesフォルダ内の画像ではなくGSAVE命令で保存した画像から作成する点が異なります。 
処理に成功した場合、非0を返します。 
指定したIDのGraphicsが既に作成されている場合はGraphicsの作成に失敗し、この命令は何もせずに0を返して終了します。

##### int SPRITECREATE(str spriteName, int gID) [ edit ]

##### int SPRITECREATE(str spriteName, int gID, int x, int y, int width, int height) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
gIDのGraphicsの一部または全部を元にして、spriteNameで指定したリソース名を持つスプライトを作成します。 
(x, y, width, height)を指定することでGraphicsのその部分をスプライトとして切り取れます。 
作成に成功した場合、非0を返します。 
同じリソース名のスプライトが既に存在するなどで失敗した場合、0を返します。 
スプライトは親GraphicsのIDと切取位置を記憶しているだけなので、親Graphicsに変更があるとスプライトも変更されます。 
また、親Graphicsが破棄されるとスプライトも破棄された扱いになります。 
作成したスプライトはresoucesフォルダ内のcsvで宣言したリソースとほぼ同様に扱うことができます。 
例えばPRINT_IMG命令やHTML_PRINTのimgタグなどで使用できます。

##### int SPRITEANIMECREATE(str spriteName, int width, int height) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
spriteNameで指定したリソース名を持つ、width及びheightで指定したサイズのアニメーションスプライトを作成します。
作成に成功した場合、非0を返します。 
同じリソース名のスプライトが既に存在するなどで失敗した場合、0を返します。 
アニメーションさせるためにはSPRITEANIMEADDFRAME命令によってフレームを追加する必要があります。 
アニメーションスプライトに関する注意点については、[resouces]も参照してください。

##### int SPRITEANIMEADDFRAME(string spriteName, int gID, int x, int y, int width, int height, int offsetx, int offsety, int delay) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
spriteNameで指定したリソース名を持つアニメーションスプライトにフレームを追加します。 
gIDで指定したGraphicsの(x, y, width, height)で指定した四角形の範囲をフレームとし、スプライト左上からoffsetx, offsetyの位置に配置します。 
アニメーションスプライトの作成時に設定したサイズの範囲から外れた部分は描画されません。 
delayにはこのフレームを表示する時間をミリ秒単位で指定します。 
spriteNameのリソース名が存在しない、又はアニメーションスプライトでない場合はこの命令は失敗し、何もしません。 
フレームの追加に成功した場合1を、失敗した場合は0を返します。

##### int SPRITEDISPOSE(string spriteName) [ edit ]

spriteNameで指定したリソース名を持つスプライトを廃棄します。 
廃棄に成功した場合、非0を返します。 
この命令はスプライトの元となったGraphics等には影響しません。 
Graphicsに割り当てられたメモリを解放するにはGDISPOSE命令を使用してください。

##### int SPRITEGETCOLOR(string spriteName, int x, int y) [ edit ]

spriteNameで指定したリソース名を持つスプライトの指定位置の色を0xAARRGGBB形式の整数値で取得します。 
spriteNameが未作成又は廃棄済み、あるいはx,yが画像外の位置であるなら-1を返します。 
 
この命令のみ、失敗した場合に0ではなく-1を返すことに注意してください。 
黒色かつ完全透明の位置の色を取得した場合に、この命令は0を返します。

##### int CBGSETG(int ID, int x, int y, int zdepth) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
IDで指定するGraphicsをクライアント領域に表示するように設定します。 
(x, y)に0を指定した場合、クライアント領域の左下と画像の左下が一致するように表示されます。 
xは右方向が正、yは下方向が正、zdepthは画面奥方向が正です。 
zdepthは0以外の値を指定します。通常の文字描画がzdepth==0に相当し、zdepthが負であれば文字よりも手前に描画されます。

##### int CBGSETSPRITE(str spriteName, int x, int y, int zdepth) [ edit ]

spriteNameで指定したリソース名を持つスプライトをクライアント領域に表示するように設定します。 
(x, y)に0を指定した場合、クライアント領域の左下と画像の左下が一致するように表示されます。 
xは右方向が正、yは下方向が正、zdepthは画面奥方向が正です。 
zdepthは0以外の値を指定します。通常の文字描画がzdepth==0に相当し、zdepthが負であれば文字よりも手前に描画されます。

##### int CBGCLEAR() [ edit ]

CBGで始まる各種CBG系命令で設定した背景画像の設定を全て解除します。

##### int CBGCLEARBUTTON() [ edit ]

CBGSETBUTTONSPRITE命令で設定したボタンの設定を解除します。

##### int CBGREMOVERANGE(int zmin, int zmax) [ edit ]

CBGSETG、CBGSETSPRITE、CBGSETBUTTONSPRITE命令で設定した画像のうち、Z深度がzmin以上zmax以下である画像を設定解除します。

##### int CBGREMOVEBMAP() [ edit ]

CBGSETBMAPG命令で設定したボタンマップの設定を解除します。

##### int CBGSETBMAPG(int ID) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
IDで指定するGraphicsをクライアント領域のボタンマップとして設定します。 
ここで設定したボタンマップはCBGSETBUTTONSPRITE命令及びINPUTMOUSEKEY命令に影響します。 
ボタンマップ画像は表示はされませんがCBGSETG命令で設定した画像と同様に画面左下と画像左下位置が一致するように配置されます。 
マウスカーソル直下のボタンマップ画像の色が、ボタンの値として認識されます。 
ただし、色のアルファ値が255でない（透明又は半透明である）ときにはボタンの値としては認識されません。

##### int CBGSETBUTTONSPRITE(int button, str spriteName, str spriteNameB, int x, int y,int zdepth) [ edit ]

##### int CBGSETBUTTONSPRITE(int button, str spriteName, str spriteNameB, int x, int y,int zdepth, str tooltipmes) [ edit ]

（※描画方法がGRAPHICS又はTEXTRENDERERのときのみ使用可能） 
CBGSETBMAPG命令で設定したボタンマップと連動して選択可能なボタンを設定します。 
マウス直下のボタンマップ画像の色の0xRRGGBB値が、引数buttonに等しい時に表示されるスプライトをspriteNameB、それ以外のときに表示されるスプライトをspriteNameに指定します。 
spriteName又はspriteNameBは空文字列を指定することができ、そうした場合には非選択中又は選択中に何も表示しません。 
x, y, zdepthについてはCBGSETSPRITEと同じです。基準位置(x,y) = (0,0)が画面左下と画像左下が一致する位置であることに注意してください。 
オプションで当該ボタンを選択中に表示されるツールチップ文字列をtooltipmesで指定することができます。 
同じbuttonの値に複数のCBGSETBUTTONSPRITEを割り当てることができ、また、ボタンの位置と一致する必要はありません。 
そうした場合、ツールチップについては画像のxy位置とは関係なく、ツールチップ文字列が設定された中で最もzdepthが大きいもの（最先に描画され、最奥に見えるもの）の表示が優先されます。
spriteNameで指定したリソース名を持つスプライトをクライアント領域に表示するように設定します。 
(x, y)に0を指定した場合、クライアント領域の左下と画像の左下が一致するように表示されます。 
xは右方向が正、yは下方向が正、zdepthは画面奥方向が正です。 
zdepthは0以外の値を指定します。通常の文字描画がzdepth==0に相当し、zdepthが負であれば文字よりも手前に描画されます。
NewPP limit report
Cached time: 20260903055806
Cache expiry: 86400
Dynamic content: false
CPU time usage: 0.222 seconds
Real time usage: 0.227 seconds
Preprocessor visited node count: 1078/1000000
Preprocessor generated node count: 1524/1000000
Post‐expand include size: 0/2097152 bytes
Template argument size: 0/2097152 bytes
Highest expansion depth: 2/40
Expensive parser function count: 0/100
Unstrip recursion depth: 0/20
Unstrip post‐expand size: 8735/5000000 bytes
Transclusion expansion time report (%,ms,calls,template)
100.00%    0.000      1 -total
