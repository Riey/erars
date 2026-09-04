# URL: https://wiki.eragames.rip/index.php/Emuera/UserMeth
# fetch date: 2026-09-03

Contents 1 ユーザー定義の式中関数 1.1 書式 1.2 制限 1.2.1 CALLから呼び出せない 1.2.2 一部の命令は使用不可 1.2.3 オーバーロード不可 1.2.4 組み込み関数の上書き 1.3 注意事項 1.3.1 短絡評価による呼び出し省略 1.3.2 式の評価順により結果が変わる 1.3.3 デバッグ用の機能により呼び出されることがある

### ユーザー定義の式中関数 [ edit ]

「式中で使える関数」として、組み込み関数だけでなく@～～で定義した関数を式中で呼び出すことも可能です。 
「式中で使える関数」のうち組み込み関数については[exmeth 式中で使える関数]を参照してください。

#### 書式 [ edit ]

呼び出される関数は#FUNCTIONフラグまたは#FUNCTIONSフラグを持ち、RETURNFで終わる必要があります。
- FUNCTIONを付けると数値を返す関数と認識されます。
- FUNCTIONSを付けると文字列を返す関数と認識されます。
- FUNCTION(S)を付けた関数は通常のRETURNで終了することはできません。代わりにRETURNFで終了します。
RETURNFには数式または文字列式を指定します。これは#FUNCTION(S)で示した型と一致している必要があります。 
RETURNFの引数を省略した場合、またはRETURNF無しで関数の終端に到達した場合は0または""を返します。
```
X = GET_CFLAG(TARGET, Y)
STR = %GET_NAME(TARGET)%

@GET_CFLAG(ARG:0, ARG:1)
#FUNCTION
	SIF ARG:0 <= 0 || ARG:0 >= CHARANUM
		RETURNF 0
	RETURNF CFLAG:(ARG:0):(ARG:1)

@GET_NAME(ARG:0)
#FUNCTIONS
	SIF ARG:0 <= 0 || ARG:0 >= CHARANUM
		RETURNF ""
	RETURNF NAME:(ARG:0)
```
※関数定義の引数を()でくくっていますが、これは定義の場合は必須な文法ではありません。 
式中関数を呼び出す場合は()でくくった文法を用いる必要があります。 
通常の関数と同様に関数名と引数をカンマでわけることもできます。 
以下の2行は同じ意味です。
```
@GET_CFLAG(ARG:0, ARG:1)
@GET_CFLAG, ARG, ARG:1
```
また、引数に初期値を設定することができます。 
初期値についての文法は[exfunc#h4-.E8.87.AA.E4.BD.9C.E9.96.A2.E6.95.B0.E3.81.AB.E3.81.8A.E3.81.91.E3.82.8B.E5.BC.95.E6.95.B0.E6.8C.87.E5.AE.9A 自作関数における引数指定]をご覧ください。

#### 制限 [ edit ]

##### CALLから呼び出せない [ edit ]

FUNCTION(S)フラグを付けた関数はCALLなど通常の呼び出しはできません。 
式中でのみ呼び出せます。
```
	;エラー
	CALL GET_CFLAG, X, Y
@GET_CFLAG(ARG:0, ARG:1)
#FUNCTION
	SIF ARG:0 <= 0 || ARG:0 >= CHARANUM
		RETURNF 0
	RETURNF CFLAG:(ARG:0):(ARG:1)
```
- FUNCTION(S)を呼び出すための専用命令CALLF、CALLFORMFからであれば呼び出せます。

##### 一部の命令は使用不可 [ edit ]

FUNCTION(S)フラグを付けた関数の中ではWAITなど入力を伴う命令、CALLなど関数呼び出しを伴う命令は使用できません。 
使用するとエラーになります。 
 
CALL命令は使用できませんが、FUNCTION(S)フラグを持つ関数を式中で呼び出すことはできます。 
また、CALLF、CALLFORMF命令による#FUNCTION(S)の呼び出しは可能です。

##### オーバーロード不可 [ edit ]

複数の#FUNCTION(S)関数を引数の数、型の違いによって呼び分けることはできません。 
同じ名前の関数は1つしか定義できず、同名関数を複数定義した場合には最初に定義された関数のみが有効です。

##### 組み込み関数の上書き [ edit ]

組み込み関数と同名の関数を定義した場合、その組み込み関数は呼び出せなくなります。 
例えば、@ABSを定義すると元のABSを呼び出すことはできなくなります。 
組み込み関数が上書きされているとき、Emueraは起動時に警告を表示します。 
組み込み関数が上書きされていると意図した動作をしないことがあるため、コンフィグによって関数の上書きを禁止することができます。 
意図的に上書きする場合のため（推奨しません）、上書きされても警告しないようにするコンフィグオプションもあります。

#### 注意事項 [ edit ]

FUNCTION(S)フラグを付けた関数の中でローカル変数以外の変数を変化させるべきではありません。 
ローカル変数以外の変数を変化させる関数（副作用のある関数）は後述する短絡評価や式の評価順序などにより動作が変わるおそれがあります。 
また、デバッグコマンドやデバッグ用の変数ウォッチウインドウからなどの予期しない呼び出しにより意図しない動作をするおそれがあります。

##### 短絡評価による呼び出し省略 [ edit ]

式中に関数があっても短絡評価によって呼び出されないことがあります。 
 
例えば以下のスクリプトは、IF文の中でGET_ASSI_CFLAGを呼んでおり、GET_ASSI_CFLAGの中でASSIを変更しています。
```
	IF X || GET_ASSI_CFLAG(0)
		Y = CFLAG:ASSI:2
	ENDIF
@GET_ASSI_CFLAG(ARG:0)
#FUNCTION
	SIF ASSI < 0
		ASSI = 0
	RETURNF CFLAG:ASSI:(ARG:0)
```
一見すると Y = CFLAG:ASSI:2 の実行時に ASSI < 0 になることはないように見えます。 
しかし、X が0でない場合は短絡評価によりGET_ASSI_CFLAGが実行されないため、ASSI < 0のままCFLAG:ASSI:2を評価しようとしてエラーになる場合があります。

##### 式の評価順により結果が変わる [ edit ]

式中での変数・関数の評価順は不定です。 
副作用のある関数は式の中の関数がどの順番で呼び出されるかに依存する場合があります。 
そのようなコードを書かないでください。 
呼び出し順はEmueraのバージョンが同じであれば同じになるでしょうが、将来変更されるかもしれません。 
下のスクリプトでは、ADDCHARA_CFLAG中でTARGETを変更しています。
```
	X = CFLAG:TARGET:10 + ADDCHARA_CFLAG(0)
@ADDCHARA_CFLAG(ARG)
#FUNCTION
	ADDCHARA ARG
	TARGET = CHARANUM -1
	RETURNF CFLAG:TARGET:2
```
ADDCHARA_CFLAGより前にCFLAG:TARGET:10を評価するか、後でCFLAG:TARGET:10を評価するかによってCFLAG:TARGET:10の指す変数が変わってきます。 
したがってこのスクリプトは評価順に依存します。 
FUNCTION(S)フラグを付けた関数の中でADDCHARAやTARGETへの代入をすべきではありません。

##### デバッグ用の機能により呼び出されることがある [ edit ]

FUNCTION(S)フラグを付けた関数は*.ERBファイル中のスクリプトからだけではなく、デバッグコマンドやデバッグ用の変数ウォッチウインドウから動的に呼び出されることがあります。 
特に変数ウォッチは頻繁に値を更新しようとし、更新のたびにその関数を呼び出します。 
副作用を持つ関数はこのような呼び出しにより誤動作を起こす可能性があります。
NewPP limit report
Cached time: 20260903082223
Cache expiry: 86400
Dynamic content: false
CPU time usage: 0.026 seconds
Real time usage: 0.028 seconds
Preprocessor visited node count: 87/1000000
Preprocessor generated node count: 148/1000000
Post‐expand include size: 0/2097152 bytes
Template argument size: 0/2097152 bytes
Highest expansion depth: 2/40
Expensive parser function count: 0/100
Unstrip recursion depth: 0/20
Unstrip post‐expand size: 816/5000000 bytes
Transclusion expansion time report (%,ms,calls,template)
100.00%    0.000      1 -total
