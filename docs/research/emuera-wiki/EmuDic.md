# URL: https://wiki.eragames.rip/index.php/Emuera/EmuDic
# fetch date: 2026-09-03

This is a summary of the terms used in this wiki. 
I wrote it as a memo because the original coined words have increased with the extension of Emuera.
Contents 1 Startup Mode 1.1 Normal Mode 1.2 Analysis Mode 1.3 Debug Mode 2 Windows / Dialogs 2.1 Main Window 2.2 Main Console 2.3 Debug Window 2.4 Debug Console 2.5 Configuration Dialog 2.6 Clipboard Dialog 3 Functions 3.1 Commands 3.2 Functions 3.3 Event Functions 3.4 Preprocessor 3.5 Property (Preprocessor) 3.6 Definitions (Preprocessor) 3.7 Functions that can be used in expressions 3.8 Functions in an Expression 3.9 組み込み関数 3.10 ユーザー定義関数 3.11 #FUNCTION(S)関数 4 行・文・式 4.1 行 4.2 文 4.3 式 4.4 数式 4.5 文字列式 5 変数 5.1 擬似変数 5.2 配列変数 5.3 キャラクタ変数 5.4 二重配列変数 5.5 多次元配列変数 5.6 キャラクタ多次元配列変数 5.7 ローカル変数 5.8 広域変数 5.9 グローバル変数 5.10 プライベート変数

### Startup Mode [ edit ]

#### Normal Mode [ edit ]

This is the mode when Emuera is started normally. 
You will be in this mode when you double-click on EmueraXXXX.exe to launch it.

#### Analysis Mode [ edit ]

This is the mode when a file is passed as a command line argument. 
It also becomes this mode when a file is drag and dropped into EmueraXXXX.exe. 
In analysis mode, a grammar check is performed for dragged and dropped files. 
This allows you to check for grammatical errors in the files you are developing. 
However, you will need the CSV folder of the variant you plan to use to check the type of the elements string. 
Just drag and drop the file into the EmueraXXXX.exe of the variant you plan to use. 
In addition, from 1738g, when "Report is displayed when loading" is enabled in the analysis mode, it is possible to display the report when loading. 
The list of functions used in each loaded ERB file is now displayed. 
If you have too many functions, increase the number of lines in the log in your config.

#### Debug Mode [ edit ]

This is the mode when the command is started by passing "-debug" as command line argument. See  Debug Mode  for details.

### Windows / Dialogs [ edit ]

#### Main Window [ edit ]

This is the first window that opens when you normally start up.

#### Main Console [ edit ]

The name of the part of the Main Window that is used for input and output.

#### Debug Window [ edit ]

This is a window that can be opened when started in Debug Mode.

#### Debug Console [ edit ]

The name of the input/output part that is displayed when the "Console" tab is selected in the Debug Window.

#### Configuration Dialog [ edit ]

It is a dialog that can be opened from the main window menu help or settings.

#### Clipboard Dialog [ edit ]

It is a dialog that can be opened by Ctrl+C from the main console.

### Functions [ edit ]

#### Commands [ edit ]

PRINT or WAIT, for example.

#### Functions [ edit ]

It is a name that is defined in an ERB script with @~~ statements and called with a CALL instruction, etc. 
Among the functions that can be used in expressions, those that define their names in sentences @~~ are included.

#### Event Functions [ edit ]

A function whose name starts with "EVENT" and is called by the system. 
If you define more than one, all of them will be called.

#### Preprocessor [ edit ]

A line in a file in the ERB folder that is processed before any other instruction. 
A line that begins with # or consists of a phrase enclosed in []. 
Lines that begin with # are divided into attributes and definitions. Please see them for more information. 
Lines consisting of words or phrases enclosed in [] represent special blocks. 
See the page  here  for more information

#### Property (Preprocessor) [ edit ]

A preprocessor that determines the type and behavior of a function with a line beginning with a # specified for the function. 
There are #PRI, #LATER, #SINGLE, and #ONLY to control the behavior of when the event function is executed, 
and #FUNCTION and #FUNCTIONS to specify the type of the function in the expression.

#### Definitions (Preprocessor) [ edit ]

A preprocessor that defines the name of a variable and its type on a line beginning with #. 
Functions include #LOCALSIZE and #LOCALSSIZE to specify the number of elements in a LOCAL or LOCALS variable, 
#DIM and #DIMS to define a variable, and #DEFINE to define a DEFINE macro in the ERH.

#### Functions that can be used in expressions [ edit ]

It is a "function" that can be called from within an expression. 
The "function" includes not only the above functions but also the built-in functions described below. 
In many programming languages they are simply called "functions".

#### Functions in an Expression [ edit ]

Abbreviation for a function that can be used in an expression. 
It has nothing to do with inexpression functions (anonymous functions) or inline functions in programming languages.

#### 組み込み関数 [ edit ]

式中関数のうち、Emueraにもともと組み込まれていて@～～文による定義なしで使えるものです。 
ABS(X)やGETTIME()などのことです。 
ややこしいですが、上記の関数の定義には該当しません。 
「式中で使える命令」と考えるとわかりやすいかもしれません。

#### ユーザー定義関数 [ edit ]

ユーザーが定義した関数のことです。 
ERBスクリプト中で@～～文で名称を定義してCALL命令などで呼び出すもののことです。 
つまり上記の関数と同じ概念です。

#### #FUNCTION(S)関数 [ edit ]

@～～文で名称を定義し、#FUNCTION(S)属性を持つものです。 
関数かつ式中関数である関数です。

### 行・文・式 [ edit ]

#### 行 [ edit ]

改行コードから次の改行コードまでのことです。 
プログラミング関連では物理行とも呼ばれます。 
ややこしいことにエディター関係ではこれを論理行と呼ぶことが多いです。

#### 文 [ edit ]

文あるいは論理行はEmueraにおける1つの処理単位です。 
多くの文は、命令1つとその引数、あるいは変数と代入演算子と式からなります。 
ERBでは1行につき1文という鉄則のため、行と文はほぼ同じ意味です。 
当Wikiでも特に区別しません。

#### 式 [ edit ]

変数、定数、式中関数、非代入演算子、括弧とその組み合わせです。 
代入演算子は代入文の最初の演算子としてのみ使用可能で式中では使えません。

#### 数式 [ edit ]

式の評価結果（演算結果）が数値になる式です。 
例えば、A+B、STR == "あいう" などです。

#### 文字列式 [ edit ]

式の評価結果（演算結果）が文字列になる式です。 
例えば、STR + STR:1、"あ" * 10 などです。

### 変数 [ edit ]

#### 擬似変数 [ edit ]

RANDやCHARANUMのように変数のように記述できるが実態は変数ではないものです。 
内部の動作としては式中関数に近い動きをしています。

#### 配列変数 [ edit ]

複数の要素を持つ変数です。 
配列変数の要素数は!VariableSize.csvで変更でき、通常はスクリプト中では増減しませんが、 
一部ローカル変数については、スクリプト中で配列要素数を指定できます。

#### キャラクタ変数 [ edit ]

キャラクタの状態を記録する変数です。 
C言語などのchar型変数とは無関係です。 
その性質上、ADDCHARAやDELCHARAによるキャラの増減に伴い、要素数が増減します。 
NO:TARGETなど、配列変数と同じ形式で要素を指定するためにeramakerの解説では配列変数の一つと扱われていますが、当Wikiではキャラクタ変数と配列変数とを区別しています。

#### 二重配列変数 [ edit ]

キャラクタ変数かつ、配列変数である変数のことです。 
CFLAG:TARGET:2のように、2つの引数をとります（省略可）。 
キャラクタ変数なので第一引数はキャラNoを表わします。 
また、キャラの増減に伴い第一次元の要素数が増減します。 
第二次元の要素数は!VariableSize.csvでのみ変更でき、スクリプト中では増減しません。 
 
当Wikiではeramakerの仕様の説明を除き、"二重配列"という用語を使用しません。 
代わりに"キャラクタ変数かつ配列変数"のような呼び方をします。

#### 多次元配列変数 [ edit ]

DITEMYTPEなどの二次元配列変数及びTAなどの三次元配列変数のことです。 
DA:0:1やTA:1:2:3のように2つまたは3つの引数をとります。 
多次元配列変数の要素数は!VariableSize.csvでのみ変更でき、スクリプト中で増減しません。 
また、多次元配列変数の引数は省略できません。

#### キャラクタ多次元配列変数 [ edit ]

キャラクタ変数かつ、多次元配列変数である変数のことです。 
CFLAG:TARGET:0:2のように、3つの引数をとります（省略不可）。 
キャラクタ変数なので第一引数はキャラNoを表わします。 
また、キャラの増減に伴い第一次元の要素数が増減します。 
第二次元・第三次元の要素数は!VariableSize.csvでのみ変更でき、スクリプト中では増減しません。 
 
ver1807現在、この変数に当てはまるものは CDFLAG のみです。 
詳しくは CDFLAG をご覧ください。

#### ローカル変数 [ edit ]

LOCALやLOCALSやプライベート変数など、関数（関数名）ごとに用意される変数です。 
プライベート変数については別項を参照してください。 
プライベート変数以外のLOCALやLOCALSに関しては、実際にはいわゆるローカル変数ではなく、 
"LOCAL@関数名"や"LOCALS@関数名"という名のpublic staticな変数です。 
関数を抜けても値は保持され関数の外からでも代入、参照できてしまいます。 
また、再帰呼び出しのように複数回呼んだ場合にも値が共有されてしまいます。

#### 広域変数 [ edit ]

LOCALやLOCALSやプライベート変数などを除くほとんどの変数が属する、すべての関数で値が共有の変数です。 
一般的なプログラミング言語におけるグローバル変数の概念ともいえます。 
また、ERH中で#DIMもしくは#DIMSを用いることで広域変数を定義することができます。 
詳しくは ヘッダーファイル（ERH） のページを参照してください。

#### グローバル変数 [ edit ]

異なるセーブデータ間で値を共有することができる変数です。グローバル変数も広域変数に含まれます。 
通常の変数とはセーブロードや初期化のタイミングが異なります。 
Emueraのグローバル変数はこれは一般的なプログラミング言語におけるグローバル変数の概念とは無関係です。

#### プライベート変数 [ edit ]

関数（関数名）ごとに#DIMもしくは#DIMSによって定義される変数です。 
これらもローカル変数でありますので、関数ごとに別個のものです。 
ただし、同じローカル変数とされているLOCALなどとは異なり、@関数名などを使っての関数の外からの代入、参照はできません。 
詳しくは ユーザー定義の変数 のページを参照してください。
NewPP limit report
Cached time: 20260902121216
Cache expiry: 86400
Dynamic content: false
CPU time usage: 0.058 seconds
Real time usage: 0.064 seconds
Preprocessor visited node count: 168/1000000
Preprocessor generated node count: 184/1000000
Post‐expand include size: 0/2097152 bytes
Template argument size: 0/2097152 bytes
Highest expansion depth: 2/40
Expensive parser function count: 0/100
Unstrip recursion depth: 0/20
Unstrip post‐expand size: 4/5000000 bytes
Transclusion expansion time report (%,ms,calls,template)
100.00%    0.000      1 -total
