# URL: https://wiki.eragames.rip/index.php/Emuera/exvar
# fetch date: 2026-09-03

Contents 1 Constants (Literals) 1.1 Constant Notation 2 変数 2.1 変数サイズの指定 2.2 ローカル変数 2.2.1 LOCAL 2.2.2 LOCALS 2.2.3 ARG 2.2.4 ARGS 2.2.5 （ユーザー定義のプライベート変数） 2.3 セーブデータ間で共有される変数 2.3.1 GLOBAL 2.3.2 GLOBALS 2.3.3 （ユーザー定義のグローバル変数） 2.4 キャラクタ変数 2.4.1 NICKNAME 2.4.2 MASTERNAME 2.4.3 CSTR 2.4.4 CUP 2.4.5 CDOWN 2.4.6 DOWNBASE 2.4.7 TCVAR 2.4.8 CDFLAG 2.4.9 （ユーザー定義のキャラクタ変数） 2.5 csv関連 2.5.1 csv変数 2.5.2 gamebase.csv Variables 2.5.3 WINDOW_TITLE 2.5.4 その他のcsvに関わる変数 2.6 セーブロード関連 2.6.1 LASTLOAD_* 2.6.2 SAVEDATA_TEXT 2.6.3 （ユーザー定義のセーブ可能な広域変数） 2.7 多次元配列変数 2.7.1 DITEMTYPE 2.7.2 DA ～ DE 2.7.3 TA, TB 2.7.4 （ユーザー定義の多次元配列変数） 2.8 デバッグ変数 2.8.1 !__FILE!__ 2.8.2 !__LINE!__ 2.8.3 !__FUNCTION!__ 2.9 その他 2.9.1 TSTR 2.9.2 RANDDATA 2.9.3 LINECOUNT 2.9.4 ISTIMEOUT 2.9.5 !__INT_MAX!__ 2.9.6 !__INT_MIN!__ 2.9.7 （ユーザー定義の広域変数） 2.9.8 （ユーザー定義の定数） 2.9.9 （ユーザー定義の参照型変数） 2.10 makerとの仕様の違い 2.10.1 NAME 2.10.2 CALLNAME 2.10.3 RAND 2.10.4 CHARANUM 2.11 暫定的な仕様表 2.11.1 eramakerにも存在していた変数 2.11.2 Emuera専用変数

### Constants (Literals) [ edit ]

#### Constant Notation [ edit ]

With the exception of Octal notation, Emuera supports the same notations as  吉里吉里 (KiriKiri) . 
For example, the following lines all have the same meaning.
```
	X = 32
	X = 0b100000
	X = 0x20
	X = 1p5
```
From top to bottom, the usual decimal notation, binary, hexadecimal, and 1 × 2 to the power of 5. 
Notation like 1p5 is convenient when you want to get or set bits with bit operators. 
For example, the conditional statement below becomes true when the lower 0th or 3rd bit of A stands.
```
	IF (A & 1p0)||(A & 1p3)
```
Also, by using e instead of p, you can express n × 10 to the power of m. 
For example, 13e3 equals 13000. 
Since the above is only a notation of a constant, you can not use it as an expression. 
 The following notation is wrong.
```
	X = 13e(A + 1)
```
We have not adopted octal notation because of compatibility issues with eramaker. 
"012" is interpreted as 12 instead of 10.

### 変数 [ edit ]

#### 変数サイズの指定 [ edit ]

Emueraではcsvフォルダに!VariableSize.csvという名前のファイルを置くことで既存の変数の要素数を指定できます。 
また、要素数に-1を指定することにより、ERB中でのその変数の使用を禁止することができます。 
 
禁止設定された変数をERB中で代入や参照するとエラーになります。
禁止設定された変数をシステムが必要とする場合、代入処理は無視され、値は常に-1と扱われます。 
（MONEYやNEXTCOMを禁止した場合に発生する状況です）

#### ローカル変数 [ edit ]

##### LOCAL [ edit ]

##### LOCALS [ edit ]

※この変数はobsoleteです。代わりに#DIM、#DIMSの使用を検討してください。 
詳しくは[UserVars ユーザー定義の変数]を参照してください。 
 
ローカル変数（局所変数）です。 
LOCALは整数、LOCALSは文字列となります 
サイズは基本はLOCALが1000、LOCALSが100です。 
また、「#LOCALSIZE <設定する要素数>」および「#LOCALSSIZE <設定する要素数>」 
によって関数ごとに個別に要素数を変更できます。（ただし設定できる値は0より大きい整数） 
セーブはされません。
```
	@EVENTFIRST
		LOCAL:10 = 123
		CALL FUNC001
		PRINTV LOCAL:10
		WAIT
	@FUNC001
		LOCAL:10 = 567
		RETURN
```
上のコードのPRINTVの結果は"123"になります。 
FUNC001の中でLOCAL:10を変更していますが、EVENTFIRST内のLOCALには変更が及びません。 
多くの言語のローカル変数とは異なり、関数の呼び出しの際に初期化はされません。 
 
内部的には"LOCAL@関数名"という変数を作成しています。 
なのでイベント関数など同じ名前の関数が複数ある場合は共用です。また、再帰的に呼び出された場合は同じ変数をつかうことになります。 
LOCAL@EVENTFIRST:10 = 567のように他の関数用の変数も呼び出せますが推奨しません（デバッグ用の機能です）。 
なお、他の関数のものを呼び出す時に、呼び出し先関数名に演算子等が含まれているとエラーとなります

##### ARG [ edit ]

##### ARGS [ edit ]

ローカル変数です。 
ARGは整数、ARGSは文字列となります 
サイズは基本はARGが1000、ARGSが100で!VariableSize.csvで変更できます。 
また、関数の引数で定義された分だけを問題なく使えるだけの要素数を自動で確保します。（VariableSize.csvで指定した数未満にはなりません）
```
	@FUNC002, ARG:0, ARG:1, ARG,1100
		LOCAL = ARG:0 * ARG:1 / 100
		RETURN LOCAL
```
この場合、ARGの要素数は本来1000ですが、@FUNC002においてARGの要素数は0～1100の1101となります。 
関数における引数指定のために使うことを想定しているので、それ以外の用途に用いると可読性が下がるかもしれません。

##### （ユーザー定義のプライベート変数） [ edit ]

特定の関数中で定義#DIMまたは#DIMSを用いて定義した変数はプライベート変数であるため、ローカル変数と同様に扱えます。 
詳しくは[UserVars ユーザー定義の変数]を参照してください。

#### セーブデータ間で共有される変数 [ edit ]

##### GLOBAL [ edit ]

##### GLOBALS [ edit ]

異なるセーブデータ間で共有することができる変数です。 
GLOBALは整数、GLOBALSは文字列となります 
サイズは基本はGLOBALが1000、GLOBALSが100で!VariableSize.csvで変更できます。 
他のデータと一緒にはセーブ・ロードされません。 
グローバル変数をセーブするにはSAVEGLOBAL命令を使います。 
SAVEGLOBALを行うと、"global.sav"にGLOBALとGLOBALSが保存されます。 
書き込むときに既に"global.sav"がある場合、上書きします 
LOADGLOBAL命令により、"global.sav"からGLOBALとGLOBALSを読み込めます。 
LOADGLOBALは@EVENTFIRSTおよび@EVENTLOADのタイミングで行うことを勧めます。 
GLOBAL、GLOBALSを経由すれば異なるセーブデータ間でデータを共有することができます。

##### （ユーザー定義のグローバル変数） [ edit ]

ERH中で定義"#DIM GLOBAL"または"#DIMS GLOBAL"を用いて定義した変数はグローバル変数となります。 
また、"#DIM SAVEDATA GLOBAL"とすることでセーブされるグローバル変数となります。 
詳しくは[ERH ヘッダーファイル（ERH）]を参照してください。

#### キャラクタ変数 [ edit ]

##### NICKNAME [ edit ]

##### MASTERNAME [ edit ]

NAMEやCALLNAMEと同様のセーブされる文字列型変数です。 
chara*.csvでは"NICKNAME"、"MASTERNAME"または"あだ名"、"主人の呼び方"として指定します。

##### CSTR [ edit ]

セーブされる文字列配列変数です。 
CFLAGの文字列版です。 
chara*.csvでは"CSTR"として指定します。

##### CUP [ edit ]

##### CDOWN [ edit ]

##### DOWNBASE [ edit ]

##### TCVAR [ edit ]

数値型配列変数です。 
それぞれ、UP、DOWN、LOSEBASE、TFLAGのキャラクタ変数版として使用することが想定されています。 
そのため初期化タイミングやセーブの可不可もこれらの変数と同じです。 
ただし、CUPとCDOWNはUPCHECK命令の代わりにCUPCHECK命令を使います。

##### CDFLAG [ edit ]

数値型キャラクター三次元配列変数です。
```
CDFLAG:MASTER:0:2 
```
などのように第一引数がキャラ登録番号なのは従来のキャラクター変数と同じですが、 
第二引数と第三引数が必要になります。

##### （ユーザー定義のキャラクタ変数） [ edit ]

ERH中で定義"#DIM CHARADATA"または"#DIMS CHARADATA"を用いて定義した変数はキャラクタ変数となり、CFLAGなどと同様自由に扱えます。 
詳しくは[ERH ヘッダーファイル（ERH）]を参照してください。

#### csv関連 [ edit ]

##### csv変数 [ edit ]

各csvで定義された値を参照するための変数です。 
使い方は例えばTALENTNAMEとtalent.csvの関係と同じです。 
全て代入不可でセーブされない1次元配列変数です。 
csvで定義されていない場合、0または空文字列を返します。
変数名 | ファイル | 型 | 要素数
ITEMPRICE | item.csv | 整数型 | 1000
TRAINNAME | train.csv | 文字列型 | 1000
BASENAME | base.csv | 文字列型 | 100
EQUIPNAME | equip.csv | 文字列型 | 100
TEQUIPNAME | tequip.csv | 文字列型 | 100
STAINNAME | stain.csv | 文字列型 | 1000
EXNAME | ex.csv | 文字列型 | 100
SOURCENAME | source.csv | 文字列型 | 100
FLAGNAME | flag.csv | 文字列型 | 10000
TFLAGNAME | tflag.csv | 文字列型 | 1000
CFLAGNAME | cflag.csv | 文字列型 | 1000
TCVARNAME | tcvar.csv | 文字列型 | 100
STRNAME | strname.csv | 文字列型 | 20000
TSTRNAME | tstr.csv | 文字列型 | 100
CSTRNAME | cstr.csv | 文字列型 | 100
SAVESTRNAME | savestr.csv | 文字列型 | 100
CDFLAGNAME1 | cdflag1.csv | 文字列型 | 1
CDFLAGNAME2 | cdflag2.csv | 文字列型 | 1
GLOBALNAME | global.csv | 文字列型 | 100
GLOBALSNAME | globals.csv | 文字列型 | 100
cstr.csv等とstr.csvの役割を混同しないでください。 
str.csvは変数STRに代入される値を決めるファイルですが、cstr.csvはCSTRNAMEを定めるファイルです。 
STRNAMEを定めるファイルはstrname.csvとなっています。str.csv及びstrname.csvの使い方に注意してください。

##### gamebase.csv Variables [ edit ]

gamebase.csvで定義された値を参照するための変数です。 
すべて非配列、代入不可、セーブされない変数です。
Var Name | Keyword | Type | Description
GAMEBASE_AUTHOR | 作者 | String | Author
GAMEBASE_INFO | 追加情報 | String | Additional information
GAMEBASE_YEAR | 製作年 | String | Production year
GAMEBASE_TITLE | タイトル | String | Title
GAMEBASE_GAMECODE | コード | Integer | Code
GAMEBASE_VERSION | バージョン | Integer | Version
GAMEBASE_ALLOWVERSION | バージョン違い認める | Integer | バージョン違い認める
GAMEBASE_DEFAULTCHARA | 最初からいるキャラ | Integer | 最初からいるキャラ
GAMEBASE_NOITEM | アイテムなし | Integer | No items

##### WINDOW_TITLE [ edit ]

Emueraのウィンドウのタイトルバーに表示されている文字列です。 
非配列の文字列型変数です。初期値はgamebase.csvの"ウィンドウタイトル"に設定された値です。 
"ウィンドウタイトル"が設定されていなければ"タイトル"と"バージョン"から生成します。 
"タイトル"も設定されていない場合、"Emuera"になります。

##### その他のcsvに関わる変数 [ edit ]

====== MONEYLABEL
お金の単位が記録された変数です。 
非配列の文字列型変数で、代入不可、セーブされない変数です。 
初期値は_Replace.csvにて設定される"お金の単位"に設定された値です。 
"お金の単位"が設定されていない場合、eramakerと同じ"$"になります。
====== DRAWLINESTR
DRAWLINE命令を行ったときに表示される文字列が記録された変数です。 
非配列の文字列型変数で、代入不可、セーブされない変数です。 
初期値は_Replace.csvにて設定される"DRAWLINE文字"に設定された値を繰り返したものです。 
そのため"DRAWLINE文字"に設定された値そのままの文字列が入っているわけではありません。 
"DRAWLINE文字"が設定されていない場合、eramakerと同じ、例えば 
"------------------------------------------------------------------------------------------------------------"になります。

#### セーブロード関連 [ edit ]

##### LASTLOAD_* [ edit ]

最後にロードしてデータの情報を参照するための変数です。 
参照はできますが代入はできません。 
全て初期値は-1または空文字列です。 
ロードした時に更新され、RESETDATAやメニューの"タイトルに戻る"を実行すると初期値に戻ります。
- LASTLOAD_VERSION
    最後にロードしたデータのバージョン（gamebase.csvで定義する値）
- LASTLOAD_NO
    最後にロードしたデータの番号（save*.savの*に相当する番号）
- LASTLOAD_TEXT
    テキスト（PUTFORMで追加するテキスト。SAVEDATA_TEXT）

##### SAVEDATA_TEXT [ edit ]

セーブデータに保存され、セーブ/ロード画面で表示されるテキストです。 
ロード後にLASTLOAD_TEXTで参照できるテキストでもあります。 
参照も代入も可能です。 
@SAVEINFOが呼ばれる時に現在時刻を表す文字列が代入され、PUTFORMで追記することができる文字列です。 
@SAVEINFO中でこの文字列に直接代入することで時刻表示もカスタマイズできます。 
SAVEGAMEとPUTFORMを使わない（SAVELOAD.ERBを使う場合）には出番がありません。

##### （ユーザー定義のセーブ可能な広域変数） [ edit ]

ERH中で定義"#DIM SAVEDATA"または"#DIMS SAVEDATA"を用いて定義した変数はセーブ可能な広域変数となります。 
ただし、"#DIMS SAVEDATA"を用いてセーブ可能な多次元広域変数を定義する場合、オプション「セーブデータをバイナリ形式で保存する」が有効になっている必要があります。 
詳しくは[ERH ヘッダーファイル（ERH）]を参照してください。

#### 多次元配列変数 [ edit ]

##### DITEMTYPE [ edit ]

##### DA ～ DE [ edit ]

※この変数はobsoleteです。代わりに#DIM、#DIMSを使用して用途に応じた名前を付けることを検討してください。 
詳しくは[UserVars ユーザー定義の変数]を参照してください。
固定長の整数型二次元配列です。 
DITEMTYPE:1:2 のように呼び出します。引数は省略できません。 
eramakerの二重配列は第一引数にはキャラクタ登録番号を指定するのでCHARANUMによって配列の大きさが異なります。 
DITEMTYPEなどの二次元配列は!VariableSize.csvで指定した大きさのまま変化することはありません。 
VARSIZE命令の対象にした場合、RESULT:0とRESULT:1にそれぞれ要素数が代入されます。 
!VariableSize.csvで DITEMTYPE,100,200としているとDITEMTYPE:99:199まで使用でき、VARSIZE命令ではRESULT:0とRESULT:1に100と200が代入されます。

##### TA, TB [ edit ]

※この変数はobsoleteです。代わりに#DIM、#DIMSを使用して用途に応じた名前を付けることを検討してください。 
詳しくは[UserVars ユーザー定義の変数]を参照してください。
固定長の整数型三次元配列です。 
TA:1:2:3 のように呼び出します。引数は省略できません。 
サイズは標準では100×100×100です。つまりTA:99:99:99まで使えます。 
!VariableSize.csvでサイズを変更することが可能ですが、100万を超えるサイズは指定できません。 
VARSIZE命令の対象にした場合、RESULT:0とRESULT:1とRESULT:2にそれぞれの要素数が代入されます。

##### （ユーザー定義の多次元配列変数） [ edit ]

ver1.808以降、定義#DIMまたは#DIMSを用いて定義した変数を多次元とすることができるようになりました。 
詳しくは[UserVars ユーザー定義の変数]を参照してください。

#### デバッグ変数 [ edit ]

デバッグ変数はデバッグ用の情報を提供するための変数です。 
デバッグ変数は[debug デバッグモード]で起動したときのみ意味のある値を返します。 
通常モードで起動したときには空文字列又は0を返します。 
 
全て名前の前と後ろにアンダースコア"_"が2つ付いています。

##### !__FILE!__ [ edit ]

一次元読み取り専用変数です。 
現在実行中のスクリプトのファイル名を返します。 
ファイル名はエラー情報などと同じようにフォルダ構造及び拡張子までを含む形式です。 
 
システム入力待ち中にデバッグコマンドや変数ウォッチから参照した場合など、 
現在実行中のスクリプトがない場合は空文字列を返します。

##### !__LINE!__ [ edit ]

一次元読み取り専用変数です。 
現在実行中のスクリプトの行番号を返します。 
行番号はエラー情報などと同じように1から始まる数字です。 
 
現在実行中のスクリプトがない場合は-1を返します。

##### !__FUNCTION!__ [ edit ]

一次元読み取り専用変数です。 
現在実行中の関数名を返します。 
関数名は"@"及び引数のリストを含みません。 
 
現在実行中のスクリプトがない場合は空文字列を返します。

#### その他 [ edit ]

##### TSTR [ edit ]

文字列型1次元配列です。一次元配列で、セーブはされません。 
TFLAGと同じタイミングで初期化されます。

##### RANDDATA [ edit ]

乱数の状態を記憶するための配列です。数値型の一次元配列で代入可、セーブされます。 
DUMPRANDによって記録され、INITRANDによって読み出されます。

##### LINECOUNT [ edit ]

これまでにPRINTした行数を返す変数です。 
LINECOUNTは起動直後から改行(ウインドウ幅による改行を含まない)のたび+1、CLEARLINEした数だけ-されます。 
ログバッファー(標準5000)をあふれたことによる削除によっては変動しません。 
非配列の数値型変数で、代入・セーブ共に不可能です。 
また、行の数え方はCLEARLINEと同様です。

##### ISTIMEOUT [ edit ]

以下、私家改造版1809+v2に添付のreadmeより
```
　○TINPUT系がタイムアウトしたかをチェックする変数ISTIMEOUT追加
　　　TINPUT系が呼び出された時に0に初期化され、タイムアウトすると1となります。
```
ver1815現在、この変数は利用不能な可能性があります。

##### !__INT_MAX!__ [ edit ]

##### !__INT_MIN!__ [ edit ]

数値型変数の定義域最大値、最小値という定数が記録されている非配列の数値型変数で、代入・セーブ共に不可能です。 
デバッグ変数ではないので普通に起動した場合でも使用できます。

##### （ユーザー定義の広域変数） [ edit ]

ERH中で定義#DIMまたは#DIMSを用いて定義した変数は広域変数となり、一文字変数（Aなど）同様自由に扱えます。 
詳しくは[ERH ヘッダーファイル（ERH）]を参照してください。

##### （ユーザー定義の定数） [ edit ]

ERH中および特定の関数中で、定義#DIMまたは#DIMSを用いて定義した変数は1次元配列の定数となり、代入できない変数として扱えます。 
詳しくは[UserVars ユーザー定義の変数]を参照してください。

##### （ユーザー定義の参照型変数） [ edit ]

特定の関数中で定義"#DIM REF"または"#DIMS REF"を用いて定義した変数は参照型変数となります。 
詳しくは[UserVars ユーザー定義の変数]を参照してください。

#### makerとの仕様の違い [ edit ]

##### NAME [ edit ]

##### CALLNAME [ edit ]

eramakerでは代入ができません。 
Emueraでは代入できるようになっています。

##### RAND [ edit ]

##### CHARANUM [ edit ]

eramakerでは代入でき、セーブ・ロードされますが代入した値を使用する方法がありません。 
Emueraでは代入を禁止しています。

#### 暫定的な仕様表 [ edit ]

##### eramakerにも存在していた変数 [ edit ]

変数名 | 型 | 配列 | 代入 | セーブ | 禁止 | 初期値 | 初期化されるタイミング | 備考
RESULT | 整数 | 一次元 | ○ | ○ | × | - | - | -
RESULTS | 文字列 | 一次元 | ○ | × | × | - | - | -
A～Z | 整数 | 一次元 | ○ | ○ | ○ | - | - | -
COUNT | 整数 | 一次元 | ○ | ○ | × | - | - | COUNT:0はREPEATにおいてカウンターとして使用される
DAY | 整数 | 一次元 | ○ | ○ | ○ | - | - | -
TIME | 整数 | 一次元 | ○ | ○ | ○ | - | - | -
MONEY | 整数 | 一次元 | ○ | ○ | ○ | - | - | -
MASTER | 整数 | 一次元 | ○ | ○ | ○ | - | - | -
TARGET | 整数 | 一次元 | ○ | ○ | × | :0 = 1 | - | -
ASSI | 整数 | 一次元 | ○ | ○ | ○ | :0 = -1 | - | -
PLAYER | 整数 | 一次元 | ○ | ○ | ○ | - | - | -
ASSIPLAY | 整数 | 一次元 | ○ | ○ | ○ | :0 = 0 | BEGIN TRAIN | -
SELECTCOM | 整数 | 一次元 | ○ | ○ | × | - | - | -
PREVCOM | 整数 | 一次元 | ○ | ○ | ○ | :0 = -1 | BEGIN TRAIN | -
NEXTCOM | 整数 | 一次元 | ○ | ○ | ○ | :0 = -1 | BEGIN TRAIN | -
LOSEBASE | 整数 | 一次元 | ○ | ○ | ○ | 全て0 | @SHOW_USERCOM終了時 | __BASENAMEによって要素の指定ができる__
UP | 整数 | 一次元 | ○ | ○ | ○ | 全て0 | @SHOW_USERCOM終了時 UPCHECK時 | __PALAMNAMEによって要素の指定ができる__
DOWN | 整数 | 一次元 | ○ | ○ | ○ | 全て0 | @SHOW_USERCOM終了時 UPCHECK時 | __PALAMNAMEによって要素の指定ができる__
PALAMLV | 整数 | 一次元 | ○ | ○ | × | ___replace.csv "PALAMLVの初期値"__ | - | -
EXPLV | 整数 | 一次元 | ○ | ○ | × | ___replace.csv "EXPLVの初期値"__ | - | -
EJAC | 整数 | 一次元 | ○ | ○ | ○ | :0 = 10000 | - | -
FLAG | 整数 | 一次元 | ○ | ○ | ○ | - | - | __FLAGNAMEによって要素の指定ができる__
TFLAG | 整数 | 一次元 | ○ | ○ | ○ | 全て0 | BEGIN TRAIN | __TFLAGNAMEによって要素の指定ができる__
ITEM | 整数 | 一次元 | ○ | ○ | ○ | - | - | __ITEMNAMEによって要素の指定ができる__
ITEMSALES | 整数 | 一次元 | ○ | ○ | ○ | - | - | __ITEMNAMEによって要素の指定ができる__
BOUGHT | 整数 | 一次元 | ○ | ○ | ○ | - | - | -
PBAND | 整数 | 一次元 | ○ | ○ | ○ | __:0 = _replace.csv "PBANDの初期値"__ | - | -
CHARANUM | 整数 | __無次元__ | __×__ | __×__ | × | - | - | どんな要素を指定してもキャラの登録数が返ってくる。
RAND | 整数 | __無次元__ | __×__ | __×__ | × | - | - | __RAND:XのXが0や負の値の場合エラーとなるように__ それ以外の場合0～(要素数-1)までのランダムな値が返ってくる。
STR | 文字列 | 一次元 | ○ | × | ○ | STR.CSV | - | __STRNAMEによって要素の指定ができる__
SAVESTR | 文字列 | 一次元 | ○ | ○ | ○ | - | - | __SAVESTRNAMEによって要素の指定ができる__
NO | 数値 | キャラ＋無次元 | ○ | ○ | × | - | - | CHARA**.CSVで「番号,**」により指定する
ISASSI | 数値 | キャラ＋無次元 | ○ | ○ | × | - | - | CHARA**.CSVで「助手,1」と指定することで初期状態から助手として扱われる
NAME | 文字列 | キャラ＋無次元 | __○__ | ○ | × | - | - | CHARA**.CSVで「名前,**」により指定する
CALLNAME | 文字列 | キャラ＋無次元 | __○__ | ○ | × | - | - | CHARA**.CSVで「呼び名,**」により指定する
BASE | 数値 | キャラ＋ー次元 | ○ | ○ | ○ | - | - | ADDCHARA時に全要素がMAXBASEと同じ値になる __BASENAMEによって要素の指定ができる__
MAXBASE | 数値 | キャラ＋ー次元 | ○ | ○ | ○ |  | - | CHARA**.CSVで「基礎,*,**」により指定する __BASENAMEによって要素の指定ができる__
ABL | 数値 | キャラ＋ー次元 | ○ | ○ | ○ | - | - | CHARA**.CSVで「能力,*,**」により指定する __ABLNAMEによって要素の指定ができる__
TALENT | 数値 | キャラ＋ー次元 | ○ | ○ | ○ | - | - | CHARA**.CSVで「素質,*」により指定する __「素質,*,**」のように3番目の値も指定できるように TALENTNAMEによって要素の指定ができる__
EXP | 数値 | キャラ＋ー次元 | ○ | ○ | ○ | - | - | CHARA**.CSVで「経験,*,**」により指定する __EXPNAMEによって要素の指定ができる__
MARK | 数値 | キャラ＋ー次元 | ○ | ○ | ○ | - | - | CHARA**.CSVで「刻印,*,**」により指定する __MARKNAMEによって要素の指定ができる__
RELATION | 数値 | キャラ＋ー次元 | ○ | ○ | ○ | __replace.csv "RELATIONの初期値"__ | - | CHARA**.CSVで「相性,*,**」により指定する __NAMEまたはCALLNAMEによって要素の指定ができる__
JUEL | 数値 | キャラ＋ー次元 | ○ | ○ | ○ | - | - | __CHARA**.CSVで「珠,*,**」により指定できるように PALAMNAMEによって要素の指定ができる__
CFLAG | 数値 | キャラ＋ー次元 | ○ | ○ | ○ | - | - | CHARA**.CSVで「フラグ,*,**」により指定する __CFLAGNAMEによって要素の指定ができる__
EQUIP | 数値 | キャラ＋ー次元 | ○ | ○ | ○ | - | - | __CHARA**.CSVで「装着物,*,**」により指定できるように EQUIPNAMEによって要素の指定ができる__
TEQUIP | 数値 | キャラ＋ー次元 | ○ | ○ | ○ | 全て0 | BEGIN TRAIN | __TEQUIPNAMEによって要素の指定ができる__
PALAM | 数値 | キャラ＋ー次元 | ○ | ○ | ○ | 全て0 | BEGIN TRAIN | __PALAMNAMEによって要素の指定ができる__
STAIN | 数値 | キャラ＋ー次元 | ○ | ○ | × | ___replace.csv "汚れの初期値"__ | BEGIN TRAIN | __STAINNAMEによって要素の指定ができる__
EX | 数値 | キャラ＋ー次元 | ○ | ○ | ○ | 全て0 | BEGIN TRAIN | __EXNAMEによって要素の指定ができる__
SOURCE | 数値 | キャラ＋ー次元 | ○ | ○ | ○ | 全て0 | BEGIN TRAIN @SOURCE_CHECK終了時 | __SOURCENAMEによって要素の指定ができる__
NOWEX | 数値 | キャラ＋ー次元 | ○ | ○ | ○ | 全て0 | @EVENTCOM直前 | @USERCOM前には更新されない __EXNAMEによって要素の指定ができる__
GOTJUEL | 数値 | キャラ＋ー次元 | ○ | ○ | ○ | 全て0 | BEGIN TRAIN | __PALAMNAMEによって要素の指定ができる__
ABLNAME | 文字列 | 一次元 | × | × | ○ | ABL.CSV | - | -
TALENTNAME | 文字列 | 一次元 | × | × | ○ | TALENT.CSV | - | -
EXPNAME | 文字列 | 一次元 | × | × | ○ | EXP.CSV | - | -
MARKNAME | 文字列 | 一次元 | × | × | ○ | MARK.CSV | - | -
PALAMNAME | 文字列 | 一次元 | × | × | ○ | PALAM.CSV | - | -
ITEMNAME | 文字列 | 一次元 | × | × | ○ | ITEM.CSV | - | -
NOITEM | 整数 | 一次元 | ○ | ○ | ○ | :0 = gamebase.csv "アイテムなし" | - | 0と1以外も指定できる
アンダーライン付の部分はeramakerとEmueraで仕様が異なる部分である

##### Emuera専用変数 [ edit ]

変数名 | 型 | 配列 | 代入 | セーブ | 禁止 | 初期値 | 初期化されるタイミング | 備考
LOCAL | 整数 | 一次元 | ○ | × | ○ | - | - | #LOCALSIZEによって関数ごとに要素数が変化
LOCALS | 文字列 | 一次元 | ○ | × | ○ | - | - | #LOCALSSIZEによって関数ごとに要素数が変化
ARG | 整数 | 一次元 | ○ | × | ○ | 任意 | 関数が呼び出された時※ | ※引数に設定されている場合のみ 関数ごとに引数で定義された分だけの要素数を確保
ARGS | 文字列 | 一次元 | ○ | × | ○ | 任意 | 関数が呼び出された時※ | ※引数に設定されている場合のみ 関数ごとに引数で定義された分だけの要素数を確保
(Private) | 任意 | 任意 | 任意 | × | × | 任意 | ゲーム開始 関数が呼び出された時※ | ※引数に設定されている場合のみ 関数中の#DIMまたは#DIMSで定義
(Refer) | 任意 | 任意 | ※ | ※ | × | - | - | ※参照先に依存 関数中の"#DIM REF"または"#DIMS REF"で定義
(Wide_area) | 任意 | 任意 | 任意 | 任意 | × | 任意 | ゲーム開始 | ERH中の#DIMまたは#DIMSで定義
GLOBAL | 整数 | 一次元 | ○ | ※ | × | - | - | ※SAVEGLOBALでセーブ、LOADGLOBALでロードする GLOBALNAMEによって要素の指定ができる
GLOBALS | 文字列 | 一次元 | ○ | ※ | × | - | - | ※SAVEGLOBALでセーブ、LOADGLOBALでロードする GLOBALSNAMEによって要素の指定ができる
LINECOUNT | 整数 | 無次元 | × | × | × | - | - | -
ISTIMEOUT | 整数 | 無次元 | × | × | × | 0 | TINPUT系命令実行時 | TINPUT系命令がタイムアウトした場合1が代入される
!__INT_MAX!__ | 整数 | 無次元 | × | × | × | 9223372036854775807 | - | -
!__INT_MIN!__ | 整数 | 無次元 | × | × | × | -9223372036854775808 | - | -
RANDDATA | 整数 | 一次元 | ○ | ○ | × | - | - | -
TSTR | 文字列 | 一次元 | ○ | × | ○ | 全て空文字列 | BEGIN TRAIN | TSTRNAMEによって要素の指定ができる
DA | 整数 | 二次元 | ○ | ○ | ○ | - | - | -
DB | 整数 | 二次元 | ○ | ○ | ○ | - | - | -
DC | 整数 | 二次元 | ○ | ○ | ○ | - | - | -
DD | 整数 | 二次元 | ○ | ○ | ○ | - | - | -
DE | 整数 | 二次元 | ○ | ○ | ○ | - | - | -
DITEMTYPE | 整数 | 二次元 | ○ | ○ | ○ | - | - | -
TA | 整数 | 三次元 | ○ | ○ | ○ | - | - | -
TB | 整数 | 三次元 | ○ | ○ | ○ | - | - | -
NICKNAME | 文字列 | キャラ＋無次元 | ○ | ○ | × | - | - | CHARA**.CSVで「あだ名,**」により指定する
MASTERNAME | 文字列 | キャラ＋無次元 | ○ | ○ | × | - | - | CHARA**.CSVで「主人の呼び方,**」により指定する
DOWNBASE | 整数 | キャラ＋ー次元 | ○ | ○ | ○ | 全て0 | @SHOW_USERCOM終了時 | BASENAMEによって要素の指定ができる
CUP | 整数 | キャラ＋ー次元 | ○ | ○ | ○ | 全て0 | @SHOW_USERCOM終了時 UPCHECK時 | PALAMNAMEによって要素の指定ができる
CDOWN | 整数 | キャラ＋ー次元 | ○ | ○ | ○ | 全て0 | @SHOW_USERCOM終了時 UPCHECK時 | PALAMNAMEによって要素の指定ができる
TCVAR | 整数 | キャラ＋ー次元 | ○ | ○ | ○ | 全て0 | BEGIN TRAIN | TCVARNAMEによって要素の指定ができる
CSTR | 文字列 | キャラ＋ー次元 | ○ | ○ | ○ | - | - | CHARA**.CSVで「CSTR,*,**」により指定する CSTRNAMEによって要素の指定ができる
CDFLAG | 整数 | キャラ＋二次元 | ○ | ○ | ○ | - | - | CFDLAGNAME1およびCDFLAGNAME2によって要素の指定ができる 要素数の初期設定値が1･1であることに注意
ITEMPRICE | 整数 | 一次元 | × | × | ○ | item.csv | - | ITEMNAMEによって要素の指定ができる
TRAINNAME | 文字列 | 一次元 | × | × | ○ | train.csv | - | -
BASENAME | 文字列 | 一次元 | × | × | ○ | base.csv | - | -
EQUIPNAME | 文字列 | 一次元 | × | × | ○ | equip.csv | - | -
TEQUIPNAME | 文字列 | 一次元 | × | × | ○ | tequip.csv | - | -
STAINNAME | 文字列 | 一次元 | × | × | ○ | stain.csv | - | -
EXNAME | 文字列 | 一次元 | × | × | ○ | ex.csv | - | -
SOURCENAME | 文字列 | 一次元 | × | × | ○ | source.csv | - | -
FLAGNAME | 文字列 | 一次元 | × | × | ○ | flag.csv | - | -
TFLAGNAME | 文字列 | 一次元 | × | × | ○ | tflag.csv | - | -
CFLAGNAME | 文字列 | 一次元 | × | × | ○ | cflag.csv | - | -
TCVARNAME | 文字列 | 一次元 | × | × | ○ | tcvar.csv | - | -
STRNAME | 文字列 | 一次元 | × | × | ○ | strname.csv | - | str.csvで指定するのはSTRの中身であって要素名ではない
TSTRNAME | 文字列 | 一次元 | × | × | ○ | tstr.csv | - | -
CSTRNAME | 文字列 | 一次元 | × | × | ○ | cstr.csv | - | -
SAVESTRNAME | 文字列 | 一次元 | × | × | ○ | savestr.csv | - | -
CDFLAGNAME1 | 文字列 | 一次元 | × | × | ○ | cdflag1.csv | - | -
CDFLAGNAME2 | 文字列 | 一次元 | × | × | ○ | cdflag2.csv | - | -
GLOBALNAME | 文字列 | 一次元 | × | × | ○ | global.csv | - | -
GLOBALSNAME | 文字列 | 一次元 | × | × | ○ | globals.csv | - | -
GAMEBASE_AUTHOR | 文字列 | 無次元 | × | × | × | gamebase.csv "作者" | - | -
GAMEBASE_INFO | 文字列 | 無次元 | × | × | × | gamebase.csv "追加情報" | - | -
GAMEBASE_YEAR | 文字列 | 無次元 | × | × | × | gamebase.csv "製作年" | - | -
GAMEBASE_TITLE | 文字列 | 無次元 | × | × | × | gamebase.csv "タイトル" | - | -
GAMEBASE_GAMECODE | 整数 | 無次元 | × | × | × | gamebase.csv "コード" | - | -
GAMEBASE_VERSION | 整数 | 無次元 | × | × | × | gamebase.csv "バージョン" | - | -
GAMEBASE_ALLOWVERSION | 整数 | 無次元 | × | × | × | gamebase.csv "バージョン違い認める" | - | -
GAMEBASE_DEFAULTCHARA | 整数 | 無次元 | × | × | × | gamebase.csv "最初からいるキャラ" | - | -
GAMEBASE_NOITEM | 整数 | 無次元 | × | × | × | gamebase.csv "アイテムなし" | - | -
WINDOW_TITLE | 文字列 | 無次元 | ○ | × | × | gamebase.csv "ウィンドウタイトル"※ | - | ※ない場合は"タイトル"と"バージョン"から生成 "タイトル"もない場合は「Emuera」
MONEYLABEL | 文字列 | 無次元 | × | × | × | _replace.csv "お金の単位"※ | - | ※ない場合は「$」
DRAWLINESTR | 文字列 | 無次元 | × | × | × | _replace.csv "DRAWLINE文字"※ | - | ※ない場合は「-」の繰り返し
LASTLOAD_VERSION | 整数 | 無次元 | × | × | × | -1 | ゲーム開始 RESETDATA時 | ロード時に値が更新される
LASTLOAD_NO | 整数 | 無次元 | × | × | × | -1 | ゲーム開始 RESETDATA時 | ロード時に値が更新される
LASTLOAD_TEXT | 文字列 | 無次元 | × | × | × | 空文字列 | ゲーム開始 RESETDATA時 | ロード時に値が更新される
SAVEDATA_TEXT | 文字列 | 無次元 | ○ | ※ | × | ※※ | @SAVEINFO開始時 | ※セーブデータのタイトルとして保存される ※※現在時刻を表す文字列
NewPP limit report
Cached time: 20260902121224
Cache expiry: 86400
Dynamic content: false
CPU time usage: 0.145 seconds
Real time usage: 0.150 seconds
Preprocessor visited node count: 238/1000000
Preprocessor generated node count: 244/1000000
Post‐expand include size: 0/2097152 bytes
Template argument size: 0/2097152 bytes
Highest expansion depth: 2/40
Expensive parser function count: 0/100
Unstrip recursion depth: 0/20
Unstrip post‐expand size: 0/5000000 bytes
Transclusion expansion time report (%,ms,calls,template)
100.00%    0.000      1 -total
