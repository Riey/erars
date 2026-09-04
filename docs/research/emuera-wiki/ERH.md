# URL: https://wiki.eragames.rip/index.php/Emuera/ERH
# fetch date: 2026-09-03

Contents 1 ヘッダーファイル 1.1 広域変数の宣言 1.1.1 SAVEDATA キーワード 1.1.2 CHARADATA キーワード 1.1.3 GLOBAL キーワード 1.2 マクロの定義 1.2.1 基本的な使い方 1.2.2 マクロの多重展開 1.2.3 プリプロセッサ 1.2.4 マクロの制限

### ヘッダーファイル [ edit ]

ERBフォルダの中には拡張子ERBのファイルのほか、拡張子ERHのファイルを置くことができます。 
ERHファイルにはERBより先に処理すべき内容を記載します。 
具体的には#DIM及び#DIMSによる広域変数の定義と#DEFINEによるマクロの定義です。 
ヘッダー中に#DIM、#DIMSと#DEFINE以外の行を書いてはいけません。 
 
EmueraはERBフォルダ内に置かれた*.ERHを全て読んでいきます。 
処理順はcsvフォルダ内のファイル→*.ERH→*.ERBの順なのでERHの効果はCSVフォルダ内の記述には及びません。 
逆に、_rename.csvによる置換は*.ERHにも適用されます。 
eramakerEXは*.ERHには_rename.csvを適用しないため、ERHファイルを用いるとeramakerEXとの互換性は失われます。

#### 広域変数の宣言 [ edit ]

※[UserVars ユーザー定義の変数]も参照してください。 
 
ヘッダーファイルでは新しい変数を宣言することができます。 
これはERB中で宣言するプライベート変数とは異なり、ERB中の全ての箇所から参照できる広域変数になります。 
プライベート変数とは異なりDYNAMIC、STATICの区別はなく、REFを用いた参照型変数を宣言することもできませんが、CONSTを用いた定数は同様に宣言できます。 
宣言できる変数は3次元変数までを宣言することができます。 
要素数を指定しない場合、要素数1の配列になるので非配列変数のように使うこともできます。 
変数宣言は以下のように#DIM又は#DIMSにより行います。 
なお、#DIM HOGE,1,2のようにすると二次元配列になります。
```
<*.ERH>
	#DIM MY_INT
	#DIM MY_INT_ARRAY, 100
	#DIMS MY_STR
	#DIMS MY_STR_ARRAY, 100
```
ERH中で上のように定義しておくことで、ERB中では
```
<*.ERB>
	MY_INT = 100
	MY_INT_ARRAY:10 = MY_INT_ARRAY:10 + 45
	MY_STR = あああ
	PRINTFORML {MY_INT_ARRAY:10} %MY_STR%
```
のように変数として使用できます。
- DIMによる変数宣言の要素数の指定は数値で行う他に定数式でも指定できます。
ただし、*.ERB中の#DIMと異なりマクロが展開されないことに注意してください。

##### SAVEDATA キーワード [ edit ]

変数宣言時にSAVEDATAキーワードを加えることでセーブされる変数が宣言できます。 
ただしSAVEDATAキーワードを用いてセーブ可能な多次元変数を宣言する場合、オプション「セーブデータをバイナリ形式で保存する」が有効になっている必要があります。
```
<*.ERH>
	#DIM SAVEDATA MY_INT_ARRAY, 100
	#DIMS SAVEDATA MY_STR_ARRAY, 100
```
このように宣言することでMY_INT_ARRAY、MY_STR_ARRAYの内容はDAYやMONEYなどの既存変数と同様にセーブされロードされます。 
逆に言えばSAVEDATAキーワードを付けずに宣言した変数はセーブされず、ロード時には初期化されます。

##### CHARADATA キーワード [ edit ]

変数宣言時にCHARADATAキーワードを加えることでキャラクタ変数が宣言できます。 
CHARADATAはSAVEDATAキーワードと同時に使用することができます。
```
<*.ERH>
	#DIM CHARADATA C_INT_ARRAY, 100
	#DIMS CHARADATA C_STR_ARRAY, 100
	#DIM CHARADATA SAVEDATA CS_INT_ARRAY, 100
```
上の例ではC_INT_ARRAY、C_STR_ARRAYはキャラクタ変数ですがセーブロードはされません。 
CS_INT_ARRAYはキャラクタ変数であり、かつセーブもロードもされます。

##### GLOBAL キーワード [ edit ]

変数宣言時にGLOBALキーワードを加えることでグローバル変数が宣言できます。 
GLOBALはSAVEDATAキーワードと同時に使用することができます。
```
<*.ERH>
	#DIM GLOBAL G_INT_ARRAY, 100
	#DIMS GLOBAL G_STR_ARRAY, 100
	#DIM GLOBAL SAVEDATA GS_INT_ARRAY, 100
```
グローバル変数は通常のセーブロード時にはロードも初期化もされません。 
この性質のため、異なるセーブデータ間でのデータの共有に使うことができます。 
GLOBALとSAVEDATAキーワードを同時に使用した場合、SAVEGLOBAL, LOADGLOBAL命令によってglobal.savファイルに読み書きされる変数となります。
その他、初期値や定数化などの詳細は[UserVars ユーザー定義の変数]を参照してください。

#### マクロの定義 [ edit ]

ここでいうマクロとはERBコード中の文字列を、あらかじめ定義した別の文字列へ置換する機能です。 
マクロと名前は付いていますがEmueraの実行中にF1～F12キーで使えるキーボードマクロとは関係ありません。 
この機能はCやC++の#defineを参考に作られています。 
ERHファイル中でマクロを定義することで全てのERBファイル内のコードに適用されます。

##### 基本的な使い方 [ edit ]

マクロは典型的には以下のように定義します。
```
<*.ERH>
	#DEFINE <置換元識別子> <置換先式>
```
これでERB中で<置換元識別子>が<置換先式>に置換されます。
例えば、.ERH中で
```
<*.ERH>
	#DEFINE FIVE 5
```
と定義しておくと、.ERB中のFIVEという文字列が5に置換されます。
例えば
```
<*.ERB>
	X = FIVE
```
は
```
(展開後)
	X = 5
```
と展開されます。 
 
マクロには行末コメントをつけることもできます。 
セミコロン以降はコメントとして無視されます。 
セミコロン以降はマクロに含まれず、展開されることはありません。
```
<*.ERH>
	#DEFINE FIVE 5 ;コメント
<*.ERB>
	X = FIVE + FIVE
(展開後)
	X = 5 + 5
```
マクロの展開はほぼ文字列そのままで行われることに注意してください。
```
<*.ERH>
	#DEFINE SIX           1 + 5
	#DEFINE NINE          8 + 1
<*.ERB>
	X = SIX * NINE
```
はXに6*9、すなわち36が代入されると思うかもしれませんが、実際には
```
(展開後)
	X = 1 + 5 * 8 + 1
```
と展開されるので掛け算が優先されて X = 42 になります。 
 
マクロは"～～"など定数文字列にも展開できますし、変数や関数、式にもできます。
- DEFINEの右側の文字列がそのまま展開されると考えればだいたいのところは理解できるでしょう。
```
<*.ERH>
	#DEFINE HOGE        "ほげほげ"
	#DEFINE PIYO        A
	#DEFINE FUGA        DA:10
	#DEFINE HOGERA      LOCAL + MY_FUNC(X, Y)
<*.ERB>
	X = STRLEN(HOGE)
	Y = PIYO + 5
	FUGA:20 += PIYO
	LOCAL = HOGERA

	@MY_FUNC(ARG, ARG:1)
	#FUNCTION
		～略～
(展開後)
	X = STRLEN("ほげほげ")
	Y = A + 5
	DA:10:20 += A
	LOCAL = LOCAL + MY_FUNC(X, Y)

	@MY_FUNC(ARG, ARG:1)
	#FUNCTION
		～略～
```
マクロは文字列そのままで展開されるという仕様上、置換先を完結した式ではなく演算子や式の一部とすることもできます。 
ただしこのような使い方は推奨しません。 
よほど慎重に使わないとコードの可読性を著しく損ねることになります。
```
<*.ERH>
	#DEFINE PLUS       +
	#DEFINE FIVEPLUS   5 +
<*.ERB>
	X = 1 PLUS 2
	Y = FIVEPLUS 2
(展開後)
	X = 1 + 2
	Y = 5 + 2
```

##### マクロの多重展開 [ edit ]

マクロを含むマクロを定義することができます。
そのようなマクロはERBのロード時にマクロが適用できなくなるまで多重展開されます。
```
<.ERH>
	#DEFINE FIVE_1 5
	#DEFINE FIVE_2 FIVE_1 + FIVE_1
	#DEFINE FIVE_3 FIVE_2 + FIVE_2
<.ERB>
	X = FIVE_3
(展開後)
	X = 5 + 5 + 5 + 5
```
展開を一定回数繰り返してもマクロが残っている場合、Emueraは自己参照又は循環参照マクロの疑いありとして処理を打ち切り、エラー終了します。 
以下のような自己参照又は循環参照するマクロを作ってしまわないように気をつけてください。
```
<.ERH>
	#DEFINE HOGE HOGE
	#DEFINE PIYO FUGA + 1
	#DEFINE FUGA PIYO + 2
<.ERB>
;エラーになる
	X = HOGE
	Y = PIYO
```

##### プリプロセッサ [ edit ]

ある名称のマクロが定義されているかどうかによって複数の行を実行するかどうか、分岐することができます。
```
[IF XXX]行と[ENDIF]行の間の行はXXXがDEFINEされている場合のみ実行されます。
```
例えば以下のように使います
```
<*.ERB>
	[IF HOGE]
		PRINTL HOGEが定義されている
	[ELSEIF PUYO]
		PRINTL HOGEが定義されていない
		PRINTL PUYOが定義されている
	[ELSE]
		PRINTL HOGEもPUYOも定義されていない
	[ENDIF]
```
この目的のため、空のマクロ（置換先のないマクロ）も定義できます。
```
<*.ERH>
	#DEFINE HOGE
```

##### マクロの制限 [ edit ]

マクロが展開されるのは基本的に式中のみです。
```
<*.ERH>
	#DEFINE FIVE 5
<*.ERB>
	PRINT FIVE
```
は単に"FIVE"の文字がPRINTされるだけです。 
これは PRINT X がXの値ではなくXという文字をPRINTするだけなのと同じです。 
 
マクロの置換先は代入演算子及び、代入演算子を含む式にすることはできません。 
以下のマクロ定義はエラーになります。
```
<*.ERH>
;エラーになる
	#DEFINE HOGE =
	#DEFINE PUGE X = 1
```
マクロの置換先は式の部分でも可能だと書きましたが、括弧の対応だけはマクロの中で完結している必要があります。
以下のマクロ定義はエラーになります。
```
<*.ERH>
;エラーになる
	#DEFINE HOGE ( X +
	#DEFINE PUGE Y )
<*.ERB>
	Z = HOGE PUGE
```
マクロの置換先を命令にすることはできません。 
以下のマクロ定義はエラーになります。
```
<*.ERH>
	#DEFINE MY_PRINTL     PRINTL
<*.ERB>
	MY_PRINTL これはPRINTLです
(展開後)
	;エラーになる
```
前記の通り、マクロは*.ERBにのみ適用され、*.csvと*.ERHには適用されません。 
また*.ERB内でもプリプロセッサ、属性名、及び行頭の記号には適用されません。 
![SKIPSTART]など、#DIMや#FUNCTIONなど、@EVENTFIRSTなどの@の部分、は置換対象外です。 
例えば#DEFINE HOGE SKIPSTART　としても、![HOGE]でコメント化開始とはなりません。 
ただし、#以降の文字列であっても#DIMの変数名などは置換対象になります。 
例えば下記のコードは
```
<*.ERH>
	#DEFINE HOGE MY_INT
	#DEFINE FIVE 5
<*.ERB>
	@FUNC
	#DIM HOGE, FIVE
	HOGE:0 = 10
(展開後)
	@FUNC
	#DIM MY_INT, 5
	MY_INT:0 = 10
```
のように展開されるので正常に動作します。
NewPP limit report
Cached time: 20260903082217
Cache expiry: 86400
Dynamic content: false
CPU time usage: 0.034 seconds
Real time usage: 0.036 seconds
Preprocessor visited node count: 224/1000000
Preprocessor generated node count: 524/1000000
Post‐expand include size: 0/2097152 bytes
Template argument size: 0/2097152 bytes
Highest expansion depth: 2/40
Expensive parser function count: 0/100
Unstrip recursion depth: 0/20
Unstrip post‐expand size: 2882/5000000 bytes
Transclusion expansion time report (%,ms,calls,template)
100.00%    0.000      1 -total
