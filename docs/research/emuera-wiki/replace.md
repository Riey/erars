# URL: https://wiki.eragames.rip/index.php/Emuera/replace
# fetch date: 2026-09-03

Contents 1 _replace.csv 1.1 お金の単位 1.2 単位の位置 1.3 起動時簡略表示 1.4 販売アイテム数 1.5 DRAWLINE文字 1.6 BAR文字1 1.7 BAR文字2 1.8 システムメニュー0 1.9 システムメニュー1 1.10 COM_ABLE初期値 1.11 汚れの初期値 1.12 時間切れ表示 1.13 EXPLVの初期値 1.14 PALAMLVの初期値 1.15 PBANDの初期値 1.16 RELATIONの初期値

### _replace.csv [ edit ]

csvフォルダ内に_replace.csvという名称のファイルを置くことで、表示に関する設定を行うことができます。

#### お金の単位 [ edit ]

PRINT_SHOPITEMで表示されるアイテムの価格に付けられる単位です。 
標準は"$"です。全角文字や複数の文字を含む文字列でもかまいません。

#### 単位の位置 [ edit ]

PRINT_SHOPITEMで表示されるアイテムの価格に付けられる単位が数字の前に付くか後ろに付くかです。 
"前"か"後"で指定します。標準は"後"です。

#### 起動時簡略表示 [ edit ]

コンフィグで"ロード時にレポートを表示する"機能をOFFにした場合に代わりに表示する文字列です。 
標準は"Now Loading..."です。

#### 販売アイテム数 [ edit ]

eramakerの通常のSHOPの処理において、PRINT_SHOPITEMではITEMSALESが0でないITEMNAME(0～999)が全て表示されます。 
一方で、購入処理と@EVENTBUYの処理が行われるのは0～99が入力された場合のみで、その他の値が入力された場合は@USERSHOPが呼ばれます。
Emueraも基本的にはこの処理を再現しています。 
"販売アイテム数"を変更すると、購入処理が呼ばれるアイテムの範囲が変化します。 
例えば、販売アイテム数を1000にすると、0～999が入力された場合は購入処理が行われ、負の値か1000以上の値が入力された場合は@USERSHOPが呼ばれます。
0～(販売アイテム数-1)が入力された場合は購入の成否にかかわらず、@USERSHOPが呼ばれないことに気を付けて下さい。 
例えば販売アイテム数が1000であれば、"![200] - セーブ"や"![300] - ロード"は200番、300番のアイテムを買おうとする処理とみなされ、@USERSHOPは呼ばれません。
なお、PRINT_SHOPITEMで表示するアイテムを増やしたい（減らしたい）場合は!VariableSize.csvのITEMNAMEおよびITEMSALESの配列の要素数を変えてください。 
ITEMNAMEとITEMSALESの配列の要素数のうち、小さい方がPRINT_SHOPITEMで表示されるアイテムの範囲になります。

#### DRAWLINE文字 [ edit ]

DRAWLINE命令で表示される文字です。 
標準では"-"です。

#### BAR文字1 [ edit ]

#### BAR文字2 [ edit ]

BAR又はBARL命令に使われる文字を指定します。 
標準はBAR文字1が"*"であり、BAR文字2が"."です。 
この場合は![****....]のように表示されます。 
全角文字や複数の文字からなる文字列も指定できますが、表示がずれることを考慮してください。 
また、数字を指定した場合、Emueraは![88881111]などをクリック可能なボタンとみなすために不自然な動作をすることになります。

#### システムメニュー0 [ edit ]

#### システムメニュー1 [ edit ]

タイトル画面で最初に表示される選択肢の説明に使われる文字列です。 
標準ではそれぞれ "最初からはじめる"、"ロードしてはじめる"であり、
```
[0] 最初からはじめる
[1] ロードしてはじめる
```
上記のように表示されます。 
@SYSTEM_TITLEを定義して独自のタイトル画面を使う場合、ここで指定した文字列は使用されません。 
@SYSTEM_TITLEの中で表示処理を作成してください。

#### COM_ABLE初期値 [ edit ]

TRAINで@COM_ABLE{X}が見つからない場合の値を指定します。 
標準は1であり、対応する@COM_ABLEが定義されていない場合はCOMが実行可能とみなされます。 
この値を0にすると、@COM_ABLEが定義されていない場合はCOMが実行不可能とみなされます。

#### 汚れの初期値 [ edit ]

STAINが初期化されるときに代入される値です。 
"/"で区切ることでSTAIN:1以降の値を指定できます。 
標準は0, 0, 2, 1, 8 です。

#### 時間切れ表示 [ edit ]

TINPUTなど時間制限がある入力命令で時間切れの時に表示される文字列です。 
標準は"時間切れ"です。

#### EXPLVの初期値 [ edit ]

EXPLVの初期値を指定します。 
"/"で区切ることでEXPLV:1以降の値を指定できます。 
標準は0, 1, 4, 20, 50, 200です。

#### PALAMLVの初期値 [ edit ]

PALAMLVの初期値を指定します。 
"/"で区切ることでPALAMLV:1以降の値を指定できます。 
標準は0, 100, 500, 3000, 10000, 30000, 60000, 100000, 150000, 250000です。

#### PBANDの初期値 [ edit ]

PBAND:0の初期値を指定します。 
標準は4です。

#### RELATIONの初期値 [ edit ]

Chara**.csvにおいて、指定がない場合のRELATIONの初期値を指定します。 
標準は0です。
NewPP limit report
Cached time: 20260902121225
Cache expiry: 86400
Dynamic content: false
CPU time usage: 0.038 seconds
Real time usage: 0.042 seconds
Preprocessor visited node count: 75/1000000
Preprocessor generated node count: 92/1000000
Post‐expand include size: 0/2097152 bytes
Template argument size: 0/2097152 bytes
Highest expansion depth: 2/40
Expensive parser function count: 0/100
Unstrip recursion depth: 0/20
Unstrip post‐expand size: 73/5000000 bytes
Transclusion expansion time report (%,ms,calls,template)
100.00%    0.000      1 -total
