# URL: https://wiki.eragames.rip/index.php/Emuera/resources
# fetch date: 2026-09-03

Contents 1 リソースファイル 1.1 リソース指定ファイル(csv) 1.2 画像ファイル 1.3 注意点

### リソースファイル [ edit ]

ここではEmueraで画像を表示するためのリソースファイルの準備方法を解説します。 
 
リソースファイルは実行ファイルのあるフォルダにresourcesフォルダを作成しその中に置きます。 
ファイルはresourcesフォルダ内であればサブフォルダにおいてもかまいません（1.823以降）。

#### リソース指定ファイル(csv) [ edit ]

---
resourcesフォルダにcsv形式のテキストを置くとリソース指定ファイルとして読み込まれます。
書式は以下の通りです。
```
;コメント行
リソース名A, 元ファイル名, x, y, width, height, posx, posy
リソース名B, 元ファイル名, x, y, width, height, posx, posy

リソース名C, ANIME, width, height
リソース名C, 元ファイル名, x, y, width, height, posx, posy, delay
リソース名C, 元ファイル名, x, y, width, height, posx, posy, delay
```
```
* コメント行
```
セミコロンで始まる行はコメント行として無視されます。
```
* スプライト
```
```
リソース名A, 元ファイル名, x, y, width, height, posx, posy
```
上記の形式により"リソース名A"を持つスプライトを作成できます。 
リソース名は<img src='リソース名A'>のsrc属性の属性値として使用される名前です。 
また、SPRITECREATED("リソース名A")のような形でも使用されます。 
リソース名は他のリソースの名前と重複してはいけません。 
元ファイル名は画像ファイルの名前です。拡張子を含めて指定します。csvファイルからみた相対パスで指定します。 
csvファイルよりも上位の階層にある画像ファイルを指定することはできません。 
csvファイルと同一かそのサブフォルダにある画像ファイルを指定してください。 
x, y, width, heightには元画像の中の使用する部分をピクセル単位で指定します。 
x, y, width, heightは省略することもでき、その場合は画像全体が使用されます。 
posx, posyには画像の相対位置を指定します。この値はSPRITEPOSやSPRITEMOVE命令によって動的に変更できます。 
posx, posyは省略することもでき、その場合は0,0を指定したことになります。
```
* アニメーションスプライト
```
```
リソース名C, ANIME, width, height
リソース名C, 元ファイル名, x, y, width, height, offsetx, offsety, delay
リソース名C, 元ファイル名, x, y, width, height, offsetx, offsety, delay
……
```
上記の形式により"リソース名C"を持つアニメーションスプライトを作成できます。 
アニメーションスプライトを作成するには、ファイル名の代わりにANIMEと記載した行を作り、スプライト全体のサイズを指定します。 
このwidth, heightは正の整数でなければなりません。省略することはできません。 
次の行以降に、アニメーションの各フレームとなる画像を指定します。 
各フレームの定義の仕方は通常スプライトと同じです。 
delayにはそのフレームが表示される時間をミリ秒単位で指定します。省略した場合1000msになります。 
注意点として、Emueraは標準ではINPUT等の待機時間に再描画を行わないため、アニメーションスプライトは特定のフレームで静止しているように見えます。 
SETANIMETIMER命令を実行してINPUT中に再描画を行うように指示して下さい。 
SETANIMETIMER命令の詳細は命令の解説を参照してください。

#### 画像ファイル [ edit ]

---
画像の表示のためには画像ファイルが必要です。 
画像ファイルはbmp、jpg、png形式の何れかで用意しresourcesフォルダ内に置きます。 
Emuera1.823ではERBスクリプト中で画像ファイルを直接指定することはできません。 
リソース指定ファイルで指定したリソース名からのみアクセスできます。

#### 注意点 [ edit ]

---
csvファイルで指定された全ての画像ファイルはEmueraの起動時にメモリ上に展開され終了するまでメモリを占有します。 
大量の画像ファイルを読ませるよりも画像を単一のファイルに合成し範囲を指定して利用した方がメモリ的にも速度的にも有利と思います。 
コンフィグの描画インタフェースにWINAPIを指定している場合はGDIにより処理され、アルファブレンドは行われません。 
描画インタフェースがGraphics又は!TextRendererの場合にはGDI+により処理され、アルファブレンドが行われます。 
拡大縮小もWINAPI(GDI)とGraphics又は!TextRenderer(GDI+)でやや異なります。
NewPP limit report
Cached time: 20260902121226
Cache expiry: 86400
Dynamic content: false
CPU time usage: 0.028 seconds
Real time usage: 0.032 seconds
Preprocessor visited node count: 41/1000000
Preprocessor generated node count: 80/1000000
Post‐expand include size: 0/2097152 bytes
Template argument size: 0/2097152 bytes
Highest expansion depth: 2/40
Expensive parser function count: 0/100
Unstrip recursion depth: 0/20
Unstrip post‐expand size: 669/5000000 bytes
Transclusion expansion time report (%,ms,calls,template)
100.00%    0.000      1 -total
