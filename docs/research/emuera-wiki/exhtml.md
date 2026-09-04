# URL: https://wiki.eragames.rip/index.php/Emuera/exhtml
# fetch date: 2026-09-03

Contents 1 HTML_PRINT関連 1.1 HTML_PRINT 1.1.1 p 1.1.2 nobr 1.1.3 br 1.1.4 button, nonbutton 1.1.5 font 1.1.6 b, i, u, s 1.1.7 img 1.1.8 shape 1.1.9 文字参照 1.1.10 コメント 1.2 関連命令 1.2.1 HTML_TAGSPLIT <文字列式>(, <数値変数>, <文字列変数>) 1.3 関連関数 1.3.1 str HTML_POPPRINTINGSTR() 1.3.2 str HTML_GETPRINTEDSTR(int lineNo) 1.3.3 str HTML_ESCAPE(str value) 1.3.4 str HTML_TOPLAINTEXT(str value)

### HTML_PRINT関連 [ edit ]

ここではHTML_PRINT命令を始めとするhtml関連命令を解説します。 
関連命令を使うことにより、htmlと類似した文法で表示内容を指定できます。

#### HTML_PRINT [ edit ]

---
htmlっぽいタグを利用してPRINTする命令です。 
引数がPRINTのような文字列ではなくPRINTSと同じ文字列式であり、自動的に改行するので実際はPRINTSLの動作に近いです。 
HTML_PRINTによる描画はALIGNMENT、SETFONT、COLOR、FONTSTYLE命令とその類似命令の影響を受けません。 
これらの効果を得るには全てタグで指定する必要があります。 
<タグ名 属性='属性値'>テキスト</タグ名>という形で使用します。 
属性値は'～'または"～"で囲う必要があります。 
Emueraにおける文字列との区別のために、'～'で囲うことを推奨します。

##### p [ edit ]

```
<p align='～'>テキスト</p>
```
pタグは文字列の前にのみ置くことができ、</p>は最後にのみ置くことができます。 
</p>は省略してもかまいません。
```
* align属性

  必須

  ALIGNMENT命令に相当します。left, center, rightの三種が指定できます。
```

##### nobr [ edit ]

```
<nobr>テキスト</nobr>
```
PRINTSINGLE命令による描画に相当します 
このタグをつけると描画領域を越えたことによる暗黙の改行が行われなくなります（<br>による明示の改行は可能です） 
ただしEmueraはブラウザと違って横方向にスクロールできないのでウインドウ幅を越えた分は見えなくなります。 
<nobr>は最初のテキストより前にのみ置くことができ、</nobr>は最後のテキストより後のみ置くことができます。 
</nobr>は省略しても構いません

##### br [ edit ]

改行します。 
この効果は表示行の改行なのでいくつ<br>があってもCLEARLINEやLINECOUTでは一行とみなされます。

##### button, nonbutton [ edit ]

```
<button value='～' title='～' pos='～'>テキスト</button>
<nonbutton title='～' pos='～'>テキスト</nonbutton>
```
buttonは囲った部分のテキストをクリック可能なボタンにします。 
nonbuttonは囲った部分のテキストをボタンでないテキストとして表示します。
```
* value属性

  buttonにのみ指定できます。

  valueを省略した場合、クリック不可の<nonbutton>と同様の非ボタンになります。

* title属性

  ボタンをポイントしたときに表示されるツールチップの表示内容を指定します。

* pos属性

  alignがleftかつnobrタグが使用されている場合にのみ使用できます。

  画面左端からの位置をフォントサイズに対する％で指定します。

  例えば<button pos='300'>ボタン</button>ならば「　　　ボタン」とほぼ同じ位置にボタンが書かれます。
```

##### font [ edit ]

```
<font face='～' color='～' bcolor='～'>テキスト</font>
```
囲った部分のフォント、表示色、ボタンの選択中表示色、を変更します 
このタグは入れ子にすることができます
```
* face属性

  フォント名を指定します。空文字列を指定した場合、コンフィグで指定されたフォントになります。

  指定されたフォントが存在しないかサポートされない場合、「Microsoft Sans Serif」が代わりに使用されます。

  （これは .Net FrameworkのSystem.Drawing.Fontクラスの仕様によります）
* color属性

  テキストの表示色を指定します。

  色指定は'#FF0080'のような16進数形式か'red'、'blue'のような単語形式で指定します。

  色名は.Net FrameworkのColor構造体の定義色に準じます。

  ただしTransparentは色名として指定できません。
* bcolor属性

  ボタンの選択中表示色を指定します。
```

##### b, i, u, s [ edit ]

```
<b>太字</b>, <i>イタリック</i>, <u>アンダーライン</u>, <s>打消し線</s>
```
囲った部分の文字をそれぞれ太字、イタリック、アンダーライン付、打消し線付にします

##### img [ edit ]

```
<img src='～～' srcb='～～' height='～～'>
```
行内に画像を表示します。 
画像を準備する方法は[resources リソース設定]を参照してください。
```
* src属性

  必須

  resourcesフォルダのcsvに指定したリソース名を指定します

  heightやwidthを指定しない場合、縦横比を維持したまま、縦幅がフォントサイズと一致するように縮小又は拡大して表示されます。

  描画インターフェイスがWINAPIの場合、アルファブレンドは行われません。
* srcb属性

  resourcesフォルダのcsvに指定したリソース名を指定します

  srcbにはボタン選択時に表示されるべきリソース名を指定します。

  省略した場合srcと同一画像が使われます。

  画像はsrcと同じサイズに縮小又は拡大して表示されます。
* height属性

  表示サイズの縦幅をフォントサイズに対する％で指定します。省略すると100です。

  負の値を指定した場合、画像を縦に反転して表示します。
* width属性

  表示サイズの横幅をフォントサイズに対する％で指定します。省略すると0です。

  0の場合、元画像の縦横比を維持する値になります。

  負の値を指定した場合、画像を横に反転して表示します。
* ypos属性

  表示位置の縦軸位置をフォントサイズに対する％で指定します。省略すると0です。

  「行の高さ」ではなく「フォントサイズ」が基準であることに注意してください。

  横軸位置の調整には<shape type='space'>やbuttonのpos属性を利用してください。
```

##### shape [ edit ]

```
<shape type='rect' param='～～' color='～～' bcolor='～～'>
<shape type='space' param='～～'>
```
行中に指定した図形を描画します。
```
* type属性

  必須

  描画する図形のタイプを指定します。

  rect又はspaceが使用可能です。

  * type='rect'

    長方形を描画します。

    paramには1又は4つの数字を指定します。

    paramが1つのとき長方形の横幅を指定します。

    <shape type='rect' param='400'>は横幅がフォントサイズの400%である長方形を描画します。

    paramが4つのときx,y,横幅,縦幅の順で指定します。

    <shape type='rect' param='0,25,400,50'>は行の上下中央に縦幅がフォントサイズの50%の長方形を描画します。

    param='400'　はparam='0,0,400,100'と同じ意味になります。
  * type='space'

    paramに指定した幅だけ何も表示しません。

    例えば<shape type='space' param='400'>はフォントサイズの400%の区間だけ何も描画しません。

    これは全角スペース4つ分とおおよそ同じです。
* param属性

  必須

  図形描画のためのパラメータをフォントサイズとの比率(百分率)で指定します。

  複数の値を指定する場合はカンマで区切ります。
* color属性

  図形の色を指定します。指定のフォーマットは
タグと同じです。
* bcolor属性

  図形のボタン選択中の色を指定します。指定のフォーマットは
タグと同じです。
```
文字参照 [ edit ]
&と;で囲まれた単語があるとそれを文字参照として処理します。 対応している文字参照は& > < " ' 及び &#nn; &#xnn; です。
コメント [ edit ]
<!-- コメント -->
html解釈にあたり、で囲われた文字は無視されます
関連命令 [ edit ]
HTML_TAGSPLIT <文字列式>(, <数値変数>, <文字列変数>) [ edit ]
対象文字列をHTML文字列と解釈し、タグと平文に分割して分割数をRESULTに、分割後文字列をRESULTSに代入します 第二、第三引数が指定されている場合、RESULT、RESULTSの代わりに指定された変数に代入します。 分割処理中にエラーが生じた場合、RESULTに-1が代入されます。 HTML_TAGSPLITはタグの内容や対応関係の適否までは検証しません。 分割数がRESULTSの配列サイズを超えた場合、超えた分はRESULTSに代入されません。
例えば、
　HTML_TAGSPLIT "<p align='right'>あ<!--comment-->い<font color='red'>う</font></p>"
　であれば、
　RESULTS:0 = <p align='right'>
　RESULTS:1 = あ
　RESULTS:2 = <!--comment-->
　RESULTS:3 = い
　RESULTS:4 = <font color='red'>
　RESULTS:5 = う
　RESULTS:6 = </font>
　RESULTS:7 = </p>
　RESULT = 8
　と解体されます。
関連関数 [ edit ]
str HTML_POPPRINTINGSTR() [ edit ]
現在PRINT中で改行待ちの文字列バッファをHtml形式で取得し、バッファを空にします pタグはつかないのでALIGNMENT命令によるalignは反映されません。
str HTML_GETPRINTEDSTR(int lineNo) [ edit ]
表示済みのラインのうちlineNoで指定した行の内容をhtml形式の文字列として取得します。 行の数え方はLINECOUNTやCLEARLINE命令と同じです。
str HTML_ESCAPE(str value) [ edit ]
対象の文字列をHtml向けにエスケープ（文字参照に変換）します。 アンエスケープにはHTML_TOPLAINTEXT関数を使用します。
str HTML_TOPLAINTEXT(str value) [ edit ]
対象のhtml文字列をプレーンテキストに変換します。 具体的には、文字列からhtmlタグを削除し文字参照を展開します。
NewPP limit report
Cached time: 20260902121223
Cache expiry: 86400
Dynamic content: false
CPU time usage: 0.043 seconds
Real time usage: 0.047 seconds
Preprocessor visited node count: 193/1000000
Preprocessor generated node count: 344/1000000
Post‐expand include size: 0/2097152 bytes
Template argument size: 0/2097152 bytes
Highest expansion depth: 2/40
Expensive parser function count: 0/100
Unstrip recursion depth: 0/20
Unstrip post‐expand size: 1207/5000000 bytes
Transclusion expansion time report (%,ms,calls,template)
100.00%    0.000      1 -total
