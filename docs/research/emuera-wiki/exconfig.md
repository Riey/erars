# URL: https://wiki.eragames.rip/index.php/Emuera/exconfig
# fetch date: 2026-09-03

### コンフィグ設定の強制 [ edit ]

Emueraはcsvフォルダ内に"_fixed.config"及び/又は"_default.config"というファイル名のファイルが存在するとこれらのファイルを読みに行きます。 
各.configファイルの書式はemuera.configと同じです。
各項目の意味は[config コンフィグ]の項目を参照してください。
各ファイルの優先度はEmueraがconfigファイルを読む順序に依存しています。 
Emueraがコンフィグファイルを読む順序は
```
	csv¥_default.config
	emuera.config
	csv¥_fixed.config
```
の順であり、後から読まれた設定によって上書きされていきます。
すなわち、_default.configの設定はemuera.configによって上書きされ、emuera.configの設定は_fixed.configによって上書きされます。
なお、これらのファイルは上記のパス、ファイル名で存在していないと読み込まれません。 
つまり、csvフォルダの中にサブフォルダを作ってその下に_fixed.configや_default.configを置いたり、default.configといったアンダーバーの抜けたファイル名にしたりしても読み込まれることはありません。

#### _fixed.config [ edit ]

---
"_fixed.config"に設定されたオプションは"emuera.config"よりも優先されます。 
また、"_fixed.config"で指定された項目はEmueraの設定ダイアログによって変更できなくなります。 
"_fixed.config"は意図した動作をするために特定のオプションが必須である場合にのみ使用してください。 
Emueraの行折り返し位置に依存しているスクリプトでは"ボタンの途中で行を折りかえさない"オプションをYESにする必要があります。 
また、_Replace.csv・_Rename.csvを利用する必要があればこれらに関するオプションが必須です。 
他に、SETCOLORを利用する場合には、背景色・文字色などを固定する必要があるでしょう。 
しかし、特に必須でないオプションまで"_fixed.config"で設定を行うとユーザーによるカスタマイズができなくなってしまいます。 
"_fixed.config"に設定する項目は最小限に留めるようにしてください。

#### _default.config [ edit ]

---
強制するほどではないけれども推奨したいオプションがある場合は、fixedの代わりに"_default.config"を使用します。 
"_default.config"は"emuera.config"が存在しない場合、初期設定として使われます。 
"emuera.config"が存在する場合には"emuera.config"で設定したオプションが優先されるので、ユーザーが設定したオプションを上書きすることはありません。
NewPP limit report
Cached time: 20260903082236
Cache expiry: 86400
Dynamic content: false
CPU time usage: 0.020 seconds
Real time usage: 0.021 seconds
Preprocessor visited node count: 19/1000000
Preprocessor generated node count: 36/1000000
Post‐expand include size: 0/2097152 bytes
Template argument size: 0/2097152 bytes
Highest expansion depth: 2/40
Expensive parser function count: 0/100
Unstrip recursion depth: 0/20
Unstrip post‐expand size: 69/5000000 bytes
Transclusion expansion time report (%,ms,calls,template)
100.00%    0.000      1 -total
