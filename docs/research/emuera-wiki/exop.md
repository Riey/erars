# URL: https://wiki.eragames.rip/index.php/Emuera/exop
# fetch date: 2026-09-03

Contents 1 Operators 2 暫定的な演算子の優先度表 3 演算の追加 4 論理演算子の短絡評価

### Operators [ edit ]

- Unary Operators ~ ビットごとの否定(ビットごとのNOT) 単項演算子（優先度最大） ! 論理否定(NOT) 単項演算子（優先度最大）
- Binary Operators << 左ビットシフト。比較やビット演算より優先度が高く、四則演算より低い。 >> 右ビットシフト。比較やビット演算より優先度が高く、四則演算より低い。 ^  ビットごとの排他論理和(ビットごとのXOR) 優先度は&、|と同じ ^^ ビットによらない排他論理和(ビットによらないXOR) 優先度は&&、||と同じ !& ビットによらない否定論理積(ビットによらないNAND) 優先度は&&、||と同じ !| ビットによらない否定論理和(ビットによらないNOR) 優先度は&&、||と同じ
- Ternary Operators ?～# Regular Ternary Operator - Priority is lower than the other operators above = (judgment and results are processed first)
    Format (Numeric) : <Destination Variable of the Assignment> = <Conditional> ? <Assigned value if true> # <Assigned value if false> Format (String)　: <Destination Variable of the Assignment> = \@<Conditional> ? <Assigned value if true> # <Assigned value if false>\@ It is processed the same way as
```
　　　IF <Conditional>
　　　　　<Destination Variable of the Assignment> = <Assigned value if true>
　　　ELSE
　　　　　<Destination Variable of the Assignment> = <Assigned value if false>
　　　ENDIF
```
    Ternary numeric operators can be used in normal calculations by putting them in (), and ternary string operators can be used directly in PRINTFORM instructions. However, the # cannot be omitted in the ternary operator in the format of \@～\@.
- 代入演算子 '= 文字列式を用いた文字列型変数への代入を行う演算子 詳しくは こちら
- インクリメント・デクリメント ++ インクリメント -- デクリメント
    代入文の代わりに使います。他の演算子と組み合わせることはできません。

### 暫定的な演算子の優先度表 [ edit ]

分類 | 優先度 | 代入複合演算 | 記号
否定演算子 | 高 | × | ~, !
算術演算子 | ↑ | ○ | *, /, %
 |  | ○
ビットシフト演算子 |  | ○ | <<, >>
比較演算子 |  | × | <, >, <=, >=
 |  | × | ==, !=
論理演算子 |  | ○ | , !^
 | ↓ | × | ?|, !|, !^!^
三項演算子 | 低 | × | ～?…#＿

### 演算の追加 [ edit ]

- == 文字列同士の比較。数値と文字列を比較することはできません。
- != 文字列同士の比較。
- <  文字列同士の比較。比較は先頭から行われ、異なる文字が見つかった時点で決定されます。
- >  文字列同士の比較。
- <= 文字列同士の比較。
- >= 文字列同士の比較。
- + 文字列同士の連結。数値と文字列を加算・連結することはできません。 文字列と整数の乗算。文字列と文字列を乗算することはできません。
```
	STR:0 = % "あ"* 10 %
	PRINTFORML STR:0 = "%STR:0%"
	WAIT
;結果
STR:0 = "ああああああああああ"
```

### 論理演算子の短絡評価 [ edit ]

短絡評価とはたとえば(X && Y)という式でXが0である時、Yの値によらず演算結果が0になることが明らかなのでYを評価しない、という評価法です。 
吉里吉里を含む多くの言語では論理演算子を短絡評価します。 
この評価法により以下のような書き方ができます。
```
	IF (ASSI >= 0) && (NO:ASSI == 1)
		～～～
	ELSE 
		～～～
	ENDIF 
```
ASSIが0以下の場合、(NO:ASSI == 1)の結果によらず全体の結果は0なのでNO:ASSIは参照されません。
したがってエラーも発生しません。 
評価順は左項が先、右項が後です。
```
	IF (NO:ASSI == 1) && (ASSI >= 0)
```
このように書くと先に(NO:ASSI == 1)を計算しようとするのでASSI < 0のときエラーになります。
NewPP limit report
Cached time: 20260903035019
Cache expiry: 86400
Dynamic content: false
CPU time usage: 0.035 seconds
Real time usage: 0.052 seconds
Preprocessor visited node count: 14/1000000
Preprocessor generated node count: 20/1000000
Post‐expand include size: 0/2097152 bytes
Template argument size: 0/2097152 bytes
Highest expansion depth: 2/40
Expensive parser function count: 0/100
Unstrip recursion depth: 0/20
Unstrip post‐expand size: 0/5000000 bytes
Transclusion expansion time report (%,ms,calls,template)
100.00%    0.000      1 -total
