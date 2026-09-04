# URL: https://wiki.eragames.rip/index.php/Emuera/exetc
# fetch date: 2026-09-03

Contents 1 End of line comment 2 Concatenate Rows 3 Special Comment Line 3.1 ;!; 3.2 ;#; 4 Character Array 5 Integer Value Range 6 Batch Assignment to Array Variables 7 Assignment to String Variable Using FORM Syntax 8 Assignment to a String Variable Using a String Expression 9 Specifying Array Variable Elements Using Character Strings 10 Formatted String (FORM Syntax) Extension 11 Using Formatted Strings (FORM Syntax) in String Expressions 12 Use of macro syntax with INPUTS

### End of line comment [ edit ]

```
A = B ;Substitute A for B
```
You can insert a comment at the end of a line in this way. 
However, there are some exceptions, and if the argument is a simple string instruction such as the “PRINT” instruction, it will be evaluated as part of the string without commenting
```
PRINT foo;bar
```
In this case, "foo;bar" is PRINTed.

### Concatenate Rows [ edit ]

```
{
	DIM CONST HOGE =
		1,2,3,4
}
```
Will be interpreted as "#DIM CONST HOGE = 1,2,3,4" 
'{' and '}' lines must not contain any other characters besides white space. 
One-byte space is added at the position where there is a line feed symbol. 
In other words, lines cannot be split in the middle of function names and variable names, 
If you divide PRINT etc., the display character string will include the half-width space that was a line feed 
In Emuera's grammatical interpretation, line continuation processing is performed before interpretation of comments. 
In other words
```
{
	#DIM CONST HOGE =
		1,2,3,4 ;comment
		,5,6,7,8
}
```
Becomes "#DIM CONST HOGE = 1,2,3,4 ;comment ,5,6,7,8", 
",5,6,7,8" is considered part of the end-of-line comment and ignored.

### Special Comment Line [ edit ]

#### ;!; [ edit ]

In both Emuera and eramaker, lines starting with ; are considered comment lines, but Emuera considers lines starting with ;!; To be valid lines, not comments. 
Use this when writing statements that you do not want eramaker to execute. 
For example, adding the following script to @SHOWSHOP can prohibit operation on Emuera.
```
;!;PRINTW This script cannot be executed in Emuera
;!;QUIT
```
(Also, by using it together with [SKIPSTART] and [SKIPEND], you can prohibit the operation of other than Emuera like the following script.) 
Use this when writing statements that you do not want to be executed except by Emuera.
```
;!;[SKIPSTART]
PRINTW This script cannot be executed except by Emuera
QUIT
;!;[SKIPEND]
```

#### ;#; [ edit ]

Lines starting with ;#; are executed only in debug mode. 
In non-debug mode, it is regarded as a comment line and is not executed. 
However, since DEBUG instructions are originally ignored in non-debug mode, there is no need to prefix ;#; to the line.  
Similarly, debug variables are empty strings or 0 in non-debug mode, so there is no worry about errors. 
See [../debug|here] for debug mode.

### Character Array [ edit ]

eramaker probably has only 100 arrays for character creation. 
Therefore, even if different characters are defined in chara3.csv, chara03.csv, and chara3B.csv, only one person is valid. 
In Emuera, characters can be defined as many as memory allows. 
Also, if it corresponds to "chara*.csv", it will read any file such as chara101.csv, charaABC.csv 
If the character number is duplicated and there are multiple candidates for ADDCHARA or ADDSPCHARA, only the one that was read first will be valid.

### Integer Value Range [ edit ]

Integers that can be handled by eramaker are 32-bit signed integers, that is, in the range from -2147483648 to 2147483647. 
Emuera handles 64-bit signed integers in the same range as Kirikiri (吉里吉里), from -9223372036854775808 to 9223372036854775807.

### Batch Assignment to Array Variables [ edit ]

```
A:10 = 1,2,3
DA:0:0 = 1,2,3
```
When written as above, the values ​​of 1, 2, and 3 are assigned to A:10 to A:12, respectively 
In the following multidimensional array, values of 1, 2, and 3 are assigned to DA:0:0 to DA:0:2, respectively. 
DA:0:0 to DA:0:99 is not assigned to DA:1:0, and an out-of-array reference error occurs 
However, it cannot be used for compound assignment (A += 1,2,3 etc.). 
Also, when using batch assignment for assignment to a string type array variable, you must perform [../exetc#Assignment to a String Variable Using a String Expression|assignment using a string expression]
```
;The string "Strawberry, Melon, Blue Hawaii" is assigned to STR: 20
STR:20 = Strawberry, Melon, Blue Hawaii
;"Strawberry", "Melon", "Blue Hawaii" are assigned to STR:20~STR:22 respectively
STR:20 '= "Strawberry", "Melon", "Blue Hawaii"
```

### Assignment to String Variable Using FORM Syntax [ edit ]

When assigning to a string variable, you can specify the character string to be assigned in the same format as PRINTFORM.
```
SAVESTR:0 = %RESULTS%
```
In this statement, you can substitute the contents of RESULTS for SAVESTR:0. 
The same statement means that eramaker substitutes the actual string of %RESULTS% for SAVESTR:0. 
If you want to substitute for the string %RESULTS% itself in Emuera, write as follows:
```
SAVESTR:0 = \%RESULT\%
```
The character immediately after the \ symbol is not treated as a system symbol. 
If you want to include the \ symbol in the string, use \\. 
 
In the rare case, if you want the same behavior on eramaker and Emuera, you need to write as follows.
```
;!;SAVESTR:0 = \%RESULT\%
;!;[SKIPSTART]
SAVESTR:0 = %RESULTS%
;!;[SKIPEND]
```

### Assignment to a String Variable Using a String Expression [ edit ]

In Emuera after ver1813, assignment to string variables can be newly performed using the assignment operator '= and a string expression.
```
;Same as "STR = Ayu"
STR '= "Ayu"
;Same as "STR = %TSTR:0%ABC"
STR '= TSTR:0 + "ABC"
```

### Specifying Array Variable Elements Using Character Strings [ edit ]

For the following variables, the argument can be called as a character string defined in *.csv. 
For details on the new variables of Emuera, see [../exvar|Extended Syntax-Constants and Variables Added by Emuera].
```
ITEM (item.csv)
ITEMSALES (item.csv)
LOSEBASE (base.csv)
BASE (base.csv)
MAXBASE (base.csv)
ABL (abl.csv)
TALENT (talent.csv)
EXP (exp.csv)
MARK (mark.csv)
RELATION (chara*.csv)
UP (palam.csv)
DOWN (palam.csv)
PALAM (palam.csv)
JUEL (palam.csv)
GOTJUEL (palam.csv)
STAIN (stain.csv)
SOURCE (source.csv)
EX (ex.csv)
NOWEX (ex.csv)
TEQUIP (tequip.csv)
EQUIP (equip.csv)
FLAG (flag.csv)
TFLAG (tflag.csv)
CFLAG (cflag.csv)
STR (strname.csv)
SAVESTR (savestr.csv)
```
The following are variables added by Emuera
```
ITEMPRICE (item.csv)
DOWNBASE (base.csv)
CUP (palam.csv)
CDOWN (palam.csv)
TCVAR (tcvar.csv)
TSTR (tstr.csv)
CSTR (cstr.csv)
CDFLAG (cdflag1.csv, cdflag2.csv)
GLOBAL (global.csv)
GLOBALS (globals.csv)
```
For example, if abl.csv has the definition "2, Skill", the following four lines have the same meaning.
```
ABL:Skill += 1
ABL:2 += 1
ABL:"Skill" += 1
ABL:(ABLNAME:2) += 1
```
For RELATION, you can specify either NAME or CALLNAME. 
If there are multiple definitions with the same name, the one defined first will be called. 
For example, if abl.csv contains "2, Skill" and "4, Skill", and "2, Skill" is defined in the previous line, "ABL:Skill" becomes "ABL:2". 
Strings can be expressions or variables. In that case, please add () as shown below.
```
ABL:(RESULTS:0) = ABL:(RESULTS:0) + 1
```
If () is omitted, the item name and the variable name may be the same. In that case, the variable takes precedence. 
For example, if abl.csv has the definition "0, Rotor",
```
@HOGE
#DIM Rotor, 0
Rotor = 1
PRINTFORML {ABL:Rotor}
```
In this case, it is interpreted as the first ABL, not the zeroth ABL. 
Similarly, if the item name is a number, interpretation as a number takes precedence. 
For example, if you define "0,10" in abl.csv and refer to ABL: 10, it will not be interpreted as the 0th ABL and will be the 10th ABL. 
 
This can also be used in definitions in chara*.csv. 
For example, if abl.csv has the definition "2, Skill", the following two lines have the same meaning:
```
能力,2,2
能力,Skill,2
```
However, it cannot be used for RELATION(相性). 
This is because the system does not know the correspondence between chara name and NO when reading chara*.csv.

### Formatted String (FORM Syntax) Extension [ edit ]

You can specify the number of digits (characters) to be displayed in {} and %% in the formatted strings used in PRINTFORM, etc. 
In the form of:
- {Variable/Expression, Number of digits, Justification (LEFT or RIGHT)}
- %Variable/String Expression, Number of digits, Justification (LEFT or RIGHT)%
Full-width (Japanese) characters are counted as 2 characters. 
Half-size spaces will be added to the number of digits (characters) in the display. 
Normally it is right-aligned, but if you specify the keyword LEFT, it will be left-aligned. 
If the original number of digits is larger than the specified number of display digits, it is displayed as it is.
```
A = 123456
STR:0 = あいう
PRINTFORML [{A}]
PRINTFORML [{A,10}]
PRINTFORML [{A,10,LEFT}]
PRINTFORML [%STR:0%]
PRINTFORML [%STR:0,10%]
PRINTFORML [%STR:0,10,LEFT%]
PRINTFORML [{A,2}]
PRINTFORML [%STR:0,2%]
```
Results into
```
[123456]
[    123456]
[123456    ]
[あいう]
[    あいう]
[あいう    ]
[123456]
[あいう]
```

### Using Formatted Strings (FORM Syntax) in String Expressions [ edit ]

Using the FORM syntax in a string expression, such as PRINTS or arguments to a user-defined function in an expression, will result in an error. 
So when you use a formatted string in a string expression, you can use it in the same way that you use "～" when you use a defined string in a string expression. 
Use it like  @"～" . 
In addition, if the string in  @"～"  is only described by a ternary operator using  \@～\@ , you can omit  @"～"  and write directly as  \@～\@ .
Correct example
```
;Assignment is a FORM syntax
STR:0 = aiu
;Addition is a string expression
RESULTS += STR:0
;Example of using a constant string as a string expression
RESULTS += "eo"
;Example of using the FORM syntax for a string expression
PRINTS @"%RESULTS% lalilulelo"
```
```
;The following four lines are all the same
PRINTS STR:0 + "！"
PRINTFORM %STR:0%！
PRINTS @"%STR:0%！"
PRINTFORM %STR:0 + "！"%
```
Wrong example
```
;It will be "RESULTS" inside
STR:0 = RESULTS
;You'll get an error
RESULTS += eo
;You'll get an error
RESULTS += %STR:0%
;You'll also see "@" and " printed
PRINTFORM @"%RESULTS% lalilulelo"
```

### Use of macro syntax with INPUTS [ edit ]

INPUTS and other similar input acceptance instructions can be used as macro expressions. 
For the format of the macro, please refer to the Macros section in the  How to Use  section. 
If you don't use the macro syntax and want to use () as a simple string, escape it with \.
NewPP limit report
Cached time: 20260903024051
Cache expiry: 86400
Dynamic content: false
CPU time usage: 0.027 seconds
Real time usage: 0.028 seconds
Preprocessor visited node count: 54/1000000
Preprocessor generated node count: 60/1000000
Post‐expand include size: 0/2097152 bytes
Template argument size: 0/2097152 bytes
Highest expansion depth: 2/40
Expensive parser function count: 0/100
Unstrip recursion depth: 0/20
Unstrip post‐expand size: 0/5000000 bytes
Transclusion expansion time report (%,ms,calls,template)
100.00%    0.000      1 -total
