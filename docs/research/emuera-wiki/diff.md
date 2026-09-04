# URL: https://wiki.eragames.rip/index.php/Emuera/diff
# fetch date: 2026-09-03

Contents 1 Differences from eramaker 1.1 Fixed bugs and unnatural behavior 1.1.1 The last element of the array is unavailable 1.1.2 Anomaly of the unary operator "-" 1.1.3 The last line of the file is unreadable 1.1.4 If there are extra elements in the array, they're ignored 1.1.5 Can't use a specific format for array calls 1.1.6 Treating abnormal numbers in the CSV as integers 1.1.7 Awkward notation works 1.2 Other points that are different from eramaker 1.2.1 If the line immediately after the SIF is a blank line, comment line, etc. 1.2.2 Behavior when arguments such as IF and ELSEIF are omitted. 1.2.3 Characters that can be used in function names 1.2.4 RAND Specifications 1.2.5 WAIT Specifications 1.2.6 JUMP Specifications 1.2.7 CALLNAME Specifications 1.2.8 Unfolding PRINTFORM and other FORMs 1.2.9 Attributes of the EVENT Functions 1.2.10 How to read the gamebase.csv "コード" (code) 1.2.11 How to read abl.csv, etc 1.2.12 How to read train.csv 1.2.13 How to read chara*.csv 1.2.14 File Newline Code 1.3 Unfixed bugs and Unnatural behavior 1.3.1 File reading order depends on the file system 1.3.2 Count is added at the end of REPEAT-REND 1.3.3 NEXTCOM behavior 1.4 Changed Features 1.4.1 SP Characters 1.5 Added Features

## Differences from eramaker [ edit ]

### Fixed bugs and unnatural behavior [ edit ]

#### The last element of the array is unavailable [ edit ]

In eramaker, if the last element of the array is not zero, the data will be corrupted on load 
Emuera does not have this problem. 
This problem is caused by the fact that the save and load specifications are not unified in eramaker, but Emuera is. 
Therefore, saving with eramaker and loading with Emuera does not cause this problem, but saving with Emuera and loading with eramaker reproduces this problem.

#### Anomaly of the unary operator "-" [ edit ]

There are problems with eramaker, such as -100 < 0 being false. 
Emuera does not have this problem.

#### The last line of the file is unreadable [ edit ]

eramaker ignores lines without a newline. 
This means that the last line of the file, whether it is CSV or ERB, will be ignored. 
Emuera does not reproduce this behavior.

#### If there are extra elements in the array, they're ignored [ edit ]

```
	A:1:2 = 34
```
In eramaker, the above expression assigns 34 to A:1. 
Emuera makes this an error.

#### Can't use a specific format for array calls [ edit ]

In eramaker, you can use variables such as A:0 and A:(COUNT+1). 
However, writing double array variables such as ABL:0:2 or TALENT:(COUNT+1):2 will result in an error. 
Also, if the argument is omitted when calling a string variable, an error may occur. 
Emuera does not have this problem. 
No error occurs whether the argument of the double array is a constant or a formula, and the argument of a string variable can be omitted.

#### Treating abnormal numbers in the CSV as integers [ edit ]

```
	0,Rotor,200
	0xFF,Router,200
```
If the above is found in Item.csv, eramaker interprets 0xFF as 0 and ITEM:0 is defined as a Router. 
Emuera doesn't reproduce this, but invalidates this definition with an error, and ITEM:0 is defined as a Rotor.

#### Awkward notation works [ edit ]

```
	A:0:1:99999 +-RESULTS:0=@=+123|*?=Y
```
The above expression works in eramaker. 
Emuera makes this an error.

### Other points that are different from eramaker [ edit ]

#### If the line immediately after the SIF is a blank line, comment line, etc. [ edit ]

```
SIF conditional
	;comment
	PRINT hogehoge
```
eramaker always executes a PRINT line. 
This is because eramaker recognizes that the next line in the SIF is ";comment".
Emuera only executes PRINT lines if the conditional statement is true, just like Kiri Kiri (吉里吉里) and others. 
Emuera treats blank lines and comment lines as non-existent, and recognizes the next line in the SIF as "PRINT hogehoge". 
Also, eramaker can put an IF or REPEAT statement on the next line after the SIF, but this often behaves differently from the author's intentions, so Emuera limits the lines that can be brought after the SIF.

#### Behavior when arguments such as IF and ELSEIF are omitted. [ edit ]

In eramaker, if you omit IF, ELSEIF or the argument of the assignment statement, the behavior becomes indeterminate. 
However, if the RETURN argument is omitted, it acts like RETURN 0. 
Emuera always interprets the omitted argument as 0, so anything inside a blank IF is always not executed, but is subject to a warning.

#### Characters that can be used in function names [ edit ]

In eramaker, all characters, including symbols and double-byte characters, can be used. 
Emuera also allows full-width characters, but not symbols other than _ (underscores). 
Also, Emuera does not recommend to start the function name with a half-size number. 
The following script will work with eramaker, but will give an error with Emuera.
```
	CALL \.,)(][+-%* 　@&$

@\.,)(][+-%* 　@&$
	PRINTL Function @\.,)(][+-%* 　@&$ was called.
	RETURN 0
```
In Emuera, if the function name contains ( or ), it is misunderstood as a function argument. 
Also, the LOCAL@ function name does not work properly when the function name includes @ or the symbol of the operator. 
Including {} or % will interfere with the CALLFORM call. 
For this reason, Emuera, like many programming languages such as C# and Kirikiri (吉里吉里), prohibits the use of symbols in function names. 
As of ver 1.721, this is a warning level 1 and not a warning that immediately terminates the error, but it may behave unintentionally at some point.
Also, if the function name starts with a half-width number, it cannot be called as a  function that can be used in an expression .
This is to determine whether it is a number or a variable or function by looking at a single letter in the expression.

#### RAND Specifications [ edit ]

```
	A = RAND:X
```
In eramaker, it returns 0 when X is 0. 
Otherwise, it returns (a random number from 0 to 32767) % (the absolute value of X). 
This method works even when X is negative, never returns a value greater than 32767, and the bias of the value is negligible when X is greater than 1000.
Emuera does not reproduce this. 
Emuera returns (a random number from 0 to 18446744073709551615) % (X). 
When X is a value of 0 or negative, Emuera gives an error. 
(This is the result of focusing on the consistency of the return values, based on the official description that "the return value is an integer from 0 to A-1 in the case of RAND:A") 
Also, X is valid from 1 to 9223372036854775807 (positive 64-bit signed integer range). 
If X is less than about 100 trillion, there is not enough bias to be felt.

#### WAIT Specifications [ edit ]

In eramaker, the line break is printed when the Enter key is pressed, rather than when the WAIT instruction is executed. 
In Emuera, if the cursor is in the middle of a line when executing a WAIT instruction, a new line will be started, and if the cursor "pressed the Enter key" or "left clicked", it will not be accompanied by a new line.

#### JUMP Specifications [ edit ]

In eramaker, JUMP can't be done from a function called by CALL. 
In Emuera, even a called function can be JUMPed. 
RETURN in JUMP destination is the same as RETURN in JUMP source function.
```
	CALL FOOBAR

	@FOO
	PRINTL Functino @FOO
	JUMP BAR
	@BAR
	PRINTL Functino @BAR
	RETURN 0
	@FOOBAR
	PRINTL Functino @FOOBAR
	CALL FOO
	PRINTW Back to the @FOOBAR function
;Result of the execution
;eramaker(error)
CALLで呼ばれた先からJUMPで関数を呼び出そうとしました
(Tried to call a function with JUMP from a destination called with CALL)

;Emuera
Function @FOOBAR
Function @FOO
Function @BAR
Back to the @FOOBAR function
```

#### CALLNAME Specifications [ edit ]

In eramaker, when referring to CALLNAME, if CALLNAME is an empty string, it returns the value of NAME instead. 
Emuera returns an empty string if CALLNAME is an empty string. 
 
To bridge this difference, Emuera provides an option to use NAME if CALLNAME is an empty string. 
When this option is YES, if CALLNAME is not set in chara*.csv or is set to an empty string, it is treated as being set to the same string as NAME. 
However, even this option is not completely reproducible. 
For example, if you add a character in eramaker and read the save data in Emuera, the behavior may be different.

#### Unfolding PRINTFORM and other FORMs [ edit ]

eramaker repeats over and over until there is nothing left to unroll. 
If there is a self-reference or circular reference, it will freeze. 
Emuera only unrolls once.
An eramaker unfolding would probably look like this
```
 str = String to unroll
while(str with {～～} in it)
	Unroll the leftmost {～～}
while(str with %～～% in it)
	Unroll the leftmost %～～%
while(str with *** in it)
	Unroll the leftmost ***
while(str with $$$ in it)
	Unroll the leftmost $$$
while(str with +++ in it)
	Unroll the leftmost +++
while(str with /// in it)
	Unroll the leftmost ///
while(str with === in it)
	Unroll the leftmost ===
```
Because of this behavior, eramaker also allows you to do the following
```
STR:1 = S1%STR:2%3%4%
STR:2 = S2%STR:
STR:3 = S3%STR:
STR:4 = S4
PRINTFORMSL STR:1
PRINTFORML %STR:1%
DRAWLINE
;・Result
;S1S2S3S4
;S1S2S3S4
```
Emuera does not replicate this.

#### Attributes of the EVENT Functions [ edit ]

In eramaker, the event function call is done as follows.
```
foreach(Function with #PRI)
{
	Function Call
	if(#SINGLE and the return value is 1)
		break;
}
foreach(Function with no #PRI and no #LATER)
{
	Function Call
	if(#SINGLE and the return value is 1)
		break;
}
foreach(Function with #LATER)
{
	Function Call
	if(#SINGLE and the return value is 1)
		break;
}
```
Event functions with both #PRI and #LATER will be called twice.
- SINGLE interrupts subsequent function calls only when the return value is 1.
In addition, the function call is interrupted by #SINGLE for each pair with #PRI or #LATER. 
Emuera since ver1.800 (1.756alpha018 if you include the development version) reproduces this behavior exactly. 
 
Prior to that, Emuera used the following event function calls.
```
Sort the list of functions according to #PRI and #LATER
foreach(All functions)
{
	Function Call
	if(#SINGLE and the return value is 1)
		break;
}
```
If both #PRI and #LATER are added, it is treated as if neither of them were added. 
If the function call is interrupted by #SINGLE, the event function call is terminated regardless of the presence or absence of #PRI or #LATER. 
 
Note that up to Emuera 1.751b, #SINGLE interrupts subsequent function calls when the return value is not 0. 
This was fixed in 1.752, and as with eramaker, the current version only aborts further function calls when the return value is 1.

#### How to read the gamebase.csv "コード" (code) [ edit ]

If the code in gamebase.csv contains a number of values beyond the range of -2147483648 to 2147483647, which is the number that eramaker can handle, eramaker rewrites the values in the csv to hexadecimal and uses the last 8 digits as the game code. 
For example, in the case of "コード,08231000181818110", the game code will be 301712126, which is in the range that eramaker can handle. 
 
Emuera does not reproduce this behavior. 
In Emuera version 1.803 or earlier, the game code is set to 0 when the code is "08231000181818110". 
Emuera can handle numbers in the range of -9223372036854775808 to 9223372036854775807, but 
just like eramaker, GAMEBASE_GAMECODE only handles the number in the range from -2147483648 to 2147483647, and if the range is exceeded, it becomes 0. 
 
In addition, in ver. 1.804 or later Emuera, the game code is 8231000181818110 as written in the case of "コード,08231000181818110". 
(GAMEBASE_GAMECODE has been changed to a variable that handles the same range as the other variables.) 
Also, the game code is set to 0 if the game code is beyond the range that Emuera can handle, such as "コード,98231000181818110110". 
In ver. 1.805 or later, Emuera can read saved data with a game code of 0 regardless of the game's game code.

#### How to read abl.csv, etc [ edit ]

In eramaker, you can specify negative or very large values for the index, such as "99999,Technique". 
However, since the number specified here is used by PRINT_ABL, etc., ABL:99999 is referenced in PRINT_ABL (internally by eramaker) and an error occurs. 
Thus, the practical value is the same as the number of arrays in ABL and TALENT. 
An error occurs in item.csv when referring to ITEM and ITEMSALES in SHOP as well. 
In Emuera, you cannot specify values outside the range of an array such as ABL. 
Such lines will be ignored. 
Instead, you can change the range of the array by using !VariableSize.csv.

#### How to read train.csv [ edit ]

It's basically the same as any other csv file, but the circumstances are a little different from the others. 
In eramaker, for example, even if you define "XXX,99999", if @COM99999 is defined, it will be executed correctly. 
On the other hand, if you define a negative value, such as "YYY,-2", the command will be displayed, but nothing will happen when you select it.
Emuera does not reproduce this behavior. 
The range that can be defined is up to the size of the TRAINNAME specified in !VariableSize.csv, otherwise it is ignored. 
If the size of TRAINNAME is not changed, the valid range is from 0 to 999.

#### How to read chara*.csv [ edit ]

With eramaker, even if the "number" is less than 0 or more than 1000, ADDCHARA can be done normally. 
This works the same way with Emuera.
In eramaker, if a third value is required, such as "基礎,0" (BASE), it is treated as 0 if omitted. 
Also, if the third value is not needed, such as "素質,0,100" (TALENT), it will be ignored and set to 1. 
Emuera does not reproduce this behavior. 
MAXBASE:0 becomes 1 if it is set with "基礎,0" and TALENT:0 becomes 100 if it is set to "素質,0,100".

#### File Newline Code [ edit ]

In eramaker, when the line feed code is ![CR]![LF] and ![LF], it is a line feed, but when it is only ![CR], it is not a line feed and various malfunctions occur. 
Emuera does not reproduce this behavior and considers ![CR] a line break even if it is alone.

### Unfixed bugs and Unnatural behavior [ edit ]

#### File reading order depends on the file system [ edit ]

In eramaker basic, there is a case that the behavior depends on the order of reading files, for example, when CALLing a multiplexed function. 
However, eramaker may not work as expected because the order in which files are read depends on the file system. 
Emuera also reproduces this problem. 
Many of the currently released scripts assume that the file system is NTFS and will not work properly if the file system is FAT.

#### Count is added at the end of REPEAT-REND [ edit ]

In eramaker, when exiting REPEAT-REND, the count is increased by 1. 
You will also get +1 if you exit with a BREAK.
Emuera replicates this behavior. 
In the FOR-NEXT syntax, the loop variable is incremented by 1 as well. 
Note that the behavior is different from the for syntax and break statements in common programming languages.

#### NEXTCOM behavior [ edit ]

In eramaker, the initial value of NEXTCOM is -1, but after NEXTCOM is executed, the value assigned is 0 instead of -1. 
Therefore, unless the ERB side is assigned again, COM0 will continue to be repeated. 
The official description of eramaker does not mention the existence of NEXTCOM.
Emuera replicates this behavior. 
The functionality of NEXTCOM is reproduced for eramaker compatibility only and is not recommended for use. 
If you don't plan to run your code in eramaker, consider using the DOTRAIN or CALLTRAIN instructions.

### Changed Features [ edit ]

#### SP Characters [ edit ]

In eramaker, a character whose CFLAG:0 is set to non-zero in csv becomes an SP character. 
The specification was a bit confusing because it cannot be registered by ADDCHARA, but must be registered by ADDDSPCHARA. 
It was also the cause of a bug, such as unintentionally setting CFLAG:0 to non-zero and not being able to register with ADDCHARA.
Emuera has decided not to support this feature by default, starting with ver 1.816. 
CFLAG:0 is no longer singled out and all characters can now be registered from ADDCHARA. 
It is possible to reproduce eramaker's behavior with the compatibility option "SPキャラを使用する" (Use SP character), but it is not recommended to use it for any purpose other than to run old scripts.

### Added Features [ edit ]

See  Extended grammar added in Emuera
NewPP limit report
Cached time: 20260902121220
Cache expiry: 86400
Dynamic content: false
CPU time usage: 0.057 seconds
Real time usage: 0.062 seconds
Preprocessor visited node count: 221/1000000
Preprocessor generated node count: 348/1000000
Post‐expand include size: 0/2097152 bytes
Template argument size: 0/2097152 bytes
Highest expansion depth: 2/40
Expensive parser function count: 0/100
Unstrip recursion depth: 0/20
Unstrip post‐expand size: 1675/5000000 bytes
Transclusion expansion time report (%,ms,calls,template)
100.00%    0.000      1 -total
