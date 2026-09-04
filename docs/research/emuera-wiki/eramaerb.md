# URL: https://wiki.eragames.rip/index.php/Emuera/eramaerb
# fetch date: 2026-09-03

eramaker ERB file format (provisional) 
I think  it's hard to get a picture just by looking at this file.  It is easier to understand if you play the sample game first, and then look at the ERB files of the sample game while looking at it.
Contents 1 Basic Info 1.1 About the ERB file 1.2 How to write an ERB file 1.2.1 Comments & Spaces 1.2.2 Half-width input 1.2.3 Inputting a string 1.2.4 It' s all in one line 2 Variables and Instructions 2.1 About the variables 2.1.1 Putting a number in a variable 2.1.2 Putting a calculated number into a variable 2.1.3 Add or multiply variables 2.1.4 About Arrays 2.1.5 About the Double Sequence 2.1.6 Display the variable on the screen 2.1.7 About String Variables 2.1.8 Variable List 2.2 About Instructions (Basic) 2.2.1 How to write an instruction 2.2.2 Displaying Text 2.2.3 Making Conditional Decisions 2.2.4 Input and wait for input 2.2.5 Repetition and GOTO 2.2.6 About Functions 2.2.7 Other Basic Instructions 2.3 About Instructions (for Training) 2.3.1 Display of training data 2.3.2 Character Management 2.3.3 Save-Related 2.3.4 BEGIN 3 Update History

### Basic Info [ edit ]

#### About the ERB file [ edit ]

- Put a folder named ERB directly under eramaker.exe, and put ERB file in it.
- You can use any file name as long as the extension is .ERB
- Please edit it with a text editor such as Notepad.

#### How to write an ERB file [ edit ]

##### Comments & Spaces [ edit ]

In all ERB files,
- If the first character in the first column is a ; (semicolon), the line is ignored. Empty lines are also ignored.
- Don't put a semicolon at the end of a line, or a comment after it. Both ways can be done in Emuera.
- Any number of spaces or tabs at the beginning of a line will be ignored.
```
; CORRECT

;Setting up the money
MONEY = 500
;Setting the time
DAY = 10
TIME = 1
;Game start
PRINT What do we do now?

; Also CORRECT in Emuera
; The ; at the end of MONEY = 500 is not needed
; MISTAKE in eramaker

MONEY = 500;
TIME = 5; (Start on Day 5)
```

##### Half-width input [ edit ]

Please use half-width characters when inputting numbers, instructions, variable names, function names, etc.
```
; CORRECT

MONEY = 500
PRINT Let the game begin.

; MISTAKE

ＭＯＮＥＹ ＝　500
ＰＲＩＮＴ Let the game begin.
```

##### Inputting a string [ edit ]

It doesn't work properly if you enclose a string with "".
```
; CORRECT

PRINT The day is over...

; MISTAKE

PRINT "The day is over..."
```

##### It' s all in one line [ edit ]

Even when writing long instructions, don't split them into more than two lines.
```
; CORRECT

PRINT Nayuki's body was sensitive and she had a lot of experience, so she was happy even if it was a little rough. Even though she denies it with her mouth, Nayuki is getting more and more promiscuous. But the peculiar circumstance of her being asleep made me hesitant to make a bolder move.

; MISTAKE

PRINT 
Nayuki's body was sensitive and she had a lot of experience, so she was happy even if it was a little rough. Even though she denies it with her mouth, Nayuki is getting more and more promiscuous. But the peculiar circumstance of her being asleep made me hesitant to make a bolder move.
```

### Variables and Instructions [ edit ]

#### About the variables [ edit ]

In training SLG, the change of parameters is vital. Therefore, it is necessary to learn how to use "variables" that can store data and perform calculations such as adding and multiplying data.

##### Putting a number in a variable [ edit ]

Use = (equal). Please enter it in half-width characters. Before and after = is separated by a half-size space or a tab. 
The numbers that can be used in eramaker are basically integers. Do not enter a decimal point. *
```
; CORRECT

MONEY = 500

; MISTAKE

MONEY ＝ 500
MONEY=500
MONEY = 3.14
```
*Adendum:  Emuera has since added more number literal types. Check  Constants (Literals)  for more information.

##### Putting a calculated number into a variable [ edit ]

Use = in the same way. Note that * is for multiplication, / is for division, and % is for the rest of the division. 
Fractions are rounded down when the result is a decimal.
```
; CORRECT

;set MONEY to 74
MONEY = 15+34+25
;set MONEY to 650
MONEY = 150+(100-50)*10
;set MONEY to 3
MONEY = 10/3
;set MONEY to TIME multiplied by 10
MONEY = TIME*10
;If DAY is 0,1,2... then MONEY becomes 0,10,20... and returns to 0 when it exceeds 7.
MONEY = DAY%7*10

; MISTAKE

MONEY = 500×10÷4
```

##### Add or multiply variables [ edit ]

Use +=, -=, *=, /=, %=.
```
; EXAMPLE

MONEY = 100
TIME = 12
;set MONEY to 150
MONEY += 50
;set MONEY to 750
MONEY *= 7-2
;set MONEY to 80
MONEY -= 670
;set MONEY to 8
MONEY %= TIME
;set MONEY to 1
MONEY /= TIME-4
```

##### About Arrays [ edit ]

- The variables can be accessed as an "array". An "array" is for managing multiple data with the variable of the same name.
- To access the array, use : (colon). Please enter it in half-width characters. Don't put any spaces in between.
- The number that can be put after the array is at least 0. The maximum value is determined by the variable. See the list for more information.
- You can also put a variable instead of a number after the array. However, you can't put an array after an array.
```
; CORRECT

A = 35
;Set the FLAG to a value
FLAG:0 = 0
FLAG:2 = 10
FLAG:35 = 440
;Calculate with FLAG
FLAG:A += 100/FLAG:2
FLAG:2 *= FLAG:A

; MISTAKE

FLAG：0 = 10
FLAG : 52 = 1000
FLAG:FLAG:20 = 10000
FLAG:91881816 = 1
```

##### About the Double Sequence [ edit ]

- Exceptionally, some variables can be accessed using two colons. Usually variables related to your character's data.
- Use (variable name):(character number):(variable number) to access it.
```
; EXAMPLE

A = 2
;Makes the 5th character's 0th ability LV3.
ABL:5:0 = 3
;A (2nd) character's first experience is increased by 1.
EXP:A:1 += 1
```

##### Display the variable on the screen [ edit ]

- The easiest way to do this is to use the PRINTV and PRINTVL commands. We will discuss the instruction in more detail later.
```
; EXAMPLE

A = 2
PRINTV A
A = 30
PRINTVL A
B = 400
PRINTVL B

; RESULTS

230
400
```

##### About String Variables [ edit ]

- Regular variables can only handle integers, but there are string variables that can handle strings. However, the features are limited.
- To display a string variable on the screen, use the PRINTS or PRINTSL instruction.
```
; CORRECT

STR:0 = aiueo
PRINTSL STR:0

; MISTAKE

;You can't add it with +=
STR:0 += ueo
```

##### Variable List [ edit ]

- See here .

#### About Instructions (Basic) [ edit ]

Instructions can be used to display characters on the screen and to make conditional judgments.

##### How to write an instruction [ edit ]

- The basic writing style is (instruction name) (instruction content). Separate (instruction name) and (instruction content) with a half-size space or tab.
- If there is no (instruction content), start a new line as it is.
```
; CORRECT

PRINT This is a test.
SIF 3 == 1+2
   PRINT Obviously.
WAIT

; MISTAKE

PRINTThis is a test.。
;wait for input
WAIT 0
```

##### Displaying Text [ edit ]

- PRINT is an instruction to display text; PRINTL displays text and starts a new line; PRINTW displays text and waits for input.
- The PRINTV command displays the content of a variable, the PRINTVL command displays the content of the variable and makes a new line, the PRINTVW command displays the content of the variable and waits for input.
- PRINTS is an instruction that displays the contents of a string variable, PRINTSL displays the contents of the string variable and starts a new line, PRINTSW displays the contents of the string variable and waits for input, and PRINTSW displays the contents of the string variable and waits for input.
- PRINTFORM can display a combination of characters, variables, and string variables; PRINTFORML can do the same and break a new line; PRINTFORMW can do the same and wait for input.
- PRINTFORMS is the same as PRINTFORM. PRINTFORMSL is the same as PRINTFORM, but with a new line, PRINTFORMSW is the same as PRINTFORM and waits for input.
(If you are waiting for input with a W at the end of the command, press Enter to move on, and the line will eventually be broken.
```
; EXAMPLE

MONEY = 500
NAME:0 = Sato
PRINT The money is 
PRINTV MONEY
PRINTL  yen.
PRINT My name is 
PRINTS NAME:0
PRINTL .
PRINTFORML To repeat, the name is %NAME:0% and the money is {MONEY} yen.
PRINTFORMW If you get 1000 yen and pay 600 yen, you're left with {MONEY+1000-600} yen.
STR:0 = If you multiply that money by five, it's {(MONEY+1000-600)*5} yen.
PRINTFORMSW STR:0

; RESULTS

The money is 500 yen.
My name is Sato.
To repeat, the name is Sato and the money is 500 yen.
If you get 1000 yen and pay 600 yen, you're left with 900 yen.
If you multiply that money by five, it's 4500 yen.
```

##### Making Conditional Decisions [ edit ]

- The quickest way to understand conditional decisions is to look at examples .
- SIF executes the next line if the conditional expression is not 0 (if true). If it is 0 (if not true), it skips the next line.
- If the conditional expression is not 0 (if satisfied), IF executes from the next line until ELSE, ELSEIF, or ENDIF are encountered. (If it's ELSE, it will be executed from the next line until it receives an ENDIF. If the conditional expression is satisfied in ELSEIF, it executes the next lines until it encounters ELSE, ELSEIF, or ENDIF. If the conditional expression is not satisfied, skip until ELSE, ELSEIF, or ENDIF, and repeat)
```
; EXAMPLE

A = 1
B = 2
C = 4

SIF A == 1
   PRINTL Test 1
SIF B != 1
   PRINTL Test 2
SIF C < 5
   PRINTL Test 3
IF A+B > 2
   IF C >= 6
      PRINTL Test 4
   ELSE
      PRINTL Test 5
   ENDIF
   IF A == 1 && B == 3
      PRINTL Test 6
   ELSEIF A == 1 || B == 3
      PRINTL Test 7
   ELSEIF A > 1 || (B > 2 && C > 2)
      PRINTL Test 8
   ENDIF
ELSEIF A+B == 2
   PRINTL Test 9
ELSE
   PRINTL Test 10
ENDIF

; RESULTS

Test 1
Test 2
Test 3
Test 5
Test 7
```
- Use == for "equal" and != for "not equal", > for "left is greater than", >= for "left is greater or equal than right", < for "left is less than", and <= for "left is less or equal than right". All fields must be typed in half-width characters.
- Use && for "and" and || for "or". All fields must be typed in half-width characters.
- You can also use parentheses to describe complex conditions.

##### Input and wait for input [ edit ]

- Use WAIT to wait for input, e.g. to display a sentence.
（※Usually something like PRINTW is used instead. It makes it easier to see because stuff can be expressed with fewer lines）
- If you want the player to input an integer, use INPUT. The result of the input is stored in the RESULT variable.
- If you want the player to input a string, use INPUTS. The result of the input is stored in RESULTS variable.
```
; EXAMPLE

PRINT Data entry begins.
WAIT
PRINTL Please enter your age.
INPUT
PRINTL Please enter your name.
INPUTS
PRINTFORML %RESULTS% is {RESULT} years old, isn't it?
```

##### Repetition and GOTO [ edit ]

- If you want to repeat the same instruction, use REPEAT, which is repeated until REND is found. The number of times it is repeated is stored in the COUNT.
- Please note that you cannot create a REPEAT inside a REPEAT.
- If you use CONTINUE on the way from REPEAT to REND, it will go back to where REPEAT was. If you use BREAK, it will stop repeating and skip to the next line after REND.
- If you want to move to another place at once, you can use GOTO. To use GOTO, you need to register a "label" with $.
```
; Example 1

REPEAT 10
   PRINT AIU
REND
;Line break by writing 0 characters in PRINTL
PRINTL 
REPEAT 5
   PRINTFORML Score: {COUNT*5}
REND

; Results Of Example 1

AIUAIUAIUAIUAIUAIUAIUAIUAIUAIU
Score: 0
Score: 5
Score: 10
Score: 15
Score: 20

; Example 2

MONEY = 300
REPEAT 5
    SIF MONEY <= COUNT*100
        BREAK
    PRINTFORML That's more money than {COUNT*100} yen.
REND
REPEAT 5
    SIF MONEY == COUNT*100
        CONTINUE
    PRINTFORML The holdings are not {COUNT*100} yen.
REND

; Results Of Example 2

That's more money than 0 yen.
That's more money than 100 yen.
That's more money than 200 yen.
The holdings are not 0 yen.
The holdings are not 100 yen.
The holdings are not 200 yen.
The holdings are not 400 yen.

; Example 3

$INPUT_LOOP
PRINTL Enter a number from 0 to 9.
INPUT
SIF RESULT < 0 || RESULT > 9
    GOTO INPUT_LOOP
PRINTFORML {RESULT} has been entered.
```

##### About Functions [ edit ]

- It's hard to understand your program when you're writing it from start to finish.
- You can use "functions" to break them down into parts and make them easier to understand.
- Functions are registered using @. After @, write the name of the function using the alphabet and _ (underscore). Please enter the function name in half-width characters.
- The first function called in the game should be named EVENTFIRST (more on this later).
- To move to another function, use JUMP.
- If you want to move to another function and then resume from the original position when the function is finished, use CALL.
- If you use RETURN in a function called by CALL, you can terminate the function in the middle. At that time, the number specified by RETURN is stored in RESULT. If the function exits without using RETURN, RESULT will be set to 0.
- If you use RESTART, you start from the beginning of the function.
```
; EXAMPLE

@EVENTFIRST
PRINTW Game start.

CALL OPENING
PRINTFORMW The opening result was {RESULT}.
CALL GAME_MAIN
PRINTFORMW The result of the game was {RESULT}.
JUMP ENDING

PRINTL You can't see this part because I used JUMP.

@OPENING
PRINTW Doing Opening.
RETURN 25

@GAME_MAIN
PRINTW We're in the game.
PRINTL Ending without RETURN.

@ENDING
PRINTW Doing Ending.
RESTART

; RESULTS

Game start.
Doing Opening.
The opening result was 25.
We're in the game.
Ending without RETURN.
The result of the game was 0.
Doing Ending.
Doing Ending.
Doing Ending.
Doing Ending.
Doing Ending.
.......(keeps on infinitely)
```

##### Other Basic Instructions [ edit ]

- Use QUIT to quit the game.
- With DRAWLINE, you can draw a line ---- from the left edge of the screen to the right edge of the screen.
- You can use TIMES to multiply by decimals. eramaker basically treats numbers as integers, so if you want to intertwine decimals, you can use this instruction: TIMES (variable),(fractional).
- If you use BAR, you can display a graph like [*****....], if you use BARL, you can use it in the form of a new line. BARL is used the same way as BAR (variable),(maximum value),(length).
```
; EXAMPLE

MONEY = 500
DRAWLINE
BARL MONEY , 1000 , 20
PRINTFORMW I have {MONEY} yen.
DRAWLINE
TIMES MONEY , 1.25
BARL MONEY , 1000 , 20
PRINTFORMW It is now {MONEY} yen. End the game.
QUIT

; RESULTS

---------------------------------------------------------------------
[**********..........]
I have 500 yen.
---------------------------------------------------------------------
[************........]
It is now 625 yen. End the game.
```

#### About Instructions (for Training) [ edit ]

- The eramaker has a number of special instructions to use for training.

##### Display of training data [ edit ]

- PRINT_ABL displays the character's abilities.
- PRINT_TALENT displays the character's qualities.
- PRINT_MARK displays the character's marks.
- PRINT_EXP displays the experiences of the character.
- PRINT_PALAM displays the character's training parameters.
- When using the above instructions, specify which character's data you want to display. For example, PRINT_ABL 0 will usually show the ability of the protagonist.
- PRINT_ITEM displays the items in your possession.
- PRINT_SHOPITEM displays the items available in the shop.
- UPCHECK shows the change in training parameters as a result of training commands.

##### Character Management [ edit ]

- ADDCHARA adds a character. If you want to add the character number 3, you do it with ADDCHARA 3.
- ADDSPCHARA adds an SP character. If you want to add an SP character with character number 3, use ADDSPCHARA 3.
(An SP character is a character whose character flag number 0 is 1)
- DELCHARA removes characters added by ADDCHARA and others.
```
; EXAMPLE

;The name of the character with character number 0 is Hiroyuki, the main character.
;Suppose the name of the character number 3 is Tomoko, the name of the character number 5 is Remy, and the name of the character number 6 is Kotone.
PRINTFORML The number of characters you have now is {CHARANUM}.

ADDCHARA 3
ADDCHARA 5
ADDCHARA 6
PRINTFORML The number of characters you have now is {CHARANUM}.
REPEAT CHARANUM
   PRINTFORML At number {COUNT} is %NAME:COUNT%。
REND
DELCHARA 2
PRINTFORML The number of characters you have now is {CHARANUM}.
REPEAT CHARANUM
   PRINTFORML At number {COUNT} is %NAME:COUNT%。
REND

; RESULTS

The number of characters you have now is 1.
The number of characters you have now is 4.
At number 0 is Hiroyuki。
At number 1 is Tomoko。
At number 2 is Remy。
At number 3 is Kotone。
The number of characters you have now is 3.
At number 0 is Hiroyuki。
At number 1 is Tomoko。
At number 2 is Kotone。
```

##### Save-Related [ edit ]

- SAVEGAME calls the save screen and LOADGAME calls the load screen. You must be a SHOP to be able to call them.
- PUTFORM can only be used with a special function called @SAVEINFO, which can be written in the same format as PRINTFORM to give an overview of the saved data. It's a good idea to write data such as what day it is, how good your characters are, and which characters you're training.

##### BEGIN [ edit ]

- BEGIN progresses the game by invoking various system instructions.
- When BEGIN is called, the running function is terminated, and even if it is called from somewhere by CALL, it does not return to the original function.
- BEGIN TRAIN will start training.
- BEGIN AFTERTRAIN is no longer training.
- BEGIN ABLUP invokes the Ability Up screen.
- BEGIN TURNEND ends its turn.
- BEGIN SHOP calls the SHOP.

### Update History [ edit ]

- 2020/04/09　Page fully translated.
- 2006/05/05　Provisional version released.
NewPP limit report
Cached time: 20260902104645
Cache expiry: 86400
Dynamic content: false
CPU time usage: 0.039 seconds
Real time usage: 0.041 seconds
Preprocessor visited node count: 294/1000000
Preprocessor generated node count: 508/1000000
Post‐expand include size: 0/2097152 bytes
Template argument size: 0/2097152 bytes
Highest expansion depth: 2/40
Expensive parser function count: 0/100
Unstrip recursion depth: 0/20
Unstrip post‐expand size: 6415/5000000 bytes
Transclusion expansion time report (%,ms,calls,template)
100.00%    0.000      1 -total
