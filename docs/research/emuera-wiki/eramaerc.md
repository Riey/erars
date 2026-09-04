# URL: https://wiki.eragames.rip/index.php/Emuera/eramaerc
# fetch date: 2026-09-03

eramaker era basic Structure (provisional version)
I'm sure  it's hard to get a picture just by looking at this file . It is easier to understand if you play the sample game first, and then look at the ERB files of the sample game while you have it open.
Contents 1 Basic Information 1.1 When opening eramaker 1.2 SHOP 1.2.1 Entering SHOP 1.2.2 Command Selection in SHOP 1.2.3 Purchasing 1.3 TRAIN 1.3.1 Entering TRAIN 1.3.2 Command Selection in TRAIN 1.3.3 End of Command Selection in TRAIN 1.3.4 User Command 1.4 AFTERTRAIN 1.5 ABLUP 1.5.1 View ABLUP 1.5.2 ABLUP's command selection 1.6 TURNEND 1.7 Event Function 1.7.1 About Event Functions 2 Update History

### Basic Information [ edit ]

#### When opening eramaker [ edit ]

- In eramaker, when you start the game, the title screen will appear and you will be presented with a choice between 「New Game」 and 「Load Game」. If you choose 「New Game」, the function EVENTFIRST will be called in the ERB file.
- When the EVENTFIRST is over, the game stops. So you need to call SHOP, TRAIN, etc with BEGIN.
- See the beginning of the SYSTEM.ERB in the sample game.

#### SHOP [ edit ]

##### Entering SHOP [ edit ]

- When you enter SHOP, a function called @EVENTSHOP (if it exists) is called. This is an #Event Function .
- After that, the function @SHOW_SHOP will be called. This is where you can display basic information such as the date and characters being trained, use PRINT_SHOPITEM for sales, and special actions such as saving and loading.

##### Command Selection in SHOP [ edit ]

- If a number between 0 to 99 is chosen, it means to buy an item. If any other number is chosen, the function @USERSHOP is called. The selected number is stored in RESULT and should be processed accordingly.
- Both are easy to understand if you look at the sample game's SHOP.ERB.

##### Purchasing [ edit ]

- When you buy an item, the function @EVENTBUY is called (if it exists). This is an #Event Function .
(Items bought disappear from the shop lineup, so it's a good idea if you want to do something here)

#### TRAIN [ edit ]

##### Entering TRAIN [ edit ]

- When you enter TRAIN, the function @EVENTTRAIN (if it exists) is called. This is an #Event Function .
- After that, the function @SHOW_STATUS is called. This is where you can display basic information such as the date and characters being trained, as well as using PRINT_PALAM to show parameters in training.
- It will then automatically display the commands that can be executed. The function @COM_ABLExx will be called for every command, which means that it can be executed if its RETURN is 1. Moreover, if the corresponding @COM_ABLExx does not exist, it also means that it is executable. The sample game COMABLE.ERB should be easy to understand.
- In addition, the function @SHOW_USERCOM will be called. Special commands, such as ending training can be displayed here.
- Both are easy to understand by looking at the sample games SYSTEM.ERB and INFO.ERB.

##### Command Selection in TRAIN [ edit ]

- When the player chooses a command, the function @EVENTCOM (if it exists) is called first. This is an #Event Function .
- In addition, the function corresponding to the selected command will be called. For example, if 「Missionary」 is selected and the command number of 「Missionary」 is 20 in TRAIN.CSV, the function @COM20 will be called.
- In the case of commands like 「Blowjob」, you may not be able to execute them depending on your character's ability. In this case, RETURN 0 is called in the middle of @COMxx. Then, it will return to the command selection without executing the command.
- If the command can be executed, RETURN 1 is called from @COMxx. Then the function @SOURCE_CHECK will be called. This is where the results of the training command are reflected in the training parameters.
- Both COMxx.ERB and SOURCE.ERB are easy to understand by looking at the sample games.

##### End of Command Selection in TRAIN [ edit ]

- Finally, a function called @EVENTCOMEND (if it exists) will be called. This is an #Event Function .
- It's a good idea to do the dialogue of the character whose command is executed here. The sample game era light's CKOJOxx.ERB should be easy to understand.

##### User Command [ edit ]

- If there is no @COMxx corresponding to the selected command, @USERCOM will be called. RESULT contains the selected command number, so please perform the corresponding operations.
- The sample game's SYSTEM.ERB should be easy to understand when inspected.

#### AFTERTRAIN [ edit ]

- When you enter AFTERTRAIN, the function @EVENTEND (if it exists) will be called. This is an #Event Function .
- It's a good idea to deal with your character's dialogue (parting words?) when you finish training here. In addition, the calculation of the Gems obtained from the training should be done here as well. The sample game's AFTERTRA.ERB should be easy to understand.

#### ABLUP [ edit ]

##### View ABLUP [ edit ]

- First, the function @SHOW_JUEL is called. It shows the Gems you have.
- Then, the function @SHOW_ABLUP_SELECT will be called. It displays the list of Abilities, exit commands, etc.
- The sample game's ABL.ERB is easy to understand.

##### ABLUP's command selection [ edit ]

- When the player selects a command, the function corresponding to the selected command is called. For example, if 「C-Sense」 in [3] is selected, the function @ABLUP3 will be called.
- If there is no @ABLUPxx corresponding to the selected command, @USERABLUP will be called. RESULT will contain the selected ABL number, so please perform the corresponding operations.
- The sample games ABLUPxx.ERB and ABL.ERB are easy to understand.

#### TURNEND [ edit ]

- The function @EVENTTURNEND (if it exists) will be called. This is #Event Function .
- You may want to deal with time progression, physical recovery, etc.
- Please note that the game needs @EVENTTURNEND or it will stop.
- The sample game's SYSTEM.ERB is easy to understand.

#### Event Function [ edit ]

##### About Event Functions [ edit ]

- An event function is a function that is always called at a certain time in the game. It is called when you have finished executing a command or when you want to start training.
- Event functions are useful for displaying the character's dialogue. If you write instructions to display dialogue directly to functions such as @COMxx it will be confusing later, but you can use event functions to manage them separately.
- Due to the nature of event functions, there may be more than one with the same name.
```
; Example

;The line when you finish performing Missionary
@EVENTCOMEND
;20 is Missionary, if not 20, ignore it.
SIF SELECTCOM != 20
    RETURN 0
;Ignore if FLAG:1000 is not 0
SIF FLAG:1000
    RETURN 0
PRINTW 「No... Oh my God!」
;Set the FLAG. Once it appeared, it won't appear again.
FLAG:1000 = 1
RETURN 1

;The line when you finish executing DoggyStyle
@EVENTCOMEND
;21 is DoggyStyle; if it's not 21, ignore it.
SIF SELECTCOM != 21
    RETURN 0
;Ignore if FLAG:1001 is not 0
SIF FLAG:1001
    RETURN 0
PRINTW 「Not like this...」
;Set the FLAG. Once it appeared, it won't appear again.
FLAG:1001 = 1
RETURN 1
```
- Event functions can be given a "property".
- If the property #SINGLE is given, the function with the same name will not be executed if it exits with RETURN 1. This is useful when a character says several lines at a time that are unnatural.
- If you grant the property #PRI , it will be executed before any other function of the same name. This is useful for processes such as death checks that would be unnatural if not done first.
- When the property #LATER is given, it is executed after other functions of the same name. This is useful for processes that are unnatural if you don't do it at the end, such as displaying "The day is over".
```
; Example

@EVENTTURNEND
#SINGLE
SIF FLAG:1000
    RETURN 0
FLAG:1000 = 1
PRINTW 「I don't want to...」
RETURN 1

@EVENTTURNEND
#SINGLE
SIF FLAG:1001
    RETURN 0
FLAG:1001 = 1
PRINTW 「Let me go home...」
RETURN 1

@EVENTTURNEND
#SINGLE
SIF FLAG:1002
    RETURN 0
FLAG:1002 = 1
PRINTW 「I need to get out...」
RETURN 1

@EVENTTURNEND
#LATER
PRINTW The day is over...
RETURN 1

（The three lines above can only appear one at a time. The "The day is over..." is marked with #LATER, so it never appears until FLAG:1000, FLAG:1001, or FLAG:1002 become 1.）
```

### Update History [ edit ]

- 04/07/2020　The provisional version was translated.
- 2006/05/05　The provisional version was released.
NewPP limit report
Cached time: 20260903062945
Cache expiry: 86400
Dynamic content: false
CPU time usage: 0.032 seconds
Real time usage: 0.032 seconds
Preprocessor visited node count: 93/1000000
Preprocessor generated node count: 120/1000000
Post‐expand include size: 0/2097152 bytes
Template argument size: 0/2097152 bytes
Highest expansion depth: 2/40
Expensive parser function count: 0/100
Unstrip recursion depth: 0/20
Unstrip post‐expand size: 1181/5000000 bytes
Transclusion expansion time report (%,ms,calls,template)
100.00%    0.000      1 -total
