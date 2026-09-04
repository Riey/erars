# URL: https://wiki.eragames.rip/index.php/Emuera/eramavar
# fetch date: 2026-09-03

eramaker era basic variable list (provisional version) 
I think  it's hard to get a picture just by looking at this file.  It is easier to understand if you play the sample game first, and then look at the ERB files of the sample game while looking at it.
Contents 1 Basic Variables 1.1 Generic Variables 1.1.1 A-Z 1.1.2 COUNT 1.1.3 RESULT 1.1.4 RESULTS (String Variable) 1.2 Basic Information Variables 1.2.1 DAY 1.2.2 TIME 1.2.3 MONEY 1.3 Basic Training Information Variables 1.3.1 MASTER 1.3.2 TARGET 1.3.3 ASSI 1.3.4 PLAYER 1.3.5 CHARANUM 1.3.6 ASSIPLAY 1.3.7 SELECTCOM 1.3.8 PREVCOM 1.4 Training Variables 1.4.1 LOSEBASE (Array, 0-999) 1.4.2 UP (Array, 0-999) 1.4.3 DOWN (Array, 0-999) 1.4.4 PALAMLV (Array, 0-999) 1.4.5 EXPLV (Array, 0-999) 1.4.6 EJAC 1.5 Flags 1.5.1 FLAG (Array, 0-9999) 1.5.2 TFLAG (Array, 0-999) 1.6 Character Data 1.6.1 NO (Array, 0-99) 1.6.2 BASE (Double Array, 0-99) 1.6.3 MAXBASE (Double Array, 0-99) 1.6.4 ABL (Double Array, 0-99) 1.6.5 TALENT (Double Array, 0-999) 1.6.6 EXP (Double Array, 0-99) 1.6.7 MARK (Double Array, 0-99) 1.6.8 RELATION (Double Array, 0-99) 1.6.9 JUEL (Double Array, 0-199) 1.6.10 CFLAG (Double Array, 0-999) 1.6.11 ISASSI (Array, 0-99) 1.6.12 NAME (Array, 0-99) (String Variable) 1.6.13 CALLNAME (Array, 0-99) (String Variable) 1.6.14 TEQUIP (Double Array, 0-99) 1.6.15 PALAM (Double Array, 0-99) 1.6.16 STAIN (Double Array, 0-99) 1.6.17 EX (Double Array, 0-99) 1.6.18 SOURCE (Double Array, 0-99) 1.6.19 NOWEX (Double Array, 0-99) 1.6.20 GOTJUEL (Double Array, 0-99) 1.7 Item Data 1.7.1 ITEM (Array, 0-99) 1.7.2 ITEMSALES (Array, 0-99) 1.7.3 BOUGHT 1.7.4 NOITEM 1.7.5 PBAND 1.8 Name Data 1.8.1 ABLNAME (Array, 0-99) (String Variable) 1.8.2 TALENTNAME (Array, 0-99) (String Variable) 1.8.3 EXPNAME (Array, 0-99) (String Variable) 1.8.4 MARKNAME (Array, 0-99) (String Variable) 1.8.5 PALAMNAME (Array, 0-199) (String Variable) 1.8.6 ITEMNAME (Array, 0-99) (String Variable) 1.9 String Data 1.9.1 STR (Array, 0-19999) (String Variable) 1.9.2 SAVESTR (Array, 0-99) (String Variable) 1.10 Other Data 1.10.1 RAND (Pseudo-Array) 2 Character Registration Number 2.1 The difference between a character registration number and a character number 2.1.1 Unembodied Character Data 2.1.2 Registering a character 2.1.3 Removing a character 3 About Stain 3.1 Specifics of the stain data 3.1.1 Types of Stains 3.1.2 How to describe the stain data 3.1.3 How to get the stain data 3.1.4 How to add stain data 4 Update History

### Basic Variables [ edit ]

#### Generic Variables [ edit ]

##### A-Z [ edit ]

- The variable is a single letter of the alphabet, from A to Z.
- You can use it as a disposable data container as you like. However, it is not suitable for long time data storage because it is not known where it is rewritten.

##### COUNT [ edit ]

- This variable counts the number of times it is repeated when using the REPEAT instruction.
- Do not rewrite the contents of COUNT between REPEAT and REND, as this may lead to a malfunction.

##### RESULT [ edit ]

- A variable that records various results.
- Since you don't know where it could be rewritten, it is preferable to move the data to another variable unless it is used on the spot.

##### RESULTS (String Variable) [ edit ]

- A string variable that records various results.
- Since you don't know where it could be rewritten, it is preferable to move the data to another variable unless it is used on the spot.

#### Basic Information Variables [ edit ]

##### DAY [ edit ]

- Records the date. Feel free to handle it as you wish.

##### TIME [ edit ]

- Records the time. Feel free to handle it as you wish.

##### MONEY [ edit ]

- Keeps track of your money. The program will refer to it when you shop, so don't rewrite it unless you've gained or lost money.

#### Basic Training Information Variables [ edit ]

##### MASTER [ edit ]

- It refers to the protagonist's Character Registration Number , which may be different from the number specified in CharaXX.csv. It is usually 0.

##### TARGET [ edit ]

- This is the Character Registration Number of the trainee, which may be different from the number specified in CharaXX.csv.

##### ASSI [ edit ]

- It refers to the assistant's Character Registration Number , which may be different from the number specified in CharaXX.csv.

##### PLAYER [ edit ]

- Refers to the Character Registration Number of the person doing the training. Normally it should match MASTER or ASSI, but be aware that the number may be different from the one specified in CharaXX.csv.

##### CHARANUM [ edit ]

- The number of characters currently registered. Includes the protagonist. This variable cannot be changed by the user.

##### ASSIPLAY [ edit ]

- If it's a 1, the assistant is doing the training, if it's a 0, it's not.

##### SELECTCOM [ edit ]

- The selected command is the same command number as the one registered in TRAIN.CSV.

##### PREVCOM [ edit ]

- This is the previous selected command. This is used for example for penalties for executing the same command in succession.

#### Training Variables [ edit ]

##### LOSEBASE (Array, 0-999) [ edit ]

- It's how much of the basic parameters are lost by the training command. Normally, LOSEBASE:0 is for physical exhaustion and LOSEBASE:1 is for energy exhaustion.

##### UP (Array, 0-999) [ edit ]

- This is how much the training parameters are raised by the training command. A in UP:A is the parameter number specified in PALAM.CSV.

##### DOWN (Array, 0-999) [ edit ]

- This is how much the training parameters are lowered by the training command. A of DOWN:A is the parameter number specified in PALAM.CSV.

##### PALAMLV (Array, 0-999) [ edit ]

- This is the boundary value of the level of the parameter in training. If the training parameters exceed this threshold, the number of gems you receive after training will increase dramatically.

##### EXPLV (Array, 0-999) [ edit ]

- This is the boundary value for the level of experience. If your experience exceeds this threshold, your training may be more effective (especially with V and A experience).

##### EJAC [ edit ]

- This is a temporary variable that is used for ejaculation checks. It's an independent variable for readability, but it's really just a data entry.

#### Flags [ edit ]

##### FLAG (Array, 0-9999) [ edit ]

- Record the various states of the game. In the sample game, FLAG:0 is used to determine if a character in training has been rested or not. It's also frequently used to see if an event has occurred or not.

##### TFLAG (Array, 0-999) [ edit ]

- Record the various states of the game. Think of it as a Temporary Flag, or Training Flag, which is used in the sample game to record how you ejaculated and whether you performed the service training. In short, it is a flag for temporary use rather than FLAG.

#### Character Data [ edit ]

- Most of the character data is double-arranged and is accessed as EXP:1:2 (looking at the second experience of the first character).
- However, it can also be written as EXP:0. In this case, it will be interpreted as EXP:TARGET:0. In other words, you don't need to write TARGET: to access the data of the character being trained.

##### NO (Array, 0-99) [ edit ]

- Character number. Since it is not a double array, it is accessed as NO:TARGET or NO:ASSI.

##### BASE (Double Array, 0-99) [ edit ]

- These are the basic parameters of the character. In the sample game, BASE:0 represents health, BASE:1 represents energy and BASE:2 represents ejaculation gauge.

##### MAXBASE (Double Array, 0-99) [ edit ]

- This is the maximum value of the basic parameters of the character.

##### ABL (Double Array, 0-99) [ edit ]

- The ability of your character, which is accessed by the ability number registered in ABL.CSV.

##### TALENT (Double Array, 0-999) [ edit ]

- The qualities of the character. It is accessed by the quality number registered in TALENT.CSV.

##### EXP (Double Array, 0-99) [ edit ]

- The experience of the character, which is accessed by the experience number registered in EXP.CSV.

##### MARK (Double Array, 0-99) [ edit ]

- This is the engraving of the character. It is accessed by the mark number registered in MARK.CSV.

##### RELATION (Double Array, 0-99) [ edit ]

- RELATION:TARGET:3 indicates the compatibility of the character in training with the character number 3. Note that I'm not referring to the compatibility with the characters with Character Registration Number 3.

##### JUEL (Double Array, 0-199) [ edit ]

- This is the gems that character has. Access by the parameter number registered in PALAM.CSV.

##### CFLAG (Double Array, 0-999) [ edit ]

- This is a flag for each character. You can record a variety of data for each character. In the sample game, a character whose CFLAG:0 is 1 is considered an SP character.

##### ISASSI (Array, 0-99) [ edit ]

- If it's a 0, it's not an assistant. If it's a 1, it's an assistant. Since it is not a double array, it is accessed as ISASSI:TARGET or ISASSI:ASSI.

##### NAME (Array, 0-99) (String Variable) [ edit ]

- The name of the character. Since it is not a double array, it is accessed as NAME:TARGET or NAME:ASSI.

##### CALLNAME (Array, 0-99) (String Variable) [ edit ]

- This is the name of the character. Since it is not a double array, it is accessed as CALLNAME:TARGET or CALLNAME:ASSI.

##### TEQUIP (Double Array, 0-99) [ edit ]

- This is the item that Chara is wearing. You can use this when you're putting in vibrators during training, but you can also use it for character enhancement items. In the sample game, we also used it to judge the use of aphrodisiacs.

##### PALAM (Double Array, 0-99) [ edit ]

- Character's training parameters, accessed by the parameter numbers registered in PALAM.CSV.

##### STAIN (Double Array, 0-99) [ edit ]

- This is the "dirt" generated by training. It changes when you ejaculate from a blow job or have anal sex. In the sample game, STAIN:0 refers to the mouth, STAIN:1 to the hand, STAIN:2 to the penis, STAIN:3 to the vagina, and STAIN:4 to the anal stain. See here for details of the stain notation.

##### EX (Double Array, 0-99) [ edit ]

- This is how many times climaxed during this training. In the sample game, EX:0 is a C climax, EX:1 is a V climax, and EX:2 is an A climax.

##### SOURCE (Double Array, 0-99) [ edit ]

- This is the training source generated by executing commands. If you look at the COMxx.ERB and SOURCE.ERB, you will see the flow from the training source to the UP of the training parameters.

##### NOWEX (Double Array, 0-99) [ edit ]

- This is how many times climaxed during this command alone. In the sample game, NOWEX:0 is a C climax, NOWEX:1 is a V climax, and NOWEX:2 is an A climax.

##### GOTJUEL (Double Array, 0-99) [ edit ]

- This is the gems received after this training, which is accessed by the parameter number registered in PALAM.CSV.

#### Item Data [ edit ]

##### ITEM (Array, 0-99) [ edit ]

- This is the number of each item you have, accessed by the item number registered in ITEM.CSV.

##### ITEMSALES (Array, 0-99) [ edit ]

- It' s whether the item is available in the shop or not. If it's a 1, it's for sale. If it's a 0, it's not for sale. The item number registered in ITEM.CSV is used to access it.

##### BOUGHT [ edit ]

- Here's what items were bought. You can use this if you want to delete the item you just bought at @EVENTBUY from the shop.

##### NOITEM [ edit ]

- If NO ITEM is specified in GAMEBASE.CSV, it will be set to 1. In this case, the execution of the command will ignore the presence or absence of the item.

##### PBAND [ edit ]

- This is the item number for the strap-on. It is set to 4 by default. Strap-on is treated as an independent variable because it is often involved in training decisions.

#### Name Data [ edit ]

##### ABLNAME (Array, 0-99) (String Variable) [ edit ]

- The name of the ability, accessed by the parameter number registered in ABL.CSV.

##### TALENTNAME (Array, 0-99) (String Variable) [ edit ]

- The name of the property, accessed by the parameter number registered in TALENT.CSV.

##### EXPNAME (Array, 0-99) (String Variable) [ edit ]

- The name of the experience, which is accessed by the parameter number registered in EXP.CSV.

##### MARKNAME (Array, 0-99) (String Variable) [ edit ]

- The name of the mark, accessed by the parameter number registered in MARK.CSV.

##### PALAMNAME (Array, 0-199) (String Variable) [ edit ]

- The name of the parameter while training, which is accessed by the parameter number registered in PALAM.CSV.

##### ITEMNAME (Array, 0-99) (String Variable) [ edit ]

- The name of the item, which is accessed by the parameter number registered in ITEM.CSV.

#### String Data [ edit ]

##### STR (Array, 0-19999) (String Variable) [ edit ]

- String data. The data of STR.CSV is stored here. Note that modifying this variable will not save it.

##### SAVESTR (Array, 0-99) (String Variable) [ edit ]

- String data. The data recorded here will be saved when you save it. You can store the string variables you want to use all the time here.

#### Other Data [ edit ]

##### RAND (Pseudo-Array) [ edit ]

- This is a special variable that returns a random number. For example: PRINTV RAND:10 randomly displays a number from 0 to 9.
- Remember that the return value is an integer from 0 to A-1 in the case of RAND:A.

### Character Registration Number [ edit ]

#### The difference between a character registration number and a character number [ edit ]

##### Unembodied Character Data [ edit ]

- The number (番号) specified in CharaXX.CSV. That's the "character number".
- However, not all of the characters in CharaXX.CSV are real at the start of the game. They only become real when the ADDCHARA command is called.

##### Registering a character [ edit ]

- At the start of the game, the only character who is an entity is the main character. And the main character's "character registration number" is 0.
- Let's assume that a character whose character number is 5 in ADDCHARA is registered here. The "character number" of this character is 5, but the "registration number of the character" is 1 because it is next to the main character. If you add one more character with a character number of 7, the registration number of that character is 2.

##### Removing a character [ edit ]

- Now, let's assume that the DELCHARA command deleted a character whose registration number is 1. Then, the "registration number" of the character with "character number 7" added later will become 1.
- Keep in mind that the currently registered characters are given a "character registration number" from 0 without any gaps.

### About Stain [ edit ]

#### Specifics of the stain data [ edit ]

##### Types of Stains [ edit ]

- The stain data is managed by STAIN. STAIN:TARGET:0 means the dirt of the mouth of the character being trained.
- However, there are many different types of stains that can be considered here. If you give a blow job, you'll get a smear of semen, and if you give a cunnilingus, you'll get a smear of love juice.
- The sample game assumes four types of dirt: vagina, penis, semen, and anus.

##### How to describe the stain data [ edit ]

- Now, the numbers 1, 2, 4, and 8 have been assigned to this stain. In other words, if there are anal and seminal stains, 4+8 is 12, if there are love semen and seminal stains, 1+4 is 5, and so on.
- If we express it this way, one variable can handle the four types of stains well. However, "added semen stains in the mouth" and "determining whether there is love semen stains in the mouth" do not work well with the conventional +-*/%.

##### How to get the stain data [ edit ]

- So, we use the & and | operators. It's similar to && (and) and || (or), but it's used alone.
- For example, let's say that STAIN:TARGET:0 is 12. If we call STAIN:TARGET:0 & 4 here, only the part of 4 will be extracted. That is, STAIN:TARGET:0 & 4 == 4.
- Next, let's assume that STAIN:TARGET:0 is 1+2+8=11. If we call STAIN:TARGET:0 & 4 here, only the part of 4 will be extracted. That is, STAIN:TARGET:0 & 4 == 0.
- This way, you can easily tell which stains are present or not.

##### How to add stain data [ edit ]

- Next, let's say that STAIN:TARGET:0 is 1+4=5. If we type STAIN:TARGET:0 | 2, the 2 part will be added. That is, STAIN:TARGET:0 | 2 == 7.
- And let's say that STAIN:TARGET:0 is 1+2+4=7. If we set STAIN:TARGET:0 | 2 here, the 2 part will be added. But part 2 is already included, so it doesn't change. That is, STAIN:TARGET:0 | 2 == 7.
- It is also possible to write STAIN:TARGET:0 |= 2.
- This way, additional dirt can be treated without worrying about whether it already contains dirt or not.

### Update History [ edit ]

- 09/04/2020 Page fully translated.
- 2006/05/05 Provisional version released.
NewPP limit report
Cached time: 20260902111427
Cache expiry: 86400
Dynamic content: false
CPU time usage: 0.075 seconds
Real time usage: 0.077 seconds
Preprocessor visited node count: 321/1000000
Preprocessor generated node count: 328/1000000
Post‐expand include size: 0/2097152 bytes
Template argument size: 0/2097152 bytes
Highest expansion depth: 2/40
Expensive parser function count: 0/100
Unstrip recursion depth: 0/20
Unstrip post‐expand size: 0/5000000 bytes
Transclusion expansion time report (%,ms,calls,template)
100.00%    0.000      1 -total
