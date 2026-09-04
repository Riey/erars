# URL: https://wiki.eragames.rip/index.php/Emuera/debugcom
# fetch date: 2026-09-03

## Debug Commands [ edit ]

※Debug commands are not available by default. 
Check "Allow debug commands" from the Environment Settings menu if you want to use them.
※This function is a simple function of the version without  debug mode . 
We recommend that you start in  debug mode  for debugging with the current version.
During script execution (in game), if you enter a string beginning with "@", it will be accepted as a debug command. 
Case sensitivity depends on the "Ignore capitalization" config in emuera.config.
Debug commands are in the same format as ERB. 
For example, you can write like this:
```
@MONEY = 10000
@PRINTV FLAG:200
@PRINTFORM %NAME:MASTER% CFLAG(1) = {CFLAG:MASTER:1}
@ADDCHARA 1
```
Also, if you simply enter a variable or a formula, those values will be output 
（The following space after @ is not required）
```
@ FLAG:200
@ @"%NAME:MASTER% CFLAG(1) = {CFLAG:MASTER:1}"
```
However, you can not use instructions that change the execution flow such as IF and CALL, and instructions that require input such as INPUT and WAIT.
There are some instructions not in ERB.
- @REBOOT
    Restart and reread emuera.config, csv, and erb files.
- @OUTPUT
    Outputs the current log to emuera.log. If it already exists, it will be overwritten.
    This is the same operation as the OUTPUTLOG instruction.
- @EXIT
    Quit Emuera. Same operation as QUIT instruction.
- @CONFIG
    Opens the Settings dialog.
- @DEBUG
    Opens the debug dialog. This is valid only when started in debug mode .
Other than the above, if a normal ERB instruction is executed, MASTER's NAME and CALLNAME are changed to "CHEATER". 
This is a measure to prevent abuse, as debug commands are cheats.
NewPP limit report
Cached time: 20260903030922
Cache expiry: 86400
Dynamic content: false
CPU time usage: 0.016 seconds
Real time usage: 0.017 seconds
Preprocessor visited node count: 8/1000000
Preprocessor generated node count: 48/1000000
Post‐expand include size: 0/2097152 bytes
Template argument size: 0/2097152 bytes
Highest expansion depth: 2/40
Expensive parser function count: 0/100
Unstrip recursion depth: 0/20
Unstrip post‐expand size: 157/5000000 bytes
Transclusion expansion time report (%,ms,calls,template)
100.00%    0.000      1 -total
