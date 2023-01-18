// Originally copied from Emuera 1824

#[allow(non_camel_case_types)]
#[derive(
    strum::Display,
    strum::IntoStaticStr,
    strum::EnumIter,
    strum::EnumCount,
    num_derive::FromPrimitive,
    num_derive::ToPrimitive,
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
)]
pub enum InstructionCode {
    CLEARLINE,
    REUSELASTLINE,

    INPUTS, //文字列入力。入力はRESULTSへ。
    INPUT,  //整数入力。入力はRESULTへ。
    TINPUTS,
    TINPUT,
    TWAIT,
    WAITANYKEY,
    WAIT,      //改行待ち。
    FORCEWAIT, //スキップで省略できないWAIT、強制TWAITと違い、スキップを打ち切る
    ONEINPUT,
    ONEINPUTS,
    TONEINPUT,
    TONEINPUTS,
    AWAIT, //入力不可 DoEvents

    DRAWLINE, //画面の左端から右端まで----と線を引く。
    BAR,      //[*****....]のようなグラフを書く。BAR (変数) , (最大値), (長さ)
    BARL,     //改行付き。
    TIMES,    //小数計算。TIMES (変数) , (小数値)という形で使う。

    PRINT_ABL,      //能力。引数は登録番号
    PRINT_TALENT,   //素質
    PRINT_MARK,     //刻印
    PRINT_EXP,      //経験
    PRINT_PALAM,    //パラメータ
    PRINT_ITEM,     //所持アイテム
    PRINT_SHOPITEM, //ショップで売っているアイテム

    UPCHECK, //パラメータの変動
    CUPCHECK,
    ADDCHARA,   //(キャラ番号)のキャラクタを追加
    ADDSPCHARA, //(キャラ番号)のSPキャラクタを追加（フラグ0を1にして作成）
    ADDDEFCHARA,
    ADDVOIDCHARA, //変数に何の設定のないキャラを作成
    DELCHARA,     //(キャラ登録番号)のキャラクタを削除。
    GETCHARA,
    FINDCHARA,
    FINDLASTCHARA,
    FINDCHARADATA,

    GETNUM,
    GETEXPLV,
    GETPALAMLV,
    GETCONFIGS,
    GETCONFIG,

    PUTFORM, //@SAVEINFO関数でのみ使用可能。PRINTFORMと同様の書式でセーブデータに概要をつける。
    QUIT,    //ゲームを終了
    OUTPUTLOG,

    BEGIN, //システム関数の実行。実行するとCALLの呼び出し元などを忘れてしまう。

    SAVEGAME, //セーブ画面を呼ぶ。ショップのみ。
    LOADGAME, //

    SIF, //一行のみIF
    IF,
    ELSEIF,
    ELSE,
    ENDIF,

    REPEAT, //RENDまで繰り返し。繰り返した回数がCOUNTへ。ネスト不可。
    REND,
    CONTINUE, //REPEATに戻る
    BREAK,    //RENDの次の行まで

    GOTO, //$ラベルへジャンプ

    JUMP, //関数に移動
    CALLEVENT,
    CALL,       //関数に移動。移動元を記憶し、RETURNで帰る。
    RETURNFORM, //__FORM_STR__,//関数の終了。RESULTに整数を格納可能。省略した場合、０。(次の@～～がRETURNと見なされる。)
    RETURNF,
    RETURN, //__INT_EXPRESSION__,//関数の終了。RESULTに整数を格納可能。省略した場合、０。(次の@～～がRETURNと見なされる。)
    RESTART, //関数の再開。関数の最初に戻る。

    STRLEN,
    STRLENS, //
    STRLENFORM,
    STRLENU,
    STRLENSU,
    STRLENFORMU,
    UNICODE,

    SWAPCHARA,
    COPYCHARA,
    ADDCOPYCHARA,
    VARSIZE, //動作が違うので__METHOD__化できない
    SPLIT,

    SAVEDATA,
    LOADDATA,
    CHKDATA,
    DELDATA,
    GETTIMES,
    GETTIME, //2つに代入する必要があるので__METHOD__化できない
    SAVETEXT,
    LOADTEXT,

    TRYJUMP,
    TRYCALL,
    TRYGOTO,
    JUMPFORM,
    CALLFORM,
    GOTOFORM,
    TRYJUMPFORM,
    TRYCALLFORM,
    TRYGOTOFORM,
    CALLTRAIN,
    STOPCALLTRAIN,
    CATCH,
    ENDCATCH,
    TRYCJUMP,
    TRYCCALL,
    TRYCGOTO,
    TRYCJUMPFORM,
    TRYCCALLFORM,
    TRYCGOTOFORM,
    TRYCALLLIST,
    TRYJUMPLIST,
    TRYGOTOLIST,
    CALLF,
    CALLFORMF,

    GETCOLOR,
    GETDEFCOLOR,
    GETBGCOLOR,
    GETDEFBGCOLOR,
    GETFOCUSCOLOR,

    SETCOLOR,
    SETCOLORBYNAME,
    RESETCOLOR,
    SETBGCOLOR,
    SETBGCOLORBYNAME,
    RESETBGCOLOR,
    FONTBOLD,
    FONTITALIC,
    FONTREGULAR,
    SORTCHARA,
    FONTSTYLE,
    ALIGNMENT,
    CUSTOMDRAWLINE,
    DRAWLINEFORM,
    CLEARTEXTBOX,

    GETFONT,
    CHKFONT,
    SETFONT,

    FOR,
    NEXT,
    WHILE,
    WEND,

    MIN,
    MAX,
    LIMIT,
    LOG,
    LOG10,
    ABS,
    SQRT,
    SIGN,
    POWER,
    SAVEGLOBAL,
    LOADGLOBAL,
    SWAP,

    RESETDATA,
    RESETGLOBAL,

    RANDOMIZE,
    DUMPRAND,
    INITRAND,

    REDRAW,
    DOTRAIN,

    SELECTCASE,
    CASE,
    CASEELSE,
    ENDSELECT,

    DO,
    LOOP,

    ENDDATA,
    DATALIST,
    ENDLIST,
    STRDATA,
    DATAFORM,
    DATA,

    PRINTCPERLINE, //よく考えたら引数の仕様違うや

    GETBIT,
    SETBIT,
    CLEARBIT,
    INVERTBIT,
    DELALLCHARA,
    PICKUPCHARA,

    CVARSET,
    VARSET,

    RESET_STAIN,

    SAVENOS, //引数の仕様が違うので(ry

    FORCEKANA,

    SKIPDISP,
    NOSKIP,
    ENDNOSKIP,

    ARRAYSHIFT,
    ARRAYREMOVE,
    ARRAYMOVE,
    ARRAYSORT,
    ARRAYMSORT,
    ARRAYCOPY,

    REPLACE,
    ESCAPE,
    ENCODETOUNI,
    SUBSTRINGU,
    SUBSTRING,

    DEBUGPRINT,
    DEBUGPRINTL,
    DEBUGPRINTFORM,
    DEBUGPRINTFORML,
    DEBUGCLEAR,
    ASSERT,
    THROW,

    SAVEVAR,
    LOADVAR,
    CHKVARDATA,
    SAVECHARA,
    LOADCHARA,
    CHKCHARADATA,
    REFBYNAME,
    REF,

    HTML_PRINT,
    HTML_TAGSPLIT,

    TOOLTIP_SETCOLOR,
    TOOLTIP_SETDELAY,
    TOOLTIP_SETDURATION,

    PRINT_IMG,
    PRINT_RECT,
    PRINT_SPACE,

    PRINTBUTTONLC,
    PRINTBUTTONC,
    PRINTBUTTON,
    PRINTPLAINFORM,
    PRINTPLAIN,
    PRINT, //文字を表示する

    INPUTMOUSEKEY,

    INRANGE,
    LINEISEMPTY,
    GROUPMATCH,
    NOSAMES,
    ALLSAMES,
    ISSKIP,
    MOUSESKIP,
    MESSKIP,
    CONVERT,
    FINDELEMENT,
    FINDLASTELEMENT,

    STRCOUNT,
    STRFIND,
    STRFINDU,
    STRJOIN,
    BARSTR,
    MONEYSTR,
    TOUPPER,
    TOLOWER,
    TOHALF,
    TOFULL,
    ISNUMERIC,

    GETMILLISECOND,
    GETSECOND,
    CURRENTALIGN,
    CURRENTREDRAW,

    RAND,
    FIND_CHARADATA,
    EXISTCSV,

    GDISPOSE,
    GCLEAR,
    GCREATED,
    GCREATE,
    GDRAWGWITHMASK,
    GDRAWG,
    GDRAWSPRITE,
    GSETCOLOR,
    GSETBRUSH,
    GSETFONT,
    GSETPEN,
    GWIDTH,
    GHEIGHT,
    GGETCOLOR,
    GSAVE,
    GLOAD,
    SPRITECREATED,
    SPRITECREATE,
    SPRITEWIDTH,
    SPRITEHEIGHT,
    SPRITEPOSX,
    SPRITEPOSY,
    SPRITESETPOS,
    SPRITEMOVE,
    SPRITEDISPOSE,

    CSVNAME,
    CSVCALLNAME,
    CSVMASTERNAME,
    CSVNICKNAME,
    CSVBASE,
    CSVCSTR,
    CSVABL,
    CSVTALENT,
    CSVMARK,
    CSVEXP,
    CSVEX,
    CSVRELATION,
    CSVJUEL,
    CSVEQUIP,
    CSVCFLAG,
}
