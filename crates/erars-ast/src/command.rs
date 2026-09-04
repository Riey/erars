use serde::{Deserialize, Serialize};
use strum::{Display, EnumString, IntoStaticStr};

#[derive(
    Display, Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, IntoStaticStr, EnumString,
)]
#[strum(serialize_all = "UPPERCASE")]
#[strum(use_phf)]
#[serde(rename_all = "UPPERCASE")]
#[allow(non_camel_case_types)]
#[repr(u32)]
pub enum BuiltinMethod {
    ToStr = 0,
    ToInt,
    Limit,
    Min,
    Max,
    Power,
    Sqrt,
    Abs,
    Sign,
    Log,
    Log10,
    InRange,
    LineIsEmpty,
    GroupMatch,
    NoSames,
    AllSames,
    // No free form
    SumArray,
    // No free form
    SumCArray,
    IsSkip,
    MouseSkip,
    MesSkip,
    Convert,
    FindElement,
    FindLastElement,
    // No free form
    Match,
    // No free form
    CMatch,
    // No free form
    MaxArray,
    // No free form
    MinArray,
    // No free form
    MaxCArray,
    // No free form
    MinCArray,
    VarSize,

    Escape = 40,
    Replace,
    StrLenS,
    StrLenSU,
    StrCount,
    SubString,
    SubStringU,
    StrFind,
    StrFindU,
    StrJoin,
    BarStr,
    MoneyStr,
    Unicode,
    // different from command
    EncodeToUni,
    ToUpper,
    ToLower,
    ToHalf,
    ToFull,
    IsNumeric,

    GetExpLv = 60,
    GetPalamLv,
    GetColor,
    GetDefColor,
    GetBgColor,
    GetDefBgColor,
    GetFocusColor,
    GetStyle,
    GetFont,
    GetChara,
    GetBit,
    GetTime,
    GetTimeS,
    GetMillisecond,
    GetSecond,
    GetNum,
    CurrentAlign,
    CurrentRedraw,
    ChkFont,

    Rand = 90,
    ChkData,
    ChkCharaData,
    FindChara,
    #[strum(serialize = "FIND_CHARADATA")]
    FindCharaData,
    ExistCsv,
    SaveNos,
    GetConfig,
    GetConfigS,
    PrintCPerLine,

    CsvName = 100,
    CsvCallName,
    CsvMasterName,
    CsvNickName,
    CsvBase,
    CsvCstr,
    CsvAbl,
    CsvTalent,
    CsvMark,
    CsvExp,
    CsvEx,
    CsvRelation,
    CsvJuel,
    CsvEquip,
    CsvCflag,

    SpriteCreated = 200,
    SpriteCreate,
    SpriteWidth,
    SpriteHeight,
    SpritePosX,
    SpritePosY,
    SpriteSetPos,
    SpriteMove,
    SpriteDispose,

    GCreated = 250,
    GCreate,
    GDispose,
    GClear,
    GWidth,
    GHeight,
    GGetColor,
    GSetColor,
    GSetBrush,
    GSetPen,
    GSetFont,
    GDrawG,
    GDrawGWithMask,
    GDrawSprite,
    GSave,
    GLoad,

    /// Emuera `FINDLASTCHARA` — like `FINDCHARA` but scanning backwards.
    FindLastChara,

    /// Emuera `SAVETEXT(text, fileNo)` — write `sav/txtNN.txt`, 1 on success.
    SaveText,
    /// Emuera `LOADTEXT(fileNo)` — read `sav/txtNN.txt`, `""` when missing.
    LoadText,
    /// Emuera `CHKVARDATA(name)` — inspect a `SAVEVAR` file.
    ChkVarData,

    /// Emuera `ARRAYMSORT(key, arrays...)` — sort the leading run of `key` and
    /// permute every listed array by that same order. Method-only in Emuera
    /// (`Creator.cs:112`); the line-head form is `BuiltinCommand::ArrayMSort`.
    ArrayMSort,

    /// Emuera `GCREATEFROMFILE(id, filename, {isRelative})` — create bitmap
    /// `id` by decoding an image file (`Creator.cs:181`,
    /// `Creator.Method.cs:5903-5962`). 0 when `id` already exists, the file is
    /// missing, it does not decode, or it exceeds `MAX_IMAGESIZE`.
    GCreateFromFile,
    /// Emuera `GDRAWTEXT(id, text, {x, y})` — draw `text` into bitmap `id`
    /// with its `GSETFONT` font (`Creator.cs:314`,
    /// `Creator.Method.cs:5497-5566`).
    GDrawText,
    /// Emuera `SPRITEANIMECREATE(name, width, height)` — create an empty
    /// animated sprite (`Creator.cs:211`, `Creator.Method.cs:6395-6432`).
    SpriteAnimeCreate,
    /// Emuera
    /// `SPRITEANIMEADDFRAME(name, gid, x, y, w, h, offsetX, offsetY, delay)` —
    /// append one frame to an animated sprite
    /// (`Creator.Method.cs:6434-6472`).
    SpriteAnimeAddFrame,
    /// Emuera `SETANIMETIMER(ms)` — set the console's sprite-animation redraw
    /// interval (`Creator.cs:213`, `Creator.Method.cs:6800-6816`).
    SetAnimeTimer,
    /// Emuera `CLIENTWIDTH()` — the drawing area's width in pixels
    /// (`Creator.cs:160`, `Creator.Method.cs:5844-5863`,
    /// `EmueraConsole.cs:237`).
    ClientWidth,
    /// Emuera `CLIENTHEIGHT()` — the drawing area's height in pixels
    /// (`Creator.cs:161`, `EmueraConsole.cs:238`).
    ClientHeight,

    /// Emuera `HTML_ESCAPE(str)` — `& > < " '` to their character references
    /// (`Creator.cs:148`, `Creator.Method.cs:5093-5105`,
    /// `HtmlManager.Escape` `GameView/HtmlManager.cs:640-662`).
    #[strum(serialize = "HTML_ESCAPE")]
    HtmlEscape,
    /// Emuera `HTML_TOPLAINTEXT(str)` — strip every tag, then unescape
    /// (`Creator.cs:147`, `Creator.Method.cs:5080-5091`,
    /// `HtmlManager.Html2PlainText` `GameView/HtmlManager.cs:634-638`).
    #[strum(serialize = "HTML_TOPLAINTEXT")]
    HtmlToPlainText,
    /// Emuera `HTML_GETPRINTEDSTR([lineNo])` — an already-printed logical
    /// line, counted back from the last, rendered back to HTML
    /// (`Creator.cs:145`, `Creator.Method.cs:5024-5060`,
    /// `EmueraConsole.Print.cs:752-771`).
    #[strum(serialize = "HTML_GETPRINTEDSTR")]
    HtmlGetPrintedStr,
    /// Emuera `HTML_POPPRINTINGSTR()` — take the line still being built out
    /// of the console and return it as HTML (`Creator.cs:146`,
    /// `Creator.Method.cs:5062-5078`, `EmueraConsole.Print.cs:773-780`).
    #[strum(serialize = "HTML_POPPRINTINGSTR")]
    HtmlPopPrintingStr,

    /// Emuera `CHARATU(str, pos)` — the character at `pos`, or `""` out of
    /// range (`Creator.cs:136`, `Creator.Method.cs:4804-4820`).
    CharAtU,

    /// Emuera `GETKEY(keycode)` — is that virtual key down right now
    /// (`Creator.cs:163`, `Creator.Method.cs:6710-6735`)?
    GetKey,
    /// Emuera `GETKEYTRIGGERED(keycode)` — is it down *and* newly pressed
    /// since the last query (`Creator.cs:164`, same method)?
    GetKeyTriggered,
    /// Emuera `MOUSEX()` — cursor X in client pixels (`Creator.cs:165`,
    /// `Creator.Method.cs:6737-6755`, `EmueraConsole.cs:1981-1990`).
    MouseX,
    /// Emuera `MOUSEY()` — cursor Y, measured from the bottom edge.
    MouseY,
    /// Emuera `ISACTIVE()` — is the console window active
    /// (`Creator.cs:171`, `Creator.Method.cs:6783-6797`)?
    IsActive,

    /// Emuera `GETLINESTR(str)` — the filler string `CUSTOMDRAWLINE` would
    /// print for that argument, i.e. it repeated to the drawable width
    /// (`Creator.cs:137`, `Creator.Method.cs:4822-4838`,
    /// `EmueraConsole.Print.cs:632-649`).
    GetLineStr,
    /// Emuera `STRFORM(str)` — expand the argument as a `PRINTFORM` string at
    /// run time (`Creator.cs:138`, `Creator.Method.cs:4840-4871`).
    StrForm,
    /// Emuera `COLOR_FROMRGB(r, g, b)` — `0xRRGGBB`; every component must be
    /// 0–255 (`Creator.cs:56`, `Creator.Method.cs:2693-2718`).
    #[strum(serialize = "COLOR_FROMRGB")]
    ColorFromRgb,
    /// Emuera `COLOR_FROMNAME(name)` — a .NET colour name to `0xRRGGBB`, or
    /// `-1` when the name is unknown (`Creator.cs:55`,
    /// `Creator.Method.cs:2666-2691`).
    #[strum(serialize = "COLOR_FROMNAME")]
    ColorFromName,
    /// Emuera `PRINTCLENGTH()` — the configured `PRINTC` field width, not the
    /// column count `PRINTCPERLINE` returns (`Creator.cs:68`,
    /// `Creator.Method.cs:2810-2822`).
    PrintCLength,
    /// Emuera `CBRT(x)` — cube root of a non-negative value
    /// (`Creator.cs:82`, `Creator.Method.cs:3086-3101`).
    Cbrt,
    /// Emuera `EXPONENT(x)` — `e**x`, truncated (`Creator.cs:85`,
    /// `Creator.Method.cs:3148-3170`).
    Exponent,
    /// Emuera `GETSPCHARA(no)` — the registration number of the SP character
    /// with that number, or `-1` (`Creator.cs:19`,
    /// `Creator.Method.cs:2010-2026`).
    GetSpChara,

    /// Emuera `CBGSETG(id, x, y, zdepth)` — put a whole `GCREATE` bitmap on
    /// the console-background plane (`Creator.cs:197`,
    /// `Creator.Method.cs:6553-6583`).
    CbgSetG,
    /// Emuera `CBGSETSPRITE(name, x, y, zdepth)` — the same for a sprite
    /// (`Creator.cs:198`, `Creator.Method.cs:6615-6644`).
    CbgSetSprite,
    /// Emuera `CBGCLEAR()` — empty the plane and drop its button map
    /// (`Creator.cs:199`, `Creator.Method.cs:6479-6493`).
    CbgClear,
    /// Emuera `CBGCLEARBUTTON()` — drop every button entry and the map
    /// (`Creator.cs:201`, `Creator.Method.cs:6518-6536`).
    CbgClearButton,
    /// Emuera `CBGREMOVERANGE(zmin, zmax)` — drop the entries in an inclusive
    /// depth range (`Creator.cs:202`, `Creator.Method.cs:6496-6517`).
    CbgRemoveRange,
    /// Emuera `CBGREMOVEBMAP()` — drop the button map alone
    /// (`Creator.cs:203`, `Creator.Method.cs:6537-6552`).
    CbgRemoveBmap,
    /// Emuera `CBGSETBMAPG(id)` — the bitmap whose pixel colours are the
    /// plane's button values (`Creator.cs:204`,
    /// `Creator.Method.cs:6585-6612`).
    CbgSetBmapG,
    /// Emuera `CBGSETBUTTONSPRITE(value, src, srcb, x, y, zdepth[, tooltip])`
    /// — a plane entry that swaps sprite while the button map reports its
    /// value under the cursor (`Creator.cs:205`,
    /// `Creator.Method.cs:6647-6707`).
    CbgSetButtonSprite,
    /// Emuera `GFILLRECTANGLE(id, x, y, width, height)` — fill a rectangle
    /// with the bitmap's stored brush (`Creator.cs:184`,
    /// `Creator.Method.cs:6146-6169`, `GraphicsImage.cs:188-203`).
    GFillRectangle,
    /// Emuera `SPRITEGETCOLOR(name, x, y)` — the pixel under a sprite-local
    /// point, as `0xAARRGGBB`, or `-1` (`Creator.cs:178`,
    /// `Creator.Method.cs:5818-5842`, `CroppedImage.cs:78-89`).
    SpriteGetColor,
}

#[derive(Display, Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, IntoStaticStr)]
#[strum(serialize_all = "UPPERCASE")]
#[strum(use_phf)]
#[serde(rename_all = "UPPERCASE")]
#[repr(u32)]
#[allow(non_camel_case_types)]
pub enum BuiltinCommand {
    Return = 0,
    ReturnF,
    Restart,
    Quit,
    Throw,
    DoTrain,
    CallTrain,

    Input = 10,
    InputS,
    TInput,
    TInputS,
    OneInput,
    OneInputS,
    TOneInput,
    TOneInputS,
    ForceWait,
    Wait,
    WaitAnykey,
    Twait,

    ResetStain = 25,
    ResetData,
    SaveData,
    LoadData,
    DelData,
    SaveGame,
    LoadGame,
    SaveGlobal,
    LoadGlobal,
    PutForm,

    UpCheck = 40,
    CUpCheck,
    SkipDisp,
    NoSkip,
    EndNoSkip,
    Swap,
    SetBit,
    ClearBit,
    InvertBit,
    Varset,
    CVarset,
    Split,
    ForceKana,
    // different from method
    EncodeToUni,
    // different from method
    GetTime,
    // different from method
    Power,

    Redraw = 60,
    DrawLine,
    CustomDrawLine,
    ClearLine,
    SetColor,
    SetBgColor,
    ResetColor,
    ResetBgColor,
    SetColorByName,
    SetBgColorByName,

    FontBold,
    FontItalic,
    FontRegular,
    FontStyle,
    SetFont,

    BarL = 75,

    Bar = 89,

    SaveChara = 90,
    LoadChara,
    AddChara,
    AddDefChara,
    AddCopyChara,
    CopyChara,
    DelChara,
    SwapChara,
    SortChara,
    PickupChara,

    Randomize = 110,
    DumpRand,
    InitRand,

    ArrayShift = 200,
    ArrayRemove,
    ArraySort,
    ArrayCopy,
    ArrayMove,

    #[strum(serialize = "HTML_PRINT")]
    HtmlPrint = 300,

    // 301 is deliberately left unassigned. It was `SpriteCreate`, which is a
    // method in Emuera (`SpriteCreateMethod`, `GameData/Function/Creator.cs:194`)
    // and has moved to `BuiltinMethod`. Reusing 301 would make a stale
    // `game.era` decode an old `SPRITECREATE` as a different command, and
    // discriminants here are never renumbered.
    ArrayMSort = 302,
    DebugClear,
    ClearTextBox,
    PrintSpace,
    PrintRect,
    OutputLog,
    Await,
    TooltipSetColor,
    TooltipSetDelay,
    TooltipSetDuration,
    PrintImg,

    /// Emuera `ADDSPCHARA` — add the SP template with this number.
    AddSpChara,
    /// Emuera `ADDVOIDCHARA` — add a character with no template at all.
    AddVoidChara,
    /// Emuera `DELALLCHARA` — drop every character.
    DelAllChara,

    /// Emuera `SAVEVAR name, message, var...` — whole arrays to `var_NAME.dat`.
    SaveVar,
    /// Emuera `LOADVAR name` — restore them, `RESULT` = 1 on success.
    LoadVar,
    /// Emuera `RESETGLOBAL` — every global variable back to its default.
    ResetGlobal,
    /// Emuera `SAVENOS [var]` — the configured save-slot count.
    SaveNos,

    /// Emuera `ASSERT expr` — raise an error when `expr` evaluates to 0.
    Assert,
    /// Emuera `STOPCALLTRAIN` — abandon the rest of a `CALLTRAIN` sequence.
    StopCallTrain,
    /// Emuera `REF refvar, srcvar` — point a `#DIM REF` variable at `srcvar`.
    Ref,
    /// Emuera `REFBYNAME refvar, name` — same, with the target named at runtime.
    RefByName,

    /// Emuera `PRINT_ABL [chara]` — `ABLNAME` + `LV` + value for every
    /// non-zero, named ability of one character.
    PrintAbl,
    /// Emuera `PRINT_TALENT [chara]` — `[TALENTNAME]` per non-zero talent.
    PrintTalent,
    /// Emuera `PRINT_MARK [chara]` — like `PRINT_ABL` over `MARK`/`MARKNAME`.
    PrintMark,
    /// Emuera `PRINT_EXP [chara]` — `EXPNAME` + value, without `LV`.
    PrintExp,
    /// Emuera `PRINT_PALAM [chara]` — a `PALAMLV`-graded bar per parameter,
    /// laid out in `PRINTCPERLINE` columns.
    PrintPalam,
    /// Emuera `PRINT_ITEM` — the owned-item list from `ITEM`/`ITEMNAME`.
    PrintItem,
    /// Emuera `PRINT_SHOPITEM` — items on sale (`ITEMSALES`) with `ITEMPRICE`.
    PrintShopItem,

    /// Emuera `HTML_TAGSPLIT html, [strs], [count]` — split into text runs
    /// and `<...>` tags.
    HtmlTagSplit,

    /// Emuera `INPUTMOUSEKEY [time]` — wait for one raw mouse or key event
    /// and report it through `RESULT:0`-`RESULT:5`.
    InputMouseKey,

    /// Emuera `PRINTCPERLINE [var]` — store the configured `PRINTC` column
    /// count into a variable. `SP_GETINT`, so the default target is
    /// `RESULT:0`, exactly like `SAVENOS`.
    PrintCPerLine,

    /// `.NET版` fork `HTML_PRINT_ISLAND value, {layer = 0}` — draw an HTML
    /// fragment as a free-floating "island" that is not attached to any
    /// console line, so it survives scrolling until cleared, and is painted
    /// above every `PRINT` (higher `layer` = further front).
    HtmlPrintIsland,
    /// `.NET版` fork `HTML_PRINT_ISLAND_CLEAR {layer}` — discard the islands
    /// of one layer, or every island when the argument is omitted.
    HtmlPrintIslandClear,
    /// `.NET版` fork `MATCHALL var, {index,} value, {start, end}` — count the
    /// elements of `var` equal to `value` into `RESULT:0` and store the
    /// matching indices in `RESULT:1`... `end` is exclusive.
    MatchAll,
}
