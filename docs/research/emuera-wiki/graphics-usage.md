# Graphics / input usage map — eraTHYMKR vs eraMegaten (for the image/input implementer)

Research-only map of how the graphics/input command family is actually used, so the
implementing agent builds for the corpus, not the wiki. Generated 2026-09-03 from the two
checkouts. Counts are grep-derived (commands listed). **eraTHYMKR is cold: every subject
below has 0 sites there**, so eraMegaten is the only source of truth for usage shapes.

**Lead counts (eraMegaten/Data, ERB only):**
- **Genuinely-missing family (absent from erars — the real assigned-elsewhere work)**: all 8 `CBG*`, `SPRITEGETCOLOR`, `GETKEYTRIGGERED`, `ISACTIVE`, `GETKEY`, `MOUSEX`/`MOUSEY`, `GFILLRECTANGLE`. Of these only `GETKEY` (22) and `MOUSEX`/`MOUSEY` (3) have real usage; `GFILLRECTANGLE` 1 (dead-comment); **all 8 `CBG*`, `SPRITEGETCOLOR`, `GETKEYTRIGGERED`, `ISACTIVE` have 0 sites in either game** — implement from WebEmuera spec only, no corpus to test against.
- **The SPRITE\* family is NOT missing.** erars already implements `SPRITEWIDTH`/`SPRITEHEIGHT`, `SPRITEANIMECREATE`, `SPRITEANIMEADDFRAME`, `SETANIMETIMER`, and the whole `SPRITE*` helper family (§3) with full executor arms. The corpus's SPRITEANIME* usage (15/6) exercises code that already exists — **context, not work**. (Earlier drafts here said erars "lacks SPRITE*" — wrong; that claim is corrected and the phantom `SPRITEANIMETIMER` token dropped: WebEmuera knows only `SETANIMETIMER`.)
- **Already-implemented context (not work)**: `HTML_PRINT_ISLAND` (40), `HTML_PRINT_ISLAND_CLEAR` (25), `GCREATEFROMFILE` (15), `GDRAWTEXT` (5), the whole `G*` family.

---

# 1. Usage counts, per subject, both games

Counts: `files` = files containing the token; `sites` = token occurrences; `lh` = line-head
statement form (`^\s*TOKEN\b`); `inexpr` = `sites − lh` (in-expression / method use).
Commands: `grep -rl/-ro/-rE '^\s*TOKEN\b' --include='*.ERB' --include='*.erb' <corpus>`.

| subject | meg files | meg sites | meg lh | meg inexpr | thy (all) | erars status |
|---|---|---|---|---|---|---|
| CBGSETG / CBGSETSPRITE / CBGSETBMAPG / CBGSETBUTTONSPRITE / CBGCLEAR / CBGCLEARBUTTON / CBGREMOVEBMAP / CBGREMOVERANGE | **0** | **0** | 0 | 0 | 0 | **absent** (assigned-elsewhere) |
| GFILLRECTANGLE | 1 | 1 | 0 | 1 | 0 | absent (site is a comment → dead) |
| SPRITEGETCOLOR | 0 | 0 | 0 | 0 | 0 | absent |
| GETKEY | 7 | 22 | 0 | 22 | 0 | **absent** (assigned-elsewhere) |
| GETKEYTRIGGERED | 0 | 0 | 0 | 0 | 0 | absent |
| MOUSEX | 3 | 3 | 0 | 3 | 0 | absent |
| MOUSEY | 3 | 3 | 0 | 3 | 0 | absent |
| ISACTIVE | 0 | 0 | 0 | 0 | 0 | absent |
| SPRITEANIMECREATE | 4 | 6 | 5 | 1 | 0 | **present** (`BuiltinMethod::SpriteAnimeCreate`) |
| SPRITEANIMEADDFRAME | 4 | 15 | 14 | 1 | 0 | **present** (`BuiltinMethod::SpriteAnimeAddFrame`) |
| SETANIMETIMER | 0 | 0 | 0 | 0 | 0 | **present** (`BuiltinMethod::SetAnimeTimer`) |
| GCREATEFROMFILE | 8 | 15 | 2 | 13 | 0 | **present** (erars `BuiltinMethod::GCreateFromFile`) |
| GDRAWTEXT | 3 | 5 | 2 | 3 | 0 | **present** (`BuiltinMethod::GDrawText`) |
| HTML_PRINT_ISLAND | 15 | 40 | 40 | 0 | 0 | **present** (`BuiltinCommand::HtmlPrintIsland`) |
| HTML_PRINT_ISLAND_CLEAR | 15 | 25 | 25 | 0 | 0 | **present** (`BuiltinCommand::HtmlPrintIslandClear`) |

---

# 2. Representative call sites (argument shapes actually used), reachability

`file:line` = `<eraMegaten base>/Data/<path>`. "live" = reachable (not `;`-commented, in
bundled/event/TITLE/INPUT functions); "dead" = commented out.

## 2.1 GETKEY — 22 occ / 11 lines / 7 files; 10 lines live, 1 commented
All in-expression, in `IF`/`SIF`/ternary conditions; args are hex virtual-key codes.
- `IF GETKEY(0x12)|| GETKEY(0x05)` — `ERB/RPG/アイテム関連/装備品/EQUIPMENT.ERB:1930` (live; `0x12` Alt, `0x05` left-shift)
- `IF (GETKEY(0x12) || GETKEY(0x05)) && !HAVE_INSTALLSOFT:(SELL_LIST:RESULT)` — `ERB/ＳＨＯＰ関連/121_COMPSMITH.ERB:388` (live; the line above it :387 is the commented twin)
- `INPUTMOUSEKEY GETKEY(2) || GETKEY(27) ? 5 # 100` — `ERB/RPG/個別イベント/EVENT_K5371_マルフーシャ_個別イベント.ERB:6383` (live; `0x02` right-button, `0x1B` Escape)
- `IF RESULT == -2 || (RESULT == -1 && GETKEY(0x10))` — `ERB/ＳＨＯＰ関連/RAG_SHOP/RAG_SHOP奴隷交換.ERB:232` (live; `0x10` Shift)
- vkey values seen: `0x02, 0x05, 0x10, 0x12, 0x1D, 0x1B, 0x27`. All hex, always in a boolean test. **Reachable** — shop/equipment/dungeon flows.

## 2.2 SPRITEANIMEADDFRAME — 15 occ / 4 files; 14 live, 1 commented — **already implemented (context)**
9 args: `name, gID, x, y, w, h, ox, oy, delay`. Mostly line-head command form (erars accepts any method at line head).
- `SPRITEANIMEADDFRAME "＠時計回り" , L_GID+LOCAL , 0 , 0 , 16 , 16 , 0 , 0 , 125` — `ERB/関数/汎用組み込み関数/画像処理/主人マップアイコン.ERB:53` (live; full-width `＠`-ANIME sprite names, frame 16×16, delay 125ms)
- `SPRITEANIMEADDFRAME "＠中庸" , L_GID+0 , 0 , 0 , 16 , 16 , 0 , 0 , 125` — same file :62 (live)
- `SPRITEANIMEADDFRAME "BLINK_FILTER" , L_GID+LCOUNT , 0 , 0 , 100 , 100 , 0 , 0 , 100` — `ERB/関数/汎用組み込み関数/DISPLAY/DIV_DISPLAY_TEST.ERB:301` (**commented → dead**; the whole DIV_DISPLAY_TEST file's graphics are commented out)
- Companion `SPRITEANIMECREATE` before the frames: `SPRITEANIMECREATE "＠時計回り" , 16 , 16` — `主人マップアイコン.ERB:51`; `SPRITEANIMECREATE L_ANIMENAME, SPRITEWIDTH(L_SPRITE), SPRITEHEIGHT(L_SPRITE)` — `点滅アニメスプライト作成.ERB:31` (the `SPRITEWIDTH`/`SPRITEHEIGHT` it calls are **also already in erars** — `BuiltinMethod::SpriteWidth/SpriteHeight`).

## 2.3 SPRITEANIMECREATE — 6 occ / 4 files; 5 live, 1 commented — **already implemented (context)**
`name, width, height` (anime cell size).
- `SPRITEANIMECREATE L_ANIMENAME, SPRITEWIDTH(L_SPRITE), SPRITEHEIGHT(L_SPRITE)` — `点滅アニメスプライト作成.ERB:31` (live; `SPRITEWIDTH`/`SPRITEHEIGHT` are erars `BuiltinMethod::SpriteWidth/SpriteHeight`)
- `SPRITEANIMECREATE "BLINK_FILTER" , 100 , 100` — `画像フィルタ.ERB:86` (live)
- `SPRITEANIMECREATE "BLINK_FILTER" , 100 , 100` — `DIV_DISPLAY_TEST.ERB:294` (**commented → dead**)

## 2.4 GCREATEFROMFILE — 15 occ / 8 files; all live — context (already in erars)
Both command form (`GCREATEFROMFILE <gid> , <path>`) and in-expression `IF !GCREATEFROMFILE(...)`/`SIF !…`.
- `GCREATEFROMFILE TEMP_GID_START + 1, GET_ICON_FOLDER_NAME(NO:L_CHARA, "キャラ画像") + "\\" + "華恋_FACE_4" + ".png"` — `ERB/キャラクター処理/CharaERB/Chara5313_愛城華恋.ERB:81` (command form; path = icon-folder helper + `\\` + name + `.png`)
- `SIF !GCREATEFROMFILE(GID, L_ADDRESS + "_" + TOSTR(LCOUNT, "00") + ".png")` — `ERB/RPG/スキル関係/90_CSTR専用スキル/外部作品/23_溶鉄のマルフーシャ/SKILL_リラックス.ERB:112` (in-expr; CSTR-skill card art)
- `SIF !GCREATEFROMFILE(…) + ".webp")` — `ERB/タイトル表示/PRINT_TITLE.ERB:50` (**webp** — title art)

## 2.5 GDRAWTEXT — 5 occ / 3 files; 2 live, 3 commented — context (already in erars)
- `GDRAWTEXT L_HASH , L_NAME , 0 , 0` — `ERB/関数/汎用組み込み関数/画像処理/文字画像.ERB:30` (live; gID, text, x, y)
- `GDRAWTEXT L_GID+LCOUNT , TOSTR(LCOUNT) + "：" + TOSTR(100-(ABS(LCOUNT-11)*5)) + "％" , 20 , 20` — `画像フィルタ.ERB:92` (live; computed text)
- `;GDRAWTEXT gid , "eraMega en" , 1000 , 25` — `DIV_DISPLAY_TEST.ERB:225` (commented → dead; two more there)

## 2.6 MOUSEX / MOUSEY — 3 occ each, all live
Used as `MOUSEX()`/`MOUSEY()` (no args) in position math for popups.
- `L_XPOS = MAX((MOUSEX()) - GETCONFIG("フォントサイズ"), 0)` — `ERB/関数/汎用組み込み関数/メッセージ/MESSAGE_POPUP.ERB:15` (live)
- `L_YPOS = MIN((MOUSEY()) - GETCONFIG("フォントサイズ") / 2, -L_HEIGHT - GETCONFIG("フォントサイズ") * 2)` — `MESSAGE_POPUP.ERB:16`
- `DEBUGPRINTFORML X:{MOUSEX()*100/18} Y:{MOUSEY()*100/18} RESULTS：%RESULTS%` — `入力関数/INPUT_DIV_MESSAGE.ERB:105`

## 2.7 GFILLRECTANGLE — 1 occ, **commented out → dead**
`;	GFILLRECTANGLE gid+1 , 0 , 0 , 600 , MAX(200 - (200 * (L_割合) / 5000) , 1)` — `ERB/関数/汎用組み込み関数/DISPLAY/DIV_DISPLAY_TEST.ERB:242`. Only ever appears in the dead DIV_DISPLAY_TEST file. Zero live usage.

## 2.8 CBG* / SPRITEGETCOLOR / GETKEYTRIGGERED / ISACTIVE
**0 sites in both games.** No corpus shape to build from.

---

# 3. erars G* / SPRITE* family — context (what the implementer already has, what it must add)

Already in erars (`BuiltinMethod`, `crates/erars-ast/src/command.rs`):
- **G\* family**: `GCreated, GCreate, GDispose, GClear, GWidth, GHeight, GGetColor, GSetColor,
  GSetBrush, GSetPen, GSetFont, GDrawG, GDrawGWithMask, GDrawSprite, GSave, GLoad, GCreateFromFile,
  GDrawText`.
- **SPRITE\* family** (command.rs:122-183, executor arms executor.rs:1227-1308 & :1495-1505):
  `SpriteCreated, SpriteCreate, SpriteWidth, SpriteHeight, SpritePosX, SpritePosY, SpriteSetPos,
  SpriteMove, SpriteDispose, SpriteAnimeCreate, SpriteAnimeAddFrame, SetAnimeTimer`. So
  `SPRITEWIDTH`/`SPRITEHEIGHT` (called in `点滅アニメスプライト作成.ERB:31`) and
  `SPRITEANIMECREATE`/`SPRITEANIMEADDFRAME`/`SETANIMETIMER` (which the corpus calls at line head)
  are **all already implemented** — context, not work.
- commands `HtmlPrintIsland, HtmlPrintIslandClear, HtmlTagSplit`.
- *(A prior draft here asserted "No `SPRITE*` or `GETSP*` methods exist" — that was wrong, and the
  phantom token `SPRITEANIMETIMER` in §1/§2.8 came from it. WebEmuera's command is `SETANIMETIMER`
  (`Creator.cs:213`), which erars implements as `BuiltinMethod::SetAnimeTimer`.)*

**Genuinely missing (absent from erars — the assigned-elsewhere work)**: all 8 `CBG*`, `GFILLRECTANGLE`,
`SPRITEGETCOLOR`, `GETKEY`, `GETKEYTRIGGERED`, `MOUSEX`, `MOUSEY`, `ISACTIVE`. Of these the corpus
exercises only `GETKEY` (22), `MOUSEX`/`MOUSEY` (3); `GFILLRECTANGLE` 1 (dead-comment); the rest are 0-site.

---

# 4. Question 1 — resource inventory

**Answer: there is no resource layer in either checkout to build against — it is 100% external.**

- **No `resources/` folder and no resource-definition CSV in either repo.** eraMegaten `Data/` contains only `logs/ CSV/ sav/ ERB/` plus `game.era` (an identifier/name dump, *not* a resource list; its single `SKILL_ICON_EX_02_HUUJIN_A_ACT` hit is a plain text occurrence). `Data/resources/タイトル画像/` (the gitignored art bucket) is **absent**. eraTHYMKR has no images at all.
- **Image files present in checkout: meg 1 (a stray `ダメージ解析機能について.PNG` doc), thy 0.**
- **Referenced from code** (`grep`):
  - 74 unique `<img src='…'>` sprite names across 86 occurrences — **71 plain-form** (`SKILL_ICON_EX_02_HUUJIN_A_ACT`, `nekoneko`, `BLINK_FILTER`, `RED_FILTER`, `CLEAR_FILTER`, `SELECT_FILTER`, `GLAYOUT_FILTER`, `MESSAGEFADE_FILTER`, `TEMP_USERICON_{GID}`, …) and **3 ANIME-form** (full-width `＠時計回り`, `＠中庸`, `＠反時計回り`).
  - **19 file-path literals**: **17 `.png`** (`01_画像取り込み`, `10_アイコン描画`, `21_アイコンセット`, `華恋_FACE_4.png`, CSTR-card `_NN.png`, …) and **2 `.webp`** (`PRINT_TITLE.ERB:50`). No `.bmp`/`.jpg` anywhere.
  - Formats the engine must load, from code: **png and webp only**.
- **Missing-art quantification: 100% of the referenced art is absent from the checkout** — every one of the 74 sprite names and all 19 file paths point at files/resources that are not present (eraMegaten's `タイトル画像/` and the sprite sheets ship separately / are gitignored). The implementing agent **cannot render-test any sprite**; it can only shape-test (parse `<img src>`/`GCREATEFROMFILE` args and confirm resolution errors match WebEmuera).

---

# 5. Question 2 — HTML `<img>` / `<div>` usage reaching HTML_PRINT

**Answer to the other agent's open question: `<div xpos= ypos=>` with NO width is the single most
common `<div>` shape — 158 of 367 (43%), universal, not a one-off.** Implement it as the baseline;
width/height/background/border/padding are the variable parts layered on top. Same for `<img>`:
`src`-only is the plurality (45/95); `src`+`height`/`width` covers most of the rest.

Method: regex-scanned all ERB for `<img …>`/`<div …>`/`<button …>` opening tags
(`<tag\b[\s\S]*?>`, allow multiline), tallied attribute names and exact attribute-set combinations.
eraTHYMKR: **0 img, 0 div, 0 button** — all HTML is eraMegaten.

## `<img>` (95 tags; 74 unique literal src names)
attributes: `src` 95, `height` 47, `width` 24, `srcb` 3, `ypos` 3, `xpos` 2, `img_size` 2.
combos: `src` 45 · `src,height` 22 · `src,width,height` 19 · `src,srcb,width,height` 3 ·
`src,xpos,ypos` 2 · `src,img_size,height` 2 · `src,width` 1 · `src,width,height,ypos` 1.
(No bare `<img>`: an earlier census showed 2 `[]`-attr rows, but those were regex false positives
inside string literals — `DIV_MESSAGE.ERB:216` `"<img"` comparison and `:733` `"<img).* %ARGS:1%='"`
SPLIT pattern — not markup; re-running the scanner and printing the matched text removed them.
Also ~8 of the 95 are runtime-concatenation tags with a dynamic src, e.g. `<img src='" + LOCALS + "' …>`,
so the 74-name inventory counts literal-name sprites only.)
Representative: `<img src='＠時計回り' width='100' height='100'>` (SYSTEM_DUNGEON, ANIME sprite);
`<img src = '%RESULTS%' xpos='0' ypos = '{LOCAL*(L_HEIGHT/6)}'>` (キャラ・NPC顔アイコン — the xpos/ypos
img shape, 2 sites); `<img src = '%TSTR:…%' height = '\@IMG_SIZE == 1 ? 600 # 400\@'>` (KOJO, ternary).

## `<div>` (367 tags)
attributes: `xpos` 313, `ypos` 288, `width` 82, `height` 76, `padding` 56, `border_width` 58,
`display` 67, `background_color` 49, `border_color` 19, `color` 2, plus typos `yos` 1, `ypps` 4.
combos (top): **`xpos,ypos` 158** · `display,xpos,ypos` 39 · bare 30 · `xpos` 27 ·
`border_width,height,padding,width,xpos,ypos` 13 · `background_color,display,height,width,xpos,ypos` 9 ·
`border_width,height,width,xpos,ypos` 8 · full-box (`background_color,border_color,border_width,display,
height,padding,width,xpos,ypos`) 7 · `height,width,xpos,ypos` 7 · `background_color,border_color,
border_width,padding,xpos,ypos` 7 · … `xpos,ypps` 4.

## `<button>` inside island content (180)
`value` 170, `title` 72 (`SET_BUTTON_TAG` generator).

## Rendering realities the implementer will meet (verbatim examples)
- **Multiline tags** and embedded `{…}`/`%…%` interpolation inside attribute values:
  `<div display = 'absolute-leftbottom' border_width = '3px' border_color = '%TOSTR_HTML(COLOR("p-green"))%'\n  width='5000' height='800' background_color = '%TOSTR_HTML(GETBGCOLOR())%'\n  div padding='50' xpos='{L_XPOS}' ypos='{L_YPOS}'>` — `21_アイコンセット_プレイヤー.ERB:356` (note the stray literal `div` before `padding`, and `{L_XPOS}` interpolation).
- **Space around `=`** is common: `<img src = '%SRC%' height = '600' width = '600'>`; single vs double quotes both used; `@`-strings embed `"` and `\\` (e.g. `GET_ICON_FOLDER_NAME(...) + "\\" + name + ".png"`).
- **`<img>` can be a closing-adjacent inline** ending `…></div>"` (image is a div child / block).
- `<img src='＠…'>` = ANIME sprite; `<img src='SKILL_ICON_…'>` = plain sprite (no file path).
- `html`/div/button attribute **typos are real** (`yos`, `ypps`, `div padding`, `srcb`, `img_size`) — tolerate unknown attributes rather than erroring.
- `HTML_PRINT_ISLAND` is called with an optional layer argument: `HTML_PRINT_ISLAND @"<div…>…</div>" , L_LAYER_NO - 1` (`MESSAGE_POPUP.ERB:35`); `HTML_PRINT_ISLAND_CLEAR L_LAYER_NO` / `L_LAYER_NO - 1` (`MESSAGE_POPUP.ERB:38-39`); bare `HTML_PRINT_ISLAND_CLEAR` also appears (`DIV_MESSAGE_LOG.ERB:100`).

---

## Commands used to produce these numbers
- Site counts: `grep -rl/-ro/-rE '^\s*<TOKEN>\b' --include='*.ERB' --include='*.erb' <corpus>`
- img/div/button tags: python3 `re.finditer(r'<(img|div|button)\b[\s\S]*?>', txt, re.I)` over all ERB, attribute tally via `re.findall(r'([a-zA-Z_]+)\s*=', tag)`.
- sprite names: `<img…src\s*=\s*['"]([^'">]*)['"]>`; file refs: `re.findall(r'"([^"]*\.(png|webp))"', …)` + grep `\.png|\.webp`.
- reachability: `grep -rn '^\s*;\s*<TOKEN>\b'` (commented-out).

Any row with live corpus usage is in §2 with `file:line`; rows you cannot attribute to a reachable
call should be treated as zero-risk to build spec-first.