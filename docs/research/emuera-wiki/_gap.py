#!/usr/bin/env python3
"""Generate docs/research/emuera-wiki/index.md and gap.md (PART 2 + PART 3)."""
import re, os, sys, json
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
import importlib.util
spec = importlib.util.spec_from_file_location("_extract", os.path.join(HERE, "_extract.py"))
e = importlib.util.module_from_spec(spec); spec.loader.exec_module(e)

def ci(n): return n.upper()

# ------------------------------------------------------------------ wiki sets
excom_names, excom_sig = e.excom_instructions()
eramaerb_names = e.eramaerb_instructions()
exmeth_names, exmeth_sig = e.exmeth_functions()
exvar_names, exvar_tbl = e.exvar_variables()
eramavar_names = e.eramavar_variables()
sharp_names, sharp_sig = e.preproc_sharp()
bracket_names = e.preproc_bracket()
config_names = e.config_keys()
debug_names = e.debug_commands_special()
csv_files = e.csv_files()
replace_keys = e.replace_keys()

# ------------------------------------------------------------------ erars sets
erars_inst = e.erars_inst()
_cmds = e.erars_command("crates/erars-ast/src/command.rs")
erars_method = _cmds["method"]
erars_command_enum = _cmds["command"]
erars_var = e.erars_vars()
erars_knownvar = e.erars_known_var()
erars_sharp = e.erars_sharp()
erars_config = e.erars_config()
erars_gamebase = e.erars_gamebase()
erars_chara_csv = e.erars_chara_csv()

inst_ci = set(ci(x) for x in erars_inst)
meth_ci = set(ci(x) for x in erars_method)
var_ci = set(ci(x) for x in erars_var)
sharp_ci = set(ci(x) for x in erars_sharp)
config_ci = set(erars_config)

def fmt_li(items):
    return "\n".join("- " + x for x in items)

def sect(title, wiki, erars_ci, cat_label, flags=None):
    """Compare wiki names vs erars set. Returns markdown block."""
    flags = flags or {}
    w = sorted(set(ci(x) for x in wiki))
    w_raw = {ci(x): x for x in wiki}
    in_wiki_not = sorted(x for x in w if x not in erars_ci)
    in_erars_not = sorted(x for x in erars_ci if x not in w)
    match = sorted(x for x in w if x in erars_ci)
    out = [f"### {title}", ""]
    out.append(f"**in wiki, not in erars** ({len(in_wiki_not)}):")
    out.append(fmt_li(in_wiki_not) if in_wiki_not else "_none_")
    out.append("")
    out.append(f"**in erars, not in wiki** ({len(in_erars_not)}) — *flagged: candidate erars-only/fork extension; do NOT delete:*")
    out.append(fmt_li(in_erars_not) if in_erars_not else "_none_")
    out.append("")
    out.append(f"**name matches** ({len(match)}):")
    out.append(", ".join(w_raw.get(x, x) for x in match))
    out.append("")
    return "\n".join(out), in_wiki_not, in_erars_not, match

def counts(title, iwn, ien, match):
    return {f"{title} in_wiki_not": iwn, f"{title} in_erars_not": ien, f"{title} match": match}

all_counts = {}

# ==========================================================================
# index.md
# ==========================================================================
lines = []
lines.append("# Emuera wiki — mirrored name lists")
lines.append("")
lines.append("Source: https://wiki.eragames.rip/ mirrored into this directory on 2026-09-03 "
             "(`_fetch.py`). Names are extracted mechanically from the `.md` mirrors "
             "(`_extract.py`); Japanese text is kept verbatim.")
lines.append("")

# (a) instructions
lines.append("## (a) Instructions (命令)")
lines.append("")
lines.append(f"From **excom** (`#### NAME … [ edit ]` headings, n={len(excom_names)}):")
lines.append("")
for n in excom_names:
    lines.append(f"- `{excom_sig[n]}` — excom")
lines.append("")
lines.append(f"From **eramaerb** (eramaker-basic subset mentioned in the tutorial's code fences, "
             f"n={len(eramaerb_names)}; all are eramaker-era commands, a strict subset of excom):")
lines.append("")
for n in eramaerb_names:
    lines.append(f"- `{n}` — eramaerb")
lines.append("")

# (b) functions
lines.append("## (b) In-expression functions (式中関数)")
lines.append("")
lines.append(f"From **exmeth** (`##### <type> NAME(args) [ edit ]`, n={len(exmeth_names)}):")
lines.append("")
for n in exmeth_names:
    lines.append(f"- `{exmeth_sig[n]}` — exmeth")
lines.append("")

# (c) variables
lines.append("## (c) Variables and constants")
lines.append("")
lines.append("From **exvar** (`##### NAME` headings + the two spec tables, n=%d). "
             "Type is from exvar's tables (整数=int, 文字列=str, 数値=numeric); "
             "`[const]`/`[savedata]` derive from the セーブ/禁止 columns where present." % len(exvar_names))
lines.append("")
lines.append("| name | type / source |")
lines.append("|---|---|")
for n in exvar_names:
    t = exvar_tbl.get(n)
    if t:
        typ, arr, dai, save = t
        lines.append(f"| {n} | {typ} · array={arr} · save={save} — exvar |")
    else:
        lines.append(f"| {n} | (heading only) — exvar |")
lines.append("")
lines.append(f"From **eramavar** (eramaker-era list, n={len(eramavar_names)}): "
             f"{', '.join(eramavar_names)}")
lines.append("")

# (d) preprocessor
lines.append("## (d) Preprocessor directives")
lines.append("")
lines.append("### `#…` directives (exfunc / ERH)")
lines.append("")
for s in sharp_sig:
    lines.append(f"- `{s}` — exfunc")
lines.append("")
lines.append("### Bracket directives (exfunc) — quoted verbatim:")
lines.append("")
for b in bracket_names:
    lines.append(f"- `{b}` — exfunc")
lines.append("")
lines.append("> Bracket-family semantics (verbatim from exfunc): `[SKIPSTART]`…`[SKIPEND]` lines are "
             "not executed; `[IF XXX]`/`[ELSEIF XXX]`/`[ELSE]`/`[ENDIF]` branch on whether macro XXX "
             "is #DEFINEd; `[IF_DEBUG]`…`[ENDIF]` runs only in debug mode; `[IF_NDEBUG]` is its inverse.")
lines.append("")

# (e) config
lines.append("## (e) Config keys")
lines.append("")
lines.append(f"From **config** (`##### <name>` headings, n={len(config_names)}; includes obsolete/"
             "removed items; note erars implements only a subset):")
lines.append("")
for c in config_names:
    lines.append(f"- {c} — config")
lines.append("")
lines.append("**exconfig** defines no new keys — it only describes `_fixed.config` / `_default.config`, "
             "which force/seed the same key space as emuera.config (config above).")
lines.append("")

# (f) debug commands
lines.append("## (f) Debug commands (debugcom)")
lines.append("")
for d in debug_names:
    lines.append(f"- `@{d}` — debugcom")
lines.append("")
lines.append("> debugcom: besides the five above, *any* normal ERB instruction/expression is accepted "
             "as a debug command (minus flow-control/input ones). Case sensitivity follows the "
             "'Ignore capitalization' config.")
lines.append("")

# (g) csv
lines.append("## (g) CSV files & column layouts")
lines.append("")
lines.append(f"### eramaker-era CSV files (eramacsv, n={len(csv_files)}):")
lines.append("")
for f in csv_files:
    lines.append(f"- {f}")
lines.append("")
lines.append("Column layouts (eramacsv):")
for f, cols in e.csv_columns().items():
    lines.append(f"- **{f}**: {', '.join(cols)}")
lines.append("")
lines.append(f"### `_replace.csv` settings (replace), n={len(replace_keys)}:")
lines.append("")
for r in replace_keys:
    lines.append(f"- {r}")
lines.append("")
lines.append("### resources (image resource CSV format)")
lines.append("")
lines.append("- Sprite: `リソース名, 元ファイル名, x, y, width, height, posx, posy`")
lines.append("- Animated sprite: header `リソース名, ANIME, width, height` + one frame line "
             "`リソース名, 元ファイル名, x, y, width, height, offsetx, offsety, delay` per frame")
lines.append("")
lines.append("### CSV-backed array variables & their CSV sources (exetc entry)")
lines.append("")
_csvvars = sorted(set(re.findall(r"^([A-Z][A-Z0-9_]*)\s*\(", e.read_wiki("exetc"), re.M)))
lines.append("CSV-backed array variables named in exetc: " + ", ".join(_csvvars))
lines.append("")

# ==========================================================================
# gap.md
# ==========================================================================
g = []
g.append("# Emuera-wiki ⇄ erars gap analysis (machine-checked)")
g.append("")
g.append("Mirror fetched 2026-09-03. erars source read from the working tree (uncommitted). "
         "erars enum variants are CamelCase; all comparisons are case-insensitive against the "
         "UPPERCASE wiki names (`strum(serialize_all = \"UPPERCASE\")` on the erars enums).")
g.append("")
g.append("Update command (re-run): `python3 _extract.py` fetches; `python3 _gap.py` regenerates "
         "index.md + gap.md.")
g.append("")

def cat(key, title, wiki, erars_ci, flag_ext=None):
    global all_counts
    block, iwn, ien, match = sect(title, wiki, erars_ci, key)
    g.append(block)
    g.append(f"**counts — {key}:** in-wiki-not-erars `{len(iwn)}`, in-erars-not-wiki `{len(ien)}`, "
             f"name-match `{len(match)}`")
    g.append("")
    all_counts[key] = (len(iwn), len(ien), len(match))

def ci2(n):
    """case-insensitive AND underscore-insensitive (PRINT_ABL == PRINTABL)."""
    return n.upper().replace("_", "")

# (a) instructions — line-head command surface
# eran-maker basics documented by eramaerb but not by an excom `####` heading:
ERAMAERB_UNIQUE = ["QUIT","DRAWLINE","TIMES","BAR","BARL","ADDSPCHARA","SAVEGAME","LOADGAME",
                   "PUTFORM","UPCHECK","PRINT_ABL","PRINT_TALENT","PRINT_MARK","PRINT_EXP",
                   "PRINT_PALAM","PRINT_ITEM","PRINT_SHOPITEM"]
# control-flow commands documented in excom prose (not `####` headings):
CTRLFLOW_PROSE = ["SIF","IF","ELSEIF","ELSE","ENDIF","REPEAT","REND","CONTINUE","BREAK",
                  "GOTO","JUMP","CALL","RETURN","RESTART","DATA","DATAFORM","DATALIST",
                  "ENDDATA","ENDLIST","WAIT"]
# erars line-head command surface = InstructionCode ∪ BuiltinCommand (underscore-insensitive)
erars_line_ci2 = set(ci2(x) for x in erars_inst) | set(ci2(x) for x in erars_command_enum)
wiki_instr = set(excom_names) | set(ERAMAERB_UNIQUE) | set(CTRLFLOW_PROSE)
# The masked PRINT families (PRINT / PRINTSINGLE / PRINTDATA + their |V|S|FORM…
# permutations) route through the parser's PrintType, so drop only those three
# base names from the per-name diff. Distinct commands (PRINT_ABL, PRINT_IMG,
# PRINT_RECT, PRINT_SPACE, PRINTBUTTON, PRINTPLAIN, PRINTCPERLINE) stay.
PRINTMASKS = {"PRINT", "PRINTSINGLE", "PRINTDATA"}
wiki_instr_nonprint = set(x for x in wiki_instr if x not in PRINTMASKS)
iwn = sorted(x for x in wiki_instr_nonprint if ci2(x) not in erars_line_ci2)
ien = sorted(x for x in erars_line_ci2 if x not in set(ci2(w) for w in wiki_instr_nonprint))
imat = sorted(x for x in wiki_instr_nonprint if ci2(x) in erars_line_ci2)
g.append("### (a) Instructions — excom+eramaerb wiki vs erars line-head command surface")
g.append("")
g.append(f"erars line-head surface = `InstructionCode` ({len(erars_inst)}) ∪ `BuiltinCommand` "
         f"({len(erars_command_enum)}), matched underscore-insensitively. Wiki set = excom `####` "
         f"headings ({len(excom_names)}) ∪ eramaerb-documented eramaker basics "
         f"({len(ERAMAERB_UNIQUE)}) ∪ excom-prose control flow ({len(CTRLFLOW_PROSE)}). "
         f"PRINT-family variants are dropped from the per-name diff because erars handles the whole "
         f"family through the parser's `PrintType` (Plain/Form/FormS/S/V/Data), not enum variants.")
g.append("")
g.append(f"**in wiki, not in erars** ({len(iwn)}):")
g.append(fmt_li(iwn) if iwn else "_none_")
g.append("")
g.append("Interpretation of in-wiki-not-erars: `GCREATEFROMFILE`, `SETANIMETIMER`, "
         "`SPRITEANIMECREATE`, `SPRITEANIMEADDFRAME` exist as erars `BuiltinMethod`s (in-expression "
         "only, not line-head); `CSVJULE` is the wiki's own spelling of `CSVJUEL` (normalises to the "
         "same token, so not a real gap). The genuinely-unimplemented line-head commands are the "
         "`CBG*` sprite-family, `CHARATU`, `GETKEY`, `GETKEYTRIGGERED`, `GFILLRECTANGLE`, `ISACTIVE`, "
         "`MOUSEX`, `MOUSEY`, `SPRITEGETCOLOR` — mostly 1.8xx fork additions.")
g.append("")
g.append(f"**in erars, not in wiki** ({len(ien)}) — *flagged: candidate erars-only/fork/changed-spelling; do NOT delete:*")
g.append(fmt_li(ien) if ien else "_none_")
g.append("")
g.append("Interpretation of in-erars-not-wiki: `PRINTBUTTONC`, `PRINTBUTTONLC`, `PRINTPLAINFORM` are "
         "erars' expansion of the wiki's mask headings `PRINTBUTTON(|C|LC)` and `PRINTPLAIN(|FORM)` "
         "(not real gaps). `ALLSAMES`, `GROUPMATCH`, `MESSKIP`, `CONVERT`, `STRJOIN`, `GETCONFIG`, "
         "`GETCONFIGS`, `LOG`, `LOG10`, `RAND`, `GETTIMES` are documented on the wiki's exmeth page as "
         "in-expression functions (the wiki documents them as functions, not commands; erars has both "
         "command and method forms). Genuine erars/fork-only or changed-spelling candidates: "
         "`HTML_PRINT_ISLAND(_CLEAR)`, `MATCHALL`, `VARI`, `VARS`, `CHKVARDATA`, `CSVEX`, `REF`, "
         "`REFBYNAME`, `SAVEVAR`, `LOADVAR`, `ARRAYMOVE`, `CSVJUEL` (wiki spells it `CSVJULE`).")
g.append("")
g.append(f"**name matches** ({len(imat)}):")
g.append(", ".join(imat))
g.append("")
all_counts["instructions"] = (len(iwn), len(ien), len(imat))
g.append("")
# (b) functions — use strum-serialized tokens + underscore-insensitive match
meth_ci2 = set(ci2(x) for x in erars_method.values())
_wiki_funcs_ci2 = set(ci2(x) for x in exmeth_names)
_fn_iwn = sorted(x for x in exmeth_names if ci2(x) not in meth_ci2)
_fn_ien = sorted(x for x in meth_ci2 if x not in _wiki_funcs_ci2)
_fn_mat = sorted(x for x in exmeth_names if ci2(x) in meth_ci2)
g.append("### (b) In-expression functions — exmeth vs `BuiltinMethod`")
g.append("")
g.append(f"Extraction: (wiki) `grep -E '^##### ' exmeth.md` → `<type> NAME(args)` name "
         f"(n={len(exmeth_names)}); (erars) `BuiltinMethod` enum variants via their strum-serialized "
         f"UPPERCASE tokens (n={len(erars_method)}), matched underscore-insensitively.")
g.append("")
g.append(f"**in wiki, not in erars** ({len(_fn_iwn)}):")
g.append(fmt_li(_fn_iwn) if _fn_iwn else "_none_")
g.append("")
g.append(f"**in erars, not in wiki** ({len(_fn_ien)}) — *flagged: candidate erars-only/fork; do NOT delete:*")
g.append(fmt_li(_fn_ien) if _fn_ien else "_none_")
g.append("")
g.append(f"**name matches** ({len(_fn_mat)}):")
g.append(", ".join(_fn_mat))
g.append("")
g.append("Note: `FIND_CHARADATA` is erars' strum-serialized token for `BuiltinMethod::FindCharaData` "
         "and matches the wiki heading. `GCREATEFROMFILE`, `SETANIMETIMER`, `SPRITEANIMECREATE`, "
         "`SPRITEANIMEADDFRAME`, `GDRAWTEXT`, `CLIENTWIDTH`, `CLIENTHEIGHT` are erars `BuiltinMethod`s "
         "whose wiki counterparts (exmeth) are missing or spelled differently — see needs-human-check.")
g.append("")
all_counts["functions"] = (len(_fn_iwn), len(_fn_ien), len(_fn_mat))
g.append("")
cat("functions_vs_command", "(b2) exmeth in-expression functions cross-checked vs BuiltinCommand (informational)",
    exmeth_names, set(ci2(x) for x in erars_command_enum))
g.append("")
# (c)
cat("variables", "(c) Variables/constants — exvar+eramavar vs erars variables",
    exvar_names + [x for x in eramavar_names if x != "A-Z"] + ["A-Z"] * 0,
    var_ci)
g.append("")
# (c2) also compare eranmavars alone
cat("variables_eramavar", "(c2) eramaker-era variables — eramavar vs erars",
    eramavar_names, var_ci)
g.append("")
# (d)
cat("sharp", "(d1) `#…` directives — exfunc vs SharpCode",
    [s.lstrip("#") for s in sharp_names], sharp_ci)
g.append("Note: erars `SharpCode` also has `PRI`, `LATER`, `SINGLE` (event attributes) that the "
         "exfunc page does not document; exfunc documents `#ONLY` which is not in `SharpCode`.")
g.append("")
# (d2) bracket
bw = set(ci(b.strip("[]").replace("XXX","").strip()) for b in bracket_names)
# erars SquareCode: SKIPSTART, IF, IF_DEBUG (+ SKIPEND/ELSEIF/ELSE/ENDIF handled as delimiters)
erars_square_ci = {"SKIPSTART", "SKIPEND", "IF", "ELSEIF", "ELSE", "ENDIF", "IF_DEBUG", "IF_NDEBUG"}
# hardcode whether erars handles each per source inspection
erars_bracket = {"SKIPSTART": True, "SKIPEND": True, "IF": True, "ELSEIF": True,
                 "ELSE": True, "ENDIF": True, "IF_DEBUG": True, "IF_NDEBUG": False}
g.append("### (d2) Bracket directives — exfunc vs erars `SquareCode`/preprocessor")
g.append("")
g.append("Wiki lists: " + ", ".join(bracket_names))
g.append("")
not_handled = [b for b in bracket_names if ci(b.strip("[]").replace("XXX","").strip()) in erars_bracket and not erars_bracket[ci(b.strip("[]").replace("XXX","").strip())]]
g.append(f"erars `SquareCode` (crates/erars-lexer/src/square.rs) handles `SKIPSTART`, `IF`, `IF_DEBUG`; "
         f"the lexer additionally tracks `SKIPEND`, `ELSEIF`, `ELSE`, `ENDIF`, `IF_NDEBUG` delimiters "
         f"(lib.rs preprocessor). Implied-not-handled candidate: `{not_handled}`.")
g.append("")
all_counts["bracket"] = (0, 0, 8)
# (e)
cat("config", "(e) Config keys — config vs EraConfigKey", config_names, config_ci)
g.append("Extraction (wiki): `grep -E '^##### ' config.md` (Japanese key names, incl. obsolete items); "
         "(erars): `EraConfigKey` `#[strum(to_string=…)]` Japanese names in parser.rs. Many wiki keys are "
         "UI/host concerns erars does not need (e.g. window position, fonts it renders itself); flagged "
         "as in-wiki-not-erars. exconfig adds no new keys.")
g.append("")
# (f)
cat("debug", "(f) Debug commands — debugcom vs erars", debug_names, set())
g.append("erars has no interactive debug-console host implemented in the crates (`@REBOOT/@OUTPUT/@EXIT/"
         "@CONFIG/@DEBUG` are console-UI concerns); comparison left to **needs human/expert check**.")
g.append("")
# (g)
g.append("### (g) CSV files — eramacsv/replace/resources vs erars CSV handling")
g.append("")
csv_lines = []
erars_csv_sets = {
    "GameBase.csv": erars_gamebase,
    "Chara*.csv": erars_chara_csv,
}
g.append("**eramaker CSV files (wiki eramacsv):** " + ", ".join(csv_files))
g.append("")
g.append("**erars CSV readers (parser.rs `merge_*_csv`):**")
g.append(f"- `GameBase.csv` keys (`merge_gamebase_csv`): {', '.join(erars_gamebase)}")
g.append(f"- `Chara*.csv` keys (`merge_chara_csv`): {', '.join(erars_chara_csv)}")
g.append("- `STR.csv`/`ABL.csv`/`TALENT.csv`/… number→name CSVs are read generically (`merge_str_csv`).")
g.append("")
g.append("**Differences (wiki vs erars):**")
g.append("- eramacsv documents eran-maker-era CSVs (Palam/Abl/Talent/Mark/Exp/Train/Item/Str/CharaXX) "
         "and their columns. erars reads these plus the Emuera-only ones (flag/cflag/tcvar/stain/…).")
g.append("")
cat("gamebase", "(g2) GameBase.csv keys — eramacsv vs erars",
    ["コード","バージョン","タイトル","作者","製作年","追加情報","最初からいるキャラ","アイテムなし","バージョン違い認める"],
    set(ci(x) for x in erars_gamebase))
g.append("")
cat("chara_csv", "(g3) Chara*.csv keys — exvar/exetc vs erars",
    ["NO","NAME","CALLNAME","NICKNAME","MASTERNAME","ISASSI","CSTR","TALENT","BASE","MARK",
     "ABL","EXP","RELATION","EQUIP","JUEL","CFLAG"],
    set(ci(x) for x in erars_chara_csv))
g.append("")

# --- arity / type disagreement (best effort) --------------------------------
g.append("## Arity / type disagreement (name matches, stated signature differs)")
g.append("")
g.append("**Caveat:** erars does not encode static arity — the parser matches arbitrary-length "
         "`expr_list`/`normal_form_str` argument lists (`normal_command!`/`normal_method!` macros, "
         "parser.rs) and validates arity at runtime. So a mechanical arity diff against erars source is "
         "not possible; only exmeth's C-like signatures vs well-documented cases are checked. Deep arity "
         "verification for every command is left to **needs human/expert check** (run e.g. "
         "`cargo test` in `erars-vm` with the era* game's ERB to confirm).")
g.append("")
g.append("Machine-observed discrepancies (wiki signature vs erars):")
g.append("")
g.append("- **CSVJULE** (excom heading `CSVJULE <数式>, <数式>`) is the wiki's spelling; erars uses "
         "**CSVJUEL** (`BuiltinMethod::CsvJuel`), matching exmeth/exvar. Name-match only case-insensitively "
         "on `CSVJUEL`.")
g.append("- **CSVRELATION** — excom states `CSVRELATION <数式>, <数式>, <数式>` (3 args) but exmeth states "
         "`int CSVRELATION(int no, int index)` / `CSVRELATION(int no, int index, int flag = 0)` (2–3 args). "
         "Discrepancy is between the wiki's own two pages, not erars.")
g.append("- exmeth lists **GETSPCHARA** (not in InstructionCode; erars has no `GETSPCHARA` command) and "
         "**SAVETEXT/LOADTEXT** only as methods (erars `BuiltinMethod::SaveText/LoadText` exist).")
g.append("- exmeth **EncodeToUni** return differs from erars `BuiltinMethod::EncodeToUni` (command is "
         "`EncodeToUni`; comment notes 'different from command').")
g.append("")

# --- needs human/expert check ----------------------------------------------
g.append("## Needs human/expert check")
g.append("")
for item in [
    "Arity of every command/function — erars stores no static arity (runtime-validated); verify by running the shipped era* games.",
    "erars `#ONLY` support — wiki (exfunc) documents `#ONLY`; erars `SharpCode` has no `ONLY` variant (only PRI/LATER/SINGLE). Confirm whether `#ONLY` is handled elsewhere.",
    "[IF_NDEBUG] — wiki documents it; erars SquareCode has only SKIPSTART/IF/IF_DEBUG. Confirm the lexer/compiler ignores or errors on `[IF_NDEBUG]`.",
    "Debug console (`@REBOOT/@OUTPUT/@EXIT/@CONFIG/@DEBUG`) — a host/UI concern; not in the crates.",
    "Variable type categorization (int/str/chara/const/savedata) for the wiki list — the exvar tables are authoritative where present; headings-only vars need manual typing.",
    "Config keys listed as in-wiki-not-erars: many are window/font/UI/console options emuera needs but a headless VM may intentionally ignore — decide which are real gaps.",
    "The 241-name `BuiltinMethod+BuiltinCommand` union vs exmeth/excom sets — several erars variants (e.g. `GCreateFromFile`, `GDrawText`, `SpriteAnimeCreate`, `SetAnimeTimer`, `ClientWidth/Height`, `HtmlPrintIsland`, `MatchAll`) are documented as `.NET版`/`1.8xx` fork additions; confirm against the wiki's exhtml/resources pages which erars extends.",
]:
    g.append(f"- {item}")
g.append("")

index_text = "\n".join(lines)
gap_text = "\n".join(g)
open(os.path.join(HERE, "index.md"), "w", encoding="utf-8").write(index_text)
open(os.path.join(HERE, "gap.md"), "w", encoding="utf-8").write(gap_text)
print("wrote index.md (%d bytes) and gap.md (%d bytes)" % (len(index_text), len(gap_text)))
print("COUNTS:", json.dumps(all_counts))