#!/usr/bin/env python3
"""PART 2 + PART 3: extract Emuera-wiki name lists and diff against erars.

Reads the mirrored pages in docs/research/emuera-wiki/*.md and the erars
source tree, and writes index.md (name lists) and gap.md (diffs).
Extraction commands are recorded so counts are reproducible.
"""
import re, os, io, sys

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))  # .../erars

WIKI = HERE
ERARS = REPO

def R(*p):
    return os.path.join(REPO, *p)

def read(p):
    with open(p, encoding="utf-8") as f:
        return f.read()

def read_wiki(name):
    return read(os.path.join(WIKI, name + ".md"))

# --------------------------------------------------------------------------
# WIKI extraction
# --------------------------------------------------------------------------

def excom_instructions():
    """(a) instructions from excom.md: `#### NAME... [ edit ]` headings."""
    txt = read_wiki("excom")
    pat = re.compile(r"^####\s+([A-Z][A-Z0-9_]*)\b", re.M)
    names = pat.findall(txt)
    # keep the full heading (signature) too
    sig = re.findall(r"^####\s+(.+?)\s*\[ edit \]\s*$", txt, re.M)
    seen = {}
    for s in sig:
        m = re.match(r"([A-Z][A-Z0-9_]*)\b", s)
        if m and m.group(1) not in seen:
            seen[m.group(1)] = s
    return list(seen), seen

def eramaerb_instructions():
    """(a) instructions from eramaerb.md: uppercase tokens in code fences."""
    txt = read_wiki("eramaerb")
    fences = re.findall(r"```\n(.*?)```", txt, re.S)
    pat = re.compile(r"\b([A-Z][A-Z0-9_]{1,})\b")
    names = set()
    for f in fences:
        for line in f.splitlines():
            s = line.strip()
            if not s or s.startswith(";"):
                continue
            m = re.match(r"([A-Z][A-Z0-9_]{2,})\b", s)
            if m:
                names.add(m.group(1))
    return sorted(names)

def exmeth_functions():
    """(b) in-expression functions from exmeth.md: `##### <type> NAME(args)`."""
    txt = read_wiki("exmeth")
    sig = re.findall(r"^#####\s+(.+?)\s*\[ edit \]\s*$", txt, re.M)
    seen = {}
    for s in sig:
        m = re.match(r"\S+\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(", s)
        if m and m.group(1) not in seen:
            seen[m.group(1)] = s
    return list(seen), seen

def exvar_variables():
    """(c) variables from exvar.md: `##### NAME` headings + two tables."""
    txt = read_wiki("exvar")
    names = set(re.findall(r"^#####\s+([A-Za-z_][A-Za-z0-9_]*)\b", txt, re.M))
    # tables: rows "NAME | 型 | ..." in the two spec-table sections
    tbl = {}
    for row in re.findall(r"^([A-Za-z_][A-Za-z0-9_]+)\s*\|\s*([^|]+)\|([^|]+)\|([^|]+)\|([^|]+)\|", txt, re.M):
        name, typ, hairetsu, dairi, save = [x.strip() for x in row]
        tbl[name] = (typ, hairetsu, dairi, save)
        names.add(name)
    return sorted(names), tbl

def eramavar_variables():
    """(c) variables from eramavar.md: `##### NAME (...)` headings, cut at the
    trailing prose sections (Character Registration Number / About Stain)."""
    txt = read_wiki("eramavar")
    cut = txt.find("### Character Registration Number")
    if cut != -1:
        txt = txt[:cut]
    names = []
    for m in re.finditer(r"^#####\s+([A-Za-z_][A-Za-z0-9_]*)\b", txt, re.M):
        n = m.group(1)
        if n == "A-Z":
            names.append("A-Z")
        elif n not in names:
            names.append(n)
    return names

def preproc_sharp():
    """(d) # directives from exfunc.md."""
    txt = read_wiki("exfunc")
    ns = sorted(set(re.findall(r"^####\s+(#[A-Z]+)\b", txt, re.M)))
    sig = re.findall(r"^####\s+(#[A-Z]+[^\n]*?)\s*\[ edit \]\s*$", txt, re.M)
    return ns, sig

def preproc_bracket():
    """(d) bracket directives from exfunc.md — verbatim."""
    txt = read_wiki("exfunc")
    heads = re.findall(r"^####\s+(\[[^\n]*?\])\s*\[ edit \]\s*$", txt, re.M)
    return heads

def config_keys():
    """(e) config keys from config.md: `##### <name>` headings (Japanese)."""
    txt = read_wiki("config")
    sects = re.findall(r"^#####\s+([^\n]+?)\s*\[ edit \]\s*$", txt, re.M)
    # drop obsolete/removed entries marked with （...） — keep, flag later
    return sects

def debug_commands():
    """(f) debug commands from debugcom.md — every `@NAME`."""
    txt = read_wiki("debugcom")
    cmds = re.findall(r"^@([A-Z][A-Z0-9_]*)\b", txt, re.M)
    return sorted(set(cmds))

def debug_commands_special():
    """The five debug-console-only commands (`- @REBOOT` bullet list)."""
    txt = read_wiki("debugcom")
    return sorted(set(re.findall(r"^- @([A-Z][A-Z0-9_]*)\b", txt, re.M)))

def csv_columns():
    """(g) CSV file column layouts from eramacsv.md (best-effort, name→columns)."""
    txt = read_wiki("eramacsv")
    # Map eran-maker CSV -> the era-basic field names their `- key,` lines use
    return {
        "GameBase.csv": ["コード", "バージョン", "タイトル", "作者", "製作年", "追加情報",
                          "最初からいるキャラ", "アイテムなし", "バージョン違い認める"],
        "Palam.csv":    ["パラメータ番号", "パラメータ名"],
        "Abl.csv":      ["能力番号", "能力名"],
        "Talent.csv":   ["素質番号", "素質名"],
        "Mark.csv":     ["刻印番号", "刻印名"],
        "Exp.csv":      ["経験番号", "経験名"],
        "Train.csv":    ["コマンド番号", "コマンド名"],
        "Item.csv":     ["アイテム番号", "アイテム名", "値段"],
        "Str.csv":      ["文字列番号", "文字列"],
        "CharaXX.csv":  ["番号", "名前", "呼び名", "基礎", "能力", "素質", "経験",
                         "相性", "助手", "フラグ"],
    }

def csv_files():
    """(g) CSV files & columns from eramacsv.md."""
    txt = read_wiki("eramacsv")
    files = re.findall(r"(GameBase\.csv|Palam\.csv|Abl\.csv|Talent\.csv|Mark\.csv|Exp\.csv|Train\.csv|Item\.csv|Str\.csv|CharaXX\.csv)", txt)
    return sorted(set(files))

def replace_keys():
    """_replace.csv keys from replace.md: `#### <name>` headings."""
    txt = read_wiki("replace")
    return re.findall(r"^####\s+([^\n]+?)\s*\[ edit \]\s*$", txt, re.M)

# --------------------------------------------------------------------------
# erars extraction
# --------------------------------------------------------------------------

def erars_inst():
    txt = read(R("crates/erars-lexer/src/inst.rs"))
    body = txt.split("pub enum InstructionCode {", 1)[1].split("}", 1)[0]
    names = [m for m in re.findall(r"^\s+([A-Z][A-Z0-9_]*)\s*,", body, re.M)]
    return names

def erars_command(path="crates/erars-ast/src/command.rs"):
    """Parse both enums in command.rs. Returns dict name->strum-serialized (UPPERCASE)."""
    txt = read(R(path))
    def parse(body):
        ser = {v.upper(): s for v, s in
               re.findall(r"#\[\s*strum\(serialize = \"([A-Z0-9_]+)\"\)\s*\]\s*\n\s+([A-Za-z][A-Za-z0-9_]*)", body)}
        names = re.findall(r"^\s+([A-Za-z][A-Za-z0-9_]*)\s*(?:=[^,]*)?,", body, re.M)
        return {n.upper(): ser.get(n.upper(), n.upper()) for n in names}
    i = txt.index("pub enum BuiltinMethod"); j = txt.index("pub enum BuiltinCommand")
    k = txt.index("}\n", j)
    return {"method": parse(txt[i:j]), "command": parse(txt[j:k + 1])}

def erars_vars():
    txt = read(R("crates/erars-loader/src/variable.yaml"))
    keys = re.findall(r"^([A-Z][A-Z0-9_]*):", txt, re.M)
    return sorted(set(keys))

def erars_known_var():
    txt = read(R("crates/erars-vm/src/variable.rs"))
    body = txt.split("pub enum KnownVariableNames {", 1)[1].split("}", 1)[0]
    return [m for m in re.findall(r"^\s+([A-Za-z][A-Za-z0-9_]*)\s*,", body, re.M)
            if not m.startswith("#")]

def erars_sharp():
    txt = read(R("crates/erars-lexer/src/sharp.rs"))
    body = txt.split("pub enum SharpCode {", 1)[1].split("}", 1)[0]
    return [m for m in re.findall(r"^\s+([A-Z][A-Z0-9_]*)\s*,", body, re.M)]

def erars_config():
    txt = read(R("crates/erars-compiler/src/parser.rs"))
    keys = re.findall(r'#\[\s*strum\(to_string = "([^"]+)"\)\s*\]', txt)
    return keys  # Japanese names

def erars_gamebase():
    txt = read(R("crates/erars-compiler/src/parser.rs"))
    m = re.search(r'pub fn merge_gamebase_csv.*?\n    }', txt, re.S)
    keys = re.findall(r'"([^"]+)"\s*=>', m.group(0))
    return keys

def erars_chara_csv():
    txt = read(R("crates/erars-compiler/src/parser.rs"))
    m = re.search(r'pub fn merge_chara_csv.*?\n    \}', txt, re.S)
    keys = re.findall(r'"([A-Z]+)"\s*\|', m.group(0)) + re.findall(r'"([^"]+)"\s*=>', m.group(0))
    return keys

if __name__ == "__main__":
    pass