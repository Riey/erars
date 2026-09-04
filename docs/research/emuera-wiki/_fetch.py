#!/usr/bin/env python3
"""Fetch Emuera wiki pages and convert HTML -> readable markdown-ish text.
Pages are Japanese; text kept verbatim, no translation/summarising."""
import re, sys, time, os, subprocess
from bs4 import BeautifulSoup, NavigableString, Tag

PAGES = {
    "excom":     "Emuera/excom",
    "exmeth":    "Emuera/exmeth",
    "exvar":     "Emuera/exvar",
    "exfunc":    "Emuera/exfunc",
    "exop":      "Emuera/exop",
    "exetc":     "Emuera/exetc",
    "exhtml":    "Emuera/exhtml",
    "resources": "Emuera/resources",
    "ERH":       "Emuera/ERH",
    "UserVars":  "Emuera/UserVars",
    "UserMeth":  "Emuera/UserMeth",
    "config":    "Emuera/config",
    "debugcom":  "Emuera/debugcom",
    "debug":     "Emuera/debug",
    "replace":   "Emuera/replace",
    "exconfig":  "Emuera/exconfig",
    "eramacsv":  "Emuera/eramacsv",
    "eramaerb":  "Emuera/eramaerb",
    "eramavar":  "Emuera/eramavar",
    "eramaerc":  "Emuera/eramaerc",
    "flow":      "Emuera/flow",
    "diff":      "Emuera/diff",
    "EmuDic":    "Emuera/EmuDic",
}

BASE = "https://wiki.eragames.rip/index.php/"

def block_to_text(node):
    """MediaWiki -> readable text for one top-level block."""
    out = []
    for el in node.descendants:
        pass
    return node.get_text("\n")

def render(el, depth=0):
    """Render an element (or its children) to text lines."""
    lines = []
    def emit(s):
        lines.append(s)
    def walk(node):
        if isinstance(node, NavigableString):
            return str(node)
        return None
    # We'll do a manual traversal preserving block structure.
    return lines

def convert(html):
    soup = BeautifulSoup(html, "lxml")
    content = soup.select_one("div.mw-parser-output") or soup.select_one("div#mw-content-text")
    if content is None:
        return soup.get_text("\n")
    out = []
    # MediaWiki block-level structure
    for node in content.children:
        if isinstance(node, NavigableString):
            txt = str(node).strip()
            if txt:
                out.append(txt)
            continue
        if not isinstance(node, Tag):
            continue
        name = node.name
        if name in ("h1","h2","h3","h4","h5","h6"):
            lvl = int(name[1]) + 1
            out.append("\n" + "#"*lvl + " " + node.get_text(" ", strip=True) + "\n")
        elif name == "p":
            t = node.get_text(" ").strip()
            if t:
                out.append(t)
        elif name in ("ul","ol"):
            for li in node.find_all("li", recursive=False):
                t = li.get_text(" ", strip=True)
                if t:
                    out.append("- " + t)
        elif name in ("table",):
            out.append(render_table(node))
        elif name == "pre":
            code = node.get_text("\n")
            out.append("```\n" + code.strip("\n") + "\n```")
        elif name in ("div","dl",):
            # dl/dt/dd
            if name == "dl":
                for dt in node.find_all("dt", recursive=False):
                    out.append("**" + dt.get_text(" ", strip=True) + "**")
                for dd in node.find_all("dd", recursive=False):
                    out.append("    " + dd.get_text(" ", strip=True))
            else:
                sub = node.get_text(" ", strip=True)
                if sub:
                    out.append(sub)
        elif name == "hr":
            out.append("---")
        elif name in ("script","style"):
            continue
        else:
            t = node.get_text(" ", strip=True)
            if t:
                out.append(t)
    text = "\n".join(out)
    text = re.sub(r"\n{3,}", "\n\n", text)
    return text.strip()

def render_table(tbl):
    rows = []
    for tr in tbl.find_all("tr"):
        cells = []
        for cell in tr.find_all(["th","td"]):
            cells.append(cell.get_text(" ", strip=True))
        if cells:
            rows.append(" | ".join(cells))
    return "\n".join(rows)

def main():
    outdir = os.path.dirname(os.path.abspath(__file__))
    today = time.strftime("%Y-%m-%d")
    for fname, page in PAGES.items():
        url = BASE + page.replace(" ", "_")
        target = os.path.join(outdir, fname + ".md")
        if os.path.exists(target):
            print(f"[skip] {fname} exists")
            continue
        html = None
        for attempt in range(3):
            try:
                r = subprocess.run(
                    ["curl","-sL","-A","Mozilla/5.0","--max-time","60",url],
                    capture_output=True)
                html = r.stdout.decode("utf-8", errors="replace")
                break
            except Exception as e:
                print(f"[retry {fname}] {e}", file=sys.stderr)
                time.sleep(3)
        if html is None:
            print(f"[FAIL] {fname} fetch failed")
            continue
        # Detect 404 / missing page
        soup = BeautifulSoup(html, "lxml")
        # MediaWiki "no such page" -> has #firstHeading with the requested title and a 'noarticletext'
        noarticle = bool(soup.select_one("div.noarticletext"))
        redir = bool(soup.select_one("div.redirectText")) or "Redirected from" in html
        content = soup.select_one("div.mw-parser-output")
        # If page is truly empty/missing
        if noarticle or content is None or not content.get_text(strip=True):
            with open(target, "w", encoding="utf-8") as f:
                f.write(f"# URL: {url}\n# fetch date: {today}\n\n")
                f.write(f"> PAGE UNAVAILABLE: noarticle={noarticle} redirect={redir}\n")
                f.write("(no content extracted)\n")
            print(f"[warn] {fname}: noarticle={noarticle} redir={redir}, wrote stub")
            time.sleep(1.5)
            continue
        body = convert(html)
        with open(target, "w", encoding="utf-8") as f:
            f.write(f"# URL: {url}\n# fetch date: {today}\n\n")
            f.write(body + "\n")
        print(f"[ok] {fname} {len(body):6d} chars")
        time.sleep(1.5)  # polite sequential fetch

if __name__ == "__main__":
    main()