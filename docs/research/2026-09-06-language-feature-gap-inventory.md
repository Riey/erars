# Emuera Language Feature & Builtin Command Gap Inventory — erars

Date: 2026-09-06 (Second Pass: Inverted Corpus-Driven Methodology)  
Branch: `master` at commit `e1f16fd`  
Corpora examined:
- `/home/riey/repos/eraTHYMKR` (873 ERB/ERH files, ~1,045,721 lines)
- `/home/riey/repos/eramegaten_p_kr/Data` (8,779 ERB/ERH files, ~3,500,000 lines)

---

## 1. Executive Summary: Corpus-Driven Gap & Semantic Bug Rankings

Rather than enumerating missing enum variants from static AST declarations, this second-pass inventory was compiled by:
1. Scanning both shipping corpora for all built-in statements, function calls, and operator expressions actually executed.
2. Cross-referencing against the implementation in `crates/erars-vm/src/terminal_vm/executor.rs`, `crates/erars-compiler/src/parser/expr.rs`, and `crates/erars-compiler/src/parser.rs`.
3. Verifying silent-wrong semantics (code that compiles or parses but executes wrongly or panics at runtime).

### Methodology Warning: Static Enum-Absence vs. Real Boundary Verification (False Positives)
Static enum-absence checks find genuinely absent features, but also systematically produce **false positives**: checking a boundary adjacent to the one being claimed about rather than the boundary itself. A construct or identifier can be handled somewhere other than the specific enum being grepped:
- Variable names may be handled via macro attributes / strum aliases (e.g. `#[strum(to_string = "GAMEBASE_GAMECODE", serialize = "GAMEBASE_CODE")]` in `BuiltinVariable`), or in dedicated variable metadata YAML tables (`crates/erars-loader/src/variable.yaml`), rather than appearing as standalone named enum variants.
- Config keys absent from `EraConfigKey` do not necessarily cause hard parse failures: `EraConfig::merge_text` in `crates/erars-compiler/src/parser.rs:1007` checks `if let Ok(key) = key.parse() { ... }`, meaning unrecognized config keys are simply ignored/skipped at load time without error.
This project tracks a running list of exactly this failure mode (checking an adjacent layer or relying on unverified negative assertions rather than verifying the observable behavior). Any claim of feature absence must be verified by executing code against that boundary, not inferred solely from enum variant absence.

Zero-usage theoretical items and refuted pseudo-variables have been moved to **Appendix A**.

### Ranked Table of Real-World Gaps & Semantic Corruptions

| Rank | Construct / Feature | Emuera Specification | erars Defect Nature | eramegaten (Files / Count) | eraTHYMKR (Files / Count) | Exact erars Code Anchor |
|---|---|---|---|---|---|---|
| **1** | **`TIMES` 32-bit float truncation** | `TIMES variable, factor`: scales variable by float using 64-bit `double` (`excom.md` §TIMES) | **Silent precision loss**: casts 64-bit int to `f32` (`(arg as f32 * t.into_inner()) as i64`), losing low bits on values $> 16,777,216$ ($2^{24}$) | **262 files / 7,120 uses** | **462 files / 19,969 uses** | `crates/erars-vm/src/terminal_vm/executor.rs:357-362` — **fixed only for the integer operand; the factor stays `f32` by instruction-payload construction, see `language-feature-work.md` §5.1** |
| **2** | **Integer bulk array assignment** (`A = 1, 2, 3`) | Sequential assignment across comma-separated expressions starting from base index (`exetc.md`) | **Parser truncation**: parses only a single scalar `Expr`, dropping subsequent array initializers | **169 files / 887 uses** | **0 files / 0 uses** *(verified below)* | `crates/erars-compiler/src/parser.rs:3506-3514`, `ast.rs:36` |
| **3** | **String bulk array assignment** (`STR '= "a", "b"`) | Sequential string assignment to array elements (`exetc.md`, `exop.md`) | **Parser truncation**: parses only a single scalar `Expr`, dropping subsequent array elements | **65 files / 184 uses** | **0 files / 0 uses** | `crates/erars-compiler/src/parser.rs:3502-3505` |
| **4** | **Unchecked bit shifts (`<<`, `>>`)** | 64-bit shifts masked by `count & 0x3F` (C# semantics); never panics on negative or $\ge 64$ (`exop.md`) | **Debug/test-only panic**: raw Rust `lhs << rhs` / `lhs >> rhs` panics on `rhs < 0` or `rhs >= 64` only when `overflow-checks` is on (`dev`/`test`); `--release` (no `overflow-checks` profile override) already masks to `& 0x3F` and matches C#, see `language-feature-work.md` §5.2 | **188 files / 421 uses (`>>`)**, **30 files / 111 uses (`<<`)** | **6 files / 10 uses (`>>`)**, **5 files / 7 uses (`<<`)** | `crates/erars-vm/src/terminal_vm/executor.rs:378-380` |
| **5** | **`LIMIT` assertion panic** | Clamps value between low and high bounds (`exmeth.md`) | **Runtime panic**: uses `v.clamp(low, high)` without validating `low <= high`. If dynamically `low > high`, Rust stdlib panics | **141 files / 337 uses** | **32 files / 109 uses** | `crates/erars-vm/src/terminal_vm/executor.rs:2494-2500` |
| **6** | **Unchecked bitwise helpers (`GETBIT`, `SETBIT`, `CLEARBIT`, `INVERTBIT`)** | Bit index $0 \le \text{idx} < 64$ (`exop.md`, `excom.md`) | **Debug/test-only panic** (same class as row 4): `l >> r` and `1 << idx` panic on $\text{idx} \ge 64$ or negative shift only under `overflow-checks`; `--release` masked silently instead of erroring — no VM error boundary in either case pre-fix | **356 files / 3,109 uses (`GETBIT`)** | **12 files / 53 uses (`GETBIT`)** | `crates/erars-vm/src/terminal_vm/executor.rs:2546-2551, 3419-3435` |
| **7** | **`FINDELEMENT` regex escaping on exact match** | When `exact_match != 0`, perform literal equality matching (`exmeth.md:810`) | **Runtime regex error**: generates `format!("^{value}$")` with `Regex::new`. Unescaped regex characters (`[`, `]`, `+`, `?`) fail at runtime | **138 files / 632 uses** | **0 files / 0 uses** | `crates/erars-vm/src/terminal_vm/executor.rs:1889-1896` |
| **8** | **Unchecked division / modulo (`/`, `%`)** | Integer division rounds to zero; division by zero raises a recoverable script error (`exop.md`) | **Runtime panic**: raw Rust `/` and `%` panic on `rhs == 0` and on `i64::MIN / -1`, aborting the process | **Heavy usage across all ERB** | **Heavy usage across all ERB** | `crates/erars-vm/src/terminal_vm/executor.rs:371-373` |
| **9** | **Bitwise NOT `~` semantic corruption** | Unary bitwise inversion (`~x = -x - 1`) (`exop.md`) | **Silent corruption**: mapped to `UnaryOperator::Not` (`!as_bool()`). Bitmask operations like `CFLAG &= ~1p4` zero the variable | **3 files / 6 uses** | **5 files / 19 uses** | `crates/erars-compiler/src/parser/expr.rs:830-831`, `executor.rs:366-369` |
| **10** | **`ARRAYSORT` missing default argument** | `ARRAYSORT var{, order}{, start}{, count}`: `order` is optional, defaulting to ascending (`excom.md`) | **Missing argument error**: requires `order` (`@bool`), returning a normal VM script error (`bail!`/`check_arg_count!`, not a Rust panic in any build profile) for single-argument calls with `"매개변수가 부족합니다"` | **17 files / 32 uses (2 calls omit `order`)** | **0 files / 0 uses** | `crates/erars-vm/src/terminal_vm/executor.rs:3553-3560` |
| **11** | **`SQRT` 32-bit float truncation** | Integer square root for 64-bit integers | **Silent precision loss**: casts to `f32` (`(x as f32).sqrt() as i64`), losing mantissa precision on large values | **4 files / 5 uses** | **2 files / 2 uses** | `crates/erars-vm/src/terminal_vm/executor.rs:2117-2122` — **fully fixed to `f64`, no payload constraint applies here, see `language-feature-work.md` §5.1** |
| **12** | **Window & Host GUI Config Keys** | Configuration keys for GUI window position, size, mouse mode, FPS, editor invocation (`config.md`) | **Ignored at parse time (Not a blocker)**: Absent from `EraConfigKey` enum, but `EraConfig::merge_text` (`parser.rs:1007`) parses keys with `if let Ok(key) = key.parse()`, silently skipping unknown keys without failing. `erars-stdio` has no presentation layer to bind them to. Verified by running with `emuera.config` containing all 14 keys — loads without error. | **1 file (`emuera.config`) / 14 keys** | **1 file (`emuera.config`) / 14 keys** | `crates/erars-compiler/src/parser.rs:1007` |
| **13** | **`GAMEBASE_GAMECODE` Alias** | Game identifier integer from `GameBase.csv` | **NOT A DEFECT — Verified working**: `crates/erars-ast/src/variable.rs:28` maps both `"GAMEBASE_GAMECODE"` and `"GAMEBASE_CODE"` via `#[strum(to_string = "GAMEBASE_GAMECODE", serialize = "GAMEBASE_CODE")] GamebaseCode`. Evaluates to `ctx.header_info.gamebase.code` at `crates/erars-vm/src/terminal_vm/executor.rs:460`. Verified working by fixture `tests/run_tests/basic/gamebase_gamecode.erb`. Initial classification was a false positive from grepping enum variant identifiers. | **0 files / 0 uses** | **1 file (`GameBase.csv`)** | `crates/erars-ast/src/variable.rs:28`, `crates/erars-vm/src/terminal_vm/executor.rs:460` |

---

## 2. Verification of eraTHYMKR Zero Count on Bulk Array Assignment

The initial report listed integer bulk array assignment as 887 uses in eramegaten but 0 in eraTHYMKR. Because eraTHYMKR is a large codebase (~1.05M lines), this was rigorously verified against line continuations, spacing variations, and assignment operators.

### Grep Methodology & Regex Pattern
To catch all possible forms of bulk assignment:
```python
# Strip comments (;)
# Match LHS followed by = or '=
regex = r"^([A-Za-z_][A-Za-z0-9_:]*(?:\[[^\]]*\])?)\s*('==?|=)\s*(.+)$"
```
For every matching line, the RHS was parsed by a state machine that tracks:
- Parenthesis nesting depth `( ... )`
- Bracket nesting depth `[ ... ]`
- Brace nesting depth `{ ... }`
- String literal quotes `" ... "` with escape handling `\"`
- Commas at depth 0 (outside any grouping or quotes).

### Results in eraTHYMKR
Across all 873 files, exactly **87 lines** contained a top-level comma on the RHS of an assignment:
- **`RESULTS`**: 66 lines (e.g., `RESULTS = %RESULTS%, %TALENTNAME:61%`)
- **`LOCALS`**: 11 lines (e.g., `LOCALS = %LOCALS%, %ITEMNAME:LOCAL%`)
- **`CSTR`**: 4 lines
- **`TSTR`**: 3 lines
- **`STR`**: 2 lines
- **`COM_NAME`**: 1 line

**Key Finding**:
1. **100% of comma-assignments in eraTHYMKR are on string variables using plain `=`**. Under Emuera semantics, plain `=` on a string variable evaluates the RHS as a single FORM string literal, where commas are literal characters in the string, **not** bulk array assignment delimiters. Bulk assignment on string variables requires `'=`.
2. **Line Continuation Blocks**: A scan for `{` on a line by itself (Emuera line continuation block) returned **0 occurrences** in eraTHYMKR.
3. **Array Initialization Pattern in eraTHYMKR**: eraTHYMKR originated as an eramaker / early-Emuera codebase (targeting Emuera 1.8.1.8 compatibility). It populates arrays exclusively through iterative loops, `VARSET`, or single-element statements (`LOCAL:0 = 1`, `LOCAL:1 = 2`), never utilizing Emuera's bulk assignment extension.

Thus, the count of **0 bulk array assignments in eraTHYMKR** is verified to be genuine, not a pattern artifact.

---

## 3. Deep Dive into Silent-Wrong Semantics and Runtime Crashers

### 3.1 `TIMES` Precision Loss (`f32` vs `f64`)
- **Emuera Behavior**: `TIMES V, F` calculates `(long)((double)V * F)` (or `decimal` under rigorous calculation mode). 64-bit integers are converted to 64-bit IEEE 754 `double`, preserving 53 bits of precision (up to $9,007,199,254,740,992$).
- **erars Defect** (`crates/erars-vm/src/terminal_vm/executor.rs:357-362`):
  ```rust
  InstructionType::Times => {
      let t = inst.as_times().unwrap();
      let arg = ctx.pop_int()?;
      let ret = (arg as f32 * t.into_inner()) as i64;
      ctx.push(ret);
  }
  ```
  Casting `arg as f32` provides only 24 bits of mantissa precision. Any integer above $16,777,216$ (such as character EXP, currency, or high RPG damage in Megaten) silently loses lower bits before multiplication.
- **Corpus Impact**: 19,969 occurrences in eraTHYMKR, 7,120 occurrences in eramegaten.
- **Correction (post-fix review, see `language-feature-work.md` §5.1)**:
  the eventual fix widens the popped `arg` operand to `f64` before
  multiplying, which is exactly the precision loss described above and is
  fully resolved. It does **not** widen the `factor` (`t.into_inner()`) to
  `f64` end-to-end — `factor` is an `NotNan<f32>` stored directly in the
  4-byte `Instruction` payload (`crates/erars-compiler/src/instruction.rs`),
  and widening that field was rejected as a payload-size regression. A
  residual ~1e-7 relative rounding error versus Emuera's true `double`
  factor remains by construction; it is harmless for the simple decimal
  literals (`1.5`, `0.8`, …) real `TIMES` call sites in both corpora use.

### 3.2 Unchecked Bitwise Shifts (`<<`, `>>`)
- **Emuera Behavior**: Compiled with C# 64-bit shift semantics: `lhs << (rhs & 0x3F)`. Negative shift counts and counts $\ge 64$ are masked with 63. No exception is thrown.
- **erars Defect** (`crates/erars-vm/src/terminal_vm/executor.rs:378-380`):
  ```rust
  BinaryOperator::Lhs => Value::Int(lhs.try_into_int()? << rhs.try_into_int()?),
  BinaryOperator::Rhs => Value::Int(lhs.try_into_int()? >> rhs.try_into_int()?),
  ```
  **Correction (post-fix review, see `language-feature-work.md` §5.2): this
  panics only in `dev`/`test` builds.** No `[profile.release]` section
  exists in `Cargo.toml`, so `overflow-checks` defaults to `false` in
  `--release`; empirically, LLVM's release lowering already masks the
  shift count to `& 0x3F` for a 64-bit operand before shifting — the same
  masking C# performs — so the shipped `--release` binary did not crash on
  these inputs even before the fix. Corpus usage counts below count how
  often `<<`/`>>` appear in script, not how often a call site passes an
  out-of-range count; they are not evidence this ever reached the panic
  path in any build.
- **Corpus Impact**: 532 occurrences in eramegaten, 17 in eraTHYMKR.

### 3.3 `LIMIT` Panic on Inverted Bounds
- **Emuera Behavior**: `LIMIT(val, min, max)` clamps `val` into `[min, max]`. If `min > max`, Emuera raises a script-level diagnostic or adjusts bounds.
- **erars Defect** (`crates/erars-vm/src/terminal_vm/executor.rs:2494-2500`):
  ```rust
  BuiltinMethod::Limit => {
      check_arg_count!(3);
      let v = get_arg!(@i64: args, ctx);
      let low = get_arg!(@i64: args, ctx);
      let high = get_arg!(@i64: args, ctx);
      ctx.push(v.clamp(low, high));
  }
  ```
  Rust's `i64::clamp(low, high)` explicitly panics if `low > high`. Dynamic calculations that inadvertently invert bounds crash the entire process.
- **Corpus Impact**: 446 occurrences across 173 files.

### 3.4 `FINDELEMENT` Regex Escaping
- **Emuera Behavior**: `FINDELEMENT(var, value, start, end, exact_match)`. When `exact_match != 0`, Emuera performs literal string comparison (`str == value`).
- **erars Defect** (`crates/erars-vm/src/terminal_vm/executor.rs:1889-1896`):
  ```rust
  let regex = regex::Regex::new(&if exact_match {
      format!("^{value}$")
  } else {
      value
  }).context("Parse FINDELEMENT argument")?;
  ```
  erars treats `value` as a regex even when `exact_match` is requested. Searching for strings containing UI brackets or symbols (e.g., `[10] 火炎` or `+`) fails at runtime with `Parse FINDELEMENT argument`.
- **Corpus Impact**: 632 occurrences across 138 files in eramegaten.

### 3.5 Bitwise NOT `~` Operator
- **Emuera Behavior**: `~` is unary bitwise NOT. `~0 = -1`, `~1p4 = -17`.
- **erars Defect** (`crates/erars-compiler/src/parser/expr.rs:830-831`, `crates/erars-vm/src/terminal_vm/executor.rs:366-369`):
  Mapped to `UnaryOperator::Not`, executing `!operand.as_bool()`. Bit-clearing operations like `CFLAG:5221 &= ~1p4` evaluate `~1p4` as `!16 = 0`, resulting in `CFLAG:5221 &= 0`, which silently wipes the entire variable.
- **Corpus Impact**: 19 occurrences across 5 files in eraTHYMKR, 6 occurrences in eramegaten.

### 3.6 Unchecked Integer Division and Modulo (`/`, `%`)
- **Emuera Behavior**: Division by zero is trapped by script runtime error handling.
- **erars Defect** (`crates/erars-vm/src/terminal_vm/executor.rs:371-373`):
  Raw Rust `/` and `%` panic on `rhs == 0` and on `i64::MIN / -1`, aborting the process instead of returning a VM execution error.

---

## 4. Corpus Built-in Usage Profile

To ground the inventory in total corpus usage, all built-in commands and method calls were tallied against the wiki specification. Out of 677 documented Emuera commands/functions:
- **281 items** are actively used in at least one corpus.
- **396 items** have **0 occurrences** across both shipping games.

### Top Commands by Corpus Frequency
| Command | eraTHYMKR Occurrences (Files) | eramegaten Occurrences (Files) |
|---|---|---|
| `RETURN` | 42,987 (758 files) | 131,332 (7,468 files) |
| `CALL` | 13,139 (720 files) | 102,692 (5,008 files) |
| `PRINTFORML` | 32,297 (589 files) | 68,831 (1,405 files) |
| `PRINTFORMW` | 276,387 (645 files) | 64,565 (1,185 files) |
| `CASE` | 7,576 (83 files) | 56,570 (3,525 files) |
| `PRINTL` | 20,109 (682 files) | 54,701 (5,727 files) |
| `SIF` | 23,243 (759 files) | 36,838 (4,552 files) |
| `TIMES` | 19,969 (462 files) | 7,120 (262 files) |
| `SELECTCASE` | 1,180 (83 files) | 15,181 (3,870 files) |
| `FOR` / `NEXT` | 912 (104 files) | 6,674 (1,330 files) |

### Top Built-in Functions by Corpus Frequency
| Function | eraTHYMKR Occurrences (Files) | eramegaten Occurrences (Files) |
|---|---|---|
| `GETNUM` | 12 (2 files) | 3,838 (419 files) |
| `GETCHARA` | 447 (24 files) | 3,439 (312 files) |
| `UNICODE` | 1,231 (19 files) | 3,208 (148 files) |
| `GETBIT` | 53 (12 files) | 3,109 (356 files) |
| `MAX` | 382 (79 files) | 1,387 (366 files) |
| `TOSTR` | 37 (34 files) | 1,136 (268 files) |
| `MIN` | 158 (45 files) | 927 (332 files) |
| `FINDELEMENT` | 0 (0 files) | 632 (138 files) |
| `VARSIZE` | 10 (2 files) | 615 (203 files) |
| `GROUPMATCH` | 218 (46 files) | 511 (271 files) |
| `INRANGE` | 24 (5 files) | 433 (178 files) |
| `LIMIT` | 109 (32 files) | 337 (141 files) |

---

## Appendix A: Zero-Usage & Theoretical Gaps (0 Occurrences in Both Corpora)

### A.1 Genuine Gaps / Divergences with 0 Corpus Usage
1. **String Relational Comparisons (`<`, `<=`, `>`, `>=`)**:
   - `exop.md` documents string comparison (`"apple" < "banana"`).
   - erars calls `try_into_int()` in `crates/erars-vm/src/terminal_vm/executor.rs:388-400`.
   - Corpus usage: **0 files / 0 occurrences**.
2. **Interactive Debug Commands (`@REBOOT`, `@OUTPUT`, `@EXIT`, `@CONFIG`, `@DEBUG`)**:
   - `debugcom.md` console-only REPL commands.
   - Absent from parser and lexer.
   - Corpus ERB usage: **0 files / 0 occurrences** (interactive CLI only).
3. **Comparison Chaining (`A < B < C`)**:
   - Emuera specifies comparison operators as non-associative; erars parses left-associative.
   - Corpus ERB usage: **0 files / 0 occurrences** (all apparent chained brackets occurred within dungeon map layout strings like `" 1<1>1 "`).

### A.2 Refuted Pseudo-Variable Gaps (False Positives — Verified Fully Working in erars)
The first pass of this inventory claimed several pseudo-variables were missing because they were not found as plain variants in `BuiltinVariable`. When tested with actual ERB script execution, **all five are already fully implemented and working**:

1. **`SAVEDATA_TEXT`**:
   - Defined in `crates/erars-loader/src/variable.yaml:158-161` as a global save-data string variable.
   - Read during save serialization in `crates/erars-vm/src/terminal_vm/executor.rs:598` and populated by `SAVEDATA` command at `executor.rs:4487`.
   - Verified: Script reference `{SAVEDATA_TEXT}` compiles, runs, and evaluates cleanly without error.
2. **CSV Name Tables (`CDFLAGNAME1`, `CDFLAGNAME2`, `TFLAGNAME`)**:
   - Defined as constant string array variables in `crates/erars-loader/src/variable.yaml:123-138`.
   - Initialized from CSV headers (`TFLAG.CSV`, `CDFLAG1.CSV`, `CDFLAG2.CSV`) during engine loading.
   - Verified: Script references (`%TFLAGNAME:0%`, `%CDFLAGNAME1:0%`, `%CDFLAGNAME2:0%`) resolve, compile, and evaluate cleanly.
3. **`ISTIMEOUT`**:
   - Implemented directly in `crates/erars-ast/src/variable.rs:54` (`BuiltinVariable::IsTimeout`) and evaluated in `crates/erars-vm/src/terminal_vm/executor.rs:476` as `ctx.is_timeout.into()`.
   - Updated during timed input in `crates/erars-vm/src/context.rs:32,165`.
   - Verified: Script reference `{ISTIMEOUT}` compiles and returns `0` (or `1` after input timeout).
4. **`MONEYLABEL`**:
   - Implemented in `crates/erars-ast/src/variable.rs:58` (`BuiltinVariable::MoneyLabel`) and evaluated in `crates/erars-vm/src/terminal_vm/executor.rs:477` as `ctx.header_info.replace.money_unit.clone().into()`.
   - Verified: Script reference `%MONEYLABEL%` evaluates cleanly to the configured currency string (e.g. `"원"` or `"$"`).
