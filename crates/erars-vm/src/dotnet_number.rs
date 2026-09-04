//! .NET's `Int64.ToString(format)` — the formatter behind the optional second
//! argument of `MONEYSTR` (`Creator.Method.cs:2785`) and `TOSTR`
//! (`Creator.Method.cs:4430-4447`).
//!
//! Ported from the reference implementation in `dotnet/runtime`:
//! `System.Private.CoreLib/src/System/Number.Formatting.cs` (`FormatInt64`,
//! `Int64ToNumber`) and `Common/src/System/Number.Formatting.Common.cs`
//! (`ParseFormatSpecifier`, `NumberToStringFormat`, `RoundNumber`,
//! `FindSection`, `FormatExponent`). Citations of the form `Common.cs:NNN`
//! below point at the latter.
//!
//! DELIBERATE DEVIATION: every culture-sensitive value is
//! `CultureInfo.InvariantCulture`'s. Emuera inherits the host
//! `CurrentCulture`; a VM has no host locale. See
//! `docs/research/2026-09-03-emuera-command-gap.md` §5.

use anyhow::{anyhow, bail, Result};
use erars_ast::BuiltinMethod;
use std::fmt::Write as _;

/// The optional format argument of `TOSTR` and `MONEYSTR`.
///
/// Emuera catches the `FormatException` an unknown standard specifier raises
/// and reports one message for both methods, naming the method and the
/// argument index and dropping the exception's own text
/// (`Lang.Error.InvalidFormat`, `_Library/EvilMask/Lang.cs:1135`;
/// `Creator.Method.cs:2787-2791` and `:4441-4445` differ only in `Name`). The
/// offending format is appended, which Emuera has no room for in its template.
pub(crate) fn format_arg(meth: BuiltinMethod, value: i64, format: &str) -> Result<String> {
    format_number(value, format)
        .map_err(|_| anyhow!("{meth} 메소드: 제2인수의 서식 지정이 잘못되었습니다: {format}"))
}

/// `Int64.ToString(format)`. .NET first tries to read the whole string as a
/// *standard* numeric format — one ASCII letter plus an optional run of digits
/// — and otherwise treats it as a *custom* pattern. For an integral type the
/// only valid standard letters are `C D E F G N P X`; any other single letter
/// raises `FormatException`, which Emuera converts into `CodeEE` with
/// `Lang.Error.InvalidFormat` (`Creator.Method.cs:2787-2791`,
/// `_Library/EvilMask/Lang.cs:1135`). Custom patterns never throw.
///
/// Integral `ToString` formats from the exact decimal digits (`Int64ToNumber`),
/// never through `double`, and `RoundNumber` rounds half away from zero — so
/// `E`/`G` rounding here is exact decimal rounding.
///
/// DELIBERATE DEVIATION: `C` and `P` embed culture data and Emuera inherits the
/// host `CurrentCulture`; a VM has no host locale, so erars fixes both to
/// `CultureInfo.InvariantCulture` (`¤` currency symbol, `(¤n)` for negatives,
/// `n %` for percents). `,`/`.` separators are identical in the invariant,
/// `en-US` and `ja-JP` cultures, so `N`/`F`/`E`/`G` are unaffected.
fn format_number(value: i64, format: &str) -> Result<String> {
    let neg = value < 0;
    let digits = value.unsigned_abs().to_string();

    // An empty (or null) format means "G".
    let Some(letter) = format.chars().next() else {
        return Ok(sign(neg, digits));
    };

    if letter.is_ascii_alphabetic() {
        let rest = &format[letter.len_utf8()..];
        // `ParseFormatSpecifier` accepts a digit run of at most 999999999;
        // anything else demotes the string to a custom pattern.
        let precision = if rest.is_empty() {
            Some(None)
        } else if rest.bytes().all(|b| b.is_ascii_digit()) {
            rest.parse::<u32>().ok().filter(|p| *p <= 999_999_999).map(Some)
        } else {
            None
        };

        if let Some(precision) = precision {
            return standard_format(value, &digits, neg, letter, precision);
        }
    }

    Ok(custom_format(value, format))
}

fn sign(neg: bool, mut s: String) -> String {
    if neg {
        s.insert(0, '-');
    }
    s
}

fn zeros(n: usize) -> std::iter::Take<std::iter::Repeat<char>> {
    std::iter::repeat('0').take(n)
}

/// One .NET standard numeric format specifier applied to an `Int64`.
fn standard_format(
    value: i64,
    digits: &str,
    neg: bool,
    letter: char,
    precision: Option<u32>,
) -> Result<String> {
    let p = precision.map(|p| p as usize);

    Ok(match letter.to_ascii_uppercase() {
        // Zero-padded to at least `precision` digits; no grouping.
        'D' => {
            let width = p.unwrap_or(0).max(digits.len());
            let mut s = String::with_capacity(width + neg as usize);
            s.extend(zeros(width - digits.len()));
            s.push_str(digits);
            sign(neg, s)
        }
        // Two's-complement hex, zero-padded to `precision`; case follows the
        // specifier and the sign is part of the bit pattern.
        'X' => {
            let width = p.unwrap_or(0);
            if letter.is_ascii_lowercase() {
                format!("{value:0width$x}")
            } else {
                format!("{value:0width$X}")
            }
        }
        // Fixed point, default 2 decimals. An integer has no fraction, so the
        // decimals are always zeros.
        'F' => {
            let p = p.unwrap_or(2);
            let mut s = String::with_capacity(digits.len() + p + 2);
            s.push_str(digits);
            push_fraction(&mut s, p);
            sign(neg, s)
        }
        // Grouped, default 2 decimals; `NumberNegativePattern` 1 => "-n".
        'N' => {
            let p = p.unwrap_or(2);
            let mut s = group_digits(digits);
            push_fraction(&mut s, p);
            sign(neg, s)
        }
        // Invariant currency: `CurrencyDecimalDigits` 2,
        // `CurrencyPositivePattern` 0 => "¤n", `CurrencyNegativePattern` 0 =>
        // "(¤n)".
        'C' => {
            let p = p.unwrap_or(2);
            let mut s = group_digits(digits);
            push_fraction(&mut s, p);
            s.insert(0, '¤');
            if neg {
                s.insert(0, '(');
                s.push(')');
            }
            s
        }
        // Percent: the value is scaled by 100 first. Invariant
        // `PercentPositivePattern`/`PercentNegativePattern` 0 => "n %"/"-n %".
        'P' => {
            let p = p.unwrap_or(2);
            let scaled = (value.unsigned_abs() as u128 * 100).to_string();
            let mut s = group_digits(&scaled);
            push_fraction(&mut s, p);
            s.push_str(" %");
            sign(neg, s)
        }
        // Scientific: `precision` mantissa decimals (default 6), exponent
        // always signed and at least three digits.
        'E' => {
            let p = p.unwrap_or(6);
            let (mantissa, carry) = round_significant(digits, p + 1);
            let exp = digits.len() as i32 - 1 + carry;
            let mut s = String::with_capacity(p + 8);
            s.push_str(&mantissa[..1]);
            if p > 0 {
                s.push('.');
                s.push_str(&mantissa[1..]);
            }
            s.push(if letter.is_ascii_lowercase() { 'e' } else { 'E' });
            s.push(if exp < 0 { '-' } else { '+' });
            let _ = write!(s, "{:03}", exp.unsigned_abs());
            sign(neg, s)
        }
        // General: `precision` significant digits, switching to scientific
        // (two-digit exponent) once the exponent reaches the precision.
        'G' => {
            let p = p.unwrap_or(0);
            if p == 0 || digits.len() <= p {
                return Ok(sign(neg, digits.to_owned()));
            }

            let (mantissa, carry) = round_significant(digits, p);
            let exp = digits.len() as i32 - 1 + carry;
            let mantissa = mantissa.trim_end_matches('0');
            let mut s = String::with_capacity(p + 7);
            s.push_str(&mantissa[..1]);
            if mantissa.len() > 1 {
                s.push('.');
                s.push_str(&mantissa[1..]);
            }
            s.push(if letter.is_ascii_lowercase() { 'e' } else { 'E' });
            s.push(if exp < 0 { '-' } else { '+' });
            let _ = write!(s, "{:02}", exp.unsigned_abs());
            sign(neg, s)
        }
        // `R` is documented for Single/Double/BigInteger only, and `B` postdates
        // Emuera's .NET Framework target; both land here with every other
        // letter. `FormatException`'s own text never reaches the script —
        // `format_arg` renders the message Emuera reports — so this one only
        // has to say what was rejected.
        _ => bail!("`{letter}`는 정수에 사용할 수 없는 서식 지정자입니다"),
    })
}

/// A decimal point plus `precision` zeros — an integer has no fraction.
fn push_fraction(out: &mut String, precision: usize) {
    if precision > 0 {
        out.push('.');
        out.extend(zeros(precision));
    }
}

/// Digits grouped every three from the right, invariant separator `,`.
fn group_digits(digits: &str) -> String {
    let mut out = String::with_capacity(digits.len() + digits.len() / 3 + 3);
    for (i, c) in digits.char_indices() {
        if i > 0 && (digits.len() - i) % 3 == 0 {
            out.push(',');
        }
        out.push(c);
    }
    out
}

/// Round a decimal digit string to `sig` significant digits, half away from
/// zero, padding with zeros when it is already shorter. The second element is
/// `1` when the rounding carried into a new leading digit (`999` → `1.00e3`).
fn round_significant(digits: &str, sig: usize) -> (String, i32) {
    let sig = sig.max(1);
    if sig >= digits.len() {
        let mut s = String::with_capacity(sig);
        s.push_str(digits);
        s.extend(zeros(sig - digits.len()));
        return (s, 0);
    }

    let mut kept = digits.as_bytes()[..sig].to_vec();
    if digits.as_bytes()[sig] < b'5' {
        return (String::from_utf8(kept).expect("ASCII digits"), 0);
    }

    // Propagate the carry right-to-left; an all-nines mantissa becomes 1
    // followed by zeros and bumps the exponent.
    for i in (0..sig).rev() {
        if kept[i] == b'9' {
            kept[i] = b'0';
        } else {
            kept[i] += 1;
            return (String::from_utf8(kept).expect("ASCII digits"), 0);
        }
    }
    kept[0] = b'1';
    (String::from_utf8(kept).expect("ASCII digits"), 1)
}

// ---------------------------------------------------------------------------
// Custom patterns: `NumberToStringFormat` (`Common.cs:479-958`)
// ---------------------------------------------------------------------------

/// `CultureInfo.InvariantCulture`'s `NumberGroupSeparator`,
/// `NumberDecimalSeparator`, `NegativeSign`, `PositiveSign`, `PercentSymbol`
/// and `PerMilleSymbol` (`Globalization/NumberFormatInfo.cs`).
const GROUP_SEPARATOR: char = ',';
const DECIMAL_SEPARATOR: char = '.';
const NEGATIVE_SIGN: char = '-';
const POSITIVE_SIGN: char = '+';
const PERCENT_SYMBOL: char = '%';
const PER_MILLE_SYMBOL: char = '\u{2030}';
/// Invariant `NumberGroupSizes` is `[3]`, so the reference's walk over a
/// variable-size group array collapses to one repeating group of three.
const GROUP_SIZE: i64 = 3;

/// The decimal digits of an `Int64` with a decimal exponent and a sign —
/// .NET's `NumberBuffer` for `NumberBufferKind.Integer`
/// (`Number.Formatting.cs:2066`, `Int64ToNumber`).
///
/// `Digits` is NUL-terminated and every zero test in the reference is
/// `Digits[0] == 0`, i.e. the terminator rather than the character `'0'`: the
/// value zero has no digits at all.
struct NumberBuffer {
    /// NUL-terminated ASCII digits. 19 digits is `i64::MIN`'s magnitude and
    /// `RoundNumber` never lengthens the run, so 20 bytes always suffice —
    /// this is .NET's own `Int64NumberBufferLength`.
    digits: [u8; 20],
    digits_count: usize,
    scale: i64,
    is_negative: bool,
}

impl NumberBuffer {
    fn from_i64(value: i64) -> Self {
        // C# relies on an unchecked negation wrapping into the right `ulong`
        // magnitude for `long.MinValue`; `unsigned_abs` is the panic-free
        // equivalent.
        let mut magnitude = value.unsigned_abs();
        let mut digits_count = 0;
        let mut rest = magnitude;
        while rest != 0 {
            digits_count += 1;
            rest /= 10;
        }

        let mut digits = [0u8; 20];
        for slot in digits[..digits_count].iter_mut().rev() {
            *slot = b'0' + (magnitude % 10) as u8;
            magnitude /= 10;
        }

        Self {
            digits,
            digits_count,
            scale: digits_count as i64,
            is_negative: value < 0,
        }
    }

    fn digits(&self) -> &[u8] {
        &self.digits[..self.digits_count]
    }

    fn is_zero(&self) -> bool {
        self.digits[0] == 0
    }
}

/// `RoundNumber` (`Common.cs:1353`) for an integral buffer, where
/// `isCorrectlyRounded` is always false and the kind is never floating point:
/// `ShouldRoundUp` reduces to "the first dropped digit is `5` or more", i.e.
/// half away from zero. `pos` may be negative or past the digit run.
fn round_number(number: &mut NumberBuffer, pos: i64) {
    let mut i = 0;
    while (i as i64) < pos && number.digits[i] != 0 {
        i += 1;
    }

    if i as i64 == pos && number.digits[i] >= b'5' {
        while i > 0 && number.digits[i - 1] == b'9' {
            i -= 1;
        }
        if i > 0 {
            number.digits[i - 1] += 1;
        } else {
            number.scale += 1;
            number.digits[0] = b'1';
            i = 1;
        }
    } else {
        while i > 0 && number.digits[i - 1] == b'0' {
            i -= 1;
        }
    }

    if i == 0 {
        // The integer types have no concept of -0.
        number.is_negative = false;
        number.scale = 0;
    }

    number.digits[i] = 0;
    number.digits_count = i;
}

/// Offset of the `section`'th `;`-separated section, or 0 when the pattern has
/// no such section (`Common.cs:1465`). Quoted runs and `\` escapes hide a `;`.
fn find_section(format: &[char], mut section: u8) -> usize {
    if section == 0 {
        return 0;
    }

    let mut src = 0;
    loop {
        if src >= format.len() {
            return 0;
        }
        let ch = format[src];
        src += 1;
        match ch {
            '\'' | '"' => src = skip_quoted(format, src, ch),
            '\\' => src = skip_escaped(format, src),
            ';' => {
                section -= 1;
                if section == 0 {
                    return match format.get(src) {
                        Some(&next) if next != '\0' && next != ';' => src,
                        _ => 0,
                    };
                }
            }
            '\0' => return 0,
            _ => {}
        }
    }
}

/// Does the pattern define a dedicated negative section? `FindSection` returns
/// 0 both for the first section and for a missing one, so a non-zero offset is
/// the reliable test (`Common.cs:1463`).
fn has_negative_section(format: &[char]) -> bool {
    find_section(format, 1) != 0
}

/// `'…'` / `"…"`: skip past the matching quote, or to the end of the pattern
/// when it is never closed (`Common.cs:519`).
fn skip_quoted(format: &[char], mut src: usize, quote: char) -> usize {
    while src < format.len() && format[src] != '\0' {
        let ch = format[src];
        src += 1;
        if ch == quote {
            break;
        }
    }
    src
}

/// `\x`: the next character is a literal (`Common.cs:526`).
fn skip_escaped(format: &[char], src: usize) -> usize {
    match format.get(src) {
        Some(&ch) if ch != '\0' => src + 1,
        _ => src,
    }
}

/// Is the `E`/`e` just consumed the head of an exponent placeholder — `E0`,
/// `E+0` or `E-0` (`Common.cs:534`)?
fn is_exponent_pattern(format: &[char], src: usize) -> bool {
    match format.get(src).copied() {
        Some('0') => true,
        Some('+' | '-') => format.get(src + 1) == Some(&'0'),
        _ => false,
    }
}

/// Consume an exponent placeholder's optional sign and its `0` run, returning
/// the new position and the number of `0`s — the exponent's minimum width
/// (`Common.cs:536`, `Common.cs:889`).
fn scan_exponent(format: &[char], mut src: usize) -> (usize, usize) {
    let mut zero_count = usize::from(format.get(src) == Some(&'0'));
    src += 1;
    while format.get(src) == Some(&'0') {
        src += 1;
        zero_count += 1;
    }
    (src, zero_count)
}

/// `FormatExponent` (`Common.cs:1236`): the exponent character, a sign (always
/// for a negative exponent, only with `E+0` for a positive one) and the
/// magnitude zero-padded to `min_digits`.
fn format_exponent(out: &mut String, value: i64, exp_char: char, min_digits: usize, positive_sign: bool) {
    out.push(exp_char);
    if value < 0 {
        out.push(NEGATIVE_SIGN);
    } else if positive_sign {
        out.push(POSITIVE_SIGN);
    }
    let _ = write!(out, "{:0min_digits$}", value.unsigned_abs());
}

/// Where `NumberToStringFormat` inserts `NumberGroupSeparator`, precomputed so
/// the emit pass can walk the pattern forwards (`Common.cs:652-702`).
struct Grouping {
    /// Digit positions after which a separator goes; with the invariant group
    /// size of three these are 3, 6, 9, …
    positions: Vec<i64>,
    /// Index of the next separator to place, walked down to -1.
    ctr: i64,
}

impl Grouping {
    fn new(enabled: bool, dig_pos: i64, adjust: i64, first_digit: i64) -> Self {
        let mut positions = Vec::new();
        if enabled {
            let total_digits = dig_pos + adjust.min(0);
            let num_digits = first_digit.max(total_digits);
            let mut group_total = GROUP_SIZE;
            while num_digits > group_total {
                positions.push(group_total);
                group_total += GROUP_SIZE;
            }
        }
        let ctr = positions.len() as i64 - 1;
        Self { positions, ctr }
    }

    /// `digPos` is one greater than the recorded position because the
    /// separator goes *after* the digit just emitted.
    fn push_separator(&mut self, out: &mut String, dig_pos: i64) {
        if dig_pos <= 1 || self.ctr < 0 {
            return;
        }
        if self.positions.get(self.ctr as usize) == Some(&(dig_pos - 1)) {
            out.push(GROUP_SEPARATOR);
            self.ctr -= 1;
        }
    }
}

/// A .NET custom numeric format pattern applied to an `Int64`
/// (`Common.cs:479`, `NumberToStringFormat`).
///
/// The pattern is scanned twice: once to measure it — placeholder counts, the
/// decimal position, `%`/`‰`/trailing-comma scaling and whether it is
/// scientific — and once to emit. Supported: `0` and `#` placeholders, `.`,
/// `,` grouping, trailing-comma ÷1000 scaling, `%` (×100) and U+2030 (×1000)
/// scaling, `E0`/`E+0`/`E-0` exponents, `\` escapes, `'…'`/`"…"` literal runs
/// and up to three `;` sections (positive; negative; zero). Every other
/// character is copied out literally, so a custom pattern never fails.
///
/// The reference indexes UTF-16 code units and reassembles surrogate pairs
/// with `AppendSurrogatePair`; a Rust `char` is already a scalar value, so that
/// handling collapses into "copy the character".
fn custom_format(value: i64, format: &str) -> String {
    let format: Vec<char> = format.chars().collect();
    let format = format.as_slice();
    let mut number = NumberBuffer::from_i64(value);

    let mut section = find_section(
        format,
        if number.is_zero() {
            2
        } else if number.is_negative {
            1
        } else {
            0
        },
    );

    // `thousandCount` is deliberately not reset between retries.
    let mut thousand_count = 0i64;
    let mut digit_count;
    let mut decimal_pos;
    let mut first_digit;
    let mut last_digit;
    let mut scientific;
    let mut thousand_seps;
    let mut src;

    // Pass one: measure the chosen section, then round the value to the
    // precision the pattern asks for. Rounding the value away entirely hands
    // it to the zero section, which has to be measured in turn.
    loop {
        digit_count = 0i64;
        decimal_pos = -1i64;
        first_digit = i64::MAX;
        last_digit = 0i64;
        scientific = false;
        thousand_seps = false;
        let mut thousand_pos = -1i64;
        let mut scale_adjust = 0i64;
        src = section;

        while src < format.len() {
            let ch = format[src];
            src += 1;
            match ch {
                '\0' | ';' => break,
                '#' => digit_count += 1,
                '0' => {
                    if first_digit == i64::MAX {
                        first_digit = digit_count;
                    }
                    digit_count += 1;
                    last_digit = digit_count;
                }
                '.' => {
                    if decimal_pos < 0 {
                        decimal_pos = digit_count;
                    }
                }
                ',' => {
                    if digit_count > 0 && decimal_pos < 0 {
                        if thousand_pos == digit_count {
                            thousand_count += 1;
                        } else {
                            thousand_seps |= thousand_pos >= 0;
                            thousand_pos = digit_count;
                            thousand_count = 1;
                        }
                    }
                }
                '%' => scale_adjust += 2,
                PER_MILLE_SYMBOL => scale_adjust += 3,
                '\'' | '"' => src = skip_quoted(format, src, ch),
                '\\' => src = skip_escaped(format, src),
                'E' | 'e' => {
                    if is_exponent_pattern(format, src) {
                        src = scan_exponent(format, src).0;
                        scientific = true;
                    }
                }
                _ => {}
            }
        }

        if decimal_pos < 0 {
            decimal_pos = digit_count;
        }
        if thousand_pos >= 0 {
            if thousand_pos == decimal_pos {
                // Commas immediately before the decimal point are not
                // grouping: each divides the value by 1000.
                scale_adjust -= thousand_count * GROUP_SIZE;
            } else {
                thousand_seps = true;
            }
        }

        if !number.is_zero() {
            number.scale += scale_adjust;
            let pos = if scientific {
                digit_count
            } else {
                number.scale + digit_count - decimal_pos
            };
            round_number(&mut number, pos);
            if number.is_zero() {
                let zero_section = find_section(format, 2);
                if zero_section != section {
                    section = zero_section;
                    continue;
                }
            }
        } else {
            // The integer types have no concept of -0.
            number.is_negative = false;
            number.scale = 0;
        }

        break;
    }

    // `firstDigit`/`lastDigit` become offsets from the decimal point, and
    // `adjust` the number of digits the value overflows the pattern by
    // (negative when the pattern is wider than the value).
    first_digit = if first_digit < decimal_pos {
        decimal_pos - first_digit
    } else {
        0
    };
    last_digit = if last_digit > decimal_pos {
        decimal_pos - last_digit
    } else {
        0
    };
    let (mut dig_pos, mut adjust) = if scientific {
        (decimal_pos, 0)
    } else {
        (number.scale.max(decimal_pos), number.scale - decimal_pos)
    };
    let mut grouping = Grouping::new(thousand_seps, dig_pos, adjust, first_digit);

    let mut out = String::new();

    // A dedicated negative section owns the sign of negative values; emitting
    // one here as well would produce output such as "-+0.00".
    if number.is_negative && section == 0 && number.scale != 0 && !has_negative_section(format) {
        out.push(NEGATIVE_SIGN);
    }

    // Pass two: walk the same section again and emit.
    let digits = number.digits();
    let mut cur_index = 0;
    let mut decimal_written = false;
    src = section;

    while src < format.len() {
        let ch = format[src];
        src += 1;
        if ch == '\0' || ch == ';' {
            break;
        }

        // Digits that overflow the pattern on the left are flushed in front of
        // its first placeholder (`Common.cs:735`).
        if adjust > 0 && matches!(ch, '#' | '0' | '.') {
            let overflow = (adjust as usize).min(digits.len());
            for &digit in &digits[..overflow] {
                out.push(char::from(digit));
                grouping.push_separator(&mut out, dig_pos);
                dig_pos -= 1;
                adjust -= 1;
            }
            cur_index = overflow;
            while adjust > 0 {
                out.push('0');
                grouping.push_separator(&mut out, dig_pos);
                dig_pos -= 1;
                adjust -= 1;
            }
        }

        match ch {
            '#' | '0' => {
                // A `\0` here means "emit nothing": `#` past the value, or a
                // pattern position the value does not reach.
                let emit = if adjust < 0 {
                    adjust += 1;
                    (dig_pos <= first_digit).then_some('0')
                } else if cur_index < digits.len() {
                    let digit = char::from(digits[cur_index]);
                    cur_index += 1;
                    Some(digit)
                } else {
                    (dig_pos > last_digit).then_some('0')
                };

                if let Some(digit) = emit {
                    out.push(digit);
                    grouping.push_separator(&mut out, dig_pos);
                }
                dig_pos -= 1;
            }
            '.' => {
                // Repeated decimal points are not echoed, and the separator is
                // only written when the pattern has trailing zeros or digits
                // remain to fill it.
                if dig_pos == 0
                    && !decimal_written
                    && (last_digit < 0 || (decimal_pos < digit_count && cur_index < digits.len()))
                {
                    out.push(DECIMAL_SEPARATOR);
                    decimal_written = true;
                }
            }
            PER_MILLE_SYMBOL => out.push(PER_MILLE_SYMBOL),
            '%' => out.push(PERCENT_SYMBOL),
            ',' => {}
            '\'' | '"' => {
                while let Some(&quoted) = format.get(src) {
                    if quoted == '\0' || quoted == ch {
                        break;
                    }
                    src += 1;
                    out.push(quoted);
                }
                if let Some(&closing) = format.get(src) {
                    if closing != '\0' {
                        src += 1;
                    }
                }
            }
            '\\' => {
                if let Some(&literal) = format.get(src) {
                    if literal != '\0' {
                        out.push(literal);
                        src += 1;
                    }
                }
            }
            'E' | 'e' if scientific => {
                if is_exponent_pattern(format, src) {
                    let positive_sign = format.get(src) == Some(&'+');
                    let (next, zero_count) = scan_exponent(format, src);
                    src = next;
                    let exp = if number.is_zero() {
                        0
                    } else {
                        number.scale - decimal_pos
                    };
                    format_exponent(&mut out, exp, ch, zero_count.min(10), positive_sign);
                    scientific = false;
                } else {
                    out.push(ch);
                }
            }
            'E' | 'e' => {
                // Not a scientific pattern (or the exponent is already
                // written): echo the placeholder verbatim.
                out.push(ch);
                if matches!(format.get(src).copied(), Some('+' | '-')) {
                    out.push(format[src]);
                    src += 1;
                }
                while format.get(src) == Some(&'0') {
                    out.push('0');
                    src += 1;
                }
            }
            _ => out.push(ch),
        }
    }

    // A value that rounded down to a scale of 0 still carries its sign, and
    // section 0 has to supply it (for example -0.5 under "0,.0%").
    if number.is_negative && section == 0 && number.scale == 0 && !out.is_empty() && !has_negative_section(format) {
        out.insert(0, NEGATIVE_SIGN);
    }

    out
}

#[cfg(test)]
mod tests {
    use super::format_number;

    /// Standard specifiers, hand-derived from `FormatInt64`
    /// (`Number.Formatting.cs:1299`) and `NumberToString` (`Common.cs:330`).
    #[test]
    fn standard_specifiers() {
        let cases: &[(i64, &str, &str)] = &[
            (12345, "", "12345"),
            (12345, "N0", "12,345"),
            (12345, "N", "12,345.00"),
            (12345, "F2", "12345.00"),
            (5, "F", "5.00"),
            (7, "D3", "007"),
            (255, "X", "FF"),
            (255, "x4", "00ff"),
            (255, "X6", "0000FF"),
            (12, "D5", "00012"),
            (1234567, "N0", "1,234,567"),
            (12, "F2", "12.00"),
            (-1, "X", "FFFFFFFFFFFFFFFF"),
            (12345, "C", "¤12,345.00"),
            (-12345, "C", "(¤12,345.00)"),
            (12345, "C0", "¤12,345"),
            (5, "P", "500.00 %"),
            (-5, "P0", "-500 %"),
            (12345, "E", "1.234500E+004"),
            (12345, "E2", "1.23E+004"),
            (12999, "e2", "1.30e+004"),
            (999, "E1", "1.0E+003"),
            (0, "E", "0.000000E+000"),
            (12345, "G", "12345"),
            (12345, "G3", "1.23E+04"),
            (100000, "G3", "1E+05"),
            (12345, "G9", "12345"),
            (-12345, "N", "-12,345.00"),
            // `Int64ToNumber` takes the magnitude with an unchecked negation,
            // so `i64::MIN` is not a special case.
            (i64::MIN, "D", "-9223372036854775808"),
            (i64::MIN, "X", "8000000000000000"),
        ];

        for &(value, format, expect) in cases {
            let got = format_number(value, format).unwrap();
            assert_eq!(got, expect, "{value} with {format:?}");
        }
    }

    /// `NumberToString`'s `default:` arm throws `FormatException`
    /// (`Common.cs:474`); `ParseFormatSpecifier` only claims a letter followed
    /// by digits, so anything else is a custom pattern instead.
    #[test]
    fn unknown_standard_specifier_is_an_error() {
        assert!(format_number(1, "Q").is_err());
        // `R` is Single/Double/BigInteger only, and `B` postdates .NET
        // Framework, Emuera's runtime.
        assert!(format_number(1, "R").is_err());
        assert!(format_number(1, "B").is_err());
        assert!(format_number(1, "N-1").is_ok());
    }

    /// Custom patterns, hand-derived from `NumberToStringFormat`
    /// (`Common.cs:479`). Each expectation is a trace of the two passes, not an
    /// observation of this implementation.
    #[test]
    fn custom_patterns() {
        let cases: &[(i64, &str, &str)] = &[
            // `digitCount` 3, `firstDigit` 3, `adjust` -1: the first pass
            // through `#`/`0` consumes the adjustment as a padding zero.
            (12, "000", "012"),
            (12, "0", "12"),
            // `adjust` 2: the overflow digits are flushed at the first
            // placeholder, then the placeholder itself consumes the third.
            (1234, "00", "1234"),
            (-5, "000", "-005"),
            // A quoted `.` is a literal, so the pattern still has three
            // integer placeholders: "012" with the literal after the first.
            (12, "0'.'00", "0.12"),
            (12, "0\".\"00", "0.12"),
            // `adjust` 1 flushes the overflow digit before the first
            // placeholder, so the literal lands after two integer digits:
            // this is the corpus idiom for printing a scaled integer.
            (1234, "0'.'00", "12.34"),
            // A bare `.` is the decimal point: `lastDigit` -2 makes the
            // fraction two padding zeros.
            (12, "0.00", "12.00"),
            (12, "0#", "12"),
            (42, "0#", "42"),
            (12345, "0,000", "12,345"),
            (5, "0.00", "5.00"),
            (0, "0", "0"),
            // `\` escapes the next character.
            (12, "\\d0", "d12"),
            // An unterminated literal run is emitted to the end of the pattern.
            (12, "0'abc", "12abc"),
            (12, "0\"abc", "12abc"),
            // `%` adds 2 to the scale and prints the percent symbol.
            (12, "0%", "1200%"),
            // U+2030 adds 3 to the scale.
            (1234, "0\u{2030}", "1234000\u{2030}"),
            // Two commas before the (implied) decimal point divide by 10^6.
            // 1234567 -> 1.234567, and `RoundNumber(1)` sees `2` -> truncates.
            (1234567, "0,,", "1"),
            // ...and rounds half away from zero: 1.5 -> 2.
            (1500000, "0,,", "2"),
            // `thousandsSepPos` is [3]; the separator lands after digit 1.
            (1234, "#,##0", "1,234"),
            // With `digPos` 7 the positions are [3, 6], both consumed by the
            // overflow flush and the first placeholder.
            (1234567, "#,##0", "1,234,567"),
            // Sections: positive; negative; zero. A value formatted by its own
            // section gets no automatic sign.
            (7, "0;(0);zero", "7"),
            (-5, "0;(0);zero", "(5)"),
            (0, "0;(0);zero", "zero"),
            (-3, "0;(0)", "(3)"),
            // With no third section a zero value falls back to section 0.
            (0, "0;(0)", "0"),
            // No negative section, so section 0 emits the sign itself.
            (-3, "0", "-3"),
            // Scientific: `pos` is `digitCount` (2), so 1234 rounds to 1.2 and
            // the exponent is `Scale - decimalPos` = 4 - 1.
            (1234, "0.0E+0", "1.2E+3"),
            (1234, "0.0E0", "1.2E3"),
            (12, "0.0E-0", "1.2E1"),
            // An `E` not followed by a `0` run is a literal.
            (12, "0EX", "12EX"),
            // No placeholders at all: only the literals are emitted.
            (1, "N-1", "N-1"),
            // |i64::MIN| / 10^6 = 9223372036854.775808, rounded half away from
            // zero at `pos` 13.
            (i64::MIN, "0,,", "-9223372036855"),
        ];

        for &(value, format, expect) in cases {
            let got = format_number(value, format).unwrap();
            assert_eq!(got, expect, "{value} with {format:?}");
        }
    }

    /// A pattern arriving from game script must never panic, whatever it
    /// contains: unterminated quotes, trailing escapes, lone section markers
    /// and huge scale adjustments are all just text.
    #[test]
    fn malformed_patterns_never_panic() {
        let patterns = [
            "'", "\"", "\\", "0\\", ";", ";;", ";;;", "0;", "0;;", ".", ",", ",,", "%", "\u{2030}",
            "E", "E+", "0E", "0E+", "0E-", "#", "##.##", "0.0.0", "0,,,,,,,,,,", "%%%%%%%%%%",
            "\u{2030}\u{2030}\u{2030}", "0'unterminated", "\u{0}", "0\u{0}000", "가나다",
        ];

        for pattern in patterns {
            for value in [0, 1, -1, i64::MAX, i64::MIN] {
                // Custom patterns never raise `FormatException`.
                format_number(value, pattern).unwrap();
            }
        }
    }
}
