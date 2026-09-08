use byteorder::{ReadBytesExt, WriteBytesExt, LE};
use enum_map::EnumMap;
use erars_ast::{
    get_interner, literal_store_strings, restore_literals, update_interner, EventType, Interner,
    StrKey,
};
use hashbrown::HashMap;
use std::{
    io::{Read, Result, Write},
    mem::{size_of, MaybeUninit},
};

use erars_vm::{
    EventCollection, FunctionArgDef, FunctionBody, FunctionDic, FunctionGotoLabel, Instruction,
};

#[cfg(target_endian = "big")]
compile_error!("Doesn't support big endian");

// Bumped for the interner's move off `lasso`: an identifier's key can now
// have gaps behind it (a thread's unfinished reservation block), so the
// identifier block is written as explicit `(key, string)` pairs instead of
// strings alone in key order — an older reader would silently misassign
// every key after the first gap.
//
// Magic values are monotonic and NEVER reused, even when a later change
// reverts back to a format a lower value once named. 11 (pre-`bytecode-opt`),
// 12 (`d0f1162`, P1v2's positions table), 13 (`75a7315`, u16 line encoding),
// and 14 (`454fb16`, P2's LoadStr/LoadVarRef fusion) are all spent: real
// `game.era` files written under each of those still exist (caches,
// mid-arc worktrees, benchmark artifacts) and MUST be rejected, not
// silently misread as whatever format the number is reused for next.
// Reverting `d0f1162`/`75a7315` (see docs/research/2026-09-05-bytecode-dispatch-optimization.md
// §17) restored the pre-P1v2 on-disk layout but bumped to 15 rather than
// rolling back to 11 or 12 for exactly this reason.
//
// 15 (`cc0a35f`-era, the interner's move off `lasso`) is likewise spent: 16
// adds a cache-identity fingerprint word right after the magic, so a 15 file
// read by this code would take the first eight bytes of the identifier block
// as a fingerprint and then misparse everything after it.
const VERSION_MAGIC: &[u8] = &[2, 3, 2, 3, 0, 0, 0, 16];

fn write_function_body<W: Write + WriteBytesExt>(mut out: W, body: &FunctionBody) -> Result<()> {
    unsafe {
        out.write_u32::<LE>(body.file_path.to_u32())?;
        out.write_u8(body.is_function as u8)?;
        out.write_u8(body.is_functions as u8)?;

        macro_rules! write_arr {
            ($field:ident, $ty:ty) => {
                let len = body.$field.len();
                out.write_u32::<LE>(len as u32)?;
                let arr =
                    std::slice::from_raw_parts(body.$field.as_ptr().cast(), len * size_of::<$ty>());
                out.write_all(arr)?;
            };
        }

        write_arr!(goto_labels, FunctionGotoLabel);
        write_arr!(args, FunctionArgDef);
        write_arr!(body, Instruction);
    }

    Ok(())
}

fn read_function_body<R: Read + ReadBytesExt>(mut read: R) -> Result<FunctionBody> {
    unsafe {
        let file_path = StrKey::from_u32(read.read_u32::<LE>()?);
        let is_function = read.read_u8()? != 0;
        let is_functions = read.read_u8()? != 0;

        macro_rules! read_arr {
            ($ty:ty) => {{
                let len = read.read_u32::<LE>()? as usize;
                let mut args: Box<[MaybeUninit<$ty>]> =
                    vec![MaybeUninit::<$ty>::uninit(); len].into_boxed_slice();

                for arg in args.iter_mut() {
                    let arg = std::slice::from_raw_parts_mut(
                        arg.as_mut_ptr().cast(),
                        std::mem::size_of::<$ty>(),
                    );
                    read.read_exact(arg)?;
                }

                let arr: Box<[$ty]> = std::mem::transmute(args);

                arr
            }};
        }

        let goto_labels = read_arr!(FunctionGotoLabel);
        let args = read_arr!(FunctionArgDef);
        let insts = read_arr!(Instruction);

        Ok(FunctionBody {
            file_path,
            is_function,
            is_functions,
            goto_labels,
            args,
            body: insts,
        })
    }
}

/// Read the literal block and hand it back to the store.
///
/// The block is `count`, then the bytes of every literal in slot order,
/// each preceded by its length. Slot order is the point: an instruction was
/// written with its literal keys as raw `u32`, and a key *is* a slot index, so
/// only a restore in the same order makes those instructions mean again what
/// they meant when they were compiled.
fn read_literals<R: Read + ReadBytesExt>(mut read: R) -> Result<()> {
    let count = read.read_u32::<LE>()? as usize;
    let bytes = read.read_u32::<LE>()? as usize;

    // One read for the whole block, then one pass to cut it into the slices
    // `restore_literals` copies into its arena.
    let mut block = vec![0u8; bytes];
    read.read_exact(&mut block)?;

    let mut strings = Vec::with_capacity(count);
    let mut at = 0usize;

    for _ in 0..count {
        let len = u32::from_le_bytes(block[at..at + 4].try_into().unwrap()) as usize;
        at += 4;
        strings.push(std::str::from_utf8(&block[at..at + len]).unwrap());
        at += len;
    }

    restore_literals(&strings);

    Ok(())
}

/// The literal block, as [`read_literals`] expects it.
fn write_literals<W: Write + WriteBytesExt>(mut out: W) -> Result<()> {
    let strings = literal_store_strings();
    let bytes: usize = strings.iter().map(|s| 4 + s.len()).sum();

    out.write_u32::<LE>(strings.len() as u32)?;
    out.write_u32::<LE>(bytes as u32)?;

    for s in strings {
        out.write_u32::<LE>(s.len() as u32)?;
        out.write_all(s.as_bytes())?;
    }

    Ok(())
}

/// Read the identifier block and hand it back to the interner.
///
/// The block is `count`, then `count` `(key, string)` pairs. The key is
/// explicit rather than positional: `Interner::iter` walks a dedup map that
/// can have gaps behind any key — a thread's reservation block it never
/// finished — so only the key `iter` actually reported for a string
/// reproduces the key `restore` (and so every instruction compiled against
/// it) still needs.
fn read_interner<R: Read + ReadBytesExt>(mut read: R) -> Result<Interner> {
    let count = read.read_u32::<LE>()? as usize;
    let bytes = read.read_u32::<LE>()? as usize;

    let mut block = vec![0u8; bytes];
    read.read_exact(&mut block)?;

    let mut pairs = Vec::with_capacity(count);
    let mut at = 0usize;

    for _ in 0..count {
        let key = u32::from_le_bytes(block[at..at + 4].try_into().unwrap());
        at += 4;
        let len = u32::from_le_bytes(block[at..at + 4].try_into().unwrap()) as usize;
        at += 4;
        pairs.push((key, std::str::from_utf8(&block[at..at + len]).unwrap()));
        at += len;
    }

    let interner = Interner::new();
    interner.restore(&pairs);

    Ok(interner)
}

/// The identifier block, as [`read_interner`] expects it.
fn write_interner<W: Write + WriteBytesExt>(mut out: W, interner: &Interner) -> Result<()> {
    let pairs: Vec<(StrKey, &str)> = interner.iter().collect();
    let bytes: usize = pairs.iter().map(|(_, s)| 4 + 4 + s.len()).sum();

    out.write_u32::<LE>(pairs.len() as u32)?;
    out.write_u32::<LE>(bytes as u32)?;

    for (key, s) in pairs {
        out.write_u32::<LE>(key.to_u32())?;
        out.write_u32::<LE>(s.len() as u32)?;
        out.write_all(s.as_bytes())?;
    }

    Ok(())
}

/// Reads just the header: validates `VERSION_MAGIC` and returns the
/// cache-identity fingerprint the file was written with (see
/// `erars_loader::cache_fingerprint`). Comparing that fingerprint is the
/// caller's job — this crate knows the file format, not what the file was
/// built from.
///
/// Separate from `read_from` so a caller can reject a stale cache *before*
/// decoding it. That ordering is load-bearing, not tidiness:
/// `read_from` installs the file's identifier table into the process-global
/// interner (`update_interner` → `Interner::restore`, which requires an
/// untouched interner), so deciding staleness afterwards would mean having
/// already overwritten global state with the stale file's contents.
///
/// A magic mismatch is an `InvalidData` error, not a panic. A stale
/// `game.era` is an ordinary thing to find on disk — every spent magic value
/// names caches that still exist — and the caller can regenerate or report
/// it, which a panic through `--quite` (no logger, no stderr hook) turned
/// into a bare exit status 101 instead.
pub fn read_header<R: Read + ReadBytesExt>(mut read: R) -> Result<u64> {
    let mut buf = vec![0u8; VERSION_MAGIC.len()];

    read.read_exact(&mut buf)?;

    if buf != VERSION_MAGIC {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!(
                "game.era was written by a different erars build (format {:?}, expected {:?}); \
                 delete it and rebuild with --save",
                buf.last(),
                VERSION_MAGIC.last(),
            ),
        ));
    }

    read.read_u64::<LE>()
}

/// Reads a compiled dictionary back, together with its fingerprint.
pub unsafe fn read_from<R: Read + ReadBytesExt>(mut read: R) -> Result<(FunctionDic, u64)> {
    let fingerprint = read_header(&mut read)?;

    let interner = read_interner(&mut read)?;
    update_interner(interner);
    read_literals(&mut read)?;

    let mut normal = HashMap::new();

    let normal_len = read.read_u32::<LE>()?;

    for _ in 0..normal_len {
        let key = StrKey::from_u32(read.read_u32::<LE>()?);
        let body = read_function_body(&mut read)?;
        normal.insert(key, body);
    }

    let mut event: EnumMap<EventType, EventCollection> = EnumMap::default();

    let event_len = read.read_u32::<LE>()?;

    for _ in 0..event_len {
        let ty = read.read_u32::<LE>()?;
        let event_ty = EventType::from_repr(ty as _).unwrap();

        let collection = &mut event[event_ty];

        collection.single = read.read_u8()? != 0;
        collection.only = read.read_u8()? != 0;

        let empty_len = read.read_u32::<LE>()? as usize;
        collection.empty_count = empty_len;
        let events_len = read.read_u32::<LE>()? as usize;
        collection.events.reserve(events_len);

        for _ in 0..events_len {
            collection.events.push(read_function_body(&mut read)?);
        }
    }

    let mut dic = FunctionDic {
        interner: get_interner(),
        event,
        normal,
        // `システム関数の上書きを許可する`'s effect is recomputed below: the
        // override table is a pure function of the registered functions.
        method_overrides: Vec::new(),
        // `イベント関数のCALLを許可する` is applied while functions are
        // registered, so a dictionary read back from `game.era` already has
        // whatever entries it produced.
        compati_call_event: false,
    };

    dic.rebuild_method_overrides();

    Ok((dic, fingerprint))
}

/// `fingerprint` identifies what this dictionary was compiled *from* — the
/// config and the source files. It is stored, never interpreted here.
pub fn write_to<W: Write + WriteBytesExt>(
    mut out: W,
    dic: &FunctionDic,
    fingerprint: u64,
) -> Result<()> {
    out.write_all(VERSION_MAGIC)?;
    out.write_u64::<LE>(fingerprint)?;
    write_interner(&mut out, dic.interner)?;

    write_literals(&mut out)?;

    out.write_u32::<LE>(dic.normal.len() as _)?;

    for (key, body) in dic.normal.iter() {
        out.write_u32::<LE>(key.to_u32())?;
        write_function_body(&mut out, body)?;
    }

    out.write_u32::<LE>(dic.event.len() as _)?;

    for (ev, collection) in dic.event.iter() {
        out.write_u32::<LE>(ev as u32)?;
        out.write_u8(collection.single as u8)?;
        out.write_u8(collection.only as u8)?;
        out.write_u32::<LE>(collection.empty_count as u32)?;
        out.write_u32::<LE>(collection.events.len() as u32)?;
        for body in collection.events.iter() {
            write_function_body(&mut out, body)?;
        }
    }

    Ok(())
}
