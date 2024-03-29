use byteorder::{ReadBytesExt, WriteBytesExt, LE};
use enum_map::EnumMap;
use erars_ast::{get_interner, update_interner, EventType, Interner, StrKey};
use hashbrown::HashMap;
use std::{
    collections::BTreeMap,
    io::{Read, Result, Write},
    mem::{size_of, MaybeUninit},
};

use erars_vm::{
    EventCollection, FunctionArgDef, FunctionBody, FunctionDic, FunctionGotoLabel, Instruction,
};

#[cfg(target_endian = "big")]
compile_error!("Doesn't support big endian");

const VERSION_MAGIC: &[u8] = &[2, 3, 2, 3, 0, 0, 0, 8];

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

pub unsafe fn read_from<R: Read + ReadBytesExt>(mut read: R) -> Result<FunctionDic> {
    let mut buf = vec![0u8; 1024];

    read.read_exact(&mut buf[..VERSION_MAGIC.len()])?;

    if &buf[..VERSION_MAGIC.len()] != VERSION_MAGIC {
        panic!("Invalid file: VERSION MAGIC mismatched")
    }

    let interner = Interner::new();

    let strings_len = read.read_u32::<LE>()?;

    for _ in 0..strings_len {
        let str_len = read.read_u32::<LE>()? as usize;
        if buf.len() < str_len {
            buf.resize(str_len, 0);
        }
        read.read_exact(&mut buf[..str_len])?;
        interner.get_or_intern(std::str::from_utf8(&buf[..str_len]).unwrap());
    }

    update_interner(interner);

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

        let empty_len = read.read_u32::<LE>()? as usize;
        collection.empty_count = empty_len;
        let events_len = read.read_u32::<LE>()? as usize;
        collection.events.reserve(events_len);

        for _ in 0..events_len {
            collection.events.push(read_function_body(&mut read)?);
        }
    }

    let dic = FunctionDic {
        interner: get_interner(),
        event,
        normal,
    };

    Ok(dic)
}

pub fn write_to<W: Write + WriteBytesExt>(mut out: W, dic: &FunctionDic) -> Result<()> {
    let len = dic.interner.len();

    out.write_all(VERSION_MAGIC)?;
    out.write_u32::<LE>(len as _)?;

    let strings = dic.interner.iter().collect::<BTreeMap<StrKey, &str>>();

    for str in strings.values() {
        out.write_u32::<LE>(str.len() as _)?;
        out.write_all(str.as_bytes())?;
    }

    out.write_u32::<LE>(dic.normal.len() as _)?;

    for (key, body) in dic.normal.iter() {
        out.write_u32::<LE>(key.to_u32())?;
        write_function_body(&mut out, body)?;
    }

    out.write_u32::<LE>(dic.event.len() as _)?;

    for (ev, collection) in dic.event.iter() {
        out.write_u32::<LE>(ev as u32)?;
        out.write_u8(collection.single as u8)?;
        out.write_u32::<LE>(collection.empty_count as u32)?;
        out.write_u32::<LE>(collection.events.len() as u32)?;
        for body in collection.events.iter() {
            write_function_body(&mut out, body)?;
        }
    }

    Ok(())
}
