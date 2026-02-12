use super::{HeapObject, NValue, NativeError};

pub(super) fn native_set_empty(_args: &[NValue]) -> Result<NValue, NativeError> {
    Ok(NValue::set(Vec::new()))
}

pub(super) fn native_set_insert(args: &[NValue]) -> Result<NValue, NativeError> {
    let set_val = &args[0];
    let elem = args[1].clone();
    match set_val.as_heap_ref() {
        HeapObject::Set(elems) => {
            if elems.contains(&elem) {
                Ok(NValue::set(elems.clone()))
            } else {
                let mut new_elems = elems.clone();
                new_elems.push(elem);
                Ok(NValue::set(new_elems))
            }
        }
        _ => Err(NativeError("Set.insert: expected Set".into())),
    }
}

pub(super) fn native_set_remove(args: &[NValue]) -> Result<NValue, NativeError> {
    let set_val = &args[0];
    let elem = &args[1];
    match set_val.as_heap_ref() {
        HeapObject::Set(elems) => {
            let new_elems: Vec<_> = elems.iter().filter(|e| *e != elem).cloned().collect();
            Ok(NValue::set(new_elems))
        }
        _ => Err(NativeError("Set.remove: expected Set".into())),
    }
}

pub(super) fn native_set_contains(args: &[NValue]) -> Result<NValue, NativeError> {
    let set_val = &args[0];
    let elem = &args[1];
    match set_val.as_heap_ref() {
        HeapObject::Set(elems) => Ok(NValue::bool(elems.contains(elem))),
        _ => Err(NativeError("Set.contains: expected Set".into())),
    }
}

pub(super) fn native_set_union(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_heap_ref(), args[1].as_heap_ref()) {
        (HeapObject::Set(a), HeapObject::Set(b)) => {
            let mut result = a.clone();
            for elem in b {
                if !result.contains(elem) {
                    result.push(elem.clone());
                }
            }
            Ok(NValue::set(result))
        }
        _ => Err(NativeError("Set.union: expected two Sets".into())),
    }
}

pub(super) fn native_set_intersection(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_heap_ref(), args[1].as_heap_ref()) {
        (HeapObject::Set(a), HeapObject::Set(b)) => {
            let result: Vec<_> = a.iter().filter(|e| b.contains(e)).cloned().collect();
            Ok(NValue::set(result))
        }
        _ => Err(NativeError("Set.intersection: expected two Sets".into())),
    }
}

pub(super) fn native_set_len(args: &[NValue]) -> Result<NValue, NativeError> {
    let set_val = &args[0];
    match set_val.as_heap_ref() {
        HeapObject::Set(elems) => Ok(NValue::int(elems.len() as i64)),
        _ => Err(NativeError("Set.len: expected Set".into())),
    }
}

pub(super) fn native_set_from_list(args: &[NValue]) -> Result<NValue, NativeError> {
    let list_val = &args[0];
    match list_val.as_heap_ref() {
        HeapObject::List(items) => {
            let mut result = Vec::new();
            for item in items {
                if !result.contains(item) {
                    result.push(item.clone());
                }
            }
            Ok(NValue::set(result))
        }
        _ => Err(NativeError("Set.from_list: expected List".into())),
    }
}
