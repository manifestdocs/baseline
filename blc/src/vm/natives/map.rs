use super::{HeapObject, NValue, NativeError};

pub(super) fn native_map_empty(_args: &[NValue]) -> Result<NValue, NativeError> {
    Ok(NValue::map(Vec::new()))
}

pub(super) fn native_map_insert(args: &[NValue]) -> Result<NValue, NativeError> {
    let map_val = &args[0];
    let key = args[1].clone();
    let val = args[2].clone();
    match map_val.as_heap_ref() {
        HeapObject::Map(entries) => {
            let mut new_entries: Vec<(NValue, NValue)> = entries
                .iter()
                .filter(|(k, _)| *k != key)
                .cloned()
                .collect();
            new_entries.push((key, val));
            Ok(NValue::map(new_entries))
        }
        _ => Err(NativeError("Map.insert: expected Map".into())),
    }
}

pub(super) fn native_map_get(args: &[NValue]) -> Result<NValue, NativeError> {
    let map_val = &args[0];
    let key = &args[1];
    match map_val.as_heap_ref() {
        HeapObject::Map(entries) => {
            for (k, v) in entries {
                if k == key {
                    return Ok(NValue::enum_val("Some".into(), v.clone()));
                }
            }
            Ok(NValue::enum_val("None".into(), NValue::unit()))
        }
        _ => Err(NativeError("Map.get: expected Map".into())),
    }
}

pub(super) fn native_map_remove(args: &[NValue]) -> Result<NValue, NativeError> {
    let map_val = &args[0];
    let key = &args[1];
    match map_val.as_heap_ref() {
        HeapObject::Map(entries) => {
            let new_entries: Vec<_> = entries.iter().filter(|(k, _)| k != key).cloned().collect();
            Ok(NValue::map(new_entries))
        }
        _ => Err(NativeError("Map.remove: expected Map".into())),
    }
}

pub(super) fn native_map_contains(args: &[NValue]) -> Result<NValue, NativeError> {
    let map_val = &args[0];
    let key = &args[1];
    match map_val.as_heap_ref() {
        HeapObject::Map(entries) => {
            let found = entries.iter().any(|(k, _)| k == key);
            Ok(NValue::bool(found))
        }
        _ => Err(NativeError("Map.contains: expected Map".into())),
    }
}

pub(super) fn native_map_keys(args: &[NValue]) -> Result<NValue, NativeError> {
    let map_val = &args[0];
    match map_val.as_heap_ref() {
        HeapObject::Map(entries) => {
            let keys: Vec<_> = entries.iter().map(|(k, _)| k.clone()).collect();
            Ok(NValue::list(keys))
        }
        _ => Err(NativeError("Map.keys: expected Map".into())),
    }
}

pub(super) fn native_map_values(args: &[NValue]) -> Result<NValue, NativeError> {
    let map_val = &args[0];
    match map_val.as_heap_ref() {
        HeapObject::Map(entries) => {
            let vals: Vec<_> = entries.iter().map(|(_, v)| v.clone()).collect();
            Ok(NValue::list(vals))
        }
        _ => Err(NativeError("Map.values: expected Map".into())),
    }
}

pub(super) fn native_map_len(args: &[NValue]) -> Result<NValue, NativeError> {
    let map_val = &args[0];
    match map_val.as_heap_ref() {
        HeapObject::Map(entries) => Ok(NValue::int(entries.len() as i64)),
        _ => Err(NativeError("Map.len: expected Map".into())),
    }
}

pub(super) fn native_map_from_list(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_list() {
        Some(items) => {
            let mut entries: Vec<(NValue, NValue)> = Vec::new();
            for item in items {
                if let Some(tuple) = item.as_tuple() {
                    if tuple.len() >= 2 {
                        entries.push((tuple[0].clone(), tuple[1].clone()));
                    } else {
                        return Err(NativeError(
                            "Map.from_list: each element must be a (key, value) pair".into(),
                        ));
                    }
                } else {
                    return Err(NativeError(
                        "Map.from_list: each element must be a (key, value) tuple".into(),
                    ));
                }
            }
            Ok(NValue::map(entries))
        }
        None => Err(NativeError(format!(
            "Map.from_list: expected List, got {}",
            args[0]
        ))),
    }
}
