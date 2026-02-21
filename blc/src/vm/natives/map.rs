use std::collections::HashMap;

use super::{HeapObject, NValue, NativeError};

pub(super) fn native_map_empty(_args: &[NValue]) -> Result<NValue, NativeError> {
    Ok(NValue::map_from_hashmap(HashMap::new()))
}

pub(super) fn native_map_insert(args: &[NValue]) -> Result<NValue, NativeError> {
    let map_val = &args[0];
    let key = args[1].clone();
    let val = args[2].clone();
    match map_val.as_heap_ref() {
        HeapObject::Map(entries) => {
            let mut new_map = entries.clone();
            new_map.insert(key, val);
            Ok(NValue::map_from_hashmap(new_map))
        }
        _ => Err(NativeError("Map.insert: expected Map".into())),
    }
}

/// Owning CoW variant of Map.insert. When the map is uniquely owned
/// (Arc refcount == 1), mutates the HashMap in-place (O(1) amortized).
/// Otherwise falls back to clone (O(n)).
pub(super) fn native_map_insert_owning(args: Vec<NValue>) -> Result<NValue, NativeError> {
    let mut args = args;
    let val = args.pop().unwrap();
    let key = args.pop().unwrap();
    let map_val = args.pop().unwrap();

    // Try CoW: unwrap the heap object if we're the sole owner
    match map_val.try_unwrap_heap() {
        Ok(HeapObject::Map(mut entries)) => {
            // Sole owner — mutate in place, no clone needed
            entries.insert(key, val);
            Ok(NValue::map_from_hashmap(entries))
        }
        Ok(_) => Err(NativeError("Map.insert: expected Map".into())),
        Err(map_val) => {
            // Aliased — fall back to clone
            match map_val.as_heap_ref() {
                HeapObject::Map(entries) => {
                    let mut new_map = entries.clone();
                    new_map.insert(key, val);
                    Ok(NValue::map_from_hashmap(new_map))
                }
                _ => Err(NativeError("Map.insert: expected Map".into())),
            }
        }
    }
}

pub(super) fn native_map_get(args: &[NValue]) -> Result<NValue, NativeError> {
    let map_val = &args[0];
    let key = &args[1];
    match map_val.as_heap_ref() {
        HeapObject::Map(entries) => match entries.get(key) {
            Some(v) => Ok(NValue::enum_val("Some".into(), v.clone())),
            None => Ok(NValue::enum_val("None".into(), NValue::unit())),
        },
        _ => Err(NativeError("Map.get: expected Map".into())),
    }
}

pub(super) fn native_map_remove(args: &[NValue]) -> Result<NValue, NativeError> {
    let map_val = &args[0];
    let key = &args[1];
    match map_val.as_heap_ref() {
        HeapObject::Map(entries) => {
            let mut new_map = entries.clone();
            new_map.remove(key);
            Ok(NValue::map_from_hashmap(new_map))
        }
        _ => Err(NativeError("Map.remove: expected Map".into())),
    }
}

/// Owning CoW variant of Map.remove. When the map is uniquely owned,
/// removes the key in-place (O(1)). Otherwise falls back to clone.
pub(super) fn native_map_remove_owning(args: Vec<NValue>) -> Result<NValue, NativeError> {
    let mut args = args;
    let key = args.pop().unwrap();
    let map_val = args.pop().unwrap();

    match map_val.try_unwrap_heap() {
        Ok(HeapObject::Map(mut entries)) => {
            entries.remove(&key);
            Ok(NValue::map_from_hashmap(entries))
        }
        Ok(_) => Err(NativeError("Map.remove: expected Map".into())),
        Err(map_val) => {
            match map_val.as_heap_ref() {
                HeapObject::Map(entries) => {
                    let mut new_map = entries.clone();
                    new_map.remove(&key);
                    Ok(NValue::map_from_hashmap(new_map))
                }
                _ => Err(NativeError("Map.remove: expected Map".into())),
            }
        }
    }
}

pub(super) fn native_map_contains(args: &[NValue]) -> Result<NValue, NativeError> {
    let map_val = &args[0];
    let key = &args[1];
    match map_val.as_heap_ref() {
        HeapObject::Map(entries) => {
            let found = entries.contains_key(key);
            Ok(NValue::bool(found))
        }
        _ => Err(NativeError("Map.contains: expected Map".into())),
    }
}

pub(super) fn native_map_keys(args: &[NValue]) -> Result<NValue, NativeError> {
    let map_val = &args[0];
    match map_val.as_heap_ref() {
        HeapObject::Map(entries) => {
            let keys: Vec<_> = entries.keys().cloned().collect();
            Ok(NValue::list(keys))
        }
        _ => Err(NativeError("Map.keys: expected Map".into())),
    }
}

pub(super) fn native_map_values(args: &[NValue]) -> Result<NValue, NativeError> {
    let map_val = &args[0];
    match map_val.as_heap_ref() {
        HeapObject::Map(entries) => {
            let vals: Vec<_> = entries.values().cloned().collect();
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
            let mut entries = HashMap::new();
            for item in items {
                if let Some(tuple) = item.as_tuple() {
                    if tuple.len() >= 2 {
                        entries.insert(tuple[0].clone(), tuple[1].clone());
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
            Ok(NValue::map_from_hashmap(entries))
        }
        None => Err(NativeError(format!(
            "Map.from_list: expected List, got {}",
            args[0]
        ))),
    }
}
