use super::{HeapObject, NValue, NativeError};

pub(super) fn native_list_length(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_list() {
        Some(items) => Ok(NValue::int(items.len() as i64)),
        None => Err(NativeError(format!(
            "List.length: expected List, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_list_head(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_list() {
        Some(items) => {
            if let Some(first) = items.first() {
                Ok(NValue::enum_val("Some".into(), first.clone()))
            } else {
                Ok(NValue::enum_val("None".into(), NValue::unit()))
            }
        }
        None => Err(NativeError(format!(
            "List.head: expected List, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_list_tail(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_list() {
        Some(items) => {
            if items.is_empty() {
                Ok(NValue::list(Vec::new()))
            } else {
                Ok(NValue::list(items[1..].to_vec()))
            }
        }
        None => Err(NativeError(format!(
            "List.tail: expected List, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_list_reverse(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_list() {
        Some(items) => {
            let mut reversed = items.clone();
            reversed.reverse();
            Ok(NValue::list(reversed))
        }
        None => Err(NativeError(format!(
            "List.reverse: expected List, got {}",
            args[0]
        ))),
    }
}

/// Owning CoW variant of List.reverse. Reverses in-place when uniquely owned.
pub(super) fn native_list_reverse_owning(args: Vec<NValue>) -> Result<NValue, NativeError> {
    let list_val = args.into_iter().next().unwrap();
    match list_val.try_unwrap_heap() {
        Ok(HeapObject::List(mut vec)) => {
            vec.reverse();
            Ok(NValue::list(vec))
        }
        Ok(_) => Err(NativeError("List.reverse: expected List".into())),
        Err(list_val) => {
            match list_val.as_list() {
                Some(items) => {
                    let mut reversed = items.clone();
                    reversed.reverse();
                    Ok(NValue::list(reversed))
                }
                None => Err(NativeError(format!(
                    "List.reverse: expected List, got {}",
                    list_val
                ))),
            }
        }
    }
}

fn sort_comparator(a: &NValue, b: &NValue) -> std::cmp::Ordering {
    if a.is_any_int() && b.is_any_int() {
        a.as_any_int().cmp(&b.as_any_int())
    } else if a.is_float() && b.is_float() {
        a.as_float()
            .partial_cmp(&b.as_float())
            .unwrap_or(std::cmp::Ordering::Equal)
    } else if let (Some(x), Some(y)) = (a.as_string(), b.as_string()) {
        x.cmp(y)
    } else {
        std::cmp::Ordering::Equal
    }
}

pub(super) fn native_list_sort(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_list() {
        Some(items) => {
            let mut sorted = items.clone();
            sorted.sort_by(sort_comparator);
            Ok(NValue::list(sorted))
        }
        None => Err(NativeError(format!(
            "List.sort: expected List, got {}",
            args[0]
        ))),
    }
}

/// Owning CoW variant of List.sort. Sorts in-place when uniquely owned.
pub(super) fn native_list_sort_owning(args: Vec<NValue>) -> Result<NValue, NativeError> {
    let list_val = args.into_iter().next().unwrap();
    match list_val.try_unwrap_heap() {
        Ok(HeapObject::List(mut vec)) => {
            vec.sort_by(sort_comparator);
            Ok(NValue::list(vec))
        }
        Ok(_) => Err(NativeError("List.sort: expected List".into())),
        Err(list_val) => {
            match list_val.as_list() {
                Some(items) => {
                    let mut sorted = items.clone();
                    sorted.sort_by(sort_comparator);
                    Ok(NValue::list(sorted))
                }
                None => Err(NativeError(format!(
                    "List.sort: expected List, got {}",
                    list_val
                ))),
            }
        }
    }
}

pub(super) fn native_list_concat(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_list(), args[1].as_list()) {
        (Some(a), Some(b)) => {
            if a.is_empty() {
                return Ok(args[1].clone());
            }
            if b.is_empty() {
                return Ok(args[0].clone());
            }
            let mut result = Vec::with_capacity(a.len() + b.len());
            result.extend_from_slice(a);
            result.extend_from_slice(b);
            Ok(NValue::list(result))
        }
        _ => Err(NativeError("List.concat: expected (List, List)".into())),
    }
}

/// Owning CoW variant of List.concat. Extends the first list in-place when uniquely owned.
pub(super) fn native_list_concat_owning(args: Vec<NValue>) -> Result<NValue, NativeError> {
    let mut args = args;
    let b_val = args.pop().unwrap();
    let a_val = args.pop().unwrap();

    let b_items: Vec<NValue> = match b_val.as_list() {
        Some(b) => {
            if b.is_empty() {
                return Ok(a_val);
            }
            b.to_vec()
        }
        None => return Err(NativeError("List.concat: expected (List, List)".into())),
    };

    if a_val.as_list().unwrap().is_empty() {
        return Ok(b_val);
    }

    match a_val.try_unwrap_heap() {
        Ok(HeapObject::List(mut vec)) => {
            vec.extend(b_items);
            Ok(NValue::list(vec))
        }
        Ok(_) => Err(NativeError("List.concat: expected (List, List)".into())),
        Err(a_val) => {
            let a = a_val.as_list().unwrap();
            let mut result = Vec::with_capacity(a.len() + b_items.len());
            result.extend_from_slice(a);
            result.extend(b_items);
            Ok(NValue::list(result))
        }
    }
}

pub(super) fn native_list_contains(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_list() {
        Some(items) => Ok(NValue::bool(items.contains(&args[1]))),
        None => Err(NativeError(format!(
            "List.contains: expected List, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_list_get(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_list() {
        Some(items) => {
            if !args[1].is_any_int() {
                return Err(NativeError(format!(
                    "List.get: expected Int index, got {}",
                    args[1]
                )));
            }
            let idx = args[1].as_any_int();
            if idx < 0 || idx as usize >= items.len() {
                Ok(NValue::enum_val("None".into(), NValue::unit()))
            } else {
                Ok(NValue::enum_val("Some".into(), items[idx as usize].clone()))
            }
        }
        None => Err(NativeError(format!(
            "List.get: expected List, got {}",
            args[0]
        ))),
    }
}

/// Polymorphic contains for test matchers: string -> substring check, list -> element check.
pub(super) fn native_test_contains(args: &[NValue]) -> Result<NValue, NativeError> {
    if let Some(s) = args[0].as_string() {
        if let Some(sub) = args[1].as_string() {
            return Ok(NValue::bool(s.contains(&**sub)));
        }
        return Err(NativeError(
            "__test_contains: string actual requires string expected".into(),
        ));
    }
    if let Some(items) = args[0].as_list() {
        return Ok(NValue::bool(items.contains(&args[1])));
    }
    Err(NativeError(format!(
        "__test_contains: expected String or List, got {}",
        args[0]
    )))
}
