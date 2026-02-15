use super::{HeapObject, NValue, NativeError, RcStr};

pub(crate) fn serde_to_nvalue(value: serde_json::Value) -> NValue {
    match value {
        serde_json::Value::Null => NValue::unit(),
        serde_json::Value::Bool(b) => NValue::bool(b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                NValue::int(i)
            } else if let Some(f) = n.as_f64() {
                NValue::float(f)
            } else {
                NValue::float(f64::NAN)
            }
        }
        serde_json::Value::String(s) => NValue::string(s.into()),
        serde_json::Value::Array(arr) => {
            NValue::list(arr.into_iter().map(serde_to_nvalue).collect())
        }
        serde_json::Value::Object(obj) => {
            // Check for enum encoding: {"type": "Tag", "value": ...}
            if obj.len() == 2 {
                if let (Some(serde_json::Value::String(tag)), Some(val)) =
                    (obj.get("type"), obj.get("value"))
                {
                    return NValue::enum_val(
                        RcStr::from(tag.as_str()),
                        serde_to_nvalue(val.clone()),
                    );
                }
            }
            // Also handle {"type": "Tag"} with no value (unit payload)
            if obj.len() == 1 {
                if let Some(serde_json::Value::String(tag)) = obj.get("type") {
                    return NValue::enum_val(
                        RcStr::from(tag.as_str()),
                        NValue::unit(),
                    );
                }
            }
            let fields: Vec<(RcStr, NValue)> = obj
                .into_iter()
                .map(|(k, v)| (RcStr::from(k.as_str()), serde_to_nvalue(v)))
                .collect();
            NValue::record(fields)
        }
    }
}

pub(crate) fn nvalue_to_serde(value: &NValue) -> Result<serde_json::Value, NativeError> {
    if value.is_any_int() {
        return Ok(serde_json::json!(value.as_any_int()));
    }
    if value.is_float() {
        return serde_json::Number::from_f64(value.as_float())
            .map(serde_json::Value::Number)
            .ok_or_else(|| {
                NativeError(format!(
                    "Json.to_string: cannot serialize float {}",
                    value.as_float()
                ))
            });
    }
    if value.is_bool() {
        return Ok(serde_json::Value::Bool(value.as_bool()));
    }
    if value.is_unit() {
        return Ok(serde_json::Value::Null);
    }
    if value.is_heap() {
        match value.as_heap_ref() {
            HeapObject::String(s) => return Ok(serde_json::Value::String(s.to_string())),
            HeapObject::List(items) => {
                let arr: Result<Vec<_>, _> = items.iter().map(nvalue_to_serde).collect();
                return Ok(serde_json::Value::Array(arr?));
            }
            HeapObject::Record(fields) => {
                let mut map = serde_json::Map::new();
                for (k, v) in fields.iter() {
                    map.insert(k.to_string(), nvalue_to_serde(v)?);
                }
                return Ok(serde_json::Value::Object(map));
            }
            HeapObject::Struct { name, fields } => {
                let mut map = serde_json::Map::new();
                map.insert(
                    "type".to_string(),
                    serde_json::Value::String(name.to_string()),
                );
                for (k, v) in fields.iter() {
                    map.insert(k.to_string(), nvalue_to_serde(v)?);
                }
                return Ok(serde_json::Value::Object(map));
            }
            HeapObject::Tuple(items) => {
                let arr: Result<Vec<_>, _> = items.iter().map(nvalue_to_serde).collect();
                return Ok(serde_json::Value::Array(arr?));
            }
            HeapObject::Enum { tag, payload, .. } => {
                let mut map = serde_json::Map::new();
                map.insert(
                    "type".to_string(),
                    serde_json::Value::String(tag.to_string()),
                );
                if !payload.is_unit() {
                    map.insert("value".to_string(), nvalue_to_serde(payload)?);
                }
                return Ok(serde_json::Value::Object(map));
            }
            HeapObject::Map(entries) => {
                // If all keys are strings, serialize as JSON object
                let all_string_keys = entries.iter().all(|(k, _)| k.as_string().is_some());
                if all_string_keys {
                    let mut map = serde_json::Map::new();
                    for (k, v) in entries.iter() {
                        let key = k.as_string().unwrap().to_string();
                        map.insert(key, nvalue_to_serde(v)?);
                    }
                    return Ok(serde_json::Value::Object(map));
                }
                // Otherwise, serialize as array of [key, value] pairs
                let arr: Result<Vec<_>, _> = entries
                    .iter()
                    .map(|(k, v)| {
                        Ok(serde_json::Value::Array(vec![
                            nvalue_to_serde(k)?,
                            nvalue_to_serde(v)?,
                        ]))
                    })
                    .collect();
                return Ok(serde_json::Value::Array(arr?));
            }
            _ => {}
        }
    }
    Err(NativeError(format!(
        "Json.to_string: cannot serialize {}",
        value
    )))
}

pub(super) fn native_json_parse(args: &[NValue]) -> Result<NValue, NativeError> {
    let s = match args[0].as_string() {
        Some(s) => s,
        None => {
            return Err(NativeError(format!(
                "Json.parse expects String, got {}",
                args[0]
            )));
        }
    };
    let value: serde_json::Value =
        serde_json::from_str(s).map_err(|e| NativeError(format!("Json.parse: {}", e)))?;
    Ok(serde_to_nvalue(value))
}

pub(super) fn native_json_to_string(args: &[NValue]) -> Result<NValue, NativeError> {
    let serde_val = nvalue_to_serde(&args[0])?;
    let json_str = serde_json::to_string(&serde_val)
        .map_err(|e| NativeError(format!("Json.to_string: {}", e)))?;
    Ok(NValue::string(json_str.into()))
}

pub(super) fn native_json_to_string_pretty(args: &[NValue]) -> Result<NValue, NativeError> {
    let serde_val = nvalue_to_serde(&args[0])?;
    let json_str = serde_json::to_string_pretty(&serde_val)
        .map_err(|e| NativeError(format!("Json.to_string_pretty: {}", e)))?;
    Ok(NValue::string(json_str.into()))
}

// -- Field name case conversion --

fn to_camel_case(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut capitalize_next = false;
    for ch in s.chars() {
        if ch == '_' {
            capitalize_next = true;
        } else if capitalize_next {
            result.extend(ch.to_uppercase());
            capitalize_next = false;
        } else {
            result.push(ch);
        }
    }
    result
}

fn to_snake_case(s: &str) -> String {
    let mut result = String::with_capacity(s.len() + 4);
    for (i, ch) in s.chars().enumerate() {
        if ch.is_uppercase() {
            if i > 0 {
                result.push('_');
            }
            result.extend(ch.to_lowercase());
        } else {
            result.push(ch);
        }
    }
    result
}

fn convert_record_keys(value: &NValue, convert: fn(&str) -> String) -> NValue {
    if !value.is_heap() {
        return value.clone();
    }
    match value.as_heap_ref() {
        HeapObject::Record(fields) => {
            let new_fields: Vec<(RcStr, NValue)> = fields
                .iter()
                .map(|(k, v)| {
                    (RcStr::from(convert(k).as_str()), convert_record_keys(v, convert))
                })
                .collect();
            NValue::record(new_fields)
        }
        HeapObject::Struct { name, fields } => {
            let new_fields: Vec<(RcStr, NValue)> = fields
                .iter()
                .map(|(k, v)| {
                    (RcStr::from(convert(k).as_str()), convert_record_keys(v, convert))
                })
                .collect();
            NValue::struct_val(name.clone(), new_fields)
        }
        HeapObject::List(items) => {
            NValue::list(items.iter().map(|v| convert_record_keys(v, convert)).collect())
        }
        _ => value.clone(),
    }
}

pub(super) fn native_json_to_camel_case(args: &[NValue]) -> Result<NValue, NativeError> {
    Ok(convert_record_keys(&args[0], to_camel_case))
}

pub(super) fn native_json_to_snake_case(args: &[NValue]) -> Result<NValue, NativeError> {
    Ok(convert_record_keys(&args[0], to_snake_case))
}
