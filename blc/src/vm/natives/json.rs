use super::{HeapObject, NValue, NativeError, RcStr};

pub(crate) fn serde_to_nvalue(value: serde_json::Value) -> NValue {
    match value {
        serde_json::Value::Null => NValue::enum_val("Null".into(), NValue::unit()),
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
                    "_type".to_string(),
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
            HeapObject::Enum { tag, payload, .. } if &**tag == "Null" && payload.is_unit() => {
                return Ok(serde_json::Value::Null);
            }
            HeapObject::Enum { tag, .. } if &**tag == "None" => {
                return Ok(serde_json::Value::Null);
            }
            HeapObject::Enum { tag, payload, .. } if &**tag == "Some" => {
                return nvalue_to_serde(payload);
            }
            HeapObject::Enum { tag, payload, .. } if &**tag == "Ok" => {
                return nvalue_to_serde(payload);
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
