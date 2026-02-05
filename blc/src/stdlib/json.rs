use std::collections::HashMap;
use crate::interpreter::RuntimeValue;
use super::NativeRegistry;

pub fn register(registry: &mut NativeRegistry) {
    registry.register("Json.parse", json_parse);
    registry.register("Json.to_string", json_to_string);
    registry.register("Json.to_string_pretty", json_to_string_pretty);
}

// ---------------------------------------------------------------------------
// serde_json::Value <-> RuntimeValue conversion
// ---------------------------------------------------------------------------

/// Convert a serde_json::Value into a RuntimeValue.
///
/// Mapping:
///   null    -> Enum("Null", [])
///   bool    -> Bool
///   number  -> Int (if whole number fits i64) or Float
///   string  -> String
///   array   -> List (recursive)
///   object  -> Record (recursive)
fn serde_to_runtime<'a>(value: serde_json::Value) -> RuntimeValue<'a> {
    match value {
        serde_json::Value::Null => RuntimeValue::Enum("Null".to_string(), Vec::new()),
        serde_json::Value::Bool(b) => RuntimeValue::Bool(b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                RuntimeValue::Int(i)
            } else if let Some(f) = n.as_f64() {
                RuntimeValue::Float(f)
            } else {
                RuntimeValue::Float(f64::NAN)
            }
        }
        serde_json::Value::String(s) => RuntimeValue::String(s),
        serde_json::Value::Array(arr) => {
            RuntimeValue::List(arr.into_iter().map(serde_to_runtime).collect())
        }
        serde_json::Value::Object(obj) => {
            let fields: HashMap<String, RuntimeValue<'a>> = obj
                .into_iter()
                .map(|(k, v)| (k, serde_to_runtime(v)))
                .collect();
            RuntimeValue::Record(fields)
        }
    }
}

/// Convert a RuntimeValue into a serde_json::Value for serialization.
fn runtime_to_serde(value: &RuntimeValue) -> Result<serde_json::Value, String> {
    match value {
        RuntimeValue::Enum(name, payload) if name == "Null" && payload.is_empty() => {
            Ok(serde_json::Value::Null)
        }
        RuntimeValue::Enum(name, payload) if name == "None" && payload.is_empty() => {
            Ok(serde_json::Value::Null)
        }
        RuntimeValue::Bool(b) => Ok(serde_json::Value::Bool(*b)),
        RuntimeValue::Int(i) => Ok(serde_json::json!(*i)),
        RuntimeValue::Float(f) => {
            serde_json::Number::from_f64(*f)
                .map(serde_json::Value::Number)
                .ok_or_else(|| format!("Json.to_string: cannot serialize float {}", f))
        }
        RuntimeValue::String(s) => Ok(serde_json::Value::String(s.clone())),
        RuntimeValue::List(items) => {
            let arr: Result<Vec<serde_json::Value>, String> =
                items.iter().map(runtime_to_serde).collect();
            Ok(serde_json::Value::Array(arr?))
        }
        RuntimeValue::Record(fields) => {
            let mut map = serde_json::Map::new();
            for (k, v) in fields {
                map.insert(k.clone(), runtime_to_serde(v)?);
            }
            Ok(serde_json::Value::Object(map))
        }
        RuntimeValue::Struct(name, fields) => {
            let mut map = serde_json::Map::new();
            map.insert("_type".to_string(), serde_json::Value::String(name.clone()));
            for (k, v) in fields {
                map.insert(k.clone(), runtime_to_serde(v)?);
            }
            Ok(serde_json::Value::Object(map))
        }
        RuntimeValue::Tuple(items) => {
            let arr: Result<Vec<serde_json::Value>, String> =
                items.iter().map(runtime_to_serde).collect();
            Ok(serde_json::Value::Array(arr?))
        }
        RuntimeValue::Unit => Ok(serde_json::Value::Null),
        RuntimeValue::Enum(name, payload) if name == "Some" && payload.len() == 1 => {
            runtime_to_serde(&payload[0])
        }
        RuntimeValue::Enum(name, payload) if name == "Ok" && payload.len() == 1 => {
            runtime_to_serde(&payload[0])
        }
        other => Err(format!("Json.to_string: cannot serialize {}", other)),
    }
}

// ---------------------------------------------------------------------------
// Native functions
// ---------------------------------------------------------------------------

fn json_parse<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!("Json.parse expects 1 argument, got {}", args.len()));
    }
    let s = match &args[0] {
        RuntimeValue::String(s) => s,
        other => return Err(format!("Json.parse expects String, got {}", other)),
    };
    let value: serde_json::Value = serde_json::from_str(s)
        .map_err(|e| format!("Json.parse: {}", e))?;
    Ok(serde_to_runtime(value))
}

fn json_to_string<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!("Json.to_string expects 1 argument, got {}", args.len()));
    }
    let serde_val = runtime_to_serde(&args[0])?;
    let json_str = serde_json::to_string(&serde_val)
        .map_err(|e| format!("Json.to_string: {}", e))?;
    Ok(RuntimeValue::String(json_str))
}

fn json_to_string_pretty<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!("Json.to_string_pretty expects 1 argument, got {}", args.len()));
    }
    let serde_val = runtime_to_serde(&args[0])?;
    let json_str = serde_json::to_string_pretty(&serde_val)
        .map_err(|e| format!("Json.to_string_pretty: {}", e))?;
    Ok(RuntimeValue::String(json_str))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_object() {
        let input = RuntimeValue::String(r#"{"name":"Alice","age":30}"#.into());
        let result = json_parse(&[input]).unwrap();
        match result {
            RuntimeValue::Record(fields) => {
                assert_eq!(fields.get("name"), Some(&RuntimeValue::String("Alice".into())));
                assert_eq!(fields.get("age"), Some(&RuntimeValue::Int(30)));
            }
            other => panic!("Expected Record, got {:?}", other),
        }
    }

    #[test]
    fn parse_array() {
        let input = RuntimeValue::String("[1, 2, 3]".into());
        let result = json_parse(&[input]).unwrap();
        assert_eq!(
            result,
            RuntimeValue::List(vec![
                RuntimeValue::Int(1),
                RuntimeValue::Int(2),
                RuntimeValue::Int(3),
            ])
        );
    }

    #[test]
    fn parse_null() {
        let input = RuntimeValue::String("null".into());
        let result = json_parse(&[input]).unwrap();
        assert_eq!(result, RuntimeValue::Enum("Null".to_string(), vec![]));
    }

    #[test]
    fn parse_bool() {
        let input = RuntimeValue::String("true".into());
        assert_eq!(json_parse(&[input]).unwrap(), RuntimeValue::Bool(true));
    }

    #[test]
    fn parse_float() {
        let input = RuntimeValue::String("3.14".into());
        assert_eq!(json_parse(&[input]).unwrap(), RuntimeValue::Float(3.14));
    }

    #[test]
    fn parse_string() {
        let input = RuntimeValue::String(r#""hello""#.into());
        assert_eq!(json_parse(&[input]).unwrap(), RuntimeValue::String("hello".into()));
    }

    #[test]
    fn parse_invalid_json_errors() {
        let input = RuntimeValue::String("{invalid".into());
        assert!(json_parse(&[input]).is_err());
    }

    #[test]
    fn to_string_record() {
        let mut fields = HashMap::new();
        fields.insert("x".to_string(), RuntimeValue::Int(1));
        let input = RuntimeValue::Record(fields);
        let result = json_to_string(&[input]).unwrap();
        assert_eq!(result, RuntimeValue::String(r#"{"x":1}"#.into()));
    }

    #[test]
    fn to_string_list() {
        let input = RuntimeValue::List(vec![
            RuntimeValue::Int(1),
            RuntimeValue::Bool(true),
            RuntimeValue::String("hi".into()),
        ]);
        let result = json_to_string(&[input]).unwrap();
        assert_eq!(result, RuntimeValue::String(r#"[1,true,"hi"]"#.into()));
    }

    #[test]
    fn to_string_null() {
        let input = RuntimeValue::Enum("Null".to_string(), vec![]);
        let result = json_to_string(&[input]).unwrap();
        assert_eq!(result, RuntimeValue::String("null".into()));
    }

    #[test]
    fn to_string_pretty_formats() {
        let input = RuntimeValue::List(vec![RuntimeValue::Int(1), RuntimeValue::Int(2)]);
        let result = json_to_string_pretty(&[input]).unwrap();
        match result {
            RuntimeValue::String(s) => assert!(s.contains('\n'), "Expected newlines in pretty output"),
            other => panic!("Expected String, got {:?}", other),
        }
    }

    #[test]
    fn roundtrip_parse_to_string() {
        let json = r#"{"items":[1,2,3],"active":true,"name":"test"}"#;
        let input = RuntimeValue::String(json.into());
        let parsed = json_parse(&[input]).unwrap();
        let serialized = json_to_string(&[parsed]).unwrap();
        // Re-parse to compare structurally (key order may differ)
        let reparsed = json_parse(&[serialized]).unwrap();
        let original = json_parse(&[RuntimeValue::String(json.into())]).unwrap();
        assert_eq!(reparsed, original);
    }

    #[test]
    fn none_serializes_as_null() {
        let input = RuntimeValue::Enum("None".to_string(), vec![]);
        let result = json_to_string(&[input]).unwrap();
        assert_eq!(result, RuntimeValue::String("null".into()));
    }

    #[test]
    fn some_unwraps_for_serialization() {
        let input = RuntimeValue::Enum("Some".to_string(), vec![RuntimeValue::Int(42)]);
        let result = json_to_string(&[input]).unwrap();
        assert_eq!(result, RuntimeValue::String("42".into()));
    }

    #[test]
    fn unit_serializes_as_null() {
        let result = json_to_string(&[RuntimeValue::Unit]).unwrap();
        assert_eq!(result, RuntimeValue::String("null".into()));
    }
}
