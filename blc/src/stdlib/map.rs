use crate::interpreter::RuntimeValue;
use crate::stdlib::NativeRegistry;

pub fn register(registry: &mut NativeRegistry) {
    registry.register("Map.empty", map_empty);
    registry.register("Map.insert", map_insert);
    registry.register("Map.get", map_get);
    registry.register("Map.remove", map_remove);
    registry.register("Map.contains", map_contains);
    registry.register("Map.keys", map_keys);
    registry.register("Map.values", map_values);
    registry.register("Map.len", map_len);
}

fn map_empty<'a>(_args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    Ok(RuntimeValue::Map(Vec::new()))
}

fn map_insert<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 3 {
        return Err("Map.insert requires 3 arguments: map, key, value".into());
    }
    match &args[0] {
        RuntimeValue::Map(entries) => {
            let key = args[1].clone();
            let val = args[2].clone();
            let mut new_entries: Vec<(RuntimeValue<'a>, RuntimeValue<'a>)> = entries
                .iter()
                .filter(|(k, _)| *k != key)
                .cloned()
                .collect();
            new_entries.push((key, val));
            Ok(RuntimeValue::Map(new_entries))
        }
        other => Err(format!("Map.insert: expected Map, got {}", other)),
    }
}

fn map_get<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err("Map.get requires 2 arguments: map, key".into());
    }
    match &args[0] {
        RuntimeValue::Map(entries) => {
            let key = &args[1];
            for (k, v) in entries {
                if k == key {
                    return Ok(RuntimeValue::Enum("Some".into(), vec![v.clone()]));
                }
            }
            Ok(RuntimeValue::Enum("None".into(), vec![]))
        }
        other => Err(format!("Map.get: expected Map, got {}", other)),
    }
}

fn map_remove<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err("Map.remove requires 2 arguments: map, key".into());
    }
    match &args[0] {
        RuntimeValue::Map(entries) => {
            let key = &args[1];
            let new_entries: Vec<_> = entries.iter().filter(|(k, _)| k != key).cloned().collect();
            Ok(RuntimeValue::Map(new_entries))
        }
        other => Err(format!("Map.remove: expected Map, got {}", other)),
    }
}

fn map_contains<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err("Map.contains requires 2 arguments: map, key".into());
    }
    match &args[0] {
        RuntimeValue::Map(entries) => {
            let key = &args[1];
            let found = entries.iter().any(|(k, _)| k == key);
            Ok(RuntimeValue::Bool(found))
        }
        other => Err(format!("Map.contains: expected Map, got {}", other)),
    }
}

fn map_keys<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err("Map.keys requires 1 argument: map".into());
    }
    match &args[0] {
        RuntimeValue::Map(entries) => {
            let keys: Vec<_> = entries.iter().map(|(k, _)| k.clone()).collect();
            Ok(RuntimeValue::List(keys))
        }
        other => Err(format!("Map.keys: expected Map, got {}", other)),
    }
}

fn map_values<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err("Map.values requires 1 argument: map".into());
    }
    match &args[0] {
        RuntimeValue::Map(entries) => {
            let vals: Vec<_> = entries.iter().map(|(_, v)| v.clone()).collect();
            Ok(RuntimeValue::List(vals))
        }
        other => Err(format!("Map.values: expected Map, got {}", other)),
    }
}

fn map_len<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err("Map.len requires 1 argument: map".into());
    }
    match &args[0] {
        RuntimeValue::Map(entries) => Ok(RuntimeValue::Int(entries.len() as i64)),
        other => Err(format!("Map.len: expected Map, got {}", other)),
    }
}
