use crate::interpreter::RuntimeValue;
use crate::stdlib::NativeRegistry;

pub fn register(registry: &mut NativeRegistry) {
    registry.register("Set.empty", set_empty);
    registry.register("Set.insert", set_insert);
    registry.register("Set.remove", set_remove);
    registry.register("Set.contains", set_contains);
    registry.register("Set.union", set_union);
    registry.register("Set.intersection", set_intersection);
    registry.register("Set.len", set_len);
    registry.register("Set.from_list", set_from_list);
}

fn set_empty<'a>(_args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    Ok(RuntimeValue::Set(Vec::new()))
}

fn set_insert<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err("Set.insert requires 2 arguments: set, element".into());
    }
    match &args[0] {
        RuntimeValue::Set(elems) => {
            let elem = args[1].clone();
            if elems.contains(&elem) {
                Ok(RuntimeValue::Set(elems.clone()))
            } else {
                let mut new_elems = elems.clone();
                new_elems.push(elem);
                Ok(RuntimeValue::Set(new_elems))
            }
        }
        other => Err(format!("Set.insert: expected Set, got {}", other)),
    }
}

fn set_remove<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err("Set.remove requires 2 arguments: set, element".into());
    }
    match &args[0] {
        RuntimeValue::Set(elems) => {
            let elem = &args[1];
            let new_elems: Vec<_> = elems.iter().filter(|e| *e != elem).cloned().collect();
            Ok(RuntimeValue::Set(new_elems))
        }
        other => Err(format!("Set.remove: expected Set, got {}", other)),
    }
}

fn set_contains<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err("Set.contains requires 2 arguments: set, element".into());
    }
    match &args[0] {
        RuntimeValue::Set(elems) => {
            let elem = &args[1];
            Ok(RuntimeValue::Bool(elems.contains(elem)))
        }
        other => Err(format!("Set.contains: expected Set, got {}", other)),
    }
}

fn set_union<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err("Set.union requires 2 arguments: set1, set2".into());
    }
    match (&args[0], &args[1]) {
        (RuntimeValue::Set(a), RuntimeValue::Set(b)) => {
            let mut result = a.clone();
            for elem in b {
                if !result.contains(elem) {
                    result.push(elem.clone());
                }
            }
            Ok(RuntimeValue::Set(result))
        }
        _ => Err("Set.union: expected two Sets".into()),
    }
}

fn set_intersection<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err("Set.intersection requires 2 arguments: set1, set2".into());
    }
    match (&args[0], &args[1]) {
        (RuntimeValue::Set(a), RuntimeValue::Set(b)) => {
            let result: Vec<_> = a.iter().filter(|e| b.contains(e)).cloned().collect();
            Ok(RuntimeValue::Set(result))
        }
        _ => Err("Set.intersection: expected two Sets".into()),
    }
}

fn set_len<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err("Set.len requires 1 argument: set".into());
    }
    match &args[0] {
        RuntimeValue::Set(elems) => Ok(RuntimeValue::Int(elems.len() as i64)),
        other => Err(format!("Set.len: expected Set, got {}", other)),
    }
}

fn set_from_list<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err("Set.from_list requires 1 argument: list".into());
    }
    match &args[0] {
        RuntimeValue::List(items) => {
            let mut result = Vec::new();
            for item in items {
                if !result.contains(item) {
                    result.push(item.clone());
                }
            }
            Ok(RuntimeValue::Set(result))
        }
        other => Err(format!("Set.from_list: expected List, got {}", other)),
    }
}
