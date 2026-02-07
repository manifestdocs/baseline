use super::NativeRegistry;
use crate::interpreter::RuntimeValue;

pub fn register(registry: &mut NativeRegistry) {
    registry.register("List.length", list_length);
    registry.register("List.head", list_head);
    registry.register("List.tail", list_tail);
    registry.register("List.reverse", list_reverse);
    registry.register("List.sort", list_sort);
    registry.register("List.concat", list_concat);
    // List.map, List.filter, List.fold, List.find are special-cased in the
    // interpreter because they need closure invocation.
}

fn expect_list<'a>(name: &str, val: &RuntimeValue<'a>) -> Result<Vec<RuntimeValue<'a>>, String> {
    match val {
        RuntimeValue::List(items) => Ok(items.clone()),
        other => Err(format!("{} expects List, got {}", name, other)),
    }
}

fn list_length<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "List.length expects 1 argument, got {}",
            args.len()
        ));
    }
    let items = expect_list("List.length", &args[0])?;
    Ok(RuntimeValue::Int(items.len() as i64))
}

fn list_head<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!("List.head expects 1 argument, got {}", args.len()));
    }
    let items = expect_list("List.head", &args[0])?;
    if let Some(first) = items.first() {
        Ok(RuntimeValue::Enum("Some".to_string(), vec![first.clone()]))
    } else {
        Ok(RuntimeValue::Enum("None".to_string(), Vec::new()))
    }
}

fn list_tail<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!("List.tail expects 1 argument, got {}", args.len()));
    }
    let items = expect_list("List.tail", &args[0])?;
    if items.is_empty() {
        Ok(RuntimeValue::List(Vec::new()))
    } else {
        Ok(RuntimeValue::List(items[1..].to_vec()))
    }
}

fn list_reverse<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "List.reverse expects 1 argument, got {}",
            args.len()
        ));
    }
    let mut items = expect_list("List.reverse", &args[0])?;
    items.reverse();
    Ok(RuntimeValue::List(items))
}

fn list_sort<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!("List.sort expects 1 argument, got {}", args.len()));
    }
    let mut items = expect_list("List.sort", &args[0])?;
    items.sort_by(|a, b| match (a, b) {
        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => a.cmp(b),
        (RuntimeValue::Float(a), RuntimeValue::Float(b)) => {
            a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
        }
        (RuntimeValue::String(a), RuntimeValue::String(b)) => a.cmp(b),
        _ => std::cmp::Ordering::Equal,
    });
    Ok(RuntimeValue::List(items))
}

fn list_concat<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err(format!(
            "List.concat expects 2 arguments, got {}",
            args.len()
        ));
    }
    let mut a = expect_list("List.concat", &args[0])?;
    let b = expect_list("List.concat", &args[1])?;
    a.extend(b);
    Ok(RuntimeValue::List(a))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn int_list(vals: &[i64]) -> RuntimeValue<'static> {
        RuntimeValue::List(vals.iter().map(|v| RuntimeValue::Int(*v)).collect())
    }

    #[test]
    fn length() {
        assert_eq!(
            list_length(&[int_list(&[1, 2, 3])]).unwrap(),
            RuntimeValue::Int(3)
        );
    }

    #[test]
    fn length_empty() {
        assert_eq!(list_length(&[int_list(&[])]).unwrap(), RuntimeValue::Int(0));
    }

    #[test]
    fn head_some() {
        let result = list_head(&[int_list(&[1, 2, 3])]).unwrap();
        assert_eq!(
            result,
            RuntimeValue::Enum("Some".into(), vec![RuntimeValue::Int(1)])
        );
    }

    #[test]
    fn head_empty() {
        let result = list_head(&[int_list(&[])]).unwrap();
        assert_eq!(result, RuntimeValue::Enum("None".into(), vec![]));
    }

    #[test]
    fn tail() {
        assert_eq!(
            list_tail(&[int_list(&[1, 2, 3])]).unwrap(),
            int_list(&[2, 3])
        );
    }

    #[test]
    fn tail_empty() {
        assert_eq!(list_tail(&[int_list(&[])]).unwrap(), int_list(&[]));
    }

    #[test]
    fn reverse() {
        assert_eq!(
            list_reverse(&[int_list(&[1, 2, 3])]).unwrap(),
            int_list(&[3, 2, 1])
        );
    }

    #[test]
    fn sort() {
        assert_eq!(
            list_sort(&[int_list(&[3, 1, 2])]).unwrap(),
            int_list(&[1, 2, 3])
        );
    }

    #[test]
    fn concat() {
        assert_eq!(
            list_concat(&[int_list(&[1, 2]), int_list(&[3, 4])]).unwrap(),
            int_list(&[1, 2, 3, 4])
        );
    }
}
