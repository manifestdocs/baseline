use super::NativeRegistry;
use crate::interpreter::RuntimeValue;

pub fn register(registry: &mut NativeRegistry) {
    registry.register("String.length", string_length);
    registry.register("String.trim", string_trim);
    registry.register("String.contains", string_contains);
    registry.register("String.starts_with", string_starts_with);
    registry.register("String.to_upper", string_to_upper);
    registry.register("String.to_lower", string_to_lower);
    registry.register("String.split", string_split);
    registry.register("String.join", string_join);
    registry.register("String.slice", string_slice);
}

fn expect_string<'a>(name: &str, val: &RuntimeValue<'a>, pos: usize) -> Result<String, String> {
    match val {
        RuntimeValue::String(s) => Ok(s.clone()),
        other => Err(format!(
            "{} expects String for argument {}, got {}",
            name, pos, other
        )),
    }
}

fn string_length<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "String.length expects 1 argument, got {}",
            args.len()
        ));
    }
    let s = expect_string("String.length", &args[0], 1)?;
    Ok(RuntimeValue::Int(s.len() as i64))
}

fn string_trim<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "String.trim expects 1 argument, got {}",
            args.len()
        ));
    }
    let s = expect_string("String.trim", &args[0], 1)?;
    Ok(RuntimeValue::String(s.trim().to_string()))
}

fn string_contains<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err(format!(
            "String.contains expects 2 arguments, got {}",
            args.len()
        ));
    }
    let haystack = expect_string("String.contains", &args[0], 1)?;
    let needle = expect_string("String.contains", &args[1], 2)?;
    Ok(RuntimeValue::Bool(haystack.contains(&needle)))
}

fn string_starts_with<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err(format!(
            "String.starts_with expects 2 arguments, got {}",
            args.len()
        ));
    }
    let s = expect_string("String.starts_with", &args[0], 1)?;
    let prefix = expect_string("String.starts_with", &args[1], 2)?;
    Ok(RuntimeValue::Bool(s.starts_with(&prefix)))
}

fn string_to_upper<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "String.to_upper expects 1 argument, got {}",
            args.len()
        ));
    }
    let s = expect_string("String.to_upper", &args[0], 1)?;
    Ok(RuntimeValue::String(s.to_uppercase()))
}

fn string_to_lower<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "String.to_lower expects 1 argument, got {}",
            args.len()
        ));
    }
    let s = expect_string("String.to_lower", &args[0], 1)?;
    Ok(RuntimeValue::String(s.to_lowercase()))
}

fn string_split<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err(format!(
            "String.split expects 2 arguments, got {}",
            args.len()
        ));
    }
    let s = expect_string("String.split", &args[0], 1)?;
    let delim = expect_string("String.split", &args[1], 2)?;
    let parts: Vec<RuntimeValue<'a>> = s
        .split(&delim)
        .map(|p| RuntimeValue::String(p.to_string()))
        .collect();
    Ok(RuntimeValue::List(parts))
}

fn string_join<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err(format!(
            "String.join expects 2 arguments, got {}",
            args.len()
        ));
    }
    let list = match &args[0] {
        RuntimeValue::List(items) => items,
        other => {
            return Err(format!(
                "String.join expects List as first argument, got {}",
                other
            ));
        }
    };
    let sep = expect_string("String.join", &args[1], 2)?;
    let strs: Result<Vec<String>, String> = list
        .iter()
        .map(|item| match item {
            RuntimeValue::String(s) => Ok(s.clone()),
            other => Err(format!(
                "String.join list elements must be Strings, got {}",
                other
            )),
        })
        .collect();
    Ok(RuntimeValue::String(strs?.join(&sep)))
}

fn string_slice<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 3 {
        return Err(format!(
            "String.slice expects 3 arguments, got {}",
            args.len()
        ));
    }
    let s = expect_string("String.slice", &args[0], 1)?;
    let start = match &args[1] {
        RuntimeValue::Int(i) => *i as usize,
        other => return Err(format!("String.slice expects Int for start, got {}", other)),
    };
    let end = match &args[2] {
        RuntimeValue::Int(i) => *i as usize,
        other => return Err(format!("String.slice expects Int for end, got {}", other)),
    };
    if start > s.len() || end > s.len() || start > end {
        return Err(format!(
            "String.slice index out of bounds: {}..{} on string of length {}",
            start,
            end,
            s.len()
        ));
    }
    Ok(RuntimeValue::String(s[start..end].to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn length() {
        let args = vec![RuntimeValue::String("hello".into())];
        assert_eq!(string_length(&args).unwrap(), RuntimeValue::Int(5));
    }

    #[test]
    fn length_empty() {
        let args = vec![RuntimeValue::String("".into())];
        assert_eq!(string_length(&args).unwrap(), RuntimeValue::Int(0));
    }

    #[test]
    fn trim() {
        let args = vec![RuntimeValue::String(" hi ".into())];
        assert_eq!(
            string_trim(&args).unwrap(),
            RuntimeValue::String("hi".into())
        );
    }

    #[test]
    fn contains_true() {
        let args = vec![
            RuntimeValue::String("hello".into()),
            RuntimeValue::String("ell".into()),
        ];
        assert_eq!(string_contains(&args).unwrap(), RuntimeValue::Bool(true));
    }

    #[test]
    fn contains_false() {
        let args = vec![
            RuntimeValue::String("hello".into()),
            RuntimeValue::String("xyz".into()),
        ];
        assert_eq!(string_contains(&args).unwrap(), RuntimeValue::Bool(false));
    }

    #[test]
    fn starts_with_true() {
        let args = vec![
            RuntimeValue::String("hello".into()),
            RuntimeValue::String("he".into()),
        ];
        assert_eq!(string_starts_with(&args).unwrap(), RuntimeValue::Bool(true));
    }

    #[test]
    fn to_upper() {
        let args = vec![RuntimeValue::String("hi".into())];
        assert_eq!(
            string_to_upper(&args).unwrap(),
            RuntimeValue::String("HI".into())
        );
    }

    #[test]
    fn to_lower() {
        let args = vec![RuntimeValue::String("HI".into())];
        assert_eq!(
            string_to_lower(&args).unwrap(),
            RuntimeValue::String("hi".into())
        );
    }

    #[test]
    fn split() {
        let args = vec![
            RuntimeValue::String("a,b,c".into()),
            RuntimeValue::String(",".into()),
        ];
        assert_eq!(
            string_split(&args).unwrap(),
            RuntimeValue::List(vec![
                RuntimeValue::String("a".into()),
                RuntimeValue::String("b".into()),
                RuntimeValue::String("c".into()),
            ])
        );
    }

    #[test]
    fn join() {
        let args = vec![
            RuntimeValue::List(vec![
                RuntimeValue::String("a".into()),
                RuntimeValue::String("b".into()),
            ]),
            RuntimeValue::String("-".into()),
        ];
        assert_eq!(
            string_join(&args).unwrap(),
            RuntimeValue::String("a-b".into())
        );
    }

    #[test]
    fn slice() {
        let args = vec![
            RuntimeValue::String("hello".into()),
            RuntimeValue::Int(1),
            RuntimeValue::Int(3),
        ];
        assert_eq!(
            string_slice(&args).unwrap(),
            RuntimeValue::String("el".into())
        );
    }

    #[test]
    fn slice_out_of_bounds() {
        let args = vec![
            RuntimeValue::String("hi".into()),
            RuntimeValue::Int(0),
            RuntimeValue::Int(5),
        ];
        assert!(string_slice(&args).is_err());
    }
}
