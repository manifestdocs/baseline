use super::NativeRegistry;
use crate::interpreter::RuntimeValue;

pub fn register(registry: &mut NativeRegistry) {
    registry.register("Option.unwrap", option_unwrap);
    registry.register("Option.unwrap_or", option_unwrap_or);
    registry.register("Option.is_some", option_is_some);
    registry.register("Option.is_none", option_is_none);
    // Option.map is special-cased in the interpreter (needs closure invocation)
}

fn option_unwrap<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "Option.unwrap expects 1 argument, got {}",
            args.len()
        ));
    }
    match &args[0] {
        RuntimeValue::Enum(name, payload) if name == "Some" && payload.len() == 1 => {
            Ok(payload[0].clone())
        }
        RuntimeValue::Enum(name, payload) if name == "None" && payload.is_empty() => {
            Err("Option.unwrap called on None".to_string())
        }
        other => Err(format!("Option.unwrap expects Option, got {}", other)),
    }
}

fn option_unwrap_or<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err(format!(
            "Option.unwrap_or expects 2 arguments, got {}",
            args.len()
        ));
    }
    match &args[0] {
        RuntimeValue::Enum(name, payload) if name == "Some" && payload.len() == 1 => {
            Ok(payload[0].clone())
        }
        RuntimeValue::Enum(name, payload) if name == "None" && payload.is_empty() => {
            Ok(args[1].clone())
        }
        other => Err(format!(
            "Option.unwrap_or expects Option as first argument, got {}",
            other
        )),
    }
}

fn option_is_some<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "Option.is_some expects 1 argument, got {}",
            args.len()
        ));
    }
    match &args[0] {
        RuntimeValue::Enum(name, payload) if name == "Some" && payload.len() == 1 => {
            Ok(RuntimeValue::Bool(true))
        }
        RuntimeValue::Enum(name, payload) if name == "None" && payload.is_empty() => {
            Ok(RuntimeValue::Bool(false))
        }
        other => Err(format!("Option.is_some expects Option, got {}", other)),
    }
}

fn option_is_none<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "Option.is_none expects 1 argument, got {}",
            args.len()
        ));
    }
    match &args[0] {
        RuntimeValue::Enum(name, payload) if name == "Some" && payload.len() == 1 => {
            Ok(RuntimeValue::Bool(false))
        }
        RuntimeValue::Enum(name, payload) if name == "None" && payload.is_empty() => {
            Ok(RuntimeValue::Bool(true))
        }
        other => Err(format!("Option.is_none expects Option, got {}", other)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unwrap_some() {
        let val = RuntimeValue::Enum("Some".into(), vec![RuntimeValue::Int(42)]);
        assert_eq!(option_unwrap(&[val]).unwrap(), RuntimeValue::Int(42));
    }

    #[test]
    fn unwrap_none_errors() {
        let val = RuntimeValue::Enum("None".into(), vec![]);
        assert!(option_unwrap(&[val]).is_err());
    }

    #[test]
    fn unwrap_or_some() {
        let val = RuntimeValue::Enum("Some".into(), vec![RuntimeValue::Int(42)]);
        let result = option_unwrap_or(&[val, RuntimeValue::Int(0)]).unwrap();
        assert_eq!(result, RuntimeValue::Int(42));
    }

    #[test]
    fn unwrap_or_none() {
        let val = RuntimeValue::Enum("None".into(), vec![]);
        let result = option_unwrap_or(&[val, RuntimeValue::Int(99)]).unwrap();
        assert_eq!(result, RuntimeValue::Int(99));
    }

    #[test]
    fn is_some_true() {
        let val = RuntimeValue::Enum("Some".into(), vec![RuntimeValue::Int(1)]);
        assert_eq!(option_is_some(&[val]).unwrap(), RuntimeValue::Bool(true));
    }

    #[test]
    fn is_some_false() {
        let val = RuntimeValue::Enum("None".into(), vec![]);
        assert_eq!(option_is_some(&[val]).unwrap(), RuntimeValue::Bool(false));
    }

    #[test]
    fn is_none_true() {
        let val = RuntimeValue::Enum("None".into(), vec![]);
        assert_eq!(option_is_none(&[val]).unwrap(), RuntimeValue::Bool(true));
    }

    #[test]
    fn is_none_false() {
        let val = RuntimeValue::Enum("Some".into(), vec![RuntimeValue::Int(1)]);
        assert_eq!(option_is_none(&[val]).unwrap(), RuntimeValue::Bool(false));
    }
}
