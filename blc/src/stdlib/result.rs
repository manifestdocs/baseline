use super::NativeRegistry;
use crate::interpreter::RuntimeValue;

pub fn register(registry: &mut NativeRegistry) {
    registry.register("Result.unwrap", result_unwrap);
    registry.register("Result.unwrap_or", result_unwrap_or);
    registry.register("Result.is_ok", result_is_ok);
    registry.register("Result.is_err", result_is_err);
    // Result.map is special-cased in the interpreter (needs closure invocation)
}

fn result_unwrap<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "Result.unwrap expects 1 argument, got {}",
            args.len()
        ));
    }
    match &args[0] {
        RuntimeValue::Enum(name, payload) if name == "Ok" && payload.len() == 1 => {
            Ok(payload[0].clone())
        }
        RuntimeValue::Enum(name, payload) if name == "Err" && payload.len() == 1 => {
            Err(format!("Result.unwrap called on Err({})", payload[0]))
        }
        other => Err(format!("Result.unwrap expects Result, got {}", other)),
    }
}

fn result_unwrap_or<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 2 {
        return Err(format!(
            "Result.unwrap_or expects 2 arguments, got {}",
            args.len()
        ));
    }
    match &args[0] {
        RuntimeValue::Enum(name, payload) if name == "Ok" && payload.len() == 1 => {
            Ok(payload[0].clone())
        }
        RuntimeValue::Enum(name, payload) if name == "Err" && payload.len() == 1 => {
            Ok(args[1].clone())
        }
        other => Err(format!(
            "Result.unwrap_or expects Result as first argument, got {}",
            other
        )),
    }
}

fn result_is_ok<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "Result.is_ok expects 1 argument, got {}",
            args.len()
        ));
    }
    match &args[0] {
        RuntimeValue::Enum(name, payload) if name == "Ok" && payload.len() == 1 => {
            Ok(RuntimeValue::Bool(true))
        }
        RuntimeValue::Enum(name, payload) if name == "Err" && payload.len() == 1 => {
            Ok(RuntimeValue::Bool(false))
        }
        other => Err(format!("Result.is_ok expects Result, got {}", other)),
    }
}

fn result_is_err<'a>(args: &[RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, String> {
    if args.len() != 1 {
        return Err(format!(
            "Result.is_err expects 1 argument, got {}",
            args.len()
        ));
    }
    match &args[0] {
        RuntimeValue::Enum(name, payload) if name == "Ok" && payload.len() == 1 => {
            Ok(RuntimeValue::Bool(false))
        }
        RuntimeValue::Enum(name, payload) if name == "Err" && payload.len() == 1 => {
            Ok(RuntimeValue::Bool(true))
        }
        other => Err(format!("Result.is_err expects Result, got {}", other)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unwrap_ok() {
        let val = RuntimeValue::Enum("Ok".into(), vec![RuntimeValue::Int(42)]);
        assert_eq!(result_unwrap(&[val]).unwrap(), RuntimeValue::Int(42));
    }

    #[test]
    fn unwrap_err_errors() {
        let val = RuntimeValue::Enum("Err".into(), vec![RuntimeValue::String("fail".into())]);
        assert!(result_unwrap(&[val]).is_err());
    }

    #[test]
    fn unwrap_or_ok() {
        let val = RuntimeValue::Enum("Ok".into(), vec![RuntimeValue::Int(42)]);
        assert_eq!(
            result_unwrap_or(&[val, RuntimeValue::Int(0)]).unwrap(),
            RuntimeValue::Int(42)
        );
    }

    #[test]
    fn unwrap_or_err() {
        let val = RuntimeValue::Enum("Err".into(), vec![RuntimeValue::String("fail".into())]);
        assert_eq!(
            result_unwrap_or(&[val, RuntimeValue::Int(99)]).unwrap(),
            RuntimeValue::Int(99)
        );
    }

    #[test]
    fn is_ok_true() {
        let val = RuntimeValue::Enum("Ok".into(), vec![RuntimeValue::Int(1)]);
        assert_eq!(result_is_ok(&[val]).unwrap(), RuntimeValue::Bool(true));
    }

    #[test]
    fn is_ok_false() {
        let val = RuntimeValue::Enum("Err".into(), vec![RuntimeValue::String("e".into())]);
        assert_eq!(result_is_ok(&[val]).unwrap(), RuntimeValue::Bool(false));
    }

    #[test]
    fn is_err_true() {
        let val = RuntimeValue::Enum("Err".into(), vec![RuntimeValue::String("e".into())]);
        assert_eq!(result_is_err(&[val]).unwrap(), RuntimeValue::Bool(true));
    }

    #[test]
    fn is_err_false() {
        let val = RuntimeValue::Enum("Ok".into(), vec![RuntimeValue::Int(1)]);
        assert_eq!(result_is_err(&[val]).unwrap(), RuntimeValue::Bool(false));
    }
}
