use super::{NValue, NativeError};

pub(super) fn native_console_println(args: &[NValue]) -> Result<NValue, NativeError> {
    if args.is_empty() {
        println!();
    } else {
        println!("{}", args[0]);
    }
    Ok(NValue::unit())
}

pub(super) fn native_console_print(args: &[NValue]) -> Result<NValue, NativeError> {
    if !args.is_empty() {
        print!("{}", args[0]);
    }
    Ok(NValue::unit())
}

pub(super) fn native_console_error(args: &[NValue]) -> Result<NValue, NativeError> {
    if !args.is_empty() {
        eprintln!("{}", args[0]);
    }
    Ok(NValue::unit())
}

pub(super) fn native_console_read_line(_args: &[NValue]) -> Result<NValue, NativeError> {
    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .map_err(|e| NativeError(format!("Console.read_line!: {}", e)))?;
    // Trim trailing newline
    if input.ends_with('\n') {
        input.pop();
        if input.ends_with('\r') {
            input.pop();
        }
    }
    Ok(NValue::string(input.into()))
}
