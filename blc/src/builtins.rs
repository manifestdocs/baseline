use std::collections::HashMap;
use std::io::{self, BufRead, Write};
use std::time::{SystemTime, UNIX_EPOCH, Duration};

/// A native builtin function that takes a list of string-formatted arguments
/// and returns a string result (or error).
///
/// We use String-based I/O to avoid coupling to RuntimeValue (which has lifetime params).
/// The interpreter will convert RuntimeValue -> String before calling, and parse the result back.
pub type BuiltinFn = fn(args: &[String]) -> Result<String, String>;

/// Registry of all builtin functions, keyed by qualified name (e.g., "Console.println!").
pub struct BuiltinRegistry {
    builtins: HashMap<String, BuiltinFn>,
}

impl BuiltinRegistry {
    pub fn new() -> Self {
        let mut registry = Self {
            builtins: HashMap::new(),
        };

        // Console
        registry.register("Console.println!", console_println);
        registry.register("Console.print!", console_print);
        registry.register("Console.error!", console_error);
        registry.register("Console.read_line!", console_read_line);

        // Log
        registry.register("Log.info!", log_info);
        registry.register("Log.warn!", log_warn);
        registry.register("Log.error!", log_error);
        registry.register("Log.debug!", log_debug);

        // Time
        registry.register("Time.now!", time_now);
        registry.register("Time.sleep!", time_sleep);

        // Random
        registry.register("Random.int!", random_int);
        registry.register("Random.bool!", random_bool);

        // Env
        registry.register("Env.get!", env_get);
        registry.register("Env.set!", env_set);

        // Fs
        registry.register("Fs.read!", fs_read);
        registry.register("Fs.write!", fs_write);
        registry.register("Fs.exists!", fs_exists);
        registry.register("Fs.delete!", fs_delete);

        registry
    }

    pub fn get(&self, name: &str) -> Option<&BuiltinFn> {
        self.builtins.get(name)
    }

    fn register(&mut self, name: &str, f: BuiltinFn) {
        self.builtins.insert(name.to_string(), f);
    }
}

// ---------------------------------------------------------------------------
// Argument validation helper
// ---------------------------------------------------------------------------

fn expect_args(name: &str, args: &[String], expected: usize) -> Result<(), String> {
    if args.len() != expected {
        return Err(format!(
            "{name} expects {expected} argument{}, got {}",
            if expected == 1 { "" } else { "s" },
            args.len()
        ));
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Console builtins
// ---------------------------------------------------------------------------

fn console_println(args: &[String]) -> Result<String, String> {
    expect_args("Console.println!", args, 1)?;
    println!("{}", args[0]);
    Ok("()".to_string())
}

fn console_print(args: &[String]) -> Result<String, String> {
    expect_args("Console.print!", args, 1)?;
    print!("{}", args[0]);
    io::stdout().flush().map_err(|e| e.to_string())?;
    Ok("()".to_string())
}

fn console_error(args: &[String]) -> Result<String, String> {
    expect_args("Console.error!", args, 1)?;
    eprintln!("{}", args[0]);
    Ok("()".to_string())
}

fn console_read_line(args: &[String]) -> Result<String, String> {
    expect_args("Console.read_line!", args, 0)?;
    let stdin = io::stdin();
    let mut line = String::new();
    stdin
        .lock()
        .read_line(&mut line)
        .map_err(|e| e.to_string())?;
    // Trim the trailing newline that read_line includes
    if line.ends_with('\n') {
        line.pop();
        if line.ends_with('\r') {
            line.pop();
        }
    }
    Ok(line)
}

// ---------------------------------------------------------------------------
// Log builtins
// ---------------------------------------------------------------------------

fn log_info(args: &[String]) -> Result<String, String> {
    expect_args("Log.info!", args, 1)?;
    eprintln!("[INFO] {}", args[0]);
    Ok("()".to_string())
}

fn log_warn(args: &[String]) -> Result<String, String> {
    expect_args("Log.warn!", args, 1)?;
    eprintln!("[WARN] {}", args[0]);
    Ok("()".to_string())
}

fn log_error(args: &[String]) -> Result<String, String> {
    expect_args("Log.error!", args, 1)?;
    eprintln!("[ERROR] {}", args[0]);
    Ok("()".to_string())
}

fn log_debug(args: &[String]) -> Result<String, String> {
    expect_args("Log.debug!", args, 1)?;
    eprintln!("[DEBUG] {}", args[0]);
    Ok("()".to_string())
}

// ---------------------------------------------------------------------------
// Time builtins
// ---------------------------------------------------------------------------

fn time_now(args: &[String]) -> Result<String, String> {
    expect_args("Time.now!", args, 0)?;
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|e| e.to_string())?
        .as_millis();
    Ok(millis.to_string())
}

fn time_sleep(args: &[String]) -> Result<String, String> {
    expect_args("Time.sleep!", args, 1)?;
    let ms: u64 = args[0]
        .parse()
        .map_err(|_| format!("Time.sleep! expected integer milliseconds, got \"{}\"", args[0]))?;
    std::thread::sleep(Duration::from_millis(ms));
    Ok("()".to_string())
}

// ---------------------------------------------------------------------------
// Random builtins (simple LCG seeded from SystemTime, no external crate)
// ---------------------------------------------------------------------------

/// Produce a pseudo-random u64 from the current system time.
/// Not cryptographic -- sufficient for scripting / demo use.
fn simple_random_u64() -> u64 {
    let seed = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or(Duration::from_nanos(42))
        .as_nanos() as u64;
    // Splitmix64-style mixing for better distribution from a time-based seed
    let mut z = seed.wrapping_add(0x9e3779b97f4a7c15);
    z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
    z ^ (z >> 31)
}

fn random_int(args: &[String]) -> Result<String, String> {
    expect_args("Random.int!", args, 2)?;
    let min: i64 = args[0]
        .parse()
        .map_err(|_| format!("Random.int! expected integer min, got \"{}\"", args[0]))?;
    let max: i64 = args[1]
        .parse()
        .map_err(|_| format!("Random.int! expected integer max, got \"{}\"", args[1]))?;
    if min > max {
        return Err(format!(
            "Random.int! min ({min}) must be <= max ({max})"
        ));
    }
    let range = (max - min) as u64 + 1;
    let value = min + (simple_random_u64() % range) as i64;
    Ok(value.to_string())
}

fn random_bool(args: &[String]) -> Result<String, String> {
    expect_args("Random.bool!", args, 0)?;
    let value = simple_random_u64() % 2 == 0;
    Ok(value.to_string())
}

// ---------------------------------------------------------------------------
// Env builtins
// ---------------------------------------------------------------------------

fn env_get(args: &[String]) -> Result<String, String> {
    expect_args("Env.get!", args, 1)?;
    std::env::var(&args[0]).map_err(|_| {
        format!("Env.get! environment variable \"{}\" is not set", args[0])
    })
}

fn env_set(args: &[String]) -> Result<String, String> {
    expect_args("Env.set!", args, 2)?;
    // SAFETY: single-threaded interpreter context
    unsafe {
        std::env::set_var(&args[0], &args[1]);
    }
    Ok("()".to_string())
}

// ---------------------------------------------------------------------------
// Fs builtins
// ---------------------------------------------------------------------------

fn fs_read(args: &[String]) -> Result<String, String> {
    expect_args("Fs.read!", args, 1)?;
    std::fs::read_to_string(&args[0]).map_err(|e| {
        format!("Fs.read! failed for \"{}\": {e}", args[0])
    })
}

fn fs_write(args: &[String]) -> Result<String, String> {
    expect_args("Fs.write!", args, 2)?;
    std::fs::write(&args[0], &args[1]).map_err(|e| {
        format!("Fs.write! failed for \"{}\": {e}", args[0])
    })?;
    Ok("()".to_string())
}

fn fs_exists(args: &[String]) -> Result<String, String> {
    expect_args("Fs.exists!", args, 1)?;
    Ok(std::path::Path::new(&args[0]).exists().to_string())
}

fn fs_delete(args: &[String]) -> Result<String, String> {
    expect_args("Fs.delete!", args, 1)?;
    std::fs::remove_file(&args[0]).map_err(|e| {
        format!("Fs.delete! failed for \"{}\": {e}", args[0])
    })?;
    Ok("()".to_string())
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn registry_contains_all_builtins() {
        let reg = BuiltinRegistry::new();
        let expected = [
            "Console.println!",
            "Console.print!",
            "Console.error!",
            "Console.read_line!",
            "Log.info!",
            "Log.warn!",
            "Log.error!",
            "Log.debug!",
            "Time.now!",
            "Time.sleep!",
            "Random.int!",
            "Random.bool!",
            "Env.get!",
            "Env.set!",
            "Fs.read!",
            "Fs.write!",
            "Fs.exists!",
            "Fs.delete!",
        ];
        for name in &expected {
            assert!(reg.get(name).is_some(), "missing builtin: {name}");
        }
    }

    #[test]
    fn unknown_builtin_returns_none() {
        let reg = BuiltinRegistry::new();
        assert!(reg.get("Nope.nope!").is_none());
    }

    #[test]
    fn arg_count_validation() {
        let one_arg = vec!["hello".to_string()];
        let no_args: Vec<String> = vec![];

        // Too few
        assert!(console_println(&no_args).is_err());
        // Too many
        assert!(console_read_line(&one_arg).is_err());
    }

    #[test]
    fn time_now_returns_positive_millis() {
        let result = time_now(&[]).unwrap();
        let millis: u128 = result.parse().unwrap();
        assert!(millis > 0);
    }

    #[test]
    fn random_int_within_range() {
        let args = vec!["1".to_string(), "10".to_string()];
        let result = random_int(&args).unwrap();
        let val: i64 = result.parse().unwrap();
        assert!((1..=10).contains(&val));
    }

    #[test]
    fn random_int_rejects_inverted_range() {
        let args = vec!["10".to_string(), "1".to_string()];
        assert!(random_int(&args).is_err());
    }

    #[test]
    fn random_bool_returns_valid_string() {
        let result = random_bool(&[]).unwrap();
        assert!(result == "true" || result == "false");
    }

    #[test]
    fn env_roundtrip() {
        let key = "BASELINE_TEST_BUILTINS_42".to_string();
        let val = "hello_baseline".to_string();

        env_set(&[key.clone(), val.clone()]).unwrap();
        let got = env_get(&[key]).unwrap();
        assert_eq!(got, val);
    }

    #[test]
    fn fs_roundtrip() {
        let path = std::env::temp_dir()
            .join("baseline_builtin_test.txt")
            .to_string_lossy()
            .to_string();
        let content = "baseline test data".to_string();

        // Write
        fs_write(&[path.clone(), content.clone()]).unwrap();
        // Exists
        assert_eq!(fs_exists(&[path.clone()]).unwrap(), "true");
        // Read
        assert_eq!(fs_read(&[path.clone()]).unwrap(), content);
        // Delete
        fs_delete(&[path.clone()]).unwrap();
        assert_eq!(fs_exists(&[path.clone()]).unwrap(), "false");
    }

    #[test]
    fn fs_read_missing_file_returns_error() {
        let result = fs_read(&["/tmp/no_such_file_baseline_99.txt".to_string()]);
        assert!(result.is_err());
    }
}
