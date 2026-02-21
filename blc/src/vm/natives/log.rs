use std::sync::OnceLock;

use super::{NValue, NativeError};

// ---------------------------------------------------------------------------
// Configuration (read from environment once, cached for process lifetime)
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum LogLevel {
    Debug = 0,
    Info = 1,
    Warn = 2,
    Error = 3,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LogFormat {
    Text,
    Json,
}

struct LogConfig {
    min_level: LogLevel,
    format: LogFormat,
}

fn config() -> &'static LogConfig {
    static CONFIG: OnceLock<LogConfig> = OnceLock::new();
    CONFIG.get_or_init(|| {
        let min_level = match std::env::var("LOG_LEVEL")
            .unwrap_or_default()
            .to_lowercase()
            .as_str()
        {
            "debug" => LogLevel::Debug,
            "warn" | "warning" => LogLevel::Warn,
            "error" => LogLevel::Error,
            // Default to info (also covers empty / unrecognised values)
            _ => LogLevel::Info,
        };

        let format = match std::env::var("LOG_FORMAT")
            .unwrap_or_default()
            .to_lowercase()
            .as_str()
        {
            "json" => LogFormat::Json,
            _ => LogFormat::Text,
        };

        LogConfig { min_level, format }
    })
}

// ---------------------------------------------------------------------------
// Shared emit helper
// ---------------------------------------------------------------------------

fn emit_log(level: LogLevel, label: &str, args: &[NValue]) -> Result<NValue, NativeError> {
    let cfg = config();

    // Level filtering — silently drop messages below the configured minimum.
    if level < cfg.min_level {
        return Ok(NValue::unit());
    }

    let msg = if args.is_empty() {
        String::new()
    } else {
        args[0].to_string()
    };

    // Extract optional structured fields (second arg, must be a record).
    let fields: Vec<(&str, String)> = if args.len() >= 2 {
        if let Some(record) = args[1].as_record() {
            record
                .iter()
                .map(|(k, v)| (k.as_ref(), v.to_string()))
                .collect()
        } else {
            Vec::new()
        }
    } else {
        Vec::new()
    };

    let timestamp = now_iso8601();

    match cfg.format {
        LogFormat::Text => {
            if fields.is_empty() {
                eprintln!("{} [{}] {}", timestamp, label, msg);
            } else {
                let kvs: Vec<String> = fields.iter().map(|(k, v)| format!("{}={}", k, v)).collect();
                eprintln!("{} [{}] {} {}", timestamp, label, msg, kvs.join(" "));
            }
        }
        LogFormat::Json => {
            // Build a compact JSON object.
            let mut parts = vec![
                format!("\"timestamp\":\"{}\"", timestamp),
                format!("\"level\":\"{}\"", label),
                format!("\"message\":\"{}\"", escape_json(&msg)),
            ];
            for (k, v) in &fields {
                parts.push(format!("\"{}\":\"{}\"", escape_json(k), escape_json(v)));
            }
            eprintln!("{{{}}}", parts.join(","));
        }
    }

    Ok(NValue::unit())
}

// ---------------------------------------------------------------------------
// Timestamp helper
// ---------------------------------------------------------------------------

fn now_iso8601() -> String {
    use std::time::{SystemTime, UNIX_EPOCH};

    let duration = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();
    let secs = duration.as_secs();

    // Break epoch seconds into date + time components.
    let days = secs / 86400;
    let time_of_day = secs % 86400;
    let hours = time_of_day / 3600;
    let minutes = (time_of_day % 3600) / 60;
    let seconds = time_of_day % 60;

    // Convert days since epoch to Y-M-D (civil calendar).
    let (year, month, day) = days_to_date(days as i64);

    format!(
        "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}Z",
        year, month, day, hours, minutes, seconds
    )
}

/// Convert days since 1970-01-01 to (year, month, day).
/// Uses the algorithm from Howard Hinnant's date library.
fn days_to_date(days: i64) -> (i64, u32, u32) {
    let z = days + 719468;
    let era = if z >= 0 { z } else { z - 146096 } / 146097;
    let doe = (z - era * 146097) as u32;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let y = yoe as i64 + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let y = if m <= 2 { y + 1 } else { y };
    (y, m, d)
}

// ---------------------------------------------------------------------------
// JSON string escaping helper
// ---------------------------------------------------------------------------

fn escape_json(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if c < '\x20' => {
                out.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => out.push(c),
        }
    }
    out
}

// ---------------------------------------------------------------------------
// Public native functions
// ---------------------------------------------------------------------------

pub(super) fn native_log_info(args: &[NValue]) -> Result<NValue, NativeError> {
    emit_log(LogLevel::Info, "INFO", args)
}

pub(super) fn native_log_warn(args: &[NValue]) -> Result<NValue, NativeError> {
    emit_log(LogLevel::Warn, "WARN", args)
}

pub(super) fn native_log_error(args: &[NValue]) -> Result<NValue, NativeError> {
    emit_log(LogLevel::Error, "ERROR", args)
}

pub(super) fn native_log_debug(args: &[NValue]) -> Result<NValue, NativeError> {
    emit_log(LogLevel::Debug, "DEBUG", args)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn log_info_returns_unit() {
        let result = native_log_info(&[NValue::string("test message".into())]).unwrap();
        assert!(result.is_unit());
    }

    #[test]
    fn log_info_with_structured_fields() {
        let fields = NValue::record(vec![
            ("user_id".into(), NValue::int(42)),
            ("action".into(), NValue::string("login".into())),
        ]);
        let result = native_log_info(&[NValue::string("user action".into()), fields]).unwrap();
        assert!(result.is_unit());
    }

    #[test]
    fn log_all_levels_return_unit() {
        let msg = NValue::string("test".into());
        assert!(
            native_log_debug(std::slice::from_ref(&msg))
                .unwrap()
                .is_unit()
        );
        assert!(
            native_log_info(std::slice::from_ref(&msg))
                .unwrap()
                .is_unit()
        );
        assert!(
            native_log_warn(std::slice::from_ref(&msg))
                .unwrap()
                .is_unit()
        );
        assert!(native_log_error(&[msg]).unwrap().is_unit());
    }

    #[test]
    fn log_no_args_returns_unit() {
        assert!(native_log_info(&[]).unwrap().is_unit());
    }

    #[test]
    fn timestamp_format() {
        let ts = now_iso8601();
        // Should look like 2026-02-16T07:49:40Z
        assert_eq!(ts.len(), 20);
        assert!(ts.ends_with('Z'));
        assert_eq!(&ts[4..5], "-");
        assert_eq!(&ts[7..8], "-");
        assert_eq!(&ts[10..11], "T");
        assert_eq!(&ts[13..14], ":");
        assert_eq!(&ts[16..17], ":");
    }

    #[test]
    fn escape_json_special_chars() {
        assert_eq!(escape_json("hello"), "hello");
        assert_eq!(escape_json("he\"llo"), "he\\\"llo");
        assert_eq!(escape_json("line\nnew"), "line\\nnew");
        assert_eq!(escape_json("tab\there"), "tab\\there");
    }

    #[test]
    fn days_to_date_epoch() {
        let (y, m, d) = days_to_date(0);
        assert_eq!((y, m, d), (1970, 1, 1));
    }

    #[test]
    fn days_to_date_known() {
        // 2026-02-16 = 20500 days since epoch
        let (y, m, d) = days_to_date(20500);
        assert_eq!((y, m, d), (2026, 2, 16));
    }
}
