use std::cell::RefCell;
use std::collections::HashMap;

use regex::Regex;

use super::{NValue, NativeError};

thread_local! {
    static REGEX_CACHE: RefCell<HashMap<String, Regex>> = RefCell::new(HashMap::new());
}

fn with_regex<F, T>(pattern: &str, f: F) -> Result<T, NativeError>
where
    F: FnOnce(&Regex) -> T,
{
    REGEX_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        if !cache.contains_key(pattern) {
            let re = Regex::new(pattern).map_err(|e| {
                NativeError(format!(
                    "String.matches: invalid regex '{}': {}",
                    pattern, e
                ))
            })?;
            cache.insert(pattern.to_string(), re);
        }
        Ok(f(cache.get(pattern).unwrap()))
    })
}

pub(super) fn native_string_matches(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_string(), args[1].as_string()) {
        (Some(s), Some(pattern)) => {
            let matched = with_regex(pattern, |re| re.is_match(s))?;
            Ok(NValue::bool(matched))
        }
        _ => Err(NativeError(
            "String.matches: expected (String, String)".into(),
        )),
    }
}

pub(super) fn native_string_find_matches(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_string(), args[1].as_string()) {
        (Some(s), Some(pattern)) => {
            let matches = with_regex(pattern, |re| {
                re.find_iter(s)
                    .map(|m| NValue::string(m.as_str().into()))
                    .collect::<Vec<_>>()
            })?;
            Ok(NValue::list(matches))
        }
        _ => Err(NativeError(
            "String.find_matches: expected (String, String)".into(),
        )),
    }
}

pub(super) fn native_string_replace_regex(args: &[NValue]) -> Result<NValue, NativeError> {
    match (
        args[0].as_string(),
        args[1].as_string(),
        args[2].as_string(),
    ) {
        (Some(s), Some(pattern), Some(replacement)) => {
            let result = with_regex(pattern, |re| re.replace_all(s, &**replacement).into_owned())?;
            Ok(NValue::string(result.into()))
        }
        _ => Err(NativeError(
            "String.replace_regex: expected (String, String, String)".into(),
        )),
    }
}
