use super::{NValue, NativeError};
use super::fs_sandbox::resolve_sandboxed_path;

pub(super) fn native_fs_read_file(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(path) => {
            let resolved = resolve_sandboxed_path(&path)?;
            match std::fs::read_to_string(&resolved) {
                Ok(content) => Ok(NValue::string(content.into())),
                Err(e) => Err(NativeError(format!(
                    "Fs.read_file!: failed for \"{}\": {}",
                    path, e
                ))),
            }
        }
        None => Err(NativeError(format!(
            "Fs.read_file!: expected String, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_fs_write_file(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_string(), args[1].as_string()) {
        (Some(path), Some(content)) => {
            let resolved = resolve_sandboxed_path(&path)?;
            match std::fs::write(&resolved, &**content) {
                Ok(()) => Ok(NValue::unit()),
                Err(e) => Err(NativeError(format!(
                    "Fs.write_file!: failed for \"{}\": {}",
                    path, e
                ))),
            }
        }
        _ => Err(NativeError(
            "Fs.write_file!: expected (String, String)".into(),
        )),
    }
}

pub(super) fn native_fs_exists(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(path) => {
            let resolved = resolve_sandboxed_path(&path)?;
            Ok(NValue::bool(resolved.exists()))
        }
        None => Err(NativeError(format!(
            "Fs.exists!: expected String, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_fs_list_dir(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(path) => {
            let resolved = resolve_sandboxed_path(&path)?;
            match std::fs::read_dir(&resolved) {
                Ok(entries) => {
                    let mut names: Vec<NValue> = Vec::new();
                    for entry in entries {
                        match entry {
                            Ok(e) => {
                                let name = e.file_name().to_string_lossy().to_string();
                                names.push(NValue::string(name.into()));
                            }
                            Err(e) => {
                                return Err(NativeError(format!(
                                    "Fs.list_dir!: error reading entry: {}",
                                    e
                                )));
                            }
                        }
                    }
                    Ok(NValue::list(names))
                }
                Err(e) => Err(NativeError(format!(
                    "Fs.list_dir!: failed for \"{}\": {}",
                    path, e
                ))),
            }
        }
        None => Err(NativeError(format!(
            "Fs.list_dir!: expected String, got {}",
            args[0]
        ))),
    }
}

