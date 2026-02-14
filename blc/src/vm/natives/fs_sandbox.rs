use std::cell::RefCell;
use std::path::{Path, PathBuf};

use super::NativeError;

// ---------------------------------------------------------------------------
// Filesystem sandbox — optional configurable root directory
// ---------------------------------------------------------------------------
//
// When set, all Fs native operations are constrained to paths within the
// sandbox root. Paths are resolved relative to the root and canonicalized
// to prevent `../` traversal escapes.

thread_local! {
    static FS_SANDBOX_ROOT: RefCell<Option<PathBuf>> = const { RefCell::new(None) };
}

/// Set the filesystem sandbox root. Call from CLI before VM execution.
/// Pass `None` to disable sandboxing (default — all paths allowed).
pub fn set_fs_sandbox(root: Option<PathBuf>) {
    FS_SANDBOX_ROOT.with(|cell| {
        *cell.borrow_mut() = root.map(|p| {
            // Canonicalize at config time so we have a stable base
            p.canonicalize().unwrap_or(p)
        });
    });
}

/// Resolve and validate a user-provided path against the sandbox.
///
/// - If no sandbox is configured, returns the path as-is.
/// - If sandboxed, resolves `user_path` relative to the sandbox root,
///   canonicalizes it, and checks it remains within the root.
pub fn resolve_sandboxed_path(user_path: &str) -> Result<PathBuf, NativeError> {
    FS_SANDBOX_ROOT.with(|cell| {
        let guard = cell.borrow();
        match guard.as_ref() {
            None => Ok(PathBuf::from(user_path)),
            Some(root) => {
                let requested = Path::new(user_path);
                let resolved = if requested.is_absolute() {
                    requested.to_path_buf()
                } else {
                    root.join(requested)
                };

                // For exists/read, the path must already exist to canonicalize.
                // For write, parent must exist. Try canonicalizing as-is first,
                // then fall back to canonicalizing the parent + filename.
                let canonical = if resolved.exists() {
                    resolved.canonicalize().map_err(|e| {
                        NativeError(format!(
                            "Fs sandbox: failed to resolve \"{}\": {}",
                            user_path, e
                        ))
                    })?
                } else {
                    // Path doesn't exist yet (write case) — canonicalize parent
                    let parent = resolved.parent().ok_or_else(|| {
                        NativeError(format!(
                            "Fs sandbox: invalid path \"{}\"",
                            user_path
                        ))
                    })?;
                    let canon_parent = parent.canonicalize().map_err(|e| {
                        NativeError(format!(
                            "Fs sandbox: parent directory does not exist for \"{}\": {}",
                            user_path, e
                        ))
                    })?;
                    let filename = resolved.file_name().ok_or_else(|| {
                        NativeError(format!(
                            "Fs sandbox: invalid path \"{}\"",
                            user_path
                        ))
                    })?;
                    canon_parent.join(filename)
                };

                if canonical.starts_with(root) {
                    Ok(canonical)
                } else {
                    Err(NativeError(format!(
                        "Fs sandbox: path \"{}\" escapes sandbox root",
                        user_path
                    )))
                }
            }
        }
    })
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_no_sandbox_allows_all() {
        FS_SANDBOX_ROOT.with(|cell| *cell.borrow_mut() = None);
        let result = resolve_sandboxed_path("/etc/passwd");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), PathBuf::from("/etc/passwd"));
    }

    #[test]
    fn test_resolve_within_sandbox() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().to_path_buf();
        let test_file = root.join("hello.txt");
        fs::write(&test_file, "hi").unwrap();

        FS_SANDBOX_ROOT.with(|cell| {
            *cell.borrow_mut() = Some(root.canonicalize().unwrap());
        });

        let result = resolve_sandboxed_path("hello.txt");
        assert!(result.is_ok(), "Expected Ok, got: {:?}", result);

        // Clean up thread-local
        FS_SANDBOX_ROOT.with(|cell| *cell.borrow_mut() = None);
    }

    #[test]
    fn test_resolve_escape_blocked() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().to_path_buf();

        FS_SANDBOX_ROOT.with(|cell| {
            *cell.borrow_mut() = Some(root.canonicalize().unwrap());
        });

        let result = resolve_sandboxed_path("../../../etc/passwd");
        assert!(result.is_err(), "Expected Err for escape, got: {:?}", result);

        FS_SANDBOX_ROOT.with(|cell| *cell.borrow_mut() = None);
    }

    #[test]
    fn test_resolve_absolute_escape_blocked() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().to_path_buf();

        FS_SANDBOX_ROOT.with(|cell| {
            *cell.borrow_mut() = Some(root.canonicalize().unwrap());
        });

        let result = resolve_sandboxed_path("/etc/passwd");
        assert!(result.is_err(), "Expected Err for absolute escape, got: {:?}", result);

        FS_SANDBOX_ROOT.with(|cell| *cell.borrow_mut() = None);
    }
}
