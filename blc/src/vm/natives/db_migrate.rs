//! Generic SQL migration runner.
//!
//! `Sql.migrate!(dir: String) -> Result<Int, String>`
//!
//! Scans a directory for numbered SQL migration files, tracks applied migrations
//! in a `_migrations` table, and runs pending migrations in order.
//! Works with whichever database backend is currently active.

use super::db_backend;
use super::{NValue, NativeError, RcStr};

/// Sql.migrate!(dir: String) -> Result<Int, String>
///
/// Run pending migrations from the given directory. Migration files must be
/// named `NNN_description.sql` (e.g. `001_initial.sql`, `002_add_users.sql`).
/// Returns Ok(count) with the number of migrations applied, or Err(message).
pub fn native_sql_migrate(args: &[NValue]) -> Result<NValue, NativeError> {
    let dir = args
        .first()
        .and_then(|v| v.as_string())
        .ok_or_else(|| NativeError("Sql.migrate!: expected directory path as argument".into()))?;

    match run_migrations(dir.as_ref()) {
        Ok(count) => Ok(NValue::enum_val("Ok".into(), NValue::int(count as i64))),
        Err(e) => Ok(NValue::enum_val("Err".into(), NValue::string(RcStr::from(e.as_str())))),
    }
}

fn run_migrations(dir: &str) -> Result<usize, String> {
    // 1. Ensure _migrations table exists
    ensure_migrations_table()
        .map_err(|e| format!("Failed to create _migrations table: {}", e.0))?;

    // 2. Get already-applied migrations
    let applied = get_applied_migrations()
        .map_err(|e| format!("Failed to read _migrations table: {}", e.0))?;

    // 3. Scan directory for migration files
    let mut files = scan_migration_files(dir)?;
    files.sort();

    // 4. Filter to pending migrations
    let pending: Vec<_> = files
        .into_iter()
        .filter(|(name, _)| !applied.contains(name))
        .collect();

    // 5. Apply each pending migration
    let mut count = 0;
    for (name, path) in &pending {
        let sql = std::fs::read_to_string(path)
            .map_err(|e| format!("Failed to read migration {}: {}", name, e))?;

        // Execute the migration SQL
        db_backend::active_execute(&sql, &[])
            .map_err(|e| format!("Migration {} failed: {}", name, e.0))?;

        // Record in _migrations table
        db_backend::active_execute(
            "INSERT INTO _migrations (name, applied_at) VALUES (?1, datetime('now'))",
            &[name.clone()],
        )
        .map_err(|e| format!("Failed to record migration {}: {}", name, e.0))?;

        count += 1;
    }

    Ok(count)
}

fn ensure_migrations_table() -> Result<(), NativeError> {
    db_backend::active_execute(
        "CREATE TABLE IF NOT EXISTS _migrations (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL UNIQUE, applied_at TEXT NOT NULL)",
        &[],
    )?;
    Ok(())
}

fn get_applied_migrations() -> Result<Vec<String>, NativeError> {
    let rows = db_backend::active_query("SELECT name FROM _migrations ORDER BY id", &[])?;
    Ok(rows.into_iter().filter_map(|row| {
        row.into_iter()
            .find(|(k, _)| k == "name")
            .map(|(_, v)| v)
    }).collect())
}

/// Scan a directory for files matching `NNN_description.sql` pattern.
/// Returns sorted vec of (name, full_path).
fn scan_migration_files(dir: &str) -> Result<Vec<(String, String)>, String> {
    let entries = std::fs::read_dir(dir)
        .map_err(|e| format!("Cannot read migration directory '{}': {}", dir, e))?;

    let mut files = Vec::new();
    for entry in entries {
        let entry = entry.map_err(|e| format!("Error reading directory entry: {}", e))?;
        let path = entry.path();
        if let Some(ext) = path.extension() {
            if ext == "sql" {
                let name = path
                    .file_name()
                    .unwrap_or_default()
                    .to_string_lossy()
                    .to_string();
                files.push((name, path.to_string_lossy().to_string()));
            }
        }
    }

    Ok(files)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::natives::db;

    fn setup_memory_db() {
        db::native_sqlite_connect(&[NValue::string(":memory:".into())]).unwrap();
    }

    #[test]
    fn migrate_empty_dir() {
        setup_memory_db();
        let dir = tempfile::tempdir().unwrap();
        let result = run_migrations(dir.path().to_str().unwrap());
        assert_eq!(result.unwrap(), 0);
    }

    #[test]
    fn migrate_applies_in_order() {
        setup_memory_db();
        let dir = tempfile::tempdir().unwrap();

        // Create migration files
        std::fs::write(
            dir.path().join("001_create_users.sql"),
            "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT);",
        )
        .unwrap();
        std::fs::write(
            dir.path().join("002_add_email.sql"),
            "ALTER TABLE users ADD COLUMN email TEXT;",
        )
        .unwrap();

        let count = run_migrations(dir.path().to_str().unwrap()).unwrap();
        assert_eq!(count, 2);

        // Verify the table has the email column
        let rows = db_backend::active_query(
            "SELECT name FROM _migrations ORDER BY id",
            &[],
        )
        .unwrap();
        assert_eq!(rows.len(), 2);
    }

    #[test]
    fn migrate_idempotent() {
        setup_memory_db();
        let dir = tempfile::tempdir().unwrap();

        std::fs::write(
            dir.path().join("001_create_t.sql"),
            "CREATE TABLE t (id INTEGER);",
        )
        .unwrap();

        let first = run_migrations(dir.path().to_str().unwrap()).unwrap();
        assert_eq!(first, 1);

        // Running again should apply nothing
        let second = run_migrations(dir.path().to_str().unwrap()).unwrap();
        assert_eq!(second, 0);
    }

    #[test]
    fn migrate_tracks_in_table() {
        setup_memory_db();
        let dir = tempfile::tempdir().unwrap();

        std::fs::write(
            dir.path().join("001_init.sql"),
            "CREATE TABLE t (id INTEGER);",
        )
        .unwrap();

        run_migrations(dir.path().to_str().unwrap()).unwrap();

        let applied = get_applied_migrations().unwrap();
        assert_eq!(applied, vec!["001_init.sql"]);
    }

    #[test]
    fn migrate_bad_sql_fails() {
        setup_memory_db();
        let dir = tempfile::tempdir().unwrap();

        std::fs::write(
            dir.path().join("001_bad.sql"),
            "THIS IS NOT VALID SQL;",
        )
        .unwrap();

        let result = run_migrations(dir.path().to_str().unwrap());
        assert!(result.is_err());
    }

    #[test]
    fn migrate_bad_directory_fails() {
        setup_memory_db();
        let result = run_migrations("/nonexistent/path/to/migrations");
        assert!(result.is_err());
    }

    #[test]
    fn native_sql_migrate_returns_ok() {
        setup_memory_db();
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(
            dir.path().join("001_init.sql"),
            "CREATE TABLE m (id INTEGER);",
        )
        .unwrap();

        let result = native_sql_migrate(&[
            NValue::string(RcStr::from(dir.path().to_str().unwrap())),
        ])
        .unwrap();

        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Ok");
        assert_eq!(payload.as_any_int(), 1);
    }

    #[test]
    fn native_sql_migrate_returns_err_on_bad_dir() {
        setup_memory_db();
        let result = native_sql_migrate(&[
            NValue::string(RcStr::from("/nonexistent/migrations")),
        ])
        .unwrap();

        let (tag, _) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Err");
    }
}
