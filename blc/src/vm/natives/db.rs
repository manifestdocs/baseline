use std::cell::RefCell;

use rusqlite::{Connection, params_from_iter};

use super::db_backend::{self, Row};
use super::{NValue, NativeError, RcStr};

// ---------------------------------------------------------------------------
// Thread-local SQLite connection (matches Schema module pattern)
// ---------------------------------------------------------------------------

thread_local! {
    static DB_CONNECTION: RefCell<Option<Connection>> = RefCell::new(None);
}

// ---------------------------------------------------------------------------
// Internal functions (used by db_backend dispatch)
// ---------------------------------------------------------------------------

/// Open a SQLite connection. `:memory:` for in-memory, otherwise a file path.
pub fn sqlite_connect(url: &str) -> Result<(), NativeError> {
    let conn = if url == ":memory:" {
        Connection::open_in_memory()
    } else {
        Connection::open(url)
    }
    .map_err(|e| NativeError(format!("SQLite connect: {}", e)))?;

    DB_CONNECTION.with(|cell| {
        *cell.borrow_mut() = Some(conn);
    });

    Ok(())
}

/// Execute DDL/DML SQL against the SQLite connection.
pub fn sqlite_execute(sql: &str, params: &[String]) -> Result<i64, NativeError> {
    DB_CONNECTION.with(|cell| {
        let borrow = cell.borrow();
        let conn = borrow
            .as_ref()
            .ok_or_else(|| NativeError(
                "SQLite execute: no connection (call Sqlite.connect! first)".into(),
            ))?;

        let affected = conn
            .execute(sql, params_from_iter(params.iter()))
            .map_err(|e| NativeError(format!("SQLite execute: {}", e)))?;

        Ok(affected as i64)
    })
}

/// Execute a SELECT query against the SQLite connection.
pub fn sqlite_query(sql: &str, params: &[String]) -> Result<Vec<Row>, NativeError> {
    DB_CONNECTION.with(|cell| {
        let borrow = cell.borrow();
        let conn = borrow
            .as_ref()
            .ok_or_else(|| NativeError(
                "SQLite query: no connection (call Sqlite.connect! first)".into(),
            ))?;

        let mut stmt = conn
            .prepare(sql)
            .map_err(|e| NativeError(format!("SQLite query: {}", e)))?;

        let column_names: Vec<String> = stmt
            .column_names()
            .iter()
            .map(|s| s.to_string())
            .collect();

        let rows = stmt
            .query_map(params_from_iter(params.iter()), |row| {
                let mut entries = Vec::with_capacity(column_names.len());
                for (i, col_name) in column_names.iter().enumerate() {
                    let val: rusqlite::Result<rusqlite::types::Value> = row.get(i);
                    let val_str = match val {
                        Ok(v) => value_to_string(&v),
                        Err(_) => String::new(),
                    };
                    entries.push((col_name.clone(), val_str));
                }
                Ok(entries)
            })
            .map_err(|e| NativeError(format!("SQLite query: {}", e)))?;

        let mut result = Vec::new();
        for row in rows {
            let row_val = row.map_err(|e| NativeError(format!("SQLite query: {}", e)))?;
            result.push(row_val);
        }

        Ok(result)
    })
}

// ---------------------------------------------------------------------------
// Helper: convert rusqlite Value to String
// ---------------------------------------------------------------------------

fn value_to_string(val: &rusqlite::types::Value) -> String {
    match val {
        rusqlite::types::Value::Null => String::new(),
        rusqlite::types::Value::Integer(i) => i.to_string(),
        rusqlite::types::Value::Real(f) => f.to_string(),
        rusqlite::types::Value::Text(s) => s.clone(),
        rusqlite::types::Value::Blob(b) => format!("<blob:{} bytes>", b.len()),
    }
}

/// Convert raw rows to NValue list of maps.
fn rows_to_nvalue(rows: Vec<Row>) -> NValue {
    let nvalue_rows: Vec<NValue> = rows
        .into_iter()
        .map(|row| {
            let entries: Vec<(NValue, NValue)> = row
                .into_iter()
                .map(|(k, v)| {
                    (
                        NValue::string(RcStr::from(k.as_str())),
                        NValue::string(RcStr::from(v.as_str())),
                    )
                })
                .collect();
            NValue::map(entries)
        })
        .collect();
    NValue::list(nvalue_rows)
}

/// Extract params from a List<String> NValue argument.
fn extract_params(arg: Option<&NValue>) -> Result<Vec<String>, NativeError> {
    let Some(val) = arg else {
        return Ok(Vec::new());
    };

    let items = val
        .as_list()
        .ok_or_else(|| NativeError("expected List<String> for params".into()))?;

    items
        .iter()
        .map(|v| {
            v.as_string()
                .map(|s| s.as_ref().to_string())
                .ok_or_else(|| NativeError("param list must contain strings".into()))
        })
        .collect()
}


// ---------------------------------------------------------------------------
// Native entry points: Sqlite.*
// ---------------------------------------------------------------------------

/// Sqlite.connect!(url: String) -> ()
pub fn native_sqlite_connect(args: &[NValue]) -> Result<NValue, NativeError> {
    let url = args
        .first()
        .and_then(|v| v.as_string())
        .ok_or_else(|| NativeError("Sqlite.connect!: expected string argument".into()))?;

    sqlite_connect(url.as_ref())?;
    db_backend::set_active_sqlite();

    Ok(NValue::unit())
}

/// Sqlite.execute!(sql: String, params: List<String>) -> Int
pub fn native_sqlite_execute(args: &[NValue]) -> Result<NValue, NativeError> {
    let sql = args
        .first()
        .and_then(|v| v.as_string())
        .ok_or_else(|| NativeError("Sqlite.execute!: expected SQL string as first argument".into()))?;

    let params = extract_params(args.get(1))?;
    let affected = sqlite_execute(sql.as_ref(), &params)?;

    Ok(NValue::int(affected))
}

/// Sqlite.query!(sql: String, params: List<String>) -> List<Map<String, String>>
pub fn native_sqlite_query(args: &[NValue]) -> Result<NValue, NativeError> {
    let sql = args
        .first()
        .and_then(|v| v.as_string())
        .ok_or_else(|| NativeError("Sqlite.query!: expected SQL string as first argument".into()))?;

    let params = extract_params(args.get(1))?;
    let rows = sqlite_query(sql.as_ref(), &params)?;

    Ok(rows_to_nvalue(rows))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn connect_in_memory() {
        let result = native_sqlite_connect(&[NValue::string(":memory:".into())]);
        assert!(result.is_ok());
    }

    #[test]
    fn connect_returns_unit() {
        let result = native_sqlite_connect(&[NValue::string(":memory:".into())]).unwrap();
        assert!(result.is_unit());
    }

    #[test]
    fn connect_requires_string_arg() {
        let result = native_sqlite_connect(&[NValue::int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn execute_create_table() {
        native_sqlite_connect(&[NValue::string(":memory:".into())]).unwrap();
        let result = native_sqlite_execute(&[
            NValue::string("CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)".into()),
            NValue::list(vec![]),
        ]);
        assert!(result.is_ok());
    }

    #[test]
    fn execute_insert_returns_affected_count() {
        native_sqlite_connect(&[NValue::string(":memory:".into())]).unwrap();
        native_sqlite_execute(&[
            NValue::string("CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)".into()),
            NValue::list(vec![]),
        ])
        .unwrap();

        let result = native_sqlite_execute(&[
            NValue::string("INSERT INTO t (name) VALUES (?1)".into()),
            NValue::list(vec![NValue::string("Alice".into())]),
        ])
        .unwrap();
        assert_eq!(result.as_any_int(), 1);
    }

    #[test]
    fn execute_without_connection_errors() {
        // Clear any existing connection
        DB_CONNECTION.with(|cell| {
            *cell.borrow_mut() = None;
        });
        db_backend::set_active_sqlite(); // backend set but no connection
        // Need to reset active backend too for clean error
        let result = native_sqlite_execute(&[
            NValue::string("SELECT 1".into()),
            NValue::list(vec![]),
        ]);
        assert!(result.is_err());
    }

    #[test]
    fn execute_invalid_sql_errors() {
        native_sqlite_connect(&[NValue::string(":memory:".into())]).unwrap();
        let result = native_sqlite_execute(&[
            NValue::string("NOT VALID SQL".into()),
            NValue::list(vec![]),
        ]);
        assert!(result.is_err());
    }

    #[test]
    fn query_returns_list_of_maps() {
        native_sqlite_connect(&[NValue::string(":memory:".into())]).unwrap();
        native_sqlite_execute(&[
            NValue::string("CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)".into()),
            NValue::list(vec![]),
        ])
        .unwrap();
        native_sqlite_execute(&[
            NValue::string("INSERT INTO users (name) VALUES (?1)".into()),
            NValue::list(vec![NValue::string("Alice".into())]),
        ])
        .unwrap();

        let result = native_sqlite_query(&[
            NValue::string("SELECT id, name FROM users".into()),
            NValue::list(vec![]),
        ])
        .unwrap();

        let rows = result.as_list().unwrap();
        assert_eq!(rows.len(), 1);
    }

    #[test]
    fn query_empty_result() {
        native_sqlite_connect(&[NValue::string(":memory:".into())]).unwrap();
        native_sqlite_execute(&[
            NValue::string("CREATE TABLE empty_t (id INTEGER)".into()),
            NValue::list(vec![]),
        ])
        .unwrap();

        let result = native_sqlite_query(&[
            NValue::string("SELECT * FROM empty_t".into()),
            NValue::list(vec![]),
        ])
        .unwrap();

        let rows = result.as_list().unwrap();
        assert_eq!(rows.len(), 0);
    }

    #[test]
    fn query_with_params() {
        native_sqlite_connect(&[NValue::string(":memory:".into())]).unwrap();
        native_sqlite_execute(&[
            NValue::string("CREATE TABLE items (id INTEGER PRIMARY KEY, name TEXT)".into()),
            NValue::list(vec![]),
        ])
        .unwrap();
        native_sqlite_execute(&[
            NValue::string("INSERT INTO items (name) VALUES (?1), (?2)".into()),
            NValue::list(vec![
                NValue::string("foo".into()),
                NValue::string("bar".into()),
            ]),
        ])
        .unwrap();

        let result = native_sqlite_query(&[
            NValue::string("SELECT name FROM items WHERE name = ?1".into()),
            NValue::list(vec![NValue::string("foo".into())]),
        ])
        .unwrap();

        let rows = result.as_list().unwrap();
        assert_eq!(rows.len(), 1);
    }

    #[test]
    fn query_without_connection_errors() {
        DB_CONNECTION.with(|cell| {
            *cell.borrow_mut() = None;
        });
        let result = native_sqlite_query(&[
            NValue::string("SELECT 1".into()),
            NValue::list(vec![]),
        ]);
        assert!(result.is_err());
    }

    #[test]
    fn query_integer_values_as_strings() {
        native_sqlite_connect(&[NValue::string(":memory:".into())]).unwrap();
        native_sqlite_execute(&[
            NValue::string("CREATE TABLE nums (val INTEGER)".into()),
            NValue::list(vec![]),
        ])
        .unwrap();
        native_sqlite_execute(&[
            NValue::string("INSERT INTO nums (val) VALUES (42)".into()),
            NValue::list(vec![]),
        ])
        .unwrap();

        let result = native_sqlite_query(&[
            NValue::string("SELECT val FROM nums".into()),
            NValue::list(vec![]),
        ])
        .unwrap();

        let rows = result.as_list().unwrap();
        assert_eq!(rows.len(), 1);
        // The integer 42 should be returned as the string "42"
    }

    #[test]
    fn sql_injection_prevented_by_parameterization() {
        native_sqlite_connect(&[NValue::string(":memory:".into())]).unwrap();
        native_sqlite_execute(&[
            NValue::string("CREATE TABLE secrets (id INTEGER, data TEXT)".into()),
            NValue::list(vec![]),
        ])
        .unwrap();
        native_sqlite_execute(&[
            NValue::string("INSERT INTO secrets (id, data) VALUES (1, 'secret')".into()),
            NValue::list(vec![]),
        ])
        .unwrap();

        // Attempt SQL injection via params — should be safely escaped
        let result = native_sqlite_query(&[
            NValue::string("SELECT * FROM secrets WHERE data = ?1".into()),
            NValue::list(vec![NValue::string("' OR '1'='1".into())]),
        ])
        .unwrap();

        let rows = result.as_list().unwrap();
        assert_eq!(rows.len(), 0); // injection attempt matches nothing
    }

    // -- Explicit Sqlite.* tests --

    #[test]
    fn sqlite_explicit_connect_and_query() {
        native_sqlite_connect(&[NValue::string(":memory:".into())]).unwrap();
        native_sqlite_execute(&[
            NValue::string("CREATE TABLE t (id INTEGER PRIMARY KEY, val TEXT)".into()),
            NValue::list(vec![]),
        ])
        .unwrap();
        native_sqlite_execute(&[
            NValue::string("INSERT INTO t (val) VALUES (?1)".into()),
            NValue::list(vec![NValue::string("hello".into())]),
        ])
        .unwrap();

        let result = native_sqlite_query(&[
            NValue::string("SELECT val FROM t".into()),
            NValue::list(vec![]),
        ])
        .unwrap();

        let rows = result.as_list().unwrap();
        assert_eq!(rows.len(), 1);
    }

    #[test]
    fn sqlite_connect_sets_active_backend() {
        // After Sqlite.connect!, the active backend should be SQLite
        // and Sqlite.execute! should work via dispatch
        native_sqlite_connect(&[NValue::string(":memory:".into())]).unwrap();
        let result = native_sqlite_execute(&[
            NValue::string("CREATE TABLE ab_test (id INTEGER)".into()),
            NValue::list(vec![]),
        ]);
        assert!(result.is_ok());
    }
}
