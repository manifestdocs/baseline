//! PostgreSQL backend for the generic SQL interface.
//!
//! Behind `--features postgres`. Uses `tokio-postgres` for async connections,
//! bridged to sync via the existing tokio runtime.

#![cfg(feature = "postgres")]

use std::cell::RefCell;

use tokio_postgres::{Client, NoTls};

use std::sync::Arc;

use super::db_backend::{self, ColumnNames, Row, SqlValue};
use super::{NValue, NativeError, RcStr};

// ---------------------------------------------------------------------------
// Thread-local Postgres connection
// ---------------------------------------------------------------------------

thread_local! {
    static PG_CLIENT: RefCell<Option<Client>> = RefCell::new(None);
}

// ---------------------------------------------------------------------------
// Internal functions (used by db_backend dispatch)
// ---------------------------------------------------------------------------

pub fn postgres_connect(url: &str) -> Result<(), NativeError> {
    let rt = tokio::runtime::Handle::try_current()
        .map_err(|_| NativeError("Postgres.connect!: no tokio runtime available".into()))?;

    let (client, connection) = rt
        .block_on(tokio_postgres::connect(url, NoTls))
        .map_err(|e| NativeError(format!("Postgres.connect!: {}", e)))?;

    // Spawn the connection handler
    rt.spawn(async move {
        if let Err(e) = connection.await {
            eprintln!("Postgres connection error: {}", e);
        }
    });

    PG_CLIENT.with(|cell| {
        *cell.borrow_mut() = Some(client);
    });

    Ok(())
}

pub fn postgres_execute(sql: &str, params: &[String]) -> Result<i64, NativeError> {
    let rt = tokio::runtime::Handle::try_current()
        .map_err(|_| NativeError("Postgres: no tokio runtime available".into()))?;

    PG_CLIENT.with(|cell| {
        let borrow = cell.borrow();
        let client = borrow.as_ref().ok_or_else(|| {
            NativeError("Postgres.execute!: no connection (call Postgres.connect! first)".into())
        })?;

        let pg_params: Vec<&(dyn tokio_postgres::types::ToSql + Sync)> = params
            .iter()
            .map(|s| s as &(dyn tokio_postgres::types::ToSql + Sync))
            .collect();

        let affected = rt
            .block_on(client.execute(sql, &pg_params))
            .map_err(|e| NativeError(format!("Postgres.execute!: {}", e)))?;

        Ok(affected as i64)
    })
}

pub fn postgres_query(sql: &str, params: &[String]) -> Result<Vec<Row>, NativeError> {
    let rt = tokio::runtime::Handle::try_current()
        .map_err(|_| NativeError("Postgres: no tokio runtime available".into()))?;

    PG_CLIENT.with(|cell| {
        let borrow = cell.borrow();
        let client = borrow.as_ref().ok_or_else(|| {
            NativeError("Postgres.query!: no connection (call Postgres.connect! first)".into())
        })?;

        let pg_params: Vec<&(dyn tokio_postgres::types::ToSql + Sync)> = params
            .iter()
            .map(|s| s as &(dyn tokio_postgres::types::ToSql + Sync))
            .collect();

        let rows = rt
            .block_on(client.query(sql, &pg_params))
            .map_err(|e| NativeError(format!("Postgres.query!: {}", e)))?;

        // Build shared column names from first row (or empty query)
        let columns: ColumnNames = if let Some(first) = rows.first() {
            Arc::new(
                first
                    .columns()
                    .iter()
                    .map(|c| RcStr::from(c.name()))
                    .collect(),
            )
        } else {
            Arc::new(Vec::new())
        };

        let mut result = Vec::with_capacity(rows.len());
        for row in &rows {
            let mut values = Vec::with_capacity(columns.len());
            for i in 0..row.columns().len() {
                let val = if let Ok(v) = row.try_get::<_, i64>(i) {
                    SqlValue::Int(v)
                } else if let Ok(v) = row.try_get::<_, f64>(i) {
                    SqlValue::Float(v)
                } else if let Ok(v) = row.try_get::<_, String>(i) {
                    SqlValue::Text(RcStr::from(v.as_str()))
                } else if let Ok(v) = row.try_get::<_, bool>(i) {
                    SqlValue::Int(if v { 1 } else { 0 })
                } else {
                    SqlValue::Null
                };
                values.push(val);
            }
            result.push(Row {
                columns: columns.clone(),
                values,
            });
        }

        Ok(result)
    })
}

// ---------------------------------------------------------------------------
// Native entry points: Postgres.*
// ---------------------------------------------------------------------------

/// Postgres.connect!(url: String) -> ()
pub fn native_postgres_connect(args: &[NValue]) -> Result<NValue, NativeError> {
    let url = args
        .first()
        .and_then(|v| v.as_string())
        .ok_or_else(|| NativeError("Postgres.connect!: expected connection string".into()))?;

    postgres_connect(url.as_ref())?;
    db_backend::set_active_postgres();

    Ok(NValue::unit())
}

/// Postgres.execute!(sql: String, params: List<String>) -> Int
pub fn native_postgres_execute(args: &[NValue]) -> Result<NValue, NativeError> {
    let sql = args
        .first()
        .and_then(|v| v.as_string())
        .ok_or_else(|| NativeError("Postgres.execute!: expected SQL string".into()))?;

    let params = extract_params(args.get(1))?;
    let affected = postgres_execute(sql.as_ref(), &params)?;

    Ok(NValue::int(affected))
}

/// Postgres.query!(sql: String, params: List<String>) -> List<Row>
pub fn native_postgres_query(args: &[NValue]) -> Result<NValue, NativeError> {
    let sql = args
        .first()
        .and_then(|v| v.as_string())
        .ok_or_else(|| NativeError("Postgres.query!: expected SQL string".into()))?;

    let params = extract_params(args.get(1))?;
    let rows = postgres_query(sql.as_ref(), &params)?;

    Ok(rows_to_nvalue(rows))
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

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

fn rows_to_nvalue(rows: Vec<Row>) -> NValue {
    let nvalue_rows: Vec<NValue> = rows
        .into_iter()
        .map(|row| NValue::row(row.columns, row.values))
        .collect();
    NValue::list(nvalue_rows)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn connect_requires_string_arg() {
        let result = native_postgres_connect(&[NValue::int(42)]);
        assert!(result.is_err());
    }

    #[test]
    #[ignore] // Requires running PostgreSQL instance
    fn postgres_roundtrip() {
        // Set up tokio runtime for test
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            native_postgres_connect(&[NValue::string(
                "host=localhost user=postgres dbname=test_baseline".into(),
            )])
            .unwrap();

            native_postgres_execute(&[
                NValue::string("CREATE TABLE IF NOT EXISTS pg_test (id SERIAL, val TEXT)".into()),
                NValue::list(vec![]),
            ])
            .unwrap();

            native_postgres_execute(&[
                NValue::string("INSERT INTO pg_test (val) VALUES ($1)".into()),
                NValue::list(vec![NValue::string("hello".into())]),
            ])
            .unwrap();

            let result = native_postgres_query(&[
                NValue::string("SELECT val FROM pg_test LIMIT 1".into()),
                NValue::list(vec![]),
            ])
            .unwrap();

            let rows = result.as_list().unwrap();
            assert!(!rows.is_empty());

            // Cleanup
            native_postgres_execute(&[
                NValue::string("DROP TABLE pg_test".into()),
                NValue::list(vec![]),
            ])
            .unwrap();
        });
    }
}
