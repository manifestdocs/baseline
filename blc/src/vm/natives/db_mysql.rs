//! MySQL/MariaDB backend for the generic SQL interface.
//!
//! Behind `--features mysql`. Uses `mysql_async` for async connections,
//! bridged to sync via the existing tokio runtime.

#![cfg(feature = "mysql")]

use std::cell::RefCell;

use mysql_async::prelude::*;
use mysql_async::{Conn, Opts, Pool};

use std::sync::Arc;

use super::db_backend::{self, ColumnNames, Row, SqlValue};
use super::{NValue, NativeError, RcStr};

// ---------------------------------------------------------------------------
// Thread-local MySQL connection
// ---------------------------------------------------------------------------

thread_local! {
    static MYSQL_CONN: RefCell<Option<Conn>> = RefCell::new(None);
}

// ---------------------------------------------------------------------------
// Internal functions (used by db_backend dispatch)
// ---------------------------------------------------------------------------

pub fn mysql_connect(url: &str) -> Result<(), NativeError> {
    let rt = tokio::runtime::Handle::try_current()
        .map_err(|_| NativeError("Mysql.connect!: no tokio runtime available".into()))?;

    let opts = Opts::from_url(url)
        .map_err(|e| NativeError(format!("Mysql.connect!: invalid URL: {}", e)))?;
    let pool = Pool::new(opts);

    let conn = rt
        .block_on(pool.get_conn())
        .map_err(|e| NativeError(format!("Mysql.connect!: {}", e)))?;

    MYSQL_CONN.with(|cell| {
        *cell.borrow_mut() = Some(conn);
    });

    Ok(())
}

pub fn mysql_execute(sql: &str, params: &[String]) -> Result<i64, NativeError> {
    let rt = tokio::runtime::Handle::try_current()
        .map_err(|_| NativeError("Mysql: no tokio runtime available".into()))?;

    MYSQL_CONN.with(|cell| {
        let mut borrow = cell.borrow_mut();
        let conn = borrow.as_mut().ok_or_else(|| {
            NativeError("Mysql.execute!: no connection (call Mysql.connect! first)".into())
        })?;

        let mysql_params: Vec<mysql_async::Value> = params
            .iter()
            .map(|s| mysql_async::Value::from(s.as_str()))
            .collect();

        let result = rt
            .block_on(conn.exec_drop(sql, mysql_params))
            .map_err(|e| NativeError(format!("Mysql.execute!: {}", e)))?;

        // mysql_async doesn't easily return affected rows from exec_drop,
        // so we query it separately
        let affected = rt
            .block_on(async {
                let row: Option<(i64,)> = conn.query_first("SELECT ROW_COUNT()").await?;
                Ok::<_, mysql_async::Error>(row.map(|r| r.0).unwrap_or(0))
            })
            .map_err(|e| NativeError(format!("Mysql.execute!: {}", e)))?;

        Ok(affected)
    })
}

pub fn mysql_query(sql: &str, params: &[String]) -> Result<Vec<Row>, NativeError> {
    let rt = tokio::runtime::Handle::try_current()
        .map_err(|_| NativeError("Mysql: no tokio runtime available".into()))?;

    MYSQL_CONN.with(|cell| {
        let mut borrow = cell.borrow_mut();
        let conn = borrow.as_mut().ok_or_else(|| {
            NativeError("Mysql.query!: no connection (call Mysql.connect! first)".into())
        })?;

        let mysql_params: Vec<mysql_async::Value> = params
            .iter()
            .map(|s| mysql_async::Value::from(s.as_str()))
            .collect();

        let result = rt
            .block_on(async {
                let stmt = conn.prep(sql).await?;
                let columns: ColumnNames = Arc::new(
                    stmt.columns()
                        .iter()
                        .map(|c| RcStr::from(c.name_str().as_ref()))
                        .collect(),
                );

                let rows: Vec<mysql_async::Row> = conn.exec(stmt, mysql_params).await?;

                let mut result = Vec::with_capacity(rows.len());
                for row in rows {
                    let mut values = Vec::with_capacity(columns.len());
                    for i in 0..columns.len() {
                        let val = if let Some(v) = row.get::<i64, _>(i) {
                            SqlValue::Int(v)
                        } else if let Some(v) = row.get::<f64, _>(i) {
                            SqlValue::Float(v)
                        } else if let Some(v) = row.get::<String, _>(i) {
                            SqlValue::Text(RcStr::from(v.as_str()))
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
                Ok::<_, mysql_async::Error>(result)
            })
            .map_err(|e| NativeError(format!("Mysql.query!: {}", e)))?;

        Ok(result)
    })
}

// ---------------------------------------------------------------------------
// Native entry points: Mysql.*
// ---------------------------------------------------------------------------

/// Mysql.connect!(url: String) -> ()
pub fn native_mysql_connect(args: &[NValue]) -> Result<NValue, NativeError> {
    let url = args
        .first()
        .and_then(|v| v.as_string())
        .ok_or_else(|| NativeError("Mysql.connect!: expected connection URL string".into()))?;

    mysql_connect(url.as_ref())?;
    db_backend::set_active_mysql();

    Ok(NValue::unit())
}

/// Mysql.execute!(sql: String, params: List<String>) -> Int
pub fn native_mysql_execute(args: &[NValue]) -> Result<NValue, NativeError> {
    let sql = args
        .first()
        .and_then(|v| v.as_string())
        .ok_or_else(|| NativeError("Mysql.execute!: expected SQL string".into()))?;

    let params = extract_params(args.get(1))?;
    let affected = mysql_execute(sql.as_ref(), &params)?;

    Ok(NValue::int(affected))
}

/// Mysql.query!(sql: String, params: List<String>) -> List<Row>
pub fn native_mysql_query(args: &[NValue]) -> Result<NValue, NativeError> {
    let sql = args
        .first()
        .and_then(|v| v.as_string())
        .ok_or_else(|| NativeError("Mysql.query!: expected SQL string".into()))?;

    let params = extract_params(args.get(1))?;
    let rows = mysql_query(sql.as_ref(), &params)?;

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
        let result = native_mysql_connect(&[NValue::int(42)]);
        assert!(result.is_err());
    }

    #[test]
    #[ignore] // Requires running MySQL instance
    fn mysql_roundtrip() {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            native_mysql_connect(&[NValue::string(
                "mysql://root:password@localhost/test_baseline".into(),
            )])
            .unwrap();

            native_mysql_execute(&[
                NValue::string(
                    "CREATE TABLE IF NOT EXISTS mysql_test (id INT AUTO_INCREMENT PRIMARY KEY, val VARCHAR(255))"
                        .into(),
                ),
                NValue::list(vec![]),
            ])
            .unwrap();

            native_mysql_execute(&[
                NValue::string("INSERT INTO mysql_test (val) VALUES (?)".into()),
                NValue::list(vec![NValue::string("hello".into())]),
            ])
            .unwrap();

            let result = native_mysql_query(&[
                NValue::string("SELECT val FROM mysql_test LIMIT 1".into()),
                NValue::list(vec![]),
            ])
            .unwrap();

            let rows = result.as_list().unwrap();
            assert!(!rows.is_empty());

            // Cleanup
            native_mysql_execute(&[
                NValue::string("DROP TABLE mysql_test".into()),
                NValue::list(vec![]),
            ])
            .unwrap();
        });
    }
}
