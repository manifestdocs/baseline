//! Generic SQL backend trait and active-backend dispatch.
//!
//! All database backends (SQLite, Postgres, MySQL) implement `SqlBackend`.
//! The `ACTIVE_BACKEND` thread-local tracks which backend is currently connected,
//! allowing generic operations like `Sql.migrate!` to work with any backend.

use std::cell::RefCell;

use super::NativeError;

// ---------------------------------------------------------------------------
// SqlBackend trait
// ---------------------------------------------------------------------------

/// A row from a query: list of (column_name, string_value) pairs.
pub type Row = Vec<(String, String)>;

/// Trait that all database backends implement.
#[allow(dead_code)]
pub trait SqlBackend {
    /// Execute DDL/DML SQL. Returns number of affected rows.
    fn execute(&self, sql: &str, params: &[String]) -> Result<i64, NativeError>;

    /// Execute a SELECT query. Returns rows as list of (column, value) pairs.
    fn query(&self, sql: &str, params: &[String]) -> Result<Vec<Row>, NativeError>;
}

// ---------------------------------------------------------------------------
// Active backend tracking
// ---------------------------------------------------------------------------

/// Which backend is currently active on this thread.
enum ActiveBackendSlot {
    None,
    Sqlite,
    #[cfg(feature = "postgres")]
    Postgres,
    #[cfg(feature = "mysql")]
    Mysql,
}

thread_local! {
    static ACTIVE_BACKEND: RefCell<ActiveBackendSlot> = RefCell::new(ActiveBackendSlot::None);
}

/// Record that the SQLite backend is now active on this thread.
pub fn set_active_sqlite() {
    ACTIVE_BACKEND.with(|cell| {
        *cell.borrow_mut() = ActiveBackendSlot::Sqlite;
    });
}

/// Record that the Postgres backend is now active on this thread.
#[cfg(feature = "postgres")]
pub fn set_active_postgres() {
    ACTIVE_BACKEND.with(|cell| {
        *cell.borrow_mut() = ActiveBackendSlot::Postgres;
    });
}

/// Record that the MySQL backend is now active on this thread.
#[cfg(feature = "mysql")]
pub fn set_active_mysql() {
    ACTIVE_BACKEND.with(|cell| {
        *cell.borrow_mut() = ActiveBackendSlot::Mysql;
    });
}

// ---------------------------------------------------------------------------
// Generic dispatch (used by Sql.migrate! and other backend-agnostic operations)
// ---------------------------------------------------------------------------

/// Execute SQL against the active backend.
pub fn active_execute(sql: &str, params: &[String]) -> Result<i64, NativeError> {
    ACTIVE_BACKEND.with(|cell| {
        let backend = cell.borrow();
        match *backend {
            ActiveBackendSlot::None => Err(NativeError(
                "No database backend connected (call Sqlite.connect!, Postgres.connect!, etc. first)"
                    .into(),
            )),
            ActiveBackendSlot::Sqlite => {
                super::db::sqlite_execute(sql, params)
            }
            #[cfg(feature = "postgres")]
            ActiveBackendSlot::Postgres => {
                super::db_postgres::postgres_execute(sql, params)
            }
            #[cfg(feature = "mysql")]
            ActiveBackendSlot::Mysql => {
                super::db_mysql::mysql_execute(sql, params)
            }
        }
    })
}

/// Query SQL against the active backend.
pub fn active_query(sql: &str, params: &[String]) -> Result<Vec<Row>, NativeError> {
    ACTIVE_BACKEND.with(|cell| {
        let backend = cell.borrow();
        match *backend {
            ActiveBackendSlot::None => Err(NativeError(
                "No database backend connected (call Sqlite.connect!, Postgres.connect!, etc. first)"
                    .into(),
            )),
            ActiveBackendSlot::Sqlite => {
                super::db::sqlite_query(sql, params)
            }
            #[cfg(feature = "postgres")]
            ActiveBackendSlot::Postgres => {
                super::db_postgres::postgres_query(sql, params)
            }
            #[cfg(feature = "mysql")]
            ActiveBackendSlot::Mysql => {
                super::db_mysql::mysql_query(sql, params)
            }
        }
    })
}
