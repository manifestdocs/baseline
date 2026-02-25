//! Helper functions for parsing database query result rows.
//!
//! Row accessors (`Row.string`, `Row.int`, etc.) extract typed values directly
//! from `HeapObject::Row` — no string parsing needed.
//!
//! Legacy `Db.*` helpers accept both `HeapObject::Row` and `HeapObject::Map`
//! for backward compatibility during the transition period.

use super::db_backend::SqlValue;
use super::{HeapObject, NValue, NativeError, RcStr};

// ---------------------------------------------------------------------------
// Row column lookup (shared by all Row.* accessors)
// ---------------------------------------------------------------------------

/// Find a column by name in a Row, returning its SqlValue.
fn row_lookup<'a>(columns: &[RcStr], values: &'a [SqlValue], name: &str) -> Option<&'a SqlValue> {
    columns
        .iter()
        .position(|c: &RcStr| c.as_ref() == name)
        .map(|i| &values[i])
}

/// Extract the column name from the second argument.
fn col_name(args: &[NValue]) -> Result<&str, NativeError> {
    args.get(1)
        .and_then(|v| v.as_string())
        .map(|s| s.as_ref())
        .ok_or_else(|| NativeError("expected column name as String".into()))
}

// ---------------------------------------------------------------------------
// Row.* accessors — typed column extraction
// ---------------------------------------------------------------------------

/// Row.string(row: Row, col: String) -> String
///
/// Extract a string column. Coerces non-Text values to their string representation.
pub(super) fn native_row_string(args: &[NValue]) -> Result<NValue, NativeError> {
    let name = col_name(args)?;
    let (columns, values) = args[0]
        .as_row()
        .ok_or_else(|| NativeError("Row.string: expected Row".into()))?;

    match row_lookup(columns, values, name) {
        Some(SqlValue::Text(s)) => Ok(NValue::string(s.clone())),
        Some(SqlValue::Int(i)) => Ok(NValue::string(RcStr::from(i.to_string().as_str()))),
        Some(SqlValue::Float(f)) => Ok(NValue::string(RcStr::from(f.to_string().as_str()))),
        Some(SqlValue::Null) => Ok(NValue::string(RcStr::from(""))),
        Some(SqlValue::Blob(_)) => Ok(NValue::string(RcStr::from("<blob>"))),
        None => Err(NativeError(format!(
            "Row.string: column '{}' not found",
            name
        ))),
    }
}

/// Row.int(row: Row, col: String) -> Int
///
/// Extract an integer column directly — no string parsing.
pub(super) fn native_row_int(args: &[NValue]) -> Result<NValue, NativeError> {
    let name = col_name(args)?;
    let (columns, values) = args[0]
        .as_row()
        .ok_or_else(|| NativeError("Row.int: expected Row".into()))?;

    match row_lookup(columns, values, name) {
        Some(SqlValue::Int(i)) => Ok(NValue::int(*i)),
        Some(SqlValue::Float(f)) => Ok(NValue::int(*f as i64)),
        Some(SqlValue::Null) => Ok(NValue::int(0)),
        Some(SqlValue::Text(s)) => {
            let n = s.as_ref().parse::<i64>().unwrap_or(0);
            Ok(NValue::int(n))
        }
        Some(SqlValue::Blob(_)) => {
            Err(NativeError(format!("Row.int: column '{}' is a blob", name)))
        }
        None => Err(NativeError(format!("Row.int: column '{}' not found", name))),
    }
}

/// Row.float(row: Row, col: String) -> Float
///
/// Extract a float column directly.
pub(super) fn native_row_float(args: &[NValue]) -> Result<NValue, NativeError> {
    let name = col_name(args)?;
    let (columns, values) = args[0]
        .as_row()
        .ok_or_else(|| NativeError("Row.float: expected Row".into()))?;

    match row_lookup(columns, values, name) {
        Some(SqlValue::Float(f)) => Ok(NValue::float(*f)),
        Some(SqlValue::Int(i)) => Ok(NValue::float(*i as f64)),
        Some(SqlValue::Null) => Ok(NValue::float(0.0)),
        Some(SqlValue::Text(s)) => {
            let f = s.as_ref().parse::<f64>().unwrap_or(0.0);
            Ok(NValue::float(f))
        }
        Some(SqlValue::Blob(_)) => Err(NativeError(format!(
            "Row.float: column '{}' is a blob",
            name
        ))),
        None => Err(NativeError(format!(
            "Row.float: column '{}' not found",
            name
        ))),
    }
}

/// Row.bool(row: Row, col: String) -> Boolean
///
/// Extract a boolean column. Int(1) = true, anything else = false.
pub(super) fn native_row_bool(args: &[NValue]) -> Result<NValue, NativeError> {
    let name = col_name(args)?;
    let (columns, values) = args[0]
        .as_row()
        .ok_or_else(|| NativeError("Row.bool: expected Row".into()))?;

    match row_lookup(columns, values, name) {
        Some(SqlValue::Int(i)) => Ok(NValue::bool(*i != 0)),
        Some(SqlValue::Float(f)) => Ok(NValue::bool(*f != 0.0)),
        Some(SqlValue::Text(s)) => Ok(NValue::bool(s.as_ref() == "1" || s.as_ref() == "true")),
        Some(SqlValue::Null) => Ok(NValue::bool(false)),
        Some(SqlValue::Blob(_)) => Ok(NValue::bool(false)),
        None => Err(NativeError(format!(
            "Row.bool: column '{}' not found",
            name
        ))),
    }
}

/// Row.optional_string(row: Row, col: String) -> Option<String>
///
/// Extract a nullable string column. Null → None.
pub(super) fn native_row_optional_string(args: &[NValue]) -> Result<NValue, NativeError> {
    let name = col_name(args)?;
    let (columns, values) = args[0]
        .as_row()
        .ok_or_else(|| NativeError("Row.optional_string: expected Row".into()))?;

    match row_lookup(columns, values, name) {
        Some(SqlValue::Null) => Ok(NValue::enum_val("None".into(), NValue::unit())),
        Some(SqlValue::Text(s)) => Ok(NValue::enum_val("Some".into(), NValue::string(s.clone()))),
        Some(SqlValue::Int(i)) => Ok(NValue::enum_val(
            "Some".into(),
            NValue::string(RcStr::from(i.to_string().as_str())),
        )),
        Some(SqlValue::Float(f)) => Ok(NValue::enum_val(
            "Some".into(),
            NValue::string(RcStr::from(f.to_string().as_str())),
        )),
        Some(SqlValue::Blob(_)) => Ok(NValue::enum_val(
            "Some".into(),
            NValue::string(RcStr::from("<blob>")),
        )),
        None => Err(NativeError(format!(
            "Row.optional_string: column '{}' not found",
            name
        ))),
    }
}

/// Row.optional_int(row: Row, col: String) -> Option<Int>
///
/// Extract a nullable integer column. Null → None.
pub(super) fn native_row_optional_int(args: &[NValue]) -> Result<NValue, NativeError> {
    let name = col_name(args)?;
    let (columns, values) = args[0]
        .as_row()
        .ok_or_else(|| NativeError("Row.optional_int: expected Row".into()))?;

    match row_lookup(columns, values, name) {
        Some(SqlValue::Null) => Ok(NValue::enum_val("None".into(), NValue::unit())),
        Some(SqlValue::Int(i)) => Ok(NValue::enum_val("Some".into(), NValue::int(*i))),
        Some(SqlValue::Float(f)) => Ok(NValue::enum_val("Some".into(), NValue::int(*f as i64))),
        Some(SqlValue::Text(s)) => match s.as_ref().parse::<i64>() {
            Ok(i) => Ok(NValue::enum_val("Some".into(), NValue::int(i))),
            Err(_) => Ok(NValue::enum_val("None".into(), NValue::unit())),
        },
        Some(SqlValue::Blob(_)) => Ok(NValue::enum_val("None".into(), NValue::unit())),
        None => Err(NativeError(format!(
            "Row.optional_int: column '{}' not found",
            name
        ))),
    }
}

// ---------------------------------------------------------------------------
// Legacy Db.* helpers — accept both Row and Map for backward compat
// ---------------------------------------------------------------------------

/// Look up a key in a Map's entries, returning the value if found.
#[allow(dead_code)]
fn map_lookup<'a>(
    entries: &'a std::collections::HashMap<NValue, NValue>,
    key: &NValue,
) -> Option<&'a NValue> {
    entries.get(key)
}

/// Extract the string content from an NValue, returning None if not a string.
#[allow(dead_code)]
fn as_str(val: &NValue) -> Option<&str> {
    val.as_string().map(|s| s.as_ref())
}

/// Db.require(row, key: String) -> String
///
/// Get a required string field from a row. Accepts both Row and Map.
#[allow(dead_code)]
pub(super) fn native_db_require(args: &[NValue]) -> Result<NValue, NativeError> {
    let key = &args[1];
    match args[0].as_heap_ref() {
        HeapObject::Row { columns, values } => {
            let name = key
                .as_string()
                .ok_or_else(|| NativeError("Db.require: key must be String".into()))?;
            match row_lookup(columns, values, name.as_ref()) {
                Some(SqlValue::Text(s)) => Ok(NValue::string(s.clone())),
                Some(SqlValue::Int(i)) => Ok(NValue::string(RcStr::from(i.to_string().as_str()))),
                Some(SqlValue::Float(f)) => Ok(NValue::string(RcStr::from(f.to_string().as_str()))),
                Some(SqlValue::Null) => Ok(NValue::string(RcStr::from(""))),
                Some(SqlValue::Blob(_)) => Ok(NValue::string(RcStr::from("<blob>"))),
                None => Ok(NValue::string(RcStr::from(""))),
            }
        }
        HeapObject::Map(entries) => match map_lookup(entries, key) {
            Some(v) => Ok(v.clone()),
            None => Ok(NValue::string(RcStr::from(""))),
        },
        _ => Err(NativeError("Db.require: expected Row or Map".into())),
    }
}

/// Db.optional(row, key: String) -> Option<String>
///
/// Get an optional string field. Returns None if the key is missing or the value is ""/Null.
#[allow(dead_code)]
pub(super) fn native_db_optional(args: &[NValue]) -> Result<NValue, NativeError> {
    let key = &args[1];
    match args[0].as_heap_ref() {
        HeapObject::Row { columns, values } => {
            let name = key
                .as_string()
                .ok_or_else(|| NativeError("Db.optional: key must be String".into()))?;
            match row_lookup(columns, values, name.as_ref()) {
                Some(SqlValue::Null) => Ok(NValue::enum_val("None".into(), NValue::unit())),
                Some(SqlValue::Text(s)) if s.as_ref().is_empty() => {
                    Ok(NValue::enum_val("None".into(), NValue::unit()))
                }
                Some(SqlValue::Text(s)) => {
                    Ok(NValue::enum_val("Some".into(), NValue::string(s.clone())))
                }
                Some(SqlValue::Int(i)) => Ok(NValue::enum_val(
                    "Some".into(),
                    NValue::string(RcStr::from(i.to_string().as_str())),
                )),
                Some(SqlValue::Float(f)) => Ok(NValue::enum_val(
                    "Some".into(),
                    NValue::string(RcStr::from(f.to_string().as_str())),
                )),
                Some(SqlValue::Blob(_)) => Ok(NValue::enum_val(
                    "Some".into(),
                    NValue::string(RcStr::from("<blob>")),
                )),
                None => Ok(NValue::enum_val("None".into(), NValue::unit())),
            }
        }
        HeapObject::Map(entries) => match map_lookup(entries, key) {
            Some(v) if as_str(v) == Some("") => Ok(NValue::enum_val("None".into(), NValue::unit())),
            Some(v) => Ok(NValue::enum_val("Some".into(), v.clone())),
            None => Ok(NValue::enum_val("None".into(), NValue::unit())),
        },
        _ => Err(NativeError("Db.optional: expected Row or Map".into())),
    }
}

/// Db.int_field(row, key: String) -> Int
///
/// Parse an integer field from a row. Accepts both Row and Map.
#[allow(dead_code)]
pub(super) fn native_db_int_field(args: &[NValue]) -> Result<NValue, NativeError> {
    let key = &args[1];
    match args[0].as_heap_ref() {
        HeapObject::Row { columns, values } => {
            let name = key
                .as_string()
                .ok_or_else(|| NativeError("Db.int_field: key must be String".into()))?;
            match row_lookup(columns, values, name.as_ref()) {
                Some(SqlValue::Int(i)) => Ok(NValue::int(*i)),
                Some(SqlValue::Float(f)) => Ok(NValue::int(*f as i64)),
                Some(SqlValue::Text(s)) => Ok(NValue::int(s.as_ref().parse::<i64>().unwrap_or(0))),
                _ => Ok(NValue::int(0)),
            }
        }
        HeapObject::Map(entries) => {
            let n = map_lookup(entries, key)
                .and_then(as_str)
                .and_then(|s| s.parse::<i64>().ok())
                .unwrap_or(0);
            Ok(NValue::int(n))
        }
        _ => Err(NativeError("Db.int_field: expected Row or Map".into())),
    }
}

/// Db.bool_field(row, key: String) -> Boolean
///
/// Parse a boolean field from a row. Accepts both Row and Map.
#[allow(dead_code)]
pub(super) fn native_db_bool_field(args: &[NValue]) -> Result<NValue, NativeError> {
    let key = &args[1];
    match args[0].as_heap_ref() {
        HeapObject::Row { columns, values } => {
            let name = key
                .as_string()
                .ok_or_else(|| NativeError("Db.bool_field: key must be String".into()))?;
            match row_lookup(columns, values, name.as_ref()) {
                Some(SqlValue::Int(i)) => Ok(NValue::bool(*i != 0)),
                Some(SqlValue::Text(s)) => Ok(NValue::bool(s.as_ref() == "1")),
                _ => Ok(NValue::bool(false)),
            }
        }
        HeapObject::Map(entries) => {
            let b = map_lookup(entries, key)
                .and_then(as_str)
                .map(|s| s == "1")
                .unwrap_or(false);
            Ok(NValue::bool(b))
        }
        _ => Err(NativeError("Db.bool_field: expected Row or Map".into())),
    }
}

/// Db.first_row(rows: List<Row>) -> Option<Row>
///
/// Return the first row from a query result, or None if empty.
#[allow(dead_code)]
pub(super) fn native_db_first_row(args: &[NValue]) -> Result<NValue, NativeError> {
    let list = args[0]
        .as_list()
        .ok_or_else(|| NativeError("Db.first_row: expected List".into()))?;

    match list.first() {
        Some(row) => Ok(NValue::enum_val("Some".into(), row.clone())),
        None => Ok(NValue::enum_val("None".into(), NValue::unit())),
    }
}

/// Db.has_rows(rows: List<Row>) -> Boolean
///
/// Check whether a query result contains any rows.
#[allow(dead_code)]
pub(super) fn native_db_has_rows(args: &[NValue]) -> Result<NValue, NativeError> {
    let list = args[0]
        .as_list()
        .ok_or_else(|| NativeError("Db.has_rows: expected List".into()))?;

    Ok(NValue::bool(!list.is_empty()))
}

// ---------------------------------------------------------------------------
// Row.decode — automatic row-to-record mapping
// ---------------------------------------------------------------------------

/// Decode a single column value based on the type tag.
/// Reuses the same coercion logic as the individual Row.* accessors.
fn decode_field(
    columns: &[RcStr],
    values: &[SqlValue],
    field_name: &str,
    type_tag: &str,
    struct_name: &str,
) -> Result<NValue, NativeError> {
    match type_tag {
        "Int" => match row_lookup(columns, values, field_name) {
            Some(SqlValue::Int(i)) => Ok(NValue::int(*i)),
            Some(SqlValue::Float(f)) => Ok(NValue::int(*f as i64)),
            Some(SqlValue::Null) => Err(NativeError(format!(
                "Row.decode: column '{}' is NULL but struct {} declares it as Int — use Option<Int> for nullable columns",
                field_name, struct_name
            ))),
            Some(SqlValue::Text(s)) => Ok(NValue::int(s.as_ref().parse::<i64>().unwrap_or(0))),
            Some(SqlValue::Blob(_)) => Err(NativeError(format!(
                "Row.decode: column '{}' is a blob (struct {} requires Int)",
                field_name, struct_name
            ))),
            None => Err(NativeError(format!(
                "Row.decode: column '{}' not found in row (struct {} requires it)",
                field_name, struct_name
            ))),
        },
        "Float" => match row_lookup(columns, values, field_name) {
            Some(SqlValue::Float(f)) => Ok(NValue::float(*f)),
            Some(SqlValue::Int(i)) => Ok(NValue::float(*i as f64)),
            Some(SqlValue::Null) => Err(NativeError(format!(
                "Row.decode: column '{}' is NULL but struct {} declares it as Float — use Option<Float> for nullable columns",
                field_name, struct_name
            ))),
            Some(SqlValue::Text(s)) => Ok(NValue::float(s.as_ref().parse::<f64>().unwrap_or(0.0))),
            Some(SqlValue::Blob(_)) => Err(NativeError(format!(
                "Row.decode: column '{}' is a blob (struct {} requires Float)",
                field_name, struct_name
            ))),
            None => Err(NativeError(format!(
                "Row.decode: column '{}' not found in row (struct {} requires it)",
                field_name, struct_name
            ))),
        },
        "String" => match row_lookup(columns, values, field_name) {
            Some(SqlValue::Text(s)) => Ok(NValue::string(s.clone())),
            Some(SqlValue::Int(i)) => Ok(NValue::string(RcStr::from(i.to_string().as_str()))),
            Some(SqlValue::Float(f)) => Ok(NValue::string(RcStr::from(f.to_string().as_str()))),
            Some(SqlValue::Null) => Err(NativeError(format!(
                "Row.decode: column '{}' is NULL but struct {} declares it as String — use Option<String> for nullable columns",
                field_name, struct_name
            ))),
            Some(SqlValue::Blob(_)) => Ok(NValue::string(RcStr::from("<blob>"))),
            None => Err(NativeError(format!(
                "Row.decode: column '{}' not found in row (struct {} requires it)",
                field_name, struct_name
            ))),
        },
        "Bool" => match row_lookup(columns, values, field_name) {
            Some(SqlValue::Int(i)) => Ok(NValue::bool(*i != 0)),
            Some(SqlValue::Float(f)) => Ok(NValue::bool(*f != 0.0)),
            Some(SqlValue::Text(s)) => Ok(NValue::bool(s.as_ref() == "1" || s.as_ref() == "true")),
            Some(SqlValue::Null) => Err(NativeError(format!(
                "Row.decode: column '{}' is NULL but struct {} declares it as Bool — use Option<Bool> for nullable columns",
                field_name, struct_name
            ))),
            Some(SqlValue::Blob(_)) => Ok(NValue::bool(false)),
            None => Err(NativeError(format!(
                "Row.decode: column '{}' not found in row (struct {} requires it)",
                field_name, struct_name
            ))),
        },
        "Option<Int>" => match row_lookup(columns, values, field_name) {
            Some(SqlValue::Null) | None => Ok(NValue::enum_val("None".into(), NValue::unit())),
            Some(SqlValue::Int(i)) => Ok(NValue::enum_val("Some".into(), NValue::int(*i))),
            Some(SqlValue::Float(f)) => Ok(NValue::enum_val("Some".into(), NValue::int(*f as i64))),
            Some(SqlValue::Text(s)) => match s.as_ref().parse::<i64>() {
                Ok(i) => Ok(NValue::enum_val("Some".into(), NValue::int(i))),
                Err(_) => Ok(NValue::enum_val("None".into(), NValue::unit())),
            },
            Some(SqlValue::Blob(_)) => Ok(NValue::enum_val("None".into(), NValue::unit())),
        },
        "Option<Float>" => match row_lookup(columns, values, field_name) {
            Some(SqlValue::Null) | None => Ok(NValue::enum_val("None".into(), NValue::unit())),
            Some(SqlValue::Float(f)) => Ok(NValue::enum_val("Some".into(), NValue::float(*f))),
            Some(SqlValue::Int(i)) => Ok(NValue::enum_val("Some".into(), NValue::float(*i as f64))),
            Some(SqlValue::Text(s)) => match s.as_ref().parse::<f64>() {
                Ok(f) => Ok(NValue::enum_val("Some".into(), NValue::float(f))),
                Err(_) => Ok(NValue::enum_val("None".into(), NValue::unit())),
            },
            Some(SqlValue::Blob(_)) => Ok(NValue::enum_val("None".into(), NValue::unit())),
        },
        "Option<String>" => match row_lookup(columns, values, field_name) {
            Some(SqlValue::Null) | None => Ok(NValue::enum_val("None".into(), NValue::unit())),
            Some(SqlValue::Text(s)) => {
                Ok(NValue::enum_val("Some".into(), NValue::string(s.clone())))
            }
            Some(SqlValue::Int(i)) => Ok(NValue::enum_val(
                "Some".into(),
                NValue::string(RcStr::from(i.to_string().as_str())),
            )),
            Some(SqlValue::Float(f)) => Ok(NValue::enum_val(
                "Some".into(),
                NValue::string(RcStr::from(f.to_string().as_str())),
            )),
            Some(SqlValue::Blob(_)) => Ok(NValue::enum_val(
                "Some".into(),
                NValue::string(RcStr::from("<blob>")),
            )),
        },
        "Option<Bool>" => match row_lookup(columns, values, field_name) {
            Some(SqlValue::Null) | None => Ok(NValue::enum_val("None".into(), NValue::unit())),
            Some(SqlValue::Int(i)) => Ok(NValue::enum_val("Some".into(), NValue::bool(*i != 0))),
            Some(SqlValue::Text(s)) => Ok(NValue::enum_val(
                "Some".into(),
                NValue::bool(s.as_ref() == "1" || s.as_ref() == "true"),
            )),
            Some(SqlValue::Float(f)) => {
                Ok(NValue::enum_val("Some".into(), NValue::bool(*f != 0.0)))
            }
            Some(SqlValue::Blob(_)) => Ok(NValue::enum_val("Some".into(), NValue::bool(false))),
        },
        _ => Err(NativeError(format!(
            "Row.decode: unsupported type tag '{}' for field '{}'",
            type_tag, field_name
        ))),
    }
}

/// Row.decode(row, field_spec, struct_name) -> Struct
///
/// Automatically decode a Row into a struct using the compiler-generated field spec.
/// The field_spec is a List<List<String>> like [["id","Int"],["name","String"],...].
/// The struct_name is the target struct type name.
pub(crate) fn native_row_decode(args: &[NValue]) -> Result<NValue, NativeError> {
    // args[0] = row, args[1] = field_spec (List<List<String>>), args[2] = struct_name
    let (columns, values) = args[0]
        .as_row()
        .ok_or_else(|| NativeError("Row.decode: first argument must be Row".into()))?;

    let field_spec = args[1]
        .as_list()
        .ok_or_else(|| NativeError("Row.decode: second argument must be field spec list".into()))?;

    let struct_name = args[2]
        .as_string()
        .ok_or_else(|| NativeError("Row.decode: third argument must be struct name".into()))?;

    let mut fields = Vec::with_capacity(field_spec.len());
    for pair in field_spec {
        let pair_list = pair
            .as_list()
            .ok_or_else(|| NativeError("Row.decode: field spec entry must be a list".into()))?;
        if pair_list.len() != 2 {
            return Err(NativeError(
                "Row.decode: field spec entry must have 2 elements".into(),
            ));
        }
        let field_name = pair_list[0]
            .as_string()
            .ok_or_else(|| NativeError("Row.decode: field name must be String".into()))?;
        let type_tag = pair_list[1]
            .as_string()
            .ok_or_else(|| NativeError("Row.decode: type tag must be String".into()))?;

        let value = decode_field(
            columns,
            values,
            field_name.as_ref(),
            type_tag.as_ref(),
            struct_name.as_ref(),
        )?;
        fields.push((field_name.clone(), value));
    }

    Ok(NValue::struct_val(struct_name.clone(), fields))
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;

    fn make_map_row(fields: &[(&str, &str)]) -> NValue {
        let entries: Vec<(NValue, NValue)> = fields
            .iter()
            .map(|(k, v)| {
                (
                    NValue::string(RcStr::from(*k)),
                    NValue::string(RcStr::from(*v)),
                )
            })
            .collect();
        NValue::map(entries)
    }

    fn make_typed_row(cols: &[&str], vals: Vec<SqlValue>) -> NValue {
        let columns = Arc::new(cols.iter().map(|s| RcStr::from(*s)).collect());
        NValue::row(columns, vals)
    }

    // -- Row.string --

    #[test]
    fn row_string_text() {
        let row = make_typed_row(&["name"], vec![SqlValue::Text("Alice".into())]);
        let result = native_row_string(&[row, NValue::string("name".into())]).unwrap();
        assert_eq!(result.as_string().unwrap().as_ref(), "Alice");
    }

    #[test]
    fn row_string_int_coerced() {
        let row = make_typed_row(&["id"], vec![SqlValue::Int(42)]);
        let result = native_row_string(&[row, NValue::string("id".into())]).unwrap();
        assert_eq!(result.as_string().unwrap().as_ref(), "42");
    }

    // -- Row.int --

    #[test]
    fn row_int_direct() {
        let row = make_typed_row(&["id"], vec![SqlValue::Int(99)]);
        let result = native_row_int(&[row, NValue::string("id".into())]).unwrap();
        assert_eq!(result.as_any_int(), 99);
    }

    #[test]
    fn row_int_null_returns_zero() {
        let row = make_typed_row(&["id"], vec![SqlValue::Null]);
        let result = native_row_int(&[row, NValue::string("id".into())]).unwrap();
        assert_eq!(result.as_any_int(), 0);
    }

    // -- Row.float --

    #[test]
    fn row_float_direct() {
        let row = make_typed_row(&["price"], vec![SqlValue::Float(std::f64::consts::PI)]);
        let result = native_row_float(&[row, NValue::string("price".into())]).unwrap();
        assert!((result.as_float() - std::f64::consts::PI).abs() < f64::EPSILON);
    }

    // -- Row.bool --

    #[test]
    fn row_bool_int_one_is_true() {
        let row = make_typed_row(&["active"], vec![SqlValue::Int(1)]);
        let result = native_row_bool(&[row, NValue::string("active".into())]).unwrap();
        assert!(result.as_bool());
    }

    #[test]
    fn row_bool_int_zero_is_false() {
        let row = make_typed_row(&["active"], vec![SqlValue::Int(0)]);
        let result = native_row_bool(&[row, NValue::string("active".into())]).unwrap();
        assert!(!result.as_bool());
    }

    // -- Row.optional_string --

    #[test]
    fn row_optional_string_some() {
        let row = make_typed_row(&["email"], vec![SqlValue::Text("a@b.com".into())]);
        let result = native_row_optional_string(&[row, NValue::string("email".into())]).unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Some");
        assert_eq!(payload.as_string().unwrap().as_ref(), "a@b.com");
    }

    #[test]
    fn row_optional_string_null_is_none() {
        let row = make_typed_row(&["email"], vec![SqlValue::Null]);
        let result = native_row_optional_string(&[row, NValue::string("email".into())]).unwrap();
        let (tag, _) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "None");
    }

    // -- Row.optional_int --

    #[test]
    fn row_optional_int_some() {
        let row = make_typed_row(&["age"], vec![SqlValue::Int(30)]);
        let result = native_row_optional_int(&[row, NValue::string("age".into())]).unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Some");
        assert_eq!(payload.as_any_int(), 30);
    }

    #[test]
    fn row_optional_int_null_is_none() {
        let row = make_typed_row(&["age"], vec![SqlValue::Null]);
        let result = native_row_optional_int(&[row, NValue::string("age".into())]).unwrap();
        let (tag, _) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "None");
    }

    // -- Legacy Db.* helpers still work with Map --

    #[test]
    fn require_existing_field() {
        let row = make_map_row(&[("name", "Alice"), ("age", "30")]);
        let result = native_db_require(&[row, NValue::string("name".into())]).unwrap();
        assert_eq!(result.as_string().unwrap().as_ref(), "Alice");
    }

    #[test]
    fn require_missing_field_returns_empty() {
        let row = make_map_row(&[("name", "Alice")]);
        let result = native_db_require(&[row, NValue::string("missing".into())]).unwrap();
        assert_eq!(result.as_string().unwrap().as_ref(), "");
    }

    #[test]
    fn optional_existing_field() {
        let row = make_map_row(&[("email", "a@b.com")]);
        let result = native_db_optional(&[row, NValue::string("email".into())]).unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Some");
        assert_eq!(payload.as_string().unwrap().as_ref(), "a@b.com");
    }

    #[test]
    fn optional_empty_string_returns_none() {
        let row = make_map_row(&[("email", "")]);
        let result = native_db_optional(&[row, NValue::string("email".into())]).unwrap();
        let (tag, _) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "None");
    }

    #[test]
    fn optional_missing_field_returns_none() {
        let row = make_map_row(&[("name", "Alice")]);
        let result = native_db_optional(&[row, NValue::string("email".into())]).unwrap();
        let (tag, _) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "None");
    }

    #[test]
    fn int_field_valid_number() {
        let row = make_map_row(&[("count", "42")]);
        let result = native_db_int_field(&[row, NValue::string("count".into())]).unwrap();
        assert_eq!(result.as_any_int(), 42);
    }

    #[test]
    fn int_field_invalid_returns_zero() {
        let row = make_map_row(&[("count", "abc")]);
        let result = native_db_int_field(&[row, NValue::string("count".into())]).unwrap();
        assert_eq!(result.as_any_int(), 0);
    }

    #[test]
    fn int_field_missing_returns_zero() {
        let row = make_map_row(&[("name", "Alice")]);
        let result = native_db_int_field(&[row, NValue::string("count".into())]).unwrap();
        assert_eq!(result.as_any_int(), 0);
    }

    #[test]
    fn bool_field_one_is_true() {
        let row = make_map_row(&[("active", "1")]);
        let result = native_db_bool_field(&[row, NValue::string("active".into())]).unwrap();
        assert!(result.as_bool());
    }

    #[test]
    fn bool_field_zero_is_false() {
        let row = make_map_row(&[("active", "0")]);
        let result = native_db_bool_field(&[row, NValue::string("active".into())]).unwrap();
        assert!(!result.as_bool());
    }

    #[test]
    fn bool_field_missing_is_false() {
        let row = make_map_row(&[("name", "Alice")]);
        let result = native_db_bool_field(&[row, NValue::string("active".into())]).unwrap();
        assert!(!result.as_bool());
    }

    #[test]
    fn first_row_non_empty() {
        let row = make_map_row(&[("id", "1")]);
        let rows = NValue::list(vec![row]);
        let result = native_db_first_row(&[rows]).unwrap();
        let (tag, _) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Some");
    }

    #[test]
    fn first_row_empty() {
        let rows = NValue::list(vec![]);
        let result = native_db_first_row(&[rows]).unwrap();
        let (tag, _) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "None");
    }

    #[test]
    fn has_rows_non_empty() {
        let row = make_map_row(&[("id", "1")]);
        let rows = NValue::list(vec![row]);
        let result = native_db_has_rows(&[rows]).unwrap();
        assert!(result.as_bool());
    }

    #[test]
    fn has_rows_empty() {
        let rows = NValue::list(vec![]);
        let result = native_db_has_rows(&[rows]).unwrap();
        assert!(!result.as_bool());
    }

    // -- Db.* helpers also work with Row type --

    #[test]
    fn db_require_with_row() {
        let row = make_typed_row(&["name"], vec![SqlValue::Text("Bob".into())]);
        let result = native_db_require(&[row, NValue::string("name".into())]).unwrap();
        assert_eq!(result.as_string().unwrap().as_ref(), "Bob");
    }

    #[test]
    fn db_int_field_with_row() {
        let row = make_typed_row(&["count"], vec![SqlValue::Int(7)]);
        let result = native_db_int_field(&[row, NValue::string("count".into())]).unwrap();
        assert_eq!(result.as_any_int(), 7);
    }

    #[test]
    fn db_bool_field_with_row() {
        let row = make_typed_row(&["active"], vec![SqlValue::Int(1)]);
        let result = native_db_bool_field(&[row, NValue::string("active".into())]).unwrap();
        assert!(result.as_bool());
    }
}
