use std::cell::RefCell;
use std::collections::HashMap;

use super::{NValue, NativeError};

// ---------------------------------------------------------------------------
// In-memory session store (thread-local)
// ---------------------------------------------------------------------------

struct SessionEntry {
    data: NValue,
    created_at: u64,
    ttl_secs: u64,
}

thread_local! {
    static SESSION_STORE: RefCell<HashMap<String, SessionEntry>> = RefCell::new(HashMap::new());
}

const DEFAULT_TTL: u64 = 3600; // 1 hour

fn now_secs() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
}

fn generate_session_id() -> String {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    let bytes: [u8; 16] = rng.r#gen();
    // Format as UUID v4
    format!(
        "{:08x}-{:04x}-4{:03x}-{:04x}-{:012x}",
        u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
        u16::from_be_bytes([bytes[4], bytes[5]]),
        u16::from_be_bytes([bytes[6], bytes[7]]) & 0x0FFF,
        (u16::from_be_bytes([bytes[8], bytes[9]]) & 0x3FFF) | 0x8000,
        u64::from_be_bytes([0, 0, bytes[10], bytes[11], bytes[12], bytes[13], bytes[14], bytes[15]])
    )
}

// ---------------------------------------------------------------------------
// Session.create!(data) → String
// ---------------------------------------------------------------------------

/// `Session.create!(data)` → String (session ID)
/// Creates a new session with the given data record. Returns the session ID.
pub(super) fn native_session_create(args: &[NValue]) -> Result<NValue, NativeError> {
    let data = args[0].clone();
    let session_id = generate_session_id();

    SESSION_STORE.with(|store| {
        store.borrow_mut().insert(
            session_id.clone(),
            SessionEntry {
                data,
                created_at: now_secs(),
                ttl_secs: DEFAULT_TTL,
            },
        );
    });

    Ok(NValue::string(session_id.into()))
}

// ---------------------------------------------------------------------------
// Session.get!(id) → Option<Record>
// ---------------------------------------------------------------------------

/// `Session.get!(id)` → `Option<Record>`
/// Retrieves session data by ID. Returns None if expired or not found.
pub(super) fn native_session_get(args: &[NValue]) -> Result<NValue, NativeError> {
    let id = args[0]
        .as_str()
        .ok_or_else(|| NativeError("Session.get!: id must be a String".into()))?
        .to_string();

    SESSION_STORE.with(|store| {
        let mut store = store.borrow_mut();
        let now = now_secs();

        // Check for existence and expiration
        if let Some(entry) = store.get(&id) {
            if now - entry.created_at > entry.ttl_secs {
                store.remove(&id);
                return Ok(NValue::enum_val("None".into(), NValue::unit()));
            }
            Ok(NValue::enum_val("Some".into(), entry.data.clone()))
        } else {
            Ok(NValue::enum_val("None".into(), NValue::unit()))
        }
    })
}

// ---------------------------------------------------------------------------
// Session.delete!(id) → ()
// ---------------------------------------------------------------------------

/// `Session.delete!(id)` → `()`
/// Removes a session by ID.
pub(super) fn native_session_delete(args: &[NValue]) -> Result<NValue, NativeError> {
    let id = args[0]
        .as_str()
        .ok_or_else(|| NativeError("Session.delete!: id must be a String".into()))?
        .to_string();

    SESSION_STORE.with(|store| {
        store.borrow_mut().remove(&id);
    });

    Ok(NValue::unit())
}

// ---------------------------------------------------------------------------
// Session.set!(id, key, value) → ()
// ---------------------------------------------------------------------------

/// `Session.set!(id, key, value)` → `()`
/// Updates a single field in an existing session. No-op if session doesn't exist.
pub(super) fn native_session_set(args: &[NValue]) -> Result<NValue, NativeError> {
    let id = args[0]
        .as_str()
        .ok_or_else(|| NativeError("Session.set!: id must be a String".into()))?
        .to_string();
    let key = args[1]
        .as_string()
        .ok_or_else(|| NativeError("Session.set!: key must be a String".into()))?;
    let value = args[2].clone();

    SESSION_STORE.with(|store| {
        let mut store = store.borrow_mut();
        if let Some(entry) = store.get_mut(&id) {
            // Update the data record
            if let Some(fields) = entry.data.as_record() {
                let mut fields = fields.to_vec();
                if let Some(idx) = fields.iter().position(|(k, _)| *k == *key) {
                    fields[idx].1 = value;
                } else {
                    fields.push((key.clone(), value));
                }
                entry.data = NValue::record(fields);
            }
        }
    });

    Ok(NValue::unit())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_and_get_session() {
        let data = NValue::record(vec![
            ("user_id".into(), NValue::int(42)),
            ("role".into(), NValue::string("admin".into())),
        ]);

        let id_val = native_session_create(&[data]).unwrap();
        let id_str = id_val.as_str().unwrap();
        assert!(!id_str.is_empty());

        let result = native_session_get(&[id_val.clone()]).unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(&**tag, "Some");
        let fields = payload.as_record().unwrap();
        let user_id = fields.iter().find(|(k, _)| &**k == "user_id").unwrap();
        assert_eq!(user_id.1.as_any_int(), 42);
    }

    #[test]
    fn delete_session() {
        let data = NValue::record(vec![
            ("x".into(), NValue::int(1)),
        ]);
        let id_val = native_session_create(&[data]).unwrap();

        native_session_delete(&[id_val.clone()]).unwrap();

        let result = native_session_get(&[id_val]).unwrap();
        let (tag, _) = result.as_enum().unwrap();
        assert_eq!(&**tag, "None");
    }

    #[test]
    fn set_updates_field() {
        let data = NValue::record(vec![
            ("count".into(), NValue::int(0)),
        ]);
        let id_val = native_session_create(&[data]).unwrap();

        native_session_set(&[
            id_val.clone(),
            NValue::string("count".into()),
            NValue::int(5),
        ])
        .unwrap();

        let result = native_session_get(&[id_val]).unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(&**tag, "Some");
        let fields = payload.as_record().unwrap();
        let count = fields.iter().find(|(k, _)| &**k == "count").unwrap();
        assert_eq!(count.1.as_any_int(), 5);
    }

    #[test]
    fn get_nonexistent_returns_none() {
        let result =
            native_session_get(&[NValue::string("nonexistent-id".into())]).unwrap();
        let (tag, _) = result.as_enum().unwrap();
        assert_eq!(&**tag, "None");
    }
}
