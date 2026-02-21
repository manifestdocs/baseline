use std::sync::Arc;

use hmac::{Hmac, Mac};
use sha2::Sha256;

use super::json::{nvalue_to_serde, serde_to_nvalue};
use super::{NValue, NativeError};

// ---------------------------------------------------------------------------
// Base64url encoding/decoding (RFC 4648 §5)
// ---------------------------------------------------------------------------

fn base64url_encode(bytes: &[u8]) -> String {
    const TABLE: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
    let mut out = String::with_capacity(bytes.len().div_ceil(3) * 4);
    for chunk in bytes.chunks(3) {
        let b0 = chunk[0] as u32;
        let b1 = chunk.get(1).copied().unwrap_or(0) as u32;
        let b2 = chunk.get(2).copied().unwrap_or(0) as u32;
        let triple = (b0 << 16) | (b1 << 8) | b2;
        out.push(TABLE[((triple >> 18) & 0x3F) as usize] as char);
        out.push(TABLE[((triple >> 12) & 0x3F) as usize] as char);
        if chunk.len() > 1 {
            out.push(TABLE[((triple >> 6) & 0x3F) as usize] as char);
        }
        if chunk.len() > 2 {
            out.push(TABLE[(triple & 0x3F) as usize] as char);
        }
    }
    out
}

fn base64url_decode(input: &str) -> Option<Vec<u8>> {
    const TABLE: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
    let mut out = Vec::new();
    let mut buf: u32 = 0;
    let mut bits: u32 = 0;
    for &b in input.as_bytes() {
        if b == b'=' {
            break;
        }
        let val = TABLE.iter().position(|&c| c == b)? as u32;
        buf = (buf << 6) | val;
        bits += 6;
        if bits >= 8 {
            bits -= 8;
            out.push((buf >> bits) as u8);
            buf &= (1 << bits) - 1;
        }
    }
    Some(out)
}

// ---------------------------------------------------------------------------
// HMAC-SHA256 helpers
// ---------------------------------------------------------------------------

fn hmac_sha256(key: &[u8], message: &[u8]) -> Vec<u8> {
    let mut mac = Hmac::<Sha256>::new_from_slice(key).expect("HMAC accepts any key length");
    mac.update(message);
    mac.finalize().into_bytes().to_vec()
}

fn hmac_sha256_verify(key: &[u8], message: &[u8], signature: &[u8]) -> bool {
    let mut mac = Hmac::<Sha256>::new_from_slice(key).expect("HMAC accepts any key length");
    mac.update(message);
    mac.verify_slice(signature).is_ok()
}

// ---------------------------------------------------------------------------
// JWT header (always HS256)
// ---------------------------------------------------------------------------

const JWT_HEADER: &str = r#"{"alg":"HS256","typ":"JWT"}"#;

// ---------------------------------------------------------------------------
// Jwt.sign(claims, secret) → String
// ---------------------------------------------------------------------------

/// `Jwt.sign(claims, secret)` → String
/// Signs a claims record as a JWT using HS256.
pub(super) fn native_jwt_sign(args: &[NValue]) -> Result<NValue, NativeError> {
    let claims = &args[0];
    let secret = args[1]
        .as_string()
        .ok_or_else(|| NativeError("Jwt.sign: secret must be a String".into()))?;

    let payload_json = serde_json::to_string(&nvalue_to_serde(claims)?)
        .map_err(|e| NativeError(format!("Jwt.sign: failed to serialize claims: {}", e)))?;

    let header_b64 = base64url_encode(JWT_HEADER.as_bytes());
    let payload_b64 = base64url_encode(payload_json.as_bytes());
    let signing_input = format!("{}.{}", header_b64, payload_b64);
    let signature = hmac_sha256(secret.as_bytes(), signing_input.as_bytes());
    let signature_b64 = base64url_encode(&signature);

    let token = format!("{}.{}", signing_input, signature_b64);
    Ok(NValue::string(Arc::from(token.as_str())))
}

// ---------------------------------------------------------------------------
// Jwt.sign_with(claims, secret, options) → String
// ---------------------------------------------------------------------------

/// `Jwt.sign_with(claims, secret, options)` → String
/// Signs with options: { expires_in: Int } (seconds from now).
pub(super) fn native_jwt_sign_with(args: &[NValue]) -> Result<NValue, NativeError> {
    let claims = &args[0];
    let secret = args[1]
        .as_string()
        .ok_or_else(|| NativeError("Jwt.sign_with: secret must be a String".into()))?;
    let options = args[2]
        .as_record()
        .ok_or_else(|| NativeError("Jwt.sign_with: options must be a Record".into()))?;

    let mut serde_val = nvalue_to_serde(claims)?;

    // Inject iat and exp if expires_in is set
    if let Some(exp_val) = options.iter().find(|(k, _)| &**k == "expires_in")
        && exp_val.1.is_any_int()
    {
        let expires_in = exp_val.1.as_any_int();
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs() as i64;
        if let serde_json::Value::Object(ref mut map) = serde_val {
            map.insert("iat".to_string(), serde_json::Value::Number(now.into()));
            map.insert(
                "exp".to_string(),
                serde_json::Value::Number((now + expires_in).into()),
            );
        }
    }

    let payload_json = serde_json::to_string(&serde_val)
        .map_err(|e| NativeError(format!("Jwt.sign_with: failed to serialize: {}", e)))?;

    let header_b64 = base64url_encode(JWT_HEADER.as_bytes());
    let payload_b64 = base64url_encode(payload_json.as_bytes());
    let signing_input = format!("{}.{}", header_b64, payload_b64);
    let signature = hmac_sha256(secret.as_bytes(), signing_input.as_bytes());
    let signature_b64 = base64url_encode(&signature);

    let token = format!("{}.{}", signing_input, signature_b64);
    Ok(NValue::string(Arc::from(token.as_str())))
}

// ---------------------------------------------------------------------------
// Jwt.verify(token, secret) → Result<Record, String>
// ---------------------------------------------------------------------------

/// `Jwt.verify(token, secret)` → `Result<Record, String>`
/// Verifies the HS256 signature and checks expiration.
pub(super) fn native_jwt_verify(args: &[NValue]) -> Result<NValue, NativeError> {
    let token = args[0]
        .as_string()
        .ok_or_else(|| NativeError("Jwt.verify: token must be a String".into()))?;
    let secret = args[1]
        .as_string()
        .ok_or_else(|| NativeError("Jwt.verify: secret must be a String".into()))?;

    let parts: Vec<&str> = token.splitn(3, '.').collect();
    if parts.len() != 3 {
        return Ok(err_result("Invalid JWT format: expected 3 parts"));
    }

    let signing_input = format!("{}.{}", parts[0], parts[1]);
    let sig_bytes = base64url_decode(parts[2])
        .ok_or_else(|| NativeError("Jwt.verify: invalid base64url in signature".into()))?;

    if !hmac_sha256_verify(secret.as_bytes(), signing_input.as_bytes(), &sig_bytes) {
        return Ok(err_result("Invalid signature"));
    }

    let payload_bytes = base64url_decode(parts[1])
        .ok_or_else(|| NativeError("Jwt.verify: invalid base64url in payload".into()))?;
    let payload_str = String::from_utf8(payload_bytes)
        .map_err(|_| NativeError("Jwt.verify: invalid UTF-8 in payload".into()))?;

    let serde_val: serde_json::Value = serde_json::from_str(&payload_str)
        .map_err(|e| NativeError(format!("Jwt.verify: invalid JSON payload: {}", e)))?;

    // Check expiration
    if let Some(exp) = serde_val.get("exp").and_then(|v| v.as_i64()) {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs() as i64;
        if now > exp {
            return Ok(err_result("Token expired"));
        }
    }

    let nvalue = serde_to_nvalue(serde_val);
    Ok(NValue::enum_val("Ok".into(), nvalue))
}

// ---------------------------------------------------------------------------
// Jwt.decode(token) → Result<Record, String>
// ---------------------------------------------------------------------------

/// `Jwt.decode(token)` → `Result<Record, String>`
/// Decodes the payload WITHOUT verifying the signature.
pub(super) fn native_jwt_decode(args: &[NValue]) -> Result<NValue, NativeError> {
    let token = args[0]
        .as_string()
        .ok_or_else(|| NativeError("Jwt.decode: token must be a String".into()))?;

    let parts: Vec<&str> = token.splitn(3, '.').collect();
    if parts.len() != 3 {
        return Ok(err_result("Invalid JWT format: expected 3 parts"));
    }

    let payload_bytes = base64url_decode(parts[1])
        .ok_or_else(|| NativeError("Jwt.decode: invalid base64url in payload".into()))?;
    let payload_str = String::from_utf8(payload_bytes)
        .map_err(|_| NativeError("Jwt.decode: invalid UTF-8 in payload".into()))?;

    let serde_val: serde_json::Value = serde_json::from_str(&payload_str)
        .map_err(|e| NativeError(format!("Jwt.decode: invalid JSON payload: {}", e)))?;

    let nvalue = serde_to_nvalue(serde_val);
    Ok(NValue::enum_val("Ok".into(), nvalue))
}

fn err_result(msg: &str) -> NValue {
    NValue::enum_val("Err".into(), NValue::string(msg.into()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sign_and_verify_roundtrip() {
        let claims = NValue::record(vec![
            ("sub".into(), NValue::string("user123".into())),
            ("role".into(), NValue::string("admin".into())),
        ]);
        let secret = NValue::string("my-secret-key".into());

        let token = native_jwt_sign(&[claims, secret.clone()]).unwrap();
        let token_str = token.as_str().unwrap();

        // Must have 3 parts
        assert_eq!(token_str.matches('.').count(), 2);

        // Verify succeeds
        let result = native_jwt_verify(&[token.clone(), secret.clone()]).unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(&**tag, "Ok");
        let fields = payload.as_record().unwrap();
        let sub = fields.iter().find(|(k, _)| &**k == "sub").unwrap();
        assert_eq!(sub.1.as_str().unwrap(), "user123");
    }

    #[test]
    fn verify_detects_invalid_signature() {
        let claims = NValue::record(vec![("sub".into(), NValue::string("user123".into()))]);
        let secret = NValue::string("correct-secret".into());
        let wrong_secret = NValue::string("wrong-secret".into());

        let token = native_jwt_sign(&[claims, secret]).unwrap();
        let result = native_jwt_verify(&[token, wrong_secret]).unwrap();
        let (tag, _) = result.as_enum().unwrap();
        assert_eq!(&**tag, "Err");
    }

    #[test]
    fn decode_without_verification() {
        let claims = NValue::record(vec![("data".into(), NValue::string("hello".into()))]);
        let secret = NValue::string("secret".into());
        let token = native_jwt_sign(&[claims, secret]).unwrap();

        let result = native_jwt_decode(&[token]).unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(&**tag, "Ok");
        let fields = payload.as_record().unwrap();
        let data = fields.iter().find(|(k, _)| &**k == "data").unwrap();
        assert_eq!(data.1.as_str().unwrap(), "hello");
    }

    #[test]
    fn sign_with_expiration() {
        let claims = NValue::record(vec![("sub".into(), NValue::string("user".into()))]);
        let secret = NValue::string("secret".into());
        let options = NValue::record(vec![("expires_in".into(), NValue::int(3600))]);

        let token = native_jwt_sign_with(&[claims, secret.clone(), options]).unwrap();
        let result = native_jwt_verify(&[token, secret]).unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(&**tag, "Ok");
        // Should have iat and exp fields
        let fields = payload.as_record().unwrap();
        assert!(fields.iter().any(|(k, _)| &**k == "iat"));
        assert!(fields.iter().any(|(k, _)| &**k == "exp"));
    }
}
