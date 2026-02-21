use std::sync::Arc;

use hmac::{Hmac, Mac};
use sha2::{Digest, Sha256};

use super::{NValue, NativeError};

pub(super) fn native_crypto_sha256(args: &[NValue]) -> Result<NValue, NativeError> {
    match args[0].as_string() {
        Some(s) => {
            let hash = Sha256::digest(s.as_bytes());
            let hex = hex_encode(&hash);
            Ok(NValue::string(Arc::from(hex.as_str())))
        }
        None => Err(NativeError(format!(
            "Crypto.sha256: expected String, got {}",
            args[0]
        ))),
    }
}

pub(super) fn native_crypto_hmac_sha256(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_string(), args[1].as_string()) {
        (Some(key), Some(message)) => {
            let mut mac = Hmac::<Sha256>::new_from_slice(key.as_bytes())
                .map_err(|e| NativeError(format!("Crypto.hmac_sha256: invalid key: {}", e)))?;
            mac.update(message.as_bytes());
            let result = mac.finalize().into_bytes();
            let hex = hex_encode(&result);
            Ok(NValue::string(Arc::from(hex.as_str())))
        }
        _ => Err(NativeError(
            "Crypto.hmac_sha256: expected (String, String)".into(),
        )),
    }
}

pub(super) fn native_crypto_constant_time_eq(args: &[NValue]) -> Result<NValue, NativeError> {
    match (args[0].as_string(), args[1].as_string()) {
        (Some(a), Some(b)) => {
            let a_bytes = a.as_bytes();
            let b_bytes = b.as_bytes();
            // Constant-time comparison: always compare all bytes
            let len_eq = a_bytes.len() == b_bytes.len();
            let mut xor = 0u8;
            let len = a_bytes.len().min(b_bytes.len());
            for i in 0..len {
                xor |= a_bytes[i] ^ b_bytes[i];
            }
            Ok(NValue::bool(len_eq && xor == 0))
        }
        _ => Err(NativeError(
            "Crypto.constant_time_eq: expected (String, String)".into(),
        )),
    }
}

pub(super) fn native_random_bytes(args: &[NValue]) -> Result<NValue, NativeError> {
    use rand::Rng;
    if !args[0].is_any_int() {
        return Err(NativeError(format!(
            "Random.bytes!: expected Int, got {}",
            args[0]
        )));
    }
    let n = args[0].as_any_int();
    if !(0..=1024).contains(&n) {
        return Err(NativeError(format!(
            "Random.bytes!: n must be 0..1024, got {}",
            n
        )));
    }
    let mut bytes = vec![0u8; n as usize];
    rand::thread_rng().fill(&mut bytes[..]);
    let hex = hex_encode(&bytes);
    Ok(NValue::string(Arc::from(hex.as_str())))
}

fn hex_encode(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        s.push_str(&format!("{:02x}", b));
    }
    s
}
