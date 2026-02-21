use super::{NValue, NativeError, RcStr};

// ---------------------------------------------------------------------------
// Multipart form-data parser
// ---------------------------------------------------------------------------

/// Parse a multipart/form-data body using the provided boundary.
/// Returns a Vec of Parts, each with name, filename (optional), content_type, and body.
fn parse_multipart(body: &str, boundary: &str) -> Vec<(String, Option<String>, String, String)> {
    let delimiter = format!("--{}", boundary);
    let end_delimiter = format!("--{}--", boundary);
    let mut parts = Vec::new();

    for section in body.split(&delimiter) {
        let section = section.trim_matches(|c: char| c == '\r' || c == '\n');
        if section.is_empty() || section == "--" || section.starts_with("--") {
            continue;
        }

        // Split headers from body at the double newline
        let (headers_str, body_str) = match section.find("\r\n\r\n") {
            Some(pos) => (&section[..pos], &section[pos + 4..]),
            None => match section.find("\n\n") {
                Some(pos) => (&section[..pos], &section[pos + 2..]),
                None => continue,
            },
        };

        // Strip trailing end delimiter from body if present
        let body_str = body_str
            .trim_end_matches(&end_delimiter)
            .trim_end_matches(['\r', '\n']);

        let mut name = String::new();
        let mut filename: Option<String> = None;
        let mut content_type = "text/plain".to_string();

        for line in headers_str.lines() {
            let line_lower = line.to_lowercase();
            if line_lower.starts_with("content-disposition:") {
                // Extract name
                if let Some(pos) = line.find("name=\"") {
                    let start = pos + 6;
                    if let Some(end) = line[start..].find('"') {
                        name = line[start..start + end].to_string();
                    }
                }
                // Extract filename
                if let Some(pos) = line.find("filename=\"") {
                    let start = pos + 10;
                    if let Some(end) = line[start..].find('"') {
                        filename = Some(line[start..start + end].to_string());
                    }
                }
            } else if line_lower.starts_with("content-type:") {
                content_type = line["content-type:".len()..].trim().to_string();
            }
        }

        if !name.is_empty() {
            parts.push((name, filename, content_type, body_str.to_string()));
        }
    }

    parts
}

/// `Request.multipart(req)` → `Result<List<Part>, HttpError>`
/// Parses multipart/form-data from the request body.
/// Each Part is a record: { name: String, filename: Option<String>, content_type: String, body: String }
pub(super) fn native_request_multipart(args: &[NValue]) -> Result<NValue, NativeError> {
    let req = &args[0];
    let fields = req
        .as_record()
        .ok_or_else(|| NativeError("Request.multipart: expected Request record".into()))?;

    // Get content-type header to extract boundary
    let content_type = extract_header_value(fields, "content-type").unwrap_or_default();

    let boundary = extract_boundary(&content_type).ok_or_else(|| {
        NativeError("Request.multipart: missing or invalid multipart boundary".into())
    })?;

    // Get body
    let body = fields
        .iter()
        .find(|(k, _)| &**k == "body")
        .and_then(|(_, v)| v.as_str())
        .unwrap_or("");

    let parts = parse_multipart(body, &boundary);

    let part_values: Vec<NValue> = parts
        .into_iter()
        .map(|(name, filename, ct, body)| {
            let filename_val = match filename {
                Some(f) => NValue::enum_val("Some".into(), NValue::string(f.into())),
                None => NValue::enum_val("None".into(), NValue::unit()),
            };
            NValue::record(vec![
                ("name".into(), NValue::string(name.into())),
                ("filename".into(), filename_val),
                ("content_type".into(), NValue::string(ct.into())),
                ("body".into(), NValue::string(body.into())),
            ])
        })
        .collect();

    Ok(NValue::enum_val("Ok".into(), NValue::list(part_values)))
}

/// Extract the boundary parameter from a Content-Type header value.
fn extract_boundary(content_type: &str) -> Option<String> {
    if !content_type.contains("multipart/form-data") {
        return None;
    }
    for part in content_type.split(';') {
        let part = part.trim();
        if let Some(val) = part.strip_prefix("boundary=") {
            return Some(val.trim_matches('"').to_string());
        }
    }
    None
}

/// Extract a header value by name from request record fields.
fn extract_header_value(fields: &[(RcStr, NValue)], header_name: &str) -> Option<String> {
    let headers_val = fields.iter().find(|(k, _)| &**k == "headers")?.1.clone();
    let headers = headers_val.as_list()?;
    for h in headers {
        if let Some(tuple) = h.as_tuple()
            && tuple.len() >= 2
            && let Some(name) = tuple[0].as_string()
            && name.eq_ignore_ascii_case(header_name)
        {
            return tuple[1].as_string().map(|s| s.to_string());
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_multipart() {
        let body = "--boundary123\r\n\
            Content-Disposition: form-data; name=\"field1\"\r\n\
            \r\n\
            value1\r\n\
            --boundary123\r\n\
            Content-Disposition: form-data; name=\"file\"; filename=\"test.txt\"\r\n\
            Content-Type: text/plain\r\n\
            \r\n\
            file contents here\r\n\
            --boundary123--";
        let parts = parse_multipart(body, "boundary123");
        assert_eq!(parts.len(), 2);
        assert_eq!(parts[0].0, "field1");
        assert!(parts[0].1.is_none());
        assert_eq!(parts[0].3, "value1");
        assert_eq!(parts[1].0, "file");
        assert_eq!(parts[1].1.as_deref(), Some("test.txt"));
        assert_eq!(parts[1].3, "file contents here");
    }

    #[test]
    fn extract_boundary_from_content_type() {
        let ct = "multipart/form-data; boundary=abc123";
        assert_eq!(extract_boundary(ct), Some("abc123".to_string()));

        let ct2 = "multipart/form-data; boundary=\"quoted-boundary\"";
        assert_eq!(extract_boundary(ct2), Some("quoted-boundary".to_string()));

        let ct3 = "application/json";
        assert_eq!(extract_boundary(ct3), None);
    }

    #[test]
    fn request_multipart_parses_correctly() {
        let body = "--myboundary\r\n\
            Content-Disposition: form-data; name=\"username\"\r\n\
            \r\n\
            alice\r\n\
            --myboundary--";

        let req = NValue::record(vec![
            (
                "headers".into(),
                NValue::list(vec![NValue::tuple(vec![
                    NValue::string("content-type".into()),
                    NValue::string("multipart/form-data; boundary=myboundary".into()),
                ])]),
            ),
            ("body".into(), NValue::string(body.into())),
            ("method".into(), NValue::string("POST".into())),
            ("path".into(), NValue::string("/upload".into())),
        ]);

        let result = native_request_multipart(&[req]).unwrap();
        let (tag, payload) = result.as_enum().unwrap();
        assert_eq!(&**tag, "Ok");
        let parts = payload.as_list().unwrap();
        assert_eq!(parts.len(), 1);
        let part = parts[0].as_record().unwrap();
        let name = part.iter().find(|(k, _)| &**k == "name").unwrap();
        assert_eq!(name.1.as_str().unwrap(), "username");
        let body_val = part.iter().find(|(k, _)| &**k == "body").unwrap();
        assert_eq!(body_val.1.as_str().unwrap(), "alice");
    }
}
