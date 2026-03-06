use blc::diagnostics::Severity;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckSummary {
    pub status: String,
    pub errors: usize,
    pub warnings: usize,
    pub infos: usize,
}

pub fn check_source(source: &str) -> CheckSummary {
    let result = blc::parse::parse_source(source, "<memory>");
    let mut errors = 0usize;
    let mut warnings = 0usize;
    let mut infos = 0usize;
    for d in &result.diagnostics {
        match d.severity {
            Severity::Error => errors += 1,
            Severity::Warning => warnings += 1,
            Severity::Info => infos += 1,
        }
    }
    CheckSummary {
        status: result.status,
        errors,
        warnings,
        infos,
    }
}

pub fn is_valid(source: &str) -> bool {
    check_source(source).errors == 0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_program_has_no_errors() {
        let summary = check_source("fn main() -> Int = 1");
        assert_eq!(summary.errors, 0);
        assert_eq!(summary.status, "success");
    }

    #[test]
    fn invalid_program_reports_error() {
        let summary = check_source("fn main() -> Int = nope");
        assert!(summary.errors > 0);
        assert_eq!(summary.status, "failure");
    }

    #[test]
    fn is_valid_matches_summary() {
        assert!(is_valid("fn main() -> Int = 1"));
        assert!(!is_valid("fn main() -> Int = nope"));
    }
}
