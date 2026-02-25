// Simple test to verify high priority fixes compile
// Run with: cargo test --package blc --test audit_fixes_test

#[test]
fn test_verification_level_parsing() {
    use blc::diagnostics::VerificationLevel;

    assert_eq!(
        "types".parse::<VerificationLevel>().unwrap(),
        VerificationLevel::Types
    );
    assert_eq!(
        "refinements".parse::<VerificationLevel>().unwrap(),
        VerificationLevel::Refinements
    );
    assert_eq!(
        "full".parse::<VerificationLevel>().unwrap(),
        VerificationLevel::Full
    );
    assert_eq!(
        "skip".parse::<VerificationLevel>().unwrap(),
        VerificationLevel::Skip
    );

    // Case insensitive
    assert_eq!(
        "TYPES".parse::<VerificationLevel>().unwrap(),
        VerificationLevel::Types
    );
    assert_eq!(
        "Full".parse::<VerificationLevel>().unwrap(),
        VerificationLevel::Full
    );

    // Invalid
    assert!("invalid".parse::<VerificationLevel>().is_err());
}

#[test]
fn test_check_result_has_verification_level() {
    use blc::diagnostics::{CheckResult, VerificationLevel};

    let result = CheckResult::new(VerificationLevel::Refinements);
    assert_eq!(result.verification_level, VerificationLevel::Refinements);
    assert_eq!(result.status, "ok");
    assert!(result.checked.contains(&"types".to_string()));
    assert!(result.checked.contains(&"refinements".to_string()));
    assert!(!result.checked.contains(&"smt".to_string()));
    assert!(result.unchecked.contains(&"smt".to_string()));
}




#[test]
fn test_suggestion_has_confidence_field() {
    use blc::diagnostics::Suggestion;

    let suggestion = Suggestion {
        strategy: "fix".to_string(),
        description: "Fix the issue".to_string(),
        confidence: Some(0.8),
        patch: None,
    };

    assert_eq!(suggestion.confidence, Some(0.8));
}
