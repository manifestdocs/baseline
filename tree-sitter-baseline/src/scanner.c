/**
 * External scanner for tree-sitter-rocket.
 *
 * Handles string content and interpolation detection, which cannot be
 * expressed in the grammar DSL due to context-sensitivity.
 */

#include "tree_sitter/parser.h"
#include <wctype.h>

enum TokenType {
  STRING_CONTENT,
  INTERPOLATION_START,
};

void *tree_sitter_baseline_external_scanner_create() { return NULL; }

void tree_sitter_baseline_external_scanner_destroy(void *p) { (void)p; }

unsigned tree_sitter_baseline_external_scanner_serialize(void *p,
                                                        char *buf) {
  (void)p; (void)buf;
  return 0;
}

void tree_sitter_baseline_external_scanner_deserialize(void *p,
                                                      const char *buf,
                                                      unsigned len) {
  (void)p; (void)buf; (void)len;
}

static void advance(TSLexer *lexer) { lexer->advance(lexer, false); }

bool tree_sitter_baseline_external_scanner_scan(void *p, TSLexer *lexer,
                                               const bool *valid_symbols) {
  (void)p;
  // Handle string content and interpolation
  if (valid_symbols[STRING_CONTENT] || valid_symbols[INTERPOLATION_START]) {
    bool has_content = false;

    while (true) {
      if (lexer->lookahead == '\0') {
        break;
      }

      // Check for end of string
      if (lexer->lookahead == '"') {
        break;
      }

      // Check for escape sequence
      if (lexer->lookahead == '\\') {
        if (has_content) {
          lexer->result_symbol = STRING_CONTENT;
          return true;
        }
        break;
      }

      // Check for interpolation start
      if (lexer->lookahead == '$') {
        if (has_content) {
          lexer->result_symbol = STRING_CONTENT;
          return true;
        }

        advance(lexer);
        if (lexer->lookahead == '{') {
          advance(lexer);  // Consume '{' as well
          lexer->result_symbol = INTERPOLATION_START;
          return true;
        }
        // Not an interpolation, treat $ as content
        has_content = true;
        continue;
      }

      // Regular character
      advance(lexer);
      has_content = true;
    }

    if (has_content) {
      lexer->result_symbol = STRING_CONTENT;
      return true;
    }
  }

  return false;
}
