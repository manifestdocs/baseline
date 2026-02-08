/**
 * External scanner for tree-sitter-baseline.
 *
 * Handles string delimiters to disambiguate " from """, and scans
 * string content (both regular and multi-line).
 * Also handles raw strings: r"..." and r#"..."#.
 *
 * Uses the ERROR_SENTINEL pattern: an unused external token that is only
 * valid during error recovery (when all valid_symbols are true).
 */

#include "tree_sitter/parser.h"
#include <stdbool.h>

enum TokenType {
  MULTILINE_STRING_START,       // """
  MULTILINE_STRING_CONTENT,     // text inside """..."""
  MULTILINE_STRING_END,         // """
  STRING_START,                 // " (but not """)
  STRING_CONTENT,               // text inside "..."
  STRING_END,                   // " (closing regular string)
  RAW_STRING_START,             // r"
  RAW_STRING_CONTENT,           // text inside r"..."
  RAW_STRING_END,               // " (closing raw string)
  RAW_HASH_STRING_START,        // r#"
  RAW_HASH_STRING_CONTENT,      // text inside r#"..."#
  RAW_HASH_STRING_END,          // "# (closing raw hash string)
  ERROR_SENTINEL,               // never used in grammar; detects error recovery
};

void *tree_sitter_baseline_external_scanner_create() { return NULL; }
void tree_sitter_baseline_external_scanner_destroy(void *p) { (void)p; }

unsigned tree_sitter_baseline_external_scanner_serialize(void *p, char *buf) {
  (void)p; (void)buf;
  return 0;
}

void tree_sitter_baseline_external_scanner_deserialize(void *p, const char *buf, unsigned len) {
  (void)p; (void)buf; (void)len;
}

static void advance(TSLexer *lexer) { lexer->advance(lexer, false); }
static void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

static bool is_whitespace(int32_t c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

bool tree_sitter_baseline_external_scanner_scan(void *p, TSLexer *lexer,
                                               const bool *valid_symbols) {
  (void)p;

  // Error recovery: all symbols valid. Let internal lexer handle it.
  if (valid_symbols[ERROR_SENTINEL]) {
    return false;
  }

  // ---- String opening: r", r#", " vs """ ----
  if (valid_symbols[STRING_START] || valid_symbols[MULTILINE_STRING_START] ||
      valid_symbols[RAW_STRING_START] || valid_symbols[RAW_HASH_STRING_START]) {
    // Skip whitespace (external scanner is called before extras)
    while (is_whitespace(lexer->lookahead)) {
      skip(lexer);
    }

    // Raw strings: r" or r#"
    if (lexer->lookahead == 'r' &&
        (valid_symbols[RAW_STRING_START] || valid_symbols[RAW_HASH_STRING_START])) {
      advance(lexer);

      if (lexer->lookahead == '"' && valid_symbols[RAW_STRING_START]) {
        advance(lexer);
        lexer->mark_end(lexer);
        lexer->result_symbol = RAW_STRING_START;
        return true;
      }

      if (lexer->lookahead == '#' && valid_symbols[RAW_HASH_STRING_START]) {
        advance(lexer);
        if (lexer->lookahead == '"') {
          advance(lexer);
          lexer->mark_end(lexer);
          lexer->result_symbol = RAW_HASH_STRING_START;
          return true;
        }
      }

      // Not a raw string, don't consume the 'r'
      return false;
    }

    if (lexer->lookahead == '"') {
      advance(lexer);
      lexer->mark_end(lexer);  // After first "

      if (lexer->lookahead == '"') {
        advance(lexer);
        if (lexer->lookahead == '"') {
          advance(lexer);
          if (valid_symbols[MULTILINE_STRING_START]) {
            lexer->mark_end(lexer);
            lexer->result_symbol = MULTILINE_STRING_START;
            return true;
          }
          return false;
        }
      }

      // Single " — regular string start
      if (valid_symbols[STRING_START]) {
        lexer->result_symbol = STRING_START;
        return true;
      }
    }
    return false;
  }

  // ---- Regular string: content or closing " ----
  if (valid_symbols[STRING_CONTENT] || valid_symbols[STRING_END]) {
    // Closing quote
    if (valid_symbols[STRING_END] && lexer->lookahead == '"') {
      advance(lexer);
      lexer->result_symbol = STRING_END;
      return true;
    }

    // Content: text until ", \, or ${
    if (valid_symbols[STRING_CONTENT]) {
      bool has_content = false;

      while (true) {
        if (lexer->lookahead == '\0' || lexer->lookahead == '\n') break;
        if (lexer->lookahead == '"') break;
        if (lexer->lookahead == '\\') break;

        if (lexer->lookahead == '$') {
          lexer->mark_end(lexer);
          advance(lexer);
          if (lexer->lookahead == '{') break;  // Stop for interpolation
          // Just a $ — content
          lexer->mark_end(lexer);
          has_content = true;
          continue;
        }

        advance(lexer);
        lexer->mark_end(lexer);
        has_content = true;
      }

      if (has_content) {
        lexer->result_symbol = STRING_CONTENT;
        return true;
      }
    }
    return false;
  }

  // ---- Multi-line string: content or closing """ ----
  if (valid_symbols[MULTILINE_STRING_END] || valid_symbols[MULTILINE_STRING_CONTENT]) {
    // Closing """
    if (valid_symbols[MULTILINE_STRING_END] && lexer->lookahead == '"') {
      lexer->mark_end(lexer);
      advance(lexer);
      if (lexer->lookahead == '"') {
        advance(lexer);
        if (lexer->lookahead == '"') {
          advance(lexer);
          lexer->mark_end(lexer);
          lexer->result_symbol = MULTILINE_STRING_END;
          return true;
        }
      }
      // Not """, fall through to content
    }

    // Content: text until """, \, or ${. Newlines and single quotes are OK.
    if (valid_symbols[MULTILINE_STRING_CONTENT]) {
      bool has_content = false;

      while (true) {
        if (lexer->lookahead == '\0') break;

        if (lexer->lookahead == '"') {
          // Mark before quote(s) in case this is closing """
          lexer->mark_end(lexer);
          advance(lexer);
          if (lexer->lookahead == '"') {
            advance(lexer);
            if (lexer->lookahead == '"') break;  // Closing """: mark_end stays before first "
            // Two quotes — content. Update mark_end past them.
            lexer->mark_end(lexer);
            has_content = true;
            continue;
          }
          // Single quote — content. Update mark_end past it.
          lexer->mark_end(lexer);
          has_content = true;
          continue;
        }

        if (lexer->lookahead == '\\') break;  // Stop for escape_sequence

        if (lexer->lookahead == '$') {
          lexer->mark_end(lexer);
          advance(lexer);
          if (lexer->lookahead == '{') break;  // Stop for interpolation
          // Just a $ — content
          lexer->mark_end(lexer);
          has_content = true;
          continue;
        }

        advance(lexer);
        lexer->mark_end(lexer);
        has_content = true;
      }

      if (has_content) {
        lexer->result_symbol = MULTILINE_STRING_CONTENT;
        return true;
      }
    }
  }

  // ---- Raw string: content or closing " ----
  if (valid_symbols[RAW_STRING_CONTENT] || valid_symbols[RAW_STRING_END]) {
    // Closing quote
    if (valid_symbols[RAW_STRING_END] && lexer->lookahead == '"') {
      advance(lexer);
      lexer->result_symbol = RAW_STRING_END;
      return true;
    }

    // Content: everything until " (no escape or interpolation processing)
    if (valid_symbols[RAW_STRING_CONTENT]) {
      bool has_content = false;

      while (true) {
        if (lexer->lookahead == '\0' || lexer->lookahead == '\n') break;
        if (lexer->lookahead == '"') break;

        advance(lexer);
        has_content = true;
      }

      if (has_content) {
        lexer->mark_end(lexer);
        lexer->result_symbol = RAW_STRING_CONTENT;
        return true;
      }
    }
    return false;
  }

  // ---- Raw hash string: content or closing "# ----
  if (valid_symbols[RAW_HASH_STRING_CONTENT] || valid_symbols[RAW_HASH_STRING_END]) {
    // Closing "#
    if (valid_symbols[RAW_HASH_STRING_END] && lexer->lookahead == '"') {
      lexer->mark_end(lexer);
      advance(lexer);
      if (lexer->lookahead == '#') {
        advance(lexer);
        lexer->mark_end(lexer);
        lexer->result_symbol = RAW_HASH_STRING_END;
        return true;
      }
      // Not "# — this is content (a bare " inside raw hash string is OK)
    }

    // Content: everything until "# (no escape or interpolation; " alone is fine)
    if (valid_symbols[RAW_HASH_STRING_CONTENT]) {
      bool has_content = false;

      while (true) {
        if (lexer->lookahead == '\0') break;

        if (lexer->lookahead == '"') {
          lexer->mark_end(lexer);
          advance(lexer);
          if (lexer->lookahead == '#') break;  // Closing "# — mark_end is before "
          // Bare " is content — update mark past it
          lexer->mark_end(lexer);
          has_content = true;
          continue;
        }

        advance(lexer);
        lexer->mark_end(lexer);
        has_content = true;
      }

      if (has_content) {
        lexer->result_symbol = RAW_HASH_STRING_CONTENT;
        return true;
      }
    }
    return false;
  }

  return false;
}
