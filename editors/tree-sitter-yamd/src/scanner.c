#include "tree_sitter/parser.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// Token ids MUST match the `externals` array order in grammar.js.
enum TokenType {
  NEWLINE,
  INDENT,
  DEDENT,
  FENCE_OPEN,
  FENCE_CONTENT,
  FENCE_CLOSE,
  BLOCK_COMMENT,
  ERROR_SENTINEL,
};

#define MAX_DEPTH 256

typedef struct {
  // Stack of indentation widths (in tabs). Implicit base level 0 lives below
  // index 0, so an empty stack means "at column 0".
  uint16_t stack[MAX_DEPTH];
  uint16_t depth;

  // Indentation of the line we most recently stepped onto, waiting to be
  // reconciled against `stack` via INDENT/DEDENT tokens.
  uint16_t pending;
  bool have_pending;

  // Length of the `=` run of the fence currently open (>= 0), or -1 if none.
  int32_t fence_eq;

  // A zero-width NEWLINE was already emitted at EOF (files without a final
  // newline); guards against emitting it forever.
  bool eof_newline;
} Scanner;

static inline uint16_t current_indent(const Scanner *s) {
  return s->depth > 0 ? s->stack[s->depth - 1] : 0;
}

// -------------------------------------------------------------------------
// Fences: `|X{ ... }X|`, X a possibly-empty run of `=`.
// -------------------------------------------------------------------------

static bool scan_fence_open(Scanner *s, TSLexer *lexer) {
  // lookahead == '|'
  lexer->advance(lexer, false);
  int32_t eq = 0;
  while (lexer->lookahead == '=') {
    lexer->advance(lexer, false);
    eq++;
  }
  if (lexer->lookahead != '{') {
    return false;
  }
  lexer->advance(lexer, false);
  s->fence_eq = eq;
  lexer->result_symbol = FENCE_OPEN;
  return true;
}

// Is the lexer positioned at the closer `}X|` for the active fence? Advances
// the lookahead (used to peek); the caller manages the token end via mark_end.
static bool at_fence_closer(TSLexer *lexer, int32_t eq) {
  if (lexer->lookahead != '}') {
    return false;
  }
  lexer->advance(lexer, false);
  int32_t seen = 0;
  while (lexer->lookahead == '=' && seen < eq) {
    lexer->advance(lexer, false);
    seen++;
  }
  return seen == eq && lexer->lookahead == '|';
}

static bool scan_fence_content(Scanner *s, TSLexer *lexer) {
  bool has = false;
  for (;;) {
    if (lexer->lookahead == 0) {
      break;  // EOF terminates the (unclosed) content
    }
    if (lexer->lookahead == '}') {
      lexer->mark_end(lexer);  // content stops before this '}' if it is the closer
      if (at_fence_closer(lexer, s->fence_eq)) {
        if (!has) {
          return false;  // empty content; let FENCE_CLOSE fire
        }
        lexer->result_symbol = FENCE_CONTENT;
        return true;
      }
      // Not the closer: the '}' (and any '=' consumed) are ordinary content.
      lexer->mark_end(lexer);
      has = true;
      continue;
    }
    lexer->advance(lexer, false);
    lexer->mark_end(lexer);
    has = true;
  }
  if (!has) {
    return false;
  }
  lexer->result_symbol = FENCE_CONTENT;
  return true;
}

static bool scan_fence_close(Scanner *s, TSLexer *lexer) {
  // lookahead == '}'
  lexer->advance(lexer, false);
  int32_t seen = 0;
  while (lexer->lookahead == '=' && seen < s->fence_eq) {
    lexer->advance(lexer, false);
    seen++;
  }
  if (seen != s->fence_eq || lexer->lookahead != '|') {
    return false;
  }
  lexer->advance(lexer, false);
  s->fence_eq = -1;
  lexer->result_symbol = FENCE_CLOSE;
  return true;
}

// -------------------------------------------------------------------------
// Block comment: `#;{ ... }` with nested braces.
// -------------------------------------------------------------------------

static bool scan_block_comment(TSLexer *lexer) {
  // lookahead == '#'
  lexer->advance(lexer, false);
  if (lexer->lookahead != ';') {
    return false;
  }
  lexer->advance(lexer, false);
  if (lexer->lookahead != '{') {
    return false;
  }
  lexer->advance(lexer, false);
  int32_t depth = 1;
  while (depth > 0) {
    if (lexer->lookahead == 0) {
      break;  // unterminated: consume to EOF
    }
    if (lexer->lookahead == '{') {
      depth++;
    } else if (lexer->lookahead == '}') {
      depth--;
    }
    lexer->advance(lexer, false);
  }
  lexer->result_symbol = BLOCK_COMMENT;
  return true;
}

// -------------------------------------------------------------------------
// Indentation: NEWLINE consumes the line break, following blank lines, and the
// next content line's leading tabs; INDENT/DEDENT reconcile the measured level.
// -------------------------------------------------------------------------

static bool scan_newline(Scanner *s, TSLexer *lexer) {
  uint16_t indent = 0;
  for (;;) {
    if (lexer->lookahead == '\n') {
      indent = 0;
      lexer->advance(lexer, false);
    } else if (lexer->lookahead == '\r') {
      lexer->advance(lexer, false);
    } else if (lexer->lookahead == '\t') {
      indent++;
      lexer->advance(lexer, false);
    } else if (lexer->lookahead == ' ') {
      // Spaces are illegal indentation in YAMD; tolerate for highlighting.
      lexer->advance(lexer, false);
    } else {
      break;  // content char or EOF
    }
  }
  s->pending = indent;
  s->have_pending = true;
  lexer->result_symbol = NEWLINE;
  return true;
}

static bool reconcile(Scanner *s, TSLexer *lexer, const bool *valid) {
  uint16_t top = current_indent(s);
  if (s->pending > top) {
    if (!valid[INDENT]) {
      return false;
    }
    if (s->depth < MAX_DEPTH) {
      s->stack[s->depth++] = s->pending;
    }
    s->have_pending = false;
    lexer->result_symbol = INDENT;
    return true;
  }
  if (s->pending < top) {
    if (!valid[DEDENT]) {
      return false;
    }
    if (s->depth > 0) {
      s->depth--;
    }
    if (s->pending >= current_indent(s)) {
      s->have_pending = false;
    }
    lexer->result_symbol = DEDENT;
    return true;
  }
  // Level unchanged.
  s->have_pending = false;
  return false;
}

bool tree_sitter_yamd_external_scanner_scan(void *payload, TSLexer *lexer,
                                            const bool *valid_symbols) {
  Scanner *s = (Scanner *)payload;

  // During error recovery every external is flagged valid; do not guess.
  if (valid_symbols[ERROR_SENTINEL]) {
    return false;
  }

  if (s->have_pending &&
      (valid_symbols[INDENT] || valid_symbols[DEDENT] || valid_symbols[NEWLINE])) {
    if (reconcile(s, lexer, valid_symbols)) {
      return true;
    }
  }

  if (valid_symbols[FENCE_OPEN] && lexer->lookahead == '|') {
    if (scan_fence_open(s, lexer)) {
      return true;
    }
  }
  if (valid_symbols[FENCE_CLOSE] && s->fence_eq >= 0 && lexer->lookahead == '}') {
    if (scan_fence_close(s, lexer)) {
      return true;
    }
  }
  if (valid_symbols[FENCE_CONTENT] && s->fence_eq >= 0) {
    if (scan_fence_content(s, lexer)) {
      return true;
    }
  }

  if (valid_symbols[BLOCK_COMMENT] && lexer->lookahead == '#') {
    if (scan_block_comment(lexer)) {
      return true;
    }
  }

  if (valid_symbols[NEWLINE] &&
      (lexer->lookahead == '\n' || lexer->lookahead == '\r')) {
    return scan_newline(s, lexer);
  }

  if (lexer->lookahead == 0) {
    // Missing final newline: emit one zero-width NEWLINE so the last line's
    // statement can reduce, then fall through to DEDENT flushing next calls.
    if (valid_symbols[NEWLINE] && !s->eof_newline) {
      s->eof_newline = true;
      s->pending = 0;
      s->have_pending = true;
      lexer->result_symbol = NEWLINE;
      return true;
    }
    // EOF: flush remaining DEDENTs so open blocks close cleanly.
    if (valid_symbols[DEDENT] && s->depth > 0) {
      s->depth--;
      lexer->result_symbol = DEDENT;
      return true;
    }
  }

  return false;
}

// -------------------------------------------------------------------------
// Lifecycle & serialization.
// -------------------------------------------------------------------------

void *tree_sitter_yamd_external_scanner_create(void) {
  Scanner *s = (Scanner *)calloc(1, sizeof(Scanner));
  s->fence_eq = -1;
  return s;
}

void tree_sitter_yamd_external_scanner_destroy(void *payload) {
  free(payload);
}

unsigned tree_sitter_yamd_external_scanner_serialize(void *payload, char *buffer) {
  Scanner *s = (Scanner *)payload;
  unsigned size = 0;

  buffer[size++] = (char)(s->have_pending ? 1 : 0);
  buffer[size++] = (char)(s->eof_newline ? 1 : 0);
  memcpy(&buffer[size], &s->pending, sizeof(s->pending));
  size += sizeof(s->pending);
  memcpy(&buffer[size], &s->fence_eq, sizeof(s->fence_eq));
  size += sizeof(s->fence_eq);

  uint16_t depth = s->depth;
  if (depth > MAX_DEPTH) {
    depth = MAX_DEPTH;
  }
  // Cap so we never exceed the serialization buffer.
  unsigned max_entries =
      (TREE_SITTER_SERIALIZATION_BUFFER_SIZE - size) / sizeof(uint16_t);
  if (depth > max_entries) {
    depth = (uint16_t)max_entries;
  }
  memcpy(&buffer[size], &depth, sizeof(depth));
  size += sizeof(depth);
  for (uint16_t i = 0; i < depth; i++) {
    memcpy(&buffer[size], &s->stack[i], sizeof(uint16_t));
    size += sizeof(uint16_t);
  }
  return size;
}

void tree_sitter_yamd_external_scanner_deserialize(void *payload,
                                                   const char *buffer,
                                                   unsigned length) {
  Scanner *s = (Scanner *)payload;
  s->have_pending = false;
  s->eof_newline = false;
  s->pending = 0;
  s->fence_eq = -1;
  s->depth = 0;
  if (length == 0) {
    return;
  }

  unsigned size = 0;
  s->have_pending = buffer[size++] != 0;
  s->eof_newline = buffer[size++] != 0;
  memcpy(&s->pending, &buffer[size], sizeof(s->pending));
  size += sizeof(s->pending);
  memcpy(&s->fence_eq, &buffer[size], sizeof(s->fence_eq));
  size += sizeof(s->fence_eq);

  uint16_t depth = 0;
  memcpy(&depth, &buffer[size], sizeof(depth));
  size += sizeof(depth);
  if (depth > MAX_DEPTH) {
    depth = MAX_DEPTH;
  }
  s->depth = depth;
  for (uint16_t i = 0; i < depth; i++) {
    memcpy(&s->stack[i], &buffer[size], sizeof(uint16_t));
    size += sizeof(uint16_t);
  }
}
