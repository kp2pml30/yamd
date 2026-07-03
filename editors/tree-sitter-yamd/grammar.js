/**
 * @file YAMD grammar (reader-level forms only).
 * @license MIT
 *
 * Models the YAMD reader surface: call forms (#name, args, {body}, multi-body,
 * `: rest`, `::` hanging, indented block bodies), host escapes `#(...)`, `##`,
 * comments, `|X{ }X|` fences, and plain text. Body contents of raw/custom
 * readers are opaque (parsed as markup — see README). Host code inside `(args)`
 * and `#(...)` is exposed for a Racket injection.
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const NAME = /[A-Za-z][A-Za-z0-9_-]*/;

module.exports = grammar({
  name: 'yamd',

  externals: $ => [
    $._newline,
    $._indent,
    $._dedent,
    $.fence_open,
    $.fence_content,
    $.fence_close,
    $.block_comment,
    $._error_sentinel,
  ],

  // Whitespace is content in markup mode; the scanner eats structural tabs.
  extras: _ => [],

  conflicts: $ => [
    [$._inline, $.block_call],
  ],

  rules: {
    document: $ => repeat($._statement),

    _statement: $ => choice(
      $.block_call,
      $.hanging_call,
      $.colon_call,
      $.text_line,
      $._blank_line,
    ),

    _blank_line: $ => $._newline,

    // ---- Line-level call forms ----------------------------------------

    // `#name(args)` alone on a line, body = following indented block.
    block_call: $ => prec.dynamic(3, seq(
      $._call,
      $._newline,
      $.block,
    )),

    block: $ => seq(
      $._indent,
      repeat1($._statement),
      $._dedent,
    ),

    // `#name(args)::` — body is the rest of the enclosing block. `prec.right`
    // makes the body maximally greedy, so a later `::` at the same level nests
    // inside the earlier one (docs/lang/design.md "Hanging blocks").
    hanging_call: $ => prec.right(seq(
      $._call,
      alias('::', $.body_marker),
      $._newline,
      optional($.hanging_body),
    )),

    hanging_body: $ => prec.right(repeat1($._statement)),

    // `#name(args): rest of line`.
    colon_call: $ => prec.dynamic(2, seq(
      $._call,
      alias($._colon, $.body_marker),
      optional($.body_line),
      $._newline,
    )),

    body_line: $ => repeat1($._inline),

    // A plain line of markup (may contain inline calls) terminated by newline.
    text_line: $ => prec.dynamic(1, seq(
      repeat1($._inline),
      $._newline,
    )),

    // ---- Inline content -----------------------------------------------

    _inline: $ => choice(
      $._call,
      $.host_escape,
      $.escape,
      $.line_comment,
      $.block_comment,
      $.brace_group,
      $.fence,
      $.text,
      $._stray,
    ),

    _call: $ => prec.right(seq(
      $.call_name,
      optional($.arguments),
      repeat($.body),
    )),

    call_name: $ => seq(
      alias('#', $.sigil),
      alias(token.immediate(NAME), $.name),
    ),

    arguments: $ => seq(
      alias(token.immediate('('), $.paren_open),
      optional($.host),
      alias(')', $.paren_close),
    ),

    // `#(...)` host escape (statement- or expression-position Racket).
    host_escape: $ => seq(
      alias('#', $.sigil),
      alias(token.immediate('('), $.paren_open),
      optional($.host),
      alias(')', $.paren_close),
    ),

    // `#name{markup}` inline body (also the multi-body `#row{a}{b}` form).
    body: $ => seq(
      alias(token.immediate('{'), $.brace_open),
      repeat($._inline),
      alias('}', $.brace_close),
    ),

    // A balanced `{ ... }` group in markup not attached to a call.
    brace_group: $ => seq(
      '{',
      repeat($._inline),
      '}',
    ),

    // `|X{ ... }X|` fence; the scanner matches the `=`-run on both ends.
    fence: $ => seq(
      $.fence_open,
      optional($.fence_content),
      $.fence_close,
    ),

    escape: _ => token(prec(2, '##')),

    // `#; ...` line comment (block form `#;{...}` is the external token).
    line_comment: _ => choice(
      token(prec(1, seq('#;', /[^{\r\n][^\r\n]*/))),
      token(prec(1, seq('#;', /[\r\n]/))),
      token(prec(1, /#[ \t][^\r\n]*/)),
    ),

    _colon: _ => token(seq(':', /[ \t]/)),

    // Runs of plain content. Structural sigils (`#`, braces, `|`, parens) are
    // excluded so they can open their constructs; a stray one that opens
    // nothing falls back to `_stray` below (still highlighted as text).
    text: _ => token(/[^#{}|():\r\n]+/),

    _stray: $ => prec(-1, alias(token(/[#|():}]/), $.text)),

    // ---- Host (Racket) sub-language -----------------------------------
    //
    // We do NOT parse Racket; we only balance delimiters, isolate strings,
    // and recognise markup re-entry `#name{...}` so the closing `)` is found
    // correctly. The whole `host` node is handed to the Racket injection.

    host: $ => repeat1($._host_content),

    _host_content: $ => choice(
      $.host_string,
      $.host_group,
      $.markup_reentry,
      $.host_code,
      $._host_hash,
      $._host_newline,
    ),

    // Host code may span lines (e.g. `#(for ([x items]) ...)`); structural
    // indentation is suspended inside unbalanced host parens.
    _host_newline: _ => token(/\r?\n[\t ]*/),

    host_group: $ => choice(
      seq('(', optional($.host), ')'),
      seq('[', optional($.host), ']'),
      seq('{', optional($.host), '}'),
    ),

    host_string: _ => token(seq(
      '"',
      repeat(choice(/[^"\\]/, /\\./)),
      '"',
    )),

    host_code: _ => token(/[^#()\[\]{}"\r\n]+/),

    _host_hash: $ => prec(-1, alias('#', $.host_code)),

    // `#name{markup}` / `#{markup}` re-entry into text mode inside host code.
    markup_reentry: $ => seq(
      alias('#', $.sigil),
      optional(alias(token.immediate(NAME), $.name)),
      $.body,
    ),
  },
});
