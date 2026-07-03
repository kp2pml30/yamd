#lang racket/base
;; Error paths for tables. Successes live in golden .html/.sexp
;; files under tests/golden/table/; here we assert malformed input is rejected
;; with an explicit error rather than silently coerced. Covers both the
;; std/table nesting plugin and the std/pipe-table custom body reader.

(module+ test
  (require rackunit
           yamd)

  (define (run str) (yamd-string->html str))

  (define-syntax-rule (check-table-error rx str)
    (check-exn rx (lambda () (run str))))

  ;; --- std/table ---------------------------------------------------------

  ;; a non-#row child directly under #table is an error, not silently dropped
  (check-table-error #rx"only contain #row children"
                     "#use(std/table)\n#table\n\tstray text here\n")

  ;; a loose #cell (not wrapped in a #row) under #table is likewise rejected
  (check-table-error #rx"only contain #row children"
                     "#use(std/table)\n#table\n\t#cell: loose\n")

  ;; a #row body mixing cell elements with stray content is ambiguous → error
  (check-table-error #rx"not a mix"
                     "#use(std/table)\n#table\n\t#row{#cell{a}stray}\n")

  ;; header count exceeding the number of rows is an error
  (check-table-error #rx"exceeds the 1 row"
                     "#use(std/table)\n#table(header: 2)\n\t#row{a}{b}\n")

  ;; --- std/pipe-table (read-time errors) ---------------------------------

  ;; ragged row: fewer columns than the header; the error points at the
  ;; offending SOURCE LINE (body starts at line 3, bad row is body line 1 → 4)
  (check-table-error #rx":4:.*ragged row: header has 2 column"
                     "#use(std/pipe-table)\n#ptable\n\ta | b\n\tc\n")

  ;; interior blank lines count toward the reported line (bad row at line 5)
  (check-table-error #rx":5:.*ragged row: header has 2 column"
                     "#use(std/pipe-table)\n#ptable\n\ta | b\n\n\tc\n")

  ;; ragged row: more columns than the header
  (check-table-error #rx"ragged row: header has 2 column"
                     "#use(std/pipe-table)\n#ptable\n\ta | b\n\tc | d | e\n")

  ;; an empty pipe-table (no non-blank lines) has no header → error
  (check-table-error #rx"requires at least a header row"
                     "#use(std/pipe-table)\n#ptable{   }\n"))
