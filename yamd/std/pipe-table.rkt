#lang racket/base
;; std/pipe-table — a deliberately dumb, source-pretty table plugin.
;;
;;   #use(std/pipe-table)
;;   #ptable
;;     Name  | Role  | Since
;;     Kiril | infra | 2019
;;     Ann   | web   | 2021
;;
;; A CUSTOM BODY READER splits the body text at READ time: blank lines are
;; dropped, each remaining line is split on `|`, and each cell is trimmed. The
;; FIRST row is the header (<thead>/<th>); the rest are body rows
;; (<tbody>/<td>).
;;
;; Deliberate limitations (this plugin is allowed to be dumb — documented):
;;   * Cells are PLAIN TEXT. `#`-calls, markup, inline math, etc. are NOT
;;     interpreted; the raw text between pipes becomes a string.
;;   * There is no escape for a literal `|` inside a cell — `|` always splits.
;;   * One optional leading and one optional trailing `|` per line are
;;     stripped, so both `a | b` and `| a | b |` work.
;;   * Every row must have exactly as many columns as the header; a ragged row
;;     is an explicit, located error.
;;   * An empty body (no non-blank lines) is an error — a table needs a header.
;;
;; For anything richer (markup in cells, spans, header: n), use std/table.

(require yamd/runtime)

(provide ptable)

;; Runtime: build the table IR from the parsed rows (a list of rows, each a
;; list of cell strings). The reader guarantees a non-empty list and uniform
;; widths.
(define (ptable rows)
  (define header (car rows))
  (define body (cdr rows))
  (apply elem 'table
         (elem 'thead
               (apply elem 'tr (map (lambda (c) (elem 'th c)) header)))
         (if (null? body)
             '()
             (list (apply elem 'tbody
                          (map (lambda (r)
                                 (apply elem 'tr
                                        (map (lambda (c) (elem 'td c)) r)))
                               body))))))

;; ---------------------------------------------------------------------------
;; Read-time: parse the pipe-table body into quoted rows.

(module* yamd-reader #f
  (require yamd/reader/registry
           racket/string)

  (provide body-readers)

  ;; `line-offset` shifts the reported line to the offending body line: the
  ;; reader's srcloc points at the FIRST body line (block form; inline bodies
  ;; have no newlines so the offset is always 0), and blank body lines are
  ;; counted, so body line index i lives at source line (srcloc-line loc) + i.
  (define (ptable-error loc line-offset msg)
    (define line (srcloc-line loc))
    (error 'ptable "~a:~a:~a: ~a"
           (srcloc-source loc)
           (if line (+ line line-offset) line)
           (srcloc-column loc) msg))

  ;; One line -> a list of trimmed cell strings. Strips a single optional
  ;; leading/trailing `|`, then splits on `|` keeping interior empty cells.
  (define (split-row ln)
    (define t (string-trim ln))
    (define t1 (if (and (positive? (string-length t))
                        (char=? (string-ref t 0) #\|))
                   (substring t 1)
                   t))
    (define t2 (if (and (positive? (string-length t1))
                        (char=? (string-ref t1 (sub1 (string-length t1))) #\|))
                   (substring t1 0 (sub1 (string-length t1)))
                   t1))
    (map string-trim (string-split t2 "|" #:trim? #f)))

  (define (parse-ptable text loc)
    ;; Keep each line's index within the body so errors can point at it.
    (define lines ; (listof (cons body-line-index line))
      (for/list ([ln (in-list (string-split text "\n" #:trim? #f))]
                 [i (in-naturals)]
                 #:unless (regexp-match? #px"^\\s*$" ln))
        (cons i ln)))
    (when (null? lines)
      (ptable-error loc 0 "#ptable requires at least a header row"))
    (define rows (map (lambda (p) (split-row (cdr p))) lines))
    (define width (length (car rows)))
    (for ([r (in-list rows)] [p (in-list lines)])
      (unless (= (length r) width)
        (ptable-error
         loc (car p)
         (format "ragged row: header has ~a column(s) but this row has ~a: ~s"
                 width (length r) (cdr p)))))
    ;; The single body argument of `(ptable <this>)` — the rows as quoted data.
    (datum->syntax #f (list 'quote rows) loc))

  (define body-readers
    (hasheq 'ptable (body-reader parse-ptable #f))))

(module+ test
  (require rackunit)
  ;; runtime IR shape
  (check-equal?
   (ptable '(("Name" "Role") ("Kiril" "infra")))
   (elem 'table
         (elem 'thead (elem 'tr (elem 'th "Name") (elem 'th "Role")))
         (elem 'tbody (elem 'tr (elem 'td "Kiril") (elem 'td "infra")))))
  ;; header-only table: no tbody
  (check-equal?
   (ptable '(("A" "B")))
   (elem 'table (elem 'thead (elem 'tr (elem 'th "A") (elem 'th "B"))))))
