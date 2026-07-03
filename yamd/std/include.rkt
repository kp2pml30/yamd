#lang racket/base
;; #include — dumb, typst-like file splice (docs/multi-doc.md "Include
;; semantics"). A plugin: #use(std/include), then
;;
;;   #include("chapter.yamd")
;;
;; evaluates the named file AT THIS POINT of the collect pass and splices its
;; content in place. The child shares the info table (its #def/#toc/… data
;; joins this document's two-pass state), runs in a fresh namespace (host
;; definitions do not leak either way) and under a forked context — so a file
;; included inside a #section sees 'header-depth and its own sections nest one
;; level deeper automatically. Paths are relative to the INCLUDING file;
;; missing files and include cycles are explicit errors (the cycle error
;; prints the whole chain). Output is identical in single- and multi-file
;; builds: included content is simply part of the page it lands on.

(require yamd/eval)

(provide include)

(define (include path)
  (unless (string? path)
    (error 'include "path must be a string, got ~e" path))
  (yamd-eval-embedded path #:who 'include))
