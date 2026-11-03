#lang racket/base
;; Test plugin for the #image resolver hook (tests/image.rkt). `#cache-bust`
;; wraps the current resolver so every LATER local #image of the document gets
;; `?h=<first 8 hex of the file's sha1>` appended to its src. Installed by a
;; call, not at module instantiation: a plugin is instantiated once per
;; process, while the resolver is per document.

(require file/sha1
         yamd/image)

(provide cache-bust)

(define (cache-bust)
  (define next (current-image-resolver))
  (current-image-resolver
   (lambda (src path width height)
     (define attrs (next src path width height))
     (if path
         (cons (cons 'src (format "~a?h=~a" src
                                  (substring (call-with-input-file path sha1) 0 8)))
               (remove (assq 'src attrs) attrs))
         attrs))))
