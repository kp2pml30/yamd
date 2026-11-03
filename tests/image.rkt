#lang racket/base
;; #image against real files: header sizing of encoder-produced fixtures
;; (tests/fixtures/images, made with Pillow), source-relative resolution
;; through #include, explicit/partial dimensions, remote passthrough, the
;; missing-file error and the current-image-resolver hook. Synthetic
;; header edge cases live in yamd/image.rkt's own test submodule.

(module+ test
  (require rackunit
           racket/runtime-path
           racket/string
           file/sha1
           yamd
           (only-in yamd/runtime current-yamd-source))

  (define-runtime-path fixtures "fixtures/images")
  (define (fixture name) (build-path fixtures name))

  ;; --- header reader over real encoder output ----------------------------
  (for ([name '("photo.png" "photo.jpg" "progressive.jpg" "photo.gif"
                "lossy.webp" "lossless.webp" "alpha.webp")])
    (check-equal? (image-file-size (fixture name)) '(6 . 4) name))
  ;; EXIF orientation 6 (rotated 90°): laid out transposed
  (check-equal? (image-file-size (fixture "rotated.jpg")) '(4 . 6))
  (check-false (image-file-size (build-path fixtures "vector.svg")))

  ;; --- documents -----------------------------------------------------------
  (define page-html (yamd-file->html (fixture "page.yamd")))
  (define (img src rest) (format "<img src=\"~a\" ~a />" src rest))
  (check-equal?
   page-html
   (string-append
    "<p>"
    (img "photo.png" "width=\"6\" height=\"4\" alt=\"png\" loading=\"lazy\" decoding=\"async\"")
    "\n"
    ;; query stripped for the lookup, kept in the src; height from aspect
    (img "lossy.webp?v=1" "width=\"3\" height=\"2\" alt=\"webp\" loading=\"lazy\" decoding=\"async\"")
    "\n"
    (img "rotated.jpg" "width=\"2\" height=\"3\" alt=\"jpeg\" loading=\"eager\" decoding=\"async\"")
    "\n"
    (img "https://example.com/x.png" "alt=\"remote\" loading=\"lazy\" decoding=\"async\"")
    "</p><p>"
    ;; the included file resolves against ITS directory
    (img "../photo.gif" "width=\"6\" height=\"4\" alt=\"gif\" loading=\"lazy\" decoding=\"async\"")
    "</p>"))

  ;; missing file: located (file:line:col) error naming both spellings
  (check-exn #rx"^doc[.]yamd:1:[0-9]+: image: no such image file: \"no-such[.]png\" \\(resolved to "
             (lambda ()
               (yamd-string->html "#image(\"no-such.png\" alt: \"x\")\n" #:src "doc.yamd")))
  ;; a file that is not an image
  (check-exn #rx"image: cannot read dimensions of \"cache-bust.rkt\""
             (lambda ()
               (parameterize ([current-yamd-source (fixture "page.yamd")])
                 (yamd-string->html "#image(\"cache-bust.rkt\" alt: \"x\")\n"))))

  ;; --- resolver hook -------------------------------------------------------
  (define png-hash (substring (call-with-input-file (fixture "photo.png") sha1) 0 8))

  ;; a plugin installs it from inside the document…
  (check-equal?
   (yamd-file->html (fixture "hooked.yamd"))
   (string-append
    "<p>"
    (img (string-append "photo.png?h=" png-hash)
         "width=\"6\" height=\"4\" alt=\"png\" loading=\"lazy\" decoding=\"async\"")
    "\n"
    (img "https://example.com/x.png" "alt=\"remote\" loading=\"lazy\" decoding=\"async\"")
    "</p>"))
  ;; …and it does not outlive that document
  (check-equal? (yamd-file->html (fixture "page.yamd")) page-html)
  ;; nor an #include'd file that installs it
  (check-equal?
   (yamd-file->html (fixture "scoped.yamd"))
   (string-append
    "<p>"
    (img (string-append "../photo.png?h=" png-hash)
         "width=\"6\" height=\"4\" alt=\"child\" loading=\"lazy\" decoding=\"async\"")
    "</p><p>"
    (img "photo.png" "width=\"6\" height=\"4\" alt=\"after\" loading=\"lazy\" decoding=\"async\"")
    "\n"
    ;; SVG: no intrinsic size, explicit dimensions only
    (img "vector.svg" "width=\"8\" alt=\"svg\" loading=\"lazy\" decoding=\"async\"")
    "</p>"))

  ;; a `#lang yamd` module resolves next to its file, whatever the cwd
  (check-regexp-match
   #rx"width=\"6\""
   (content->html
    (parameterize ([current-directory (find-system-path 'temp-dir)])
      (dynamic-require (fixture "sub/lang-doc.rkt") 'doc))))

  ;; root-relative src under the build's --root
  (check-regexp-match
   #rx"<img src=\"/photo[.]png\" width=\"6\" height=\"4\""
   (build-output-text
    (car (build-docset (fixture "sub/rooted.yamd")
                       #:strategy single-file-strategy
                       #:backend (build-backend 'html ".html" content->html)
                       #:root-dir fixtures))))

  ;; an embedder parameterizes it around a build; the resolver's attrs win
  (check-true
   (string-prefix?
    (parameterize ([current-image-resolver
                    (lambda (src path w h)
                      (list (cons 'src (string-append "/cdn/" src)) (cons 'loading "eager")))])
      (yamd-file->html (fixture "page.yamd")))
    "<p><img src=\"/cdn/photo.png\" loading=\"eager\" alt=\"png\" decoding=\"async\" />")))
