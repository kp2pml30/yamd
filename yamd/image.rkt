#lang racket/base
;; Local image support for std/base's #image: src -> file resolution, the
;; pluggable resolver that turns it into <img> attributes, and a dependency-free
;; header reader for intrinsic pixel dimensions (PNG, JPEG, GIF, WebP).
;;
;; Resolution (`image-src->path`): a src with a URL scheme (`https:`, `data:`,
;; …) or protocol-relative (`//host/…`) is remote — #f, never touched. A
;; root-relative `/x.png` resolves under `current-docref-root` (the CLI's
;; --root) and is #f (left alone) without one: the site root is not knowable
;; otherwise. Anything else is relative to the directory of the file being
;; evaluated (current-yamd-source; the current directory for string-fed
;; documents), exactly like #include. `?query` / `#fragment` are stripped and
;; %-escapes decoded before touching the filesystem; the emitted src is
;; always the author's spelling unless a resolver rewrites it.

(require racket/path
         "runtime.rkt")

(provide current-image-resolver
         default-image-resolver
         image-src->path
         image-file-size)

;; (resolver src path width height) -> Attrs. `path` is the complete local
;; path (or #f, see above); width/height are the author's explicit positive
;; integers or #f. The result carries at least `src`; #image adds alt /
;; loading / decoding for keys the resolver did not set. Rebound per document
;; evaluation (eval.rkt), so a document or plugin that sets it affects only
;; the rest of that document and the files it embeds.
(define (default-image-resolver src path width height)
  (define natural (and path (local-image-size src path)))
  (define-values (w h) (complete-size width height natural))
  (append (list (cons 'src src))
          (if w (list (cons 'width (number->string w))) '())
          (if h (list (cons 'height (number->string h))) '())))

(define current-image-resolver (make-parameter default-image-resolver))

(define (local-image-size src path)
  (unless (file-exists? path)
    (error 'image "no such image file: ~s (resolved to ~a)" src path))
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (error 'image "cannot read dimensions of ~s (~a): ~a"
                            src path (exn-message e)))])
    (image-file-size path)))

;; Explicit dimensions win; a single explicit one scales the other by the
;; intrinsic aspect ratio. No intrinsic size (remote, SVG) = explicit only.
(define (complete-size width height natural)
  (cond
    [(or (not natural) (and width height)) (values width height)]
    [width (values width (scale width (cdr natural) (car natural)))]
    [height (values (scale height (car natural) (cdr natural)) height)]
    [else (values (car natural) (cdr natural))]))

(define (scale n num den)
  (max 1 (floor (+ 1/2 (/ (* n num) den)))))

;; ---------------------------------------------------------------------------
;; src -> local path

(define (image-src->path src)
  (cond
    [(regexp-match? #rx"^[a-zA-Z][a-zA-Z0-9+.-]*:" src) #f]
    [(regexp-match? #rx"^//" src) #f]
    [else
     (define rel (percent-decode (car (regexp-split #rx"[?#]" src))))
     (when (regexp-match? #rx#"\0" rel)
       (error 'image "src ~s decodes to a path with a NUL byte" src))
     (define root (current-docref-root))
     (cond
       [(equal? rel #"") (error 'image "src ~s names no file" src)]
       [(regexp-match? #rx#"^/" rel)
        (and root
             (simple-form-path
              (build-path (path->complete-path root) (bytes->path (bytes-append #"." rel)))))]
       [else (simple-form-path (build-path (source-directory) (bytes->path rel)))])]))

(define (source-directory)
  (define src (current-yamd-source))
  (or (and src (path-only (path->complete-path src)))
      (current-directory)))

;; -> bytes: a %-escape may spell a byte that is not valid UTF-8 on its own
(define (percent-decode s)
  (regexp-replace* #px#"%([0-9a-fA-F]{2})" (string->bytes/utf-8 s)
                   (lambda (_ hex) (bytes (string->number (bytes->string/latin-1 hex) 16)))))

;; ---------------------------------------------------------------------------
;; Header reader

;; -> (cons width height) in pixels, or #f for SVG (no intrinsic pixel size).
;; Raises exn:fail for anything else it cannot size.
(define (image-file-size path)
  (if (regexp-match? #rx"(?i:[.]svgz?)$" (if (path? path) (path->string path) path))
      #f
      (call-with-input-file path read-image-size)))

(define png-signature #"\211PNG\r\n\32\n")

(define (read-image-size in)
  (define head (let ([h (peek-bytes 12 0 in)]) (if (eof-object? h) #"" h)))
  (define (head-is? prefix [at 0])
    (and (>= (bytes-length head) (+ at (bytes-length prefix)))
         (equal? (subbytes head at (+ at (bytes-length prefix))) prefix)))
  (define size
    (cond
      [(head-is? png-signature) (read-png in)]
      [(head-is? #"\377\330") (read-jpeg in)]
      [(or (head-is? #"GIF87a") (head-is? #"GIF89a")) (read-gif in)]
      [(and (head-is? #"RIFF") (head-is? #"WEBP" 8)) (read-webp in)]
      [else (error 'image-file-size "not a PNG, JPEG, GIF, WebP or SVG image")]))
  (unless (and (positive? (car size)) (positive? (cdr size)))
    (error 'image-file-size "zero image dimension ~ax~a" (car size) (cdr size)))
  size)

(define (read-n in n)
  (define b (read-bytes n in))
  (unless (and (bytes? b) (= (bytes-length b) n))
    (error 'image-file-size "truncated image header"))
  b)

(define (u16 b at big?) (integer-bytes->integer b #f big? at (+ at 2)))
(define (u32 b at big?) (integer-bytes->integer b #f big? at (+ at 4)))
(define (u24le b at)
  (+ (bytes-ref b at) (* 256 (bytes-ref b (+ at 1))) (* 65536 (bytes-ref b (+ at 2)))))

;; signature, IHDR length, "IHDR", width, height
(define (read-png in)
  (define b (read-n in 24))
  (unless (equal? (subbytes b 12 16) #"IHDR")
    (error 'image-file-size "PNG without a leading IHDR chunk"))
  (cons (u32 b 16 #t) (u32 b 20 #t)))

;; logical screen descriptor right after the 6-byte signature
(define (read-gif in)
  (define b (read-n in 10))
  (cons (u16 b 6 #f) (u16 b 8 #f)))

(define (read-webp in)
  (define b (read-n in 20))
  (define chunk (subbytes b 12 16))
  (cond
    [(equal? chunk #"VP8 ")
     ;; frame tag (3), start code 9d 01 2a, 14-bit width/height (+2 scale bits)
     (define f (read-n in 10))
     (unless (equal? (subbytes f 3 6) #"\235\1*")
       (error 'image-file-size "WebP VP8 chunk without a keyframe start code"))
     (cons (bitwise-and (u16 f 6 #f) #x3fff) (bitwise-and (u16 f 8 #f) #x3fff))]
    [(equal? chunk #"VP8L")
     ;; signature 0x2f, then 14-bit width-1 and height-1
     (define f (read-n in 5))
     (unless (= (bytes-ref f 0) #x2f)
       (error 'image-file-size "WebP VP8L chunk without its signature byte"))
     (define bits (u32 f 1 #f))
     (cons (add1 (bitwise-and bits #x3fff))
           (add1 (bitwise-and (arithmetic-shift bits -14) #x3fff)))]
    [(equal? chunk #"VP8X")
     ;; flags (1), reserved (3), 24-bit canvas width-1 and height-1
     (define f (read-n in 10))
     (cons (add1 (u24le f 4)) (add1 (u24le f 7)))]
    [else (error 'image-file-size "unknown WebP chunk ~s" chunk)]))

;; Walks the marker segments up to the first SOFn frame header. EXIF
;; orientations 5–8 (transposed) swap the result: browsers lay the image out
;; rotated (image-orientation: from-image is the default).
(define (read-jpeg in)
  (read-n in 2)
  (let loop ([orientation #f])
    ;; like libjpeg, tolerate garbage before a marker (e.g. segment padding),
    ;; then skip 0xff fill bytes
    (let skip-garbage ()
      (unless (= (read-byte* in) #xff) (skip-garbage)))
    (define m (let skip-fill ([m (read-byte* in)])
                (if (= m #xff) (skip-fill (read-byte* in)) m)))
    (cond
      [(sof-marker? m)
       (define b (read-n in 7))
       (define h (u16 b 3 #t))
       (define w (u16 b 5 #t))
       (if (memv orientation '(5 6 7 8)) (cons h w) (cons w h))]
      [(or (= m #x01) (<= #xd0 m #xd8)) (loop orientation)]
      [(memv m '(#xd9 #xda))
       (error 'image-file-size "JPEG without a frame header before its scan data")]
      [else
       (define len (u16 (read-n in 2) 0 #t))
       (when (< len 2)
         (error 'image-file-size "corrupt JPEG: segment length ~a" len))
       (define payload (read-n in (- len 2)))
       (loop (or orientation
                 (and (= m #xe1) (exif-orientation payload))))])))

(define (read-byte* in)
  (define b (read-byte in))
  (when (eof-object? b)
    (error 'image-file-size "truncated JPEG"))
  b)

;; SOF0–SOF15 minus DHT (c4), JPG (c8) and DAC (cc)
(define (sof-marker? m)
  (and (<= #xc0 m #xcf) (not (memv m '(#xc4 #xc8 #xcc)))))

;; APP1 "Exif\0\0" + TIFF header -> IFD0 tag 0x0112, or #f. A malformed
;; EXIF block is ignored (as browsers do), never an error.
(define (exif-orientation seg)
  (define t 6) ; TIFF header offset
  (define (in-range? off n) (<= (+ t off n) (bytes-length seg)))
  (define order
    (and (in-range? 0 8)
         (equal? (subbytes seg 0 6) #"Exif\0\0")
         (case (subbytes seg t (+ t 2))
           [(#"MM") 'big]
           [(#"II") 'little]
           [else #f])))
  (define (num off n)
    (and (in-range? off n)
         (integer-bytes->integer seg #f (eq? order 'big) (+ t off) (+ t off n))))
  (define ifd (and order (num 4 4)))
  (define count (and ifd (num ifd 2)))
  (and count
       (for/first ([i (in-range count)]
                   #:when (eqv? (num (+ ifd 2 (* 12 i)) 2) #x0112))
         (num (+ ifd 2 (* 12 i) 8) 2))))

(module+ test
  (require rackunit
           racket/file)

  (define (size-of bs) (read-image-size (open-input-bytes bs)))
  (define (le16 n) (integer->integer-bytes n 2 #f #f))
  (define (be16 n) (integer->integer-bytes n 2 #f #t))
  (define (le32 n) (integer->integer-bytes n 4 #f #f))
  (define (be32 n) (integer->integer-bytes n 4 #f #t))
  (define (le24 n) (subbytes (le32 n) 0 3))

  (define png (bytes-append png-signature (be32 13) #"IHDR" (be32 300) (be32 200)))
  (define gif (bytes-append #"GIF89a" (le16 40) (le16 30) #"\0\0\0"))
  (define (riff chunk data)
    (bytes-append #"RIFF" (le32 (+ 12 (bytes-length data))) #"WEBP"
                  chunk (le32 (bytes-length data)) data))
  (define webp-lossy (riff #"VP8 " (bytes-append #"\0\0\0" #"\235\1*" (le16 640) (le16 480))))
  (define webp-lossless
    (riff #"VP8L" (bytes-append #"\57" (le32 (bitwise-ior (sub1 17)
                                                         (arithmetic-shift (sub1 9) 14))))))
  (define webp-extended (riff #"VP8X" (bytes-append #"\20\0\0\0" (le24 4999) (le24 2999))))

  (define (segment marker payload)
    (bytes-append (bytes #xff marker) (be16 (+ 2 (bytes-length payload))) payload))
  (define (exif orientation #:big? [big? #t])
    (define (n2 v) (integer->integer-bytes v 2 #f big?))
    (define (n4 v) (integer->integer-bytes v 4 #f big?))
    (bytes-append #"Exif\0\0" (if big? #"MM" #"II") (n2 42) (n4 8)
                  (n2 1) (n2 #x0112) (n2 3) (n4 1) (n2 orientation) (n2 0) (n4 0)))
  (define (jpeg #:orientation [o #f] #:sof [sof #xc0] #:big? [big? #t])
    (bytes-append #"\377\330"
                  (segment #xe0 #"JFIF\0\1\1\0\0\1\0\1\0\0")
                  (if o (segment #xe1 (exif o #:big? big?)) #"")
                  (segment #xdb (make-bytes 65 0))
                  #"\377\377" ; fill bytes before a marker are legal
                  (segment sof (bytes-append #"\10" (be16 120) (be16 160) #"\3"))
                  (segment #xda #"\0")))

  (check-equal? (size-of png) '(300 . 200))
  (check-equal? (size-of gif) '(40 . 30))
  (check-equal? (size-of webp-lossy) '(640 . 480))
  (check-equal? (size-of webp-lossless) '(17 . 9))
  (check-equal? (size-of webp-extended) '(5000 . 3000))
  (check-equal? (size-of (jpeg)) '(160 . 120))
  (check-equal? (size-of (jpeg #:sof #xc2)) '(160 . 120)) ; progressive
  (check-equal? (size-of (jpeg #:orientation 1)) '(160 . 120))
  (check-equal? (size-of (jpeg #:orientation 6)) '(120 . 160))
  (check-equal? (size-of (jpeg #:orientation 8 #:big? #f)) '(120 . 160))
  ;; malformed EXIF is ignored, not an error
  (check-equal? (size-of (bytes-append #"\377\330" (segment #xe1 #"Exif\0\0MM")
                                       (segment #xc0 (bytes-append #"\10" (be16 2) (be16 3) #"\3"))))
                '(3 . 2))

  (check-exn #rx"not a PNG" (lambda () (size-of #"hello world, not an image")))
  (check-exn #rx"not a PNG" (lambda () (size-of #"")))
  (check-exn #rx"truncated" (lambda () (size-of (subbytes png 0 20))))
  (check-exn #rx"truncated" (lambda () (size-of (subbytes (jpeg) 0 30))))
  (check-exn #rx"before its scan" (lambda () (size-of (bytes-append #"\377\330" (segment #xda #"\0")))))
  ;; padding between segments is skipped, as libjpeg does
  (check-equal? (size-of (bytes-append #"\377\330" (segment #xe0 #"JFIF\0") #"\0\0"
                                       (segment #xc0 (bytes-append #"\10" (be16 2) (be16 3) #"\3"))))
                '(3 . 2))
  (check-exn #rx"zero image dimension"
             (lambda () (size-of (bytes-append png-signature (be32 13) #"IHDR" (be32 0) (be32 5)))))
  (check-exn #rx"unknown WebP chunk" (lambda () (size-of (riff #"ALPH" #"0123456789"))))

  ;; --- src -> path ---------------------------------------------------------
  (define dir (make-temporary-file "yamd-image-~a" 'directory))
  (define doc (build-path dir "sub" "doc.yamd"))
  (check-false (image-src->path "https://example.com/a.png"))
  (check-false (image-src->path "data:image/png;base64,AAAA"))
  (check-false (image-src->path "//cdn.example.com/a.png"))
  (parameterize ([current-yamd-source doc])
    (check-equal? (image-src->path "a.png") (build-path dir "sub" "a.png"))
    (check-equal? (image-src->path "../img/a%20b.png?v=2#x") (build-path dir "img" "a b.png"))
    (check-false (image-src->path "/static/a.png"))
    (check-equal? (image-src->path "%E2%9C%93.png") (build-path dir "sub" (bytes->path #"\342\234\223.png")))
    (check-exn #rx"names no file" (lambda () (image-src->path "")))
    (check-exn #rx"names no file" (lambda () (image-src->path "?v=1")))
    (check-exn #rx"NUL byte" (lambda () (image-src->path "a%00b.png")))
    ;; a relative root is taken against the current directory
    (parameterize ([current-docref-root "."])
      (check-equal? (image-src->path "/a.png") (build-path (current-directory) "a.png")))
    (parameterize ([current-docref-root dir])
      (check-equal? (image-src->path "/static/a.png") (build-path dir "static" "a.png"))))

  ;; --- default resolver ----------------------------------------------------
  (define img (build-path dir "a.png"))
  (display-to-file png img)
  (define (resolve path w h) (default-image-resolver "a.png" path w h))
  (check-equal? (resolve img #f #f) '((src . "a.png") (width . "300") (height . "200")))
  (check-equal? (resolve img 150 #f) '((src . "a.png") (width . "150") (height . "100")))
  (check-equal? (resolve img #f 50) '((src . "a.png") (width . "75") (height . "50")))
  (check-equal? (resolve img 10 10) '((src . "a.png") (width . "10") (height . "10")))
  (check-equal? (resolve #f 10 #f) '((src . "a.png") (width . "10")))
  (check-equal? (resolve #f #f #f) '((src . "a.png")))
  (check-exn #rx"no such image file: \"a.png\"" (lambda () (resolve (build-path dir "nope.png") #f #f)))
  (define svg (build-path dir "v.svg"))
  (display-to-file "<svg/>" svg)
  (check-equal? (resolve svg 20 #f) '((src . "a.png") (width . "20")))
  (define txt (build-path dir "t.png"))
  (display-to-file "not really a png" txt)
  (check-exn #rx"cannot read dimensions of \"a.png\".*not a PNG" (lambda () (resolve txt #f #f)))
  (delete-directory/files dir))
