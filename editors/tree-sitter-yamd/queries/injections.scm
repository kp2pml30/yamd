; Host-language injection: `(args)` and `#(...)` contents are Racket.
; `injection.combined` merges all host fragments into one virtual document so
; cross-fragment constructs (a `#(define ...)` used later) highlight sanely.
((host) @injection.content
 (#set! injection.language "racket")
 (#set! injection.combined))

; Block comments may contain anything; keep them opaque (no injection).
