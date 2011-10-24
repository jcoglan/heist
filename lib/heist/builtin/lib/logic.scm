; (eq? x y)
; Currently an alias for (eqv? x y). TODO implement properly
(define eq? eqv?)

; (not x)
; Boolean inverse of x
(define (not x)
  (if x #f #t))

; Longhand aliases for boolean constants
(define true #t)
(define false #f)

; (boolean? x)
; Returns true iff x is a boolean value
(define (boolean? x)
  (or (eqv? x #t) (eqv? x #f)))
