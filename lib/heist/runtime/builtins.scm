; Any built-in functions that we can implement directly
; in Scheme should go here. If at all possible, write
; builtins in Scheme rather than Ruby.

(define (<= x y)
  (not (> x y)))

(define (>= x y)
  (not (< x y)))

