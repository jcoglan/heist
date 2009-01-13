; Any built-in functions that we can implement directly
; in Scheme should go here. If at all possible, write
; builtins in Scheme rather than Ruby.

; less than or equal to
; Returns true iff x <= y
(define (<= x y)
  (not (> x y)))

; greater than or equal to
; Returns true iff x >= y
(define (>= x y)
  (not (< x y)))

; if -- only works using normal order (lazy) evaluation
(define (if condition consequence alternative)
  (cond (condition consequence)
        (else alternative)))

; K combinator
; Returns its input
(define (K x) x)

; Y combinator
; Returns fixed points of higher order functions
; Used to implement anonymous recursion
(define (Y f)
  ((lambda (g) (g g))
  (lambda (h)
    (lambda (x)
      ((f (h h)) x)))))

