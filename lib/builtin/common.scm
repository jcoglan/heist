; Any built-in functions that we can implement directly
; in Scheme should go here. If at all possible, write
; builtins in Scheme rather than Ruby.

; (not x)
; Boolean inverse of x
(define (not x)
  (if x #f #t))

; Longhand aliases for boolean constants
(define true #t)
(define false #f)

; (<= x y)
; Returns true iff x <= y
(define (<= x y)
  (not (> x y)))

; (>= x y)
; Returns true iff x >= y
(define (>= x y)
  (not (< x y)))

; (boolean? x)
; Returns true iff x is a boolean value
(define (boolean? x)
  (or (eqv? x #t) (eqv? x #f)))

; (number? x)
; Returns true iff x is any type of number
(define number? complex?)

; (abs x)
; Returns the absolute value of a number
(define (abs x)
  (if (>= x 0)
      x
      (- x)))

; (newline)
; prints a new-line character
(define (newline)
  (display "\n"))

; (fact x)
; Returns the factorial of x
(define (fact x)
  (define (rec y acc)
    (cond ((= y 0) acc)
          (else (rec (- y 1) (* y acc)))))
  (rec x 1))

