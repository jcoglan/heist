; Any built-in functions that we can implement directly
; in Scheme should go here. If at all possible, write
; builtins in Scheme rather than Ruby.

; (newline)
; prints a new-line character
(define (newline)
  (display "\n"))

; (force)
; Extracts the value of a promise created using (delay)
(define (force promise) (promise))

; (call/cc)
; Alias for (call-with-current-continuation)
(define call/cc call-with-current-continuation)

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

;----------------------------------------------------------------

; Numerical functions

; (number? x)
; Returns true iff x is any type of number
(define number? complex?)

; (zero? x)
; Returns true iff x is zero
(define (zero? x)
  (eqv? x 0))

; (positive? x)
; Returns true iff x > 0
(define (positive? x)
  (> x 0))

; (negative? x)
; Returns true iff x < 0
(define (negative? x)
  (< x 0))

; (odd? x)
; Returns true iff x is odd
(define (odd? x)
  (= 1 (remainder x 2)))

; (even? x)
; Returns true iff x is even
(define (even? x)
  (zero? (remainder x 2)))

; (max arg1 arg2 ...)
; Returns the maximum value in the list of arguments
(define (max first . rest)
  (cond [(null? rest) first]
        [else
          (let* ([head (car rest)]
                 [x (if (>= first head) first head)])
            (apply max (cons x (cdr rest))))]))

; (min arg1 arg2 ...)
; Returns the minimum value in the list of arguments
(define (min first . rest)
  (cond [(null? rest) first]
        [else
          (let* ([head (car rest)]
                 [x (if (<= first head) first head)])
            (apply min (cons x (cdr rest))))]))

; (abs x)
; Returns the absolute value of a number
(define (abs x)
  (if (negative? x)
      (- x)
      x))

; (gcd x y)
; Returns the greatest common divisor of two numbers
; http://en.wikipedia.org/wiki/Euclidean_algorithm
; TODO take >2 arguments
(define (gcd x y)
  (if (zero? y)
      (abs x)
      (gcd y (remainder x y))))

; (lcm x y)
; Returns the lowest common multiple of two numbers
; http://en.wikipedia.org/wiki/Least_common_multiple
; TODO take >2 arguments
(define (lcm x y)
  (/ (abs (* x y))
     (gcd x y)))

(define ceiling ceil)

; (factorial x)
; Returns factorial of x
(define (factorial x)
  (define (iter y acc)
    (if (zero? y)
        acc
        (iter (- y 1) (* y acc))))
  (iter x 1))

;----------------------------------------------------------------

; List/pair functions

; (null? object)
; Returns true iff object is the empty list
(define (null? object)
  (eqv? '() object))

; (list? object)
; Returns true iff object is a proper list
(define (list? object)
  (or (null? object)
      (and (pair? object)
           (list? (cdr object)))))

; (list arg ...)
; Allocates and returns a new list from its arguments
(define (list . args) args)

; (length object)
; Returns the length of a proper list
(define (length object)
  (define (iter list acc)
    (if (null? list)
        acc
        (iter (cdr list) (+ 1 acc))))
  (iter object 0))

