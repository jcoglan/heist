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

; (fact x)
; Returns factorial of x
(define (fact x)
  (define (iter y acc)
    (if (= y 0)
        acc
        (iter (- y 1) (* y acc))))
  (iter x 1))

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

; (benchmark)
; Runs body a given number of times and prints
; execution time.
(define-syntax benchmark
  (syntax-rules (times)
    [(_ name n times body ...)
      (letrec ([start (runtime)]
               [loop (lambda (count)
                        body ...
                        (if (> count 1)
                            (loop (- count 1))))])
        (loop n)
        (display (+ name ": "
                    n " iterations: "
                    (- (runtime) start))))]))

