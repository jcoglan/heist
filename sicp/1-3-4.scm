; Section 1.3.4
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-12.html#%_sec_1.3.4

(load "helpers")


; Higher-order functions for the final few exercises

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))


(exercise "1.40")
; Functions as return values

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))


(exercise "1.41")
; Double application

(define (double f)
  (lambda (x)
    (f (f x))))

(define (inc x) (+ 1 x))
(output '(((double (double double)) inc) 5))


(exercise "1.42")
; Composition

(define (compose f g)
  (lambda (x)
    (f (g x))))


(exercise "1.43")
; n-ary application

(define (repeated f n)
  (define (loop m x)
    (if (= m 0)
        x
        (f (loop (- m 1) x))))
  (lambda (x)
    (loop n x)))

; or, using compose:
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(output '((repeated square 2) 5))


(exercise "1.44")
; Functional smoothing

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (n-smooth f n)
  ((repeated smooth n) f))


(exercise "1.45")
; nth roots

; To find the nth root of x, we find the fixed point
; y of y -> x/y^(n-1). Provide a function to generate
; this transformation, curried on n and x
(define (nth-root-transform n)
  (lambda (x)
    (lambda (y)
      (/ x (expt y (- n 1))))))

; To converge, this must be average-damped by a factor
; given by floor(log2(n)). Again, curry on n,x
(define (nth-root n)
  (let ([transform (nth-root-transform n)]
        [k (floor (/ (log n) (log 2)))])
    (lambda (x)
      (fixed-point-of-transform (transform x)
                                (repeated average-damp k)
                                1.0))))

(output '((nth-root 2) 9))
(output '((nth-root 3) 27))
(output '((nth-root 4) 16))
(output '((nth-root 5) 32))
(output '((nth-root 6) 64))
(output '((nth-root 7) 128))
(output '((nth-root 8) 256))
(output '((nth-root 9) 512))
(output '((nth-root 10) 1024))
(output '((nth-root 11) 2048))
(output '((nth-root 12) 4096))
(output '((nth-root 13) 8192))
(output '((nth-root 14) 16384))
(output '((nth-root 15) 32768))
(output '((nth-root 16) 65536))
(output '((nth-root 17) 131072))


(exercise "1.46")
; Abstracted iterative improvement

(define (iterative-improve good-enough? improve)
  (define (iterate guess)
    (if (good-enough? guess)
        guess
        (iterate (improve guess))))
  (lambda (first-guess)
    (iterate first-guess)))

(define (sqrt x)
  ((iterative-improve (lambda (guess)
                        (< (abs (- (square guess) x))
                           0.001))
                      (lambda (guess)
                        (average guess (/ x guess))))
   1.0))

(output '(sqrt 9))

