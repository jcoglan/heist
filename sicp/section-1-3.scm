; Section 1.3
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-12.html

(load "helpers")


(exercise "1.29")
; Simpson's rule

; Version printed in SICP
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; Tail recursive version (exercise 1.30, but required
; so that Heist will actually run the integrals below)
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(output "(integral cube 0 1 0.01)")

(define (simpson-int f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (next x) (+ x 2))
  (* (/ h 3)
     (+ (y 0)
        (y n)
        (* 4 (sum y 1 next (- n 1)))
        (* 2 (sum y 2 next (- n 2))))))

(output "(simpson-int cube 0 1 100)")
(output "(simpson-int cube 0 1 1000)")


(exercise "1.31.a")
; Products

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (I x) x)
(define (next x) (+ x 1))

(define (factorial n)
  (product I 1 next n))

(output "(factorial 6)")

(define (pi n)
  (* 8 (/ (product (lambda (x)
                     (square (/ (* 2 x)
                                (- (* 2 x) 1))))
                   2 next n)
          (* 2 n))))

(output "(pi 10)")
(output "(pi 100)")


(exercise "1.31.b")
; Recursive product function

(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) next b))))
         
(output "(product-rec I 1 next 6)")


(exercise "1.32.a")
; Generalized accumulation

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(output "(sum cube 1 next 5)")
(output "(product I 1 next 6)")

(exercise "1.32.b")
; Recursive accumulator

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate-rec combiner null-value term (next a) next b))))

(output "(accumulate-rec * 1 I 1 next 6)")


(exercise "1.33")
; Accumulation using a filter

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (cond ((> a b)
            result)
          ((not (filter a))
            (iter (next a) result))
          (else
            (iter (next a) (combiner (term a) result)))))
  (iter a null-value))

; Sum of squares of primes
(define (sum-squared-primes a b)
  (filtered-accumulate + 0 square a next b prime?))

(output "(sum-squared-primes 1 10)")

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Product of numbers relatively prime to n
(define (product-relative-primes n)
  (filtered-accumulate * 1 I 1 next (- n 1)
    (lambda (x)
      (= (gcd x n) 1))))

(output "(product-relative-primes 25)")


(exercise "1.34")
; Invalid expression

(define (f g)
  (g 2))
(output "(f square)")
(output "(f (lambda (z) (* z (+ z 1))))")

; (f f) -> (f 2) -> (2 2) -> invalid expression


(exercise "1.35")
; Golden ratio as a fixed point

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

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

(output "(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)")
; 1.61803278688525


(exercise "1.36")
; Modified fixed-point that prints progress

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try n guess)
    (display (+ n ": " guess)) (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try (+ n 1) next))))
  (try 1 first-guess))

; Find solution of x^x = 1000
; Don't begin at 1.0 since (log 1.0) = 0
(output "(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)")
; 4.55553227080365, 34 guesses

; Add average damping
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try n guess)
    (display (+ n ": " guess)) (newline)
    (let ((next (/ (+ guess (f guess)) 2)))
      (if (close-enough? guess next)
          next
          (try (+ n 1) next))))
  (try 1 first-guess))

(output "(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)")
; 4.55553755199982, 9 guesses


(exercise "1.37")
; k-term finite continued fraction

(define (cont-frac n d k)
  (define (term i)
    (if (= (- k 1) i)
        (+ (d i) (/ (n k) (d k)))
        (+ (d i) (/ (n (+ 1 i)) (term (+ 1 i))))))
  (/ (n 1) (term 1)))

(output "(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 13))")
; 1.61802575107296

; Tail-recursive version
(define (cont-frac n d k)
  (define (iter i term)
    (if (= i 0)
        term
        (let ([x (n i)]
              [y (d i)])
          (iter (- i 1) (/ x (+ y term))))))
  (iter k 0))

(output "(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 13))")


(exercise "1.38")
; Euler's approximation for e
(define e (+ 2 (cont-frac (lambda (i) 1.0)
                          (lambda (i)
                            (let ([x (+ 1 i)])
                              (if (= 0 (remainder x 3))
                                  (* 2 (/ x 3))
                                  1.0)))
                          20)))
(output "e")
; 2.71828182845905


(exercise "1.39")
; J.H. Lambert's tangent formula

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= 1 i) x (- (expt x 2))))
             (lambda (i)
               (- (* 2 i) 1))
             k))
             
(define pi 3.14159)
(output "(tan-cf 0 10)")                ; 0.0
(output "(tan-cf (/ pi 8) 10)")         ; 0.41421317376392
(output "(tan-cf (/ pi 4) 10)")         ; 0.999998673205983
(output "(tan-cf (/ pi 2) 10)")         ; 753695.994435399
(output "(tan-cf (* 3 (/ pi 4)) 10)")   ; -1.00000398040374


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
(output "(((double (double double)) inc) 5)")


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

(output "((repeated square 2) 5)")


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

(output "((nth-root 2) 9)")
(output "((nth-root 3) 27)")
(output "((nth-root 4) 16)")
(output "((nth-root 5) 32)")
(output "((nth-root 6) 64)")
(output "((nth-root 7) 128)")
(output "((nth-root 8) 256)")
(output "((nth-root 9) 512)")
(output "((nth-root 10) 1024)")
(output "((nth-root 11) 2048)")
(output "((nth-root 12) 4096)")
(output "((nth-root 13) 8192)")
(output "((nth-root 14) 16384)")
(output "((nth-root 15) 32768)")
(output "((nth-root 16) 65536)")
(output "((nth-root 17) 131072)")

