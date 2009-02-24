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

