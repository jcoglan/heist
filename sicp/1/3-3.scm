; Section 1.3.3
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-12.html#%_sec_1.3.3

(load "../helpers")


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

(output '(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
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
(output '(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))
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

(output '(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))
; 4.55553755199982, 9 guesses


(exercise "1.37")
; k-term finite continued fraction

(define (cont-frac n d k)
  (define (term i)
    (if (= (- k 1) i)
        (+ (d i) (/ (n k) (d k)))
        (+ (d i) (/ (n (+ 1 i)) (term (+ 1 i))))))
  (/ (n 1) (term 1)))

(output '(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 13)))
; 1.61802575107296

; Tail-recursive version
(define (cont-frac n d k)
  (define (iter i term)
    (if (= i 0)
        term
        (let ((x (n i))
              (y (d i)))
          (iter (- i 1) (/ x (+ y term))))))
  (iter k 0))

(output '(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 13)))


(exercise "1.38")
; Euler's approximation for e
(define e (+ 2 (cont-frac (lambda (i) 1.0)
                          (lambda (i)
                            (let ((x (+ 1 i)))
                              (if (= 0 (remainder x 3))
                                  (* 2 (/ x 3))
                                  1.0)))
                          20)))
(output 'e)
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
(output '(tan-cf 0 10))                ; 0.0
(output '(tan-cf (/ pi 8) 10))         ; 0.41421317376392
(output '(tan-cf (/ pi 4) 10))         ; 0.999998673205983
(output '(tan-cf (/ pi 2) 10))         ; 753695.994435399
(output '(tan-cf (* 3 (/ pi 4)) 10))   ; -1.00000398040374

