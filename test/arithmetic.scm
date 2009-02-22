(assert-equal 486   (+ 137 349))
(assert-equal 666   (- 1000 334))
(assert-equal 495   (* 5 99))
(assert-equal 2     (/ 10 5))
(assert-equal 12.7  (+ 2.7 10))
(assert-equal 75    (+ 21 35 12 7))
(assert-equal 1200  (* 25 4 12))
(assert-equal 19    (+ (* 3 5) (- 10 6)))
(assert-equal 57    (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6)))
(assert-equal 8     (expt 2 3))
(assert-equal 2     (expt 4 1/2))

(define (sqrt x)
  (define (square x)
    (* x x))
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(assert (< (abs (- (sqrt 9) 3)) 0.0001))

; http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_idx_288
(assert-equal 1 (modulo 13 4))
(assert-equal 1 (remainder 13 4))
(assert-equal 3 (modulo -13 4))
(assert-equal -1 (remainder -13 4))
(assert-equal -3 (modulo 13 -4))
(assert-equal 1 (remainder 13 -4))
(assert-equal -1 (modulo -13 -4))
(assert-equal -1 (remainder -13 -4))
(assert-equal -1.0 (remainder -13 -4.0)) ; inexact

(assert-equal 4 (gcd 32 -36))
(assert-equal 288 (lcm 32 -36))
(assert-equal 288.0 (lcm 32.0 -36)) ; inexact

(assert-equal -5.0 (floor -4.3))
(assert-equal -4.0 (ceiling -4.3))
(assert-equal -4.0 (truncate -4.3))
(assert-equal -4.0 (round -4.3))

(assert-equal 3.0 (floor 3.5))
(assert-equal 4.0 (ceiling 3.5))
(assert-equal 3.0 (truncate 3.5))
(assert-equal 4.0 (round 3.5)) ; inexact

(assert-equal 4 (round 7/2)) ; exact
(assert-equal 7 (round 7))

