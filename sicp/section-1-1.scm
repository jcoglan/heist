; Section 1.1
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html

(load "helpers")


(exercise "1.2")
(display
  (/
    (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 3)))))
    (* 3 (- 6 2) (- 2 7))))
(newline)


(exercise "1.3")
; Using only functions discussed thus far:
(define (sum-squares-largest-two x y z)
  (cond ((and (< x y) (< x z)) (+ (* y y) (* z z)))
        ((and (< y x) (< y z)) (+ (* x x) (* z z)))
        (else                  (+ (* x x) (* y y)))))

(output "(sum-squares-largest-two 2 9 5)")
(output "(sum-squares-largest-two 4 3 1)")
(output "(sum-squares-largest-two 7 3 4)")


(exercise "1.5")

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; Don't run this unless using lazy evaluation. With Eager
; evaluation, (p) calls itself repeatedly.
; (test 0 (p))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))


(exercise "1.7")

(define (good-enough? guess last)
  (< (abs (/ (- guess last) last)) 0.0001))

(define (sqrt-iter guess last x)
  (if (good-enough? guess last)
      guess
      (sqrt-iter (improve guess x) guess x)))
(define (sqrt x)
  (sqrt-iter 0.1 1.0 x))

(output "(sqrt 40000)")


(exercise "1.8")

(define (improve-cube guess x)
  (/  (+ (/ x (square guess)) (* 2 guess))
      3))

(define (cube-root-iter guess last x)
  (if (good-enough? guess last)
      guess
      (cube-root-iter (improve-cube guess x) guess x)))
      
(define (cube-root x)
  (cube-root-iter 0.1 1.0 x))

(output "(cube-root 27)")
(output "(cube-root 64)")

