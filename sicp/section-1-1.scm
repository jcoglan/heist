; Section 1.1
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html

(load "helpers")


(exercise "1.2")
(display
  (/
    (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 3)))))
    (* 3 (- 6 2) (- 2 7))))


(exercise "1.3")
; Using only functions discussed thus far:
(define (sum-squares-largest-two x y z)
  (cond ((and (< x y) (< x z)) (+ (* y y) (* z z)))
        ((and (< y x) (< y z)) (+ (* x x) (* z z)))
        (else                  (+ (* x x) (* y y)))))

(output "(sum-squares-largest-two 2 9 5)")
(output "(sum-squares-largest-two 4 3 1)")
(output "(sum-squares-largest-two 7 3 4)")


(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

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

(define (sqrt-iter guess last x)
  (if (< (abs (/ (- guess last) last)) 0.0001)
      guess
      (sqrt-iter (improve guess x) guess x)))
(define (sqrt x)
  (sqrt-iter 0.1 1.0 x))

(output "(sqrt 40000)")

