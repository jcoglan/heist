; Section 1.1.6
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1.6

(load "../helpers")


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

(output '(sum-squares-largest-two 2 9 5))
(output '(sum-squares-largest-two 4 3 1))
(output '(sum-squares-largest-two 7 3 4))


(exercise "1.5")

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; Don't run this unless using lazy evaluation. With Eager
; evaluation, (p) calls itself repeatedly.
; (test 0 (p))

