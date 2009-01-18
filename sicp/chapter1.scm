; Section 1.2
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html

; Exercise 1.11
; 
; f(n) = | n if n < 3
;        | f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3

; Recursive solution
(define (f n)
  (if (< n 3)
      n
      (+
        (f (- n 1))
        (* 2 (f (- n 2)))
        (* 3 (f (- n 3))))))

; Iterative solution
(define (f n)
  (define (iter count x y z)
    (if (< count 3)
        z
        (iter (- count 1)
              y z
              (+ z (* 2 y) (* 3 x)))))
  (iter n 0 1 2))

