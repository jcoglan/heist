; Section 1.3.2
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-12.html#%_sec_1.3.2

(load "../helpers")


(exercise "1.34")
; Invalid expression

(define (f g)
  (g 2))
(output '(f square))
(output '(f (lambda (z) (* z (+ z 1)))))

; (f f) -> (f 2) -> (2 2) -> invalid expression

