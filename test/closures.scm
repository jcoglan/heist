(define (add n)
  (lambda (x) (+ x n)))
      
(define add4 (add 4))
(define add7 (add 7))

(assert-equal 15 (add4 11))
    
(define (weird x y)
  (begin
    (define (+ x y)
      (* x y))
    (+ x y)))
(assert-equal 7 (+ 3 4))
(assert-equal 12 (weird 3 4))

