(define (square x) (* x x))
(assert-equal 441 (square 21))
(assert-equal 49  (square (+ 2 5)))
(assert-equal 81  (square (square 3)))
    
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(assert-equal 25 (sum-of-squares 3 4))
    
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(assert-equal 136 (f 5))

(assert-equal (f 5)
  ((lambda (a)
    ((lambda (x y)
      (+ (square x) (square y)))
    (+ a 1) (* a 2)))
  5))

(define (1+ x)
  (+ 1 x))

(assert-equal 3 (1+ 2))

