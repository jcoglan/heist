(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(assert-equal 4 (abs 4))
(assert-equal 0 (abs 0))
(assert-equal 13 (abs -13))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(assert-equal 4 (abs 4))
(assert-equal 0 (abs 0))
(assert-equal 13 (abs -13))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(assert-equal 4 (abs 4))
(assert-equal 0 (abs 0))
(assert-equal 13 (abs -13))

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
    
(assert-equal 7 (a-plus-abs-b 3 4))
(assert-equal 11 (a-plus-abs-b 3 (- 8)))

(define (fact x)
  (if (= x 0)
    1
    (* x
      (fact (- x 1)))))

(assert-equal 720 (fact 6))

(define x 7)
(assert-equal #t (and (> x 5) (<= x 10)))
(assert-equal #t (or (>= x 5) (< x 3)))

