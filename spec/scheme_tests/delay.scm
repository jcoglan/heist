(define k 1)
(define p (delay (set! k (+ k 1))))
(assert-equal 1 k)
(force p)
(assert-equal 2 k)
(force p)
(assert-equal 2 k)

