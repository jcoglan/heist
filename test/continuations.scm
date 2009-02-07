(define y 2)
(define r)
(define value)

(set! value (+ 1 y (call/cc
                      (lambda (k)
                        (set! r k)
                        (k 1)))))
(assert-equal 4 value)

(set! y 5)
(r 3)
(assert-equal 6 value)

(set! y 2)
(set! value (+ 1 (call/cc
                    (lambda (k)
                      (set! r k)
                      (k 1))) y))
(assert-equal 4 value)

(set! y 5)
(r 3)
(assert-equal 9 value)

