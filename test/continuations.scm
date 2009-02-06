(define y 2)
(define r)

(display "Should produce 4, 6, 4, 9")

(display (+ 1 y (call/cc
          (lambda (k)
            (set! r k)
            (k 1)))))

(set! y 5)
(r 3)

(set! y 2)
(display (+ 1 (call/cc
          (lambda (k)
            (set! r k)
            (k 1))) y))

(set! y 5)
(r 3)

