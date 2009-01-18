(assert (boolean? #t))
(assert (not (boolean? "Hello, World!")))
(assert (not #f))
(assert (not (not #t)))
(assert (not (not "Hello, World!")))

(define x 7)
(assert-equal #t (and (> x 5) (<= x 10)))
(assert-equal #t (or (>= x 5) (< x 3)))

(define (my-and x y)
  (and x y))

(assert-equal #f (my-and #t (> x 12)))

