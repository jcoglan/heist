(define size 2)
(assert-equal 2 size)

(define pi 3.14159)
(define radius 10)
(assert-equal 314.159 (* pi (* radius radius)))

(define circumference (* 2 pi radius))
(assert-equal 62.8318 circumference)

