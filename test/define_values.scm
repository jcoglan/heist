(define size 2)
(assert-equal 2 size)

(define pi 3.14159)
(define radius 10)
(assert-equal 314.159 (* pi (* radius radius)))

(assert-equal PI pi)
(assert-equal Pi pI)

(define circumference (* 2 pi radius))
(assert-equal 62.8318 circumference)

(let* ([x 6]
       [y 7]
       [pi 12])
  (set! size 4)
  (set! pi 3))

(assert-equal 4 size)
(assert-equal 3.14159 pi)

(assert-raise UndefinedVariable (set! undef-var 10))
(assert-raise UndefinedVariable (no-fun 13))

(define 3k 9)
(assert-equal 9 3k)

