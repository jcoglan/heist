(define (pythag)
  (define x 4)
  (let ([x 3]
        [y x]
        [z 5])
    (define sum-of-sides (+ (* x x) (* y y)))
    (define hypot (* z z))
    (- hypot sum-of-sides)))

(assert-equal 0 (pythag))

(define (pythag)
  (define x 4)
  (define y 5)
  (let* ([z y]
         [x 3]
         [y x])
    (define sum-of-sides (+ (* x x) (* y y)))
    (define hypot (* z z))
    (- hypot sum-of-sides)))

(assert-equal 7 (pythag))

