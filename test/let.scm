(define (let-test)
  (define x 4)
  (let ([x 3]
        [y (lambda () x)]   ; 4
        [z 5])
    (+ x (y) z)))

(assert-equal 12 (let-test))


(define (let*-test)
  (define y 50)
  (let* ([x (lambda () y)]    ; 50
         [y 4]
         [z (lambda () y)])   ; 4
    (define y 7)    ; Make sure (z) is correctly scoped
    (+ (x) y (z))))

(assert-equal 61 (let*-test))

(define (letrec-test)
  (define z 13)
  (letrec ([x 3]
           [y (lambda () z)]  ; 5
           [z 5])
    (+ x (y) z)))

(assert-equal 13 (letrec-test))


(assert-equal 1024 (do ([x 1]
                        [i 0 (+ i 1)])
                       [(= i 10) x]
                     (set! x (* x 2))))

