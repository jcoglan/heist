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

(define (do-factorial x)
  (do ([y x (- y 1)]
       [acc 1 (* y acc)])
      ((zero? y) acc)))

(assert-equal 720 (do-factorial 6))


; R5RS compliance test -- http://okmij.org/ftp/Scheme/lambda-derived.txt
(assert-equal '(1 1 1 1 1 1 1)
              (let ((foo (lambda (x) x)))
                (list
                  (foo ((lambda () (define a 1) a)))
                  (foo (let ()  (define a 1) a))
                  (foo (let* ()  (define a 1) a))
                  (foo (letrec ()  (define a 1) a))
                  (foo (letrec ()  (define (b) (define a 1) a) (b)))
                  (foo (let-syntax () (define a 1) a))
                  (foo (letrec-syntax () (define a 1) a)))))

