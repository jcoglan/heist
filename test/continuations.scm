(define r)
(define value)

; call/cc returning normally
(set! value (+ 3 (call/cc (lambda (k) (+ 2 7)))))
(assert-equal 12 value)

; calling the continuation abandons the current expression
(set! value (+ 3 (call/cc
                    (lambda (k)
                      (set! r k)
                      (+ 2 (k 7))))))
(assert-equal 10 value)

; calling a stored continuation aborts the current stack
(define called #f)
(begin
  (r 5)
  (set! called #t))
(assert (not called))

; calling call/cc does not unwind the stack
(begin
  (call/cc (lambda (k) 3))
  (set! called #t))
(assert called)

; expressions before the call/cc have their values fixed
(define y 2)
(set! value (+ 1 y (call/cc
                      (lambda (k)
                        (set! r k)
                        (k 1)))))
(assert-equal 4 value)
(set! y 5)
(r 3)
(assert-equal 6 value)

; expressions after the call/cc are re-evaluated
(set! y 2)
(set! value (+ 1 (call/cc
                    (lambda (k)
                      (set! r k)
                      (k 1)))
                 y))
(assert-equal 4 value)
(set! y 5)
(r 3)
(assert-equal 9 value)

; more checks for re-evaluation
(define count-calls 0)
(begin
  (set! count-calls (+ count-calls 1))
  (call/cc
    (lambda (k)
      (set! r k)
      4)))
(r #t) (r #t) (r #t)
(assert-equal 1 count-calls)
(begin
  (call/cc
    (lambda (k)
      (set! r k)
      4))
  (set! count-calls (+ count-calls 1)))
(r #t) (r #t) (r #t)
(assert-equal 5 count-calls)

