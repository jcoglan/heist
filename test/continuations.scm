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

; multiple call/cc in the same expression
; http://sanjaypande.blogspot.com/2004/06/understanding-scheme-continuations.html

(define r1 #f)
(define r2 #f)

(define (somefunc x y)
  (+ (* 2 (expt x 2)) (* 3 y) 1))

(set! value
  (somefunc (call/cc
               (lambda (c1)
                 (set! r1 c1)
                 (c1 1)))
            (call/cc
               (lambda (c2)
                 (set! r2 c2)
                 (c2 1)))))
(assert-equal 6 value)
(r1 5)
(assert-equal 54 value)
(r2 5)
(assert-equal 66 value)

; test that primitive values can be thrown into (if), (and)
(set! value (if (call/cc
                  (lambda (k)
                    (set! r k)
                    #t))
                "bees"
                "honey"))
(r #t)
(assert-equal "bees" value)
(r #f)
(assert-equal "honey" value)

; test saving the continuation inside a function
(define before 0)
(define after 0)
(define (foo)
  (set! before (+ before 1))
  (set! y (call/cc
            (lambda (k)
              (set! r k)
              2)))
  (set! after (+ after 1))
  (+ 3 y))
(set! value (* 5 (foo)))
(assert-equal 25 value)
(r 7)
(assert-equal 50 value)
(assert-equal 1 before)
(assert-equal 2 after)

