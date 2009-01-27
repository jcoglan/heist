; Basic test: no subpatterns or ellipses

(define-syntax while
  (syntax-rules ()
    [(while condition expression)
      (letrec ((loop (lambda ()
                        (if condition
                            (begin
                              expression
                              (loop))
                            #f))))
        (loop))]))

(define i 5)
(while (> i 0)
  (set! i (- i 1)))

(assert-equal 0 i)


; Test ellipses

(define-syntax when
  (syntax-rules ()
    [(when test stmt1 stmt2 ...)
     (if test
         (begin stmt1
                stmt2 ...))]))

(assert-equal 'now
 (let ((if #t))
      (when if (set! if 'now))
      if))

(when true
  (set! i (+ i 1))
  (set! i (+ i 1))
  (set! i (+ i 1))
  (set! i (+ i 1)))

(assert-equal 4 i)


; Test scoping - example from R5RS

(assert-equal 'outer
  (let ((x 'outer))
    (let-syntax ((m (syntax-rules () [(m) x])))
      (let ((x 'inner))
        (m)))))


; Test literal matching

(define-syntax iffy
  (syntax-rules ()
    [(iffy x #t y) x]
    [(iffy x #f y) y]))

(assert-equal 7 (iffy 7 #t 3))
(assert-equal 3 (iffy 7 #f 3))


; Test execution scope using (swap)
; Example from PLT docs

(define-syntax swap (syntax-rules ()
  [(swap x y)
    (let ([temp x])
      (set! x y)
      (set! y temp))]))

(define a 4)
(define b 7)
(swap a b)

(assert-equal 7 a)
(assert-equal 4 b)


; Test macro bound variable renaming

(let ([temp 5]
      [other 6])
  (swap temp other)
  (assert-equal 6 temp)
  (assert-equal 5 other))

(let ([set! 5]
      [other 6])
  (swap set! other)
  (assert-equal 6 set!)
  (assert-equal 5 other))


; More ellipsis tests from PLT docs

(define-syntax rotate
  (syntax-rules ()
    [(rotate a) (void)]
    [(rotate a b c ...) (begin
                          (swap a b)
                          (rotate b c ...))]))

(define a 1)  (define b 2)
(define c 3)  (define d 4)
(define e 5)
(rotate a b c d e)

(assert-equal 2 a)  (assert-equal 3 b)
(assert-equal 4 c)  (assert-equal 5 d)
(assert-equal 1 e)


; Test input execution - example from R5RS

(define-syntax my-or
  (syntax-rules ()
    ((my-or) #f)
    ((my-or e) e)
    ((my-or e1 e2 ...)
     (let ((temp e1))
       (if temp
           temp
           (my-or e2 ...))))))

(my-or (> 0 (set! e (+ e 1)))   ; false
       (> 0 (set! e (+ e 1)))   ; false
       (> 9 6)                  ; true - should not evaluate further
       (> 0 (set! e (+ e 1)))
       (> 0 (set! e (+ e 1))))

(assert-equal 3 e)


; Test subpatterns

(define-syntax p-swap
  (syntax-rules ()
    [(swap (x y))
      (let ([temp x])
        (set! x y)
        (set! y temp))]))

(define m 3)
(define n 8)
(p-swap (m n))
(assert-equal 8 m)
(assert-equal 3 n)


(define-syntax parallel-set!
  (syntax-rules ()
    [(_ (symbol ...) (value ...))
      (begin
        (set! symbol value)
        ...)]))

(parallel-set! (a b c) (74 56 19))
(assert-equal 74 a)
(assert-equal 56 b)
(assert-equal 19 c)


; Test that ellipses are correctly matched
; to numbers of splices in subexpressions

(define-syntax p-let*
  (syntax-rules ()
    [(_ (name ...) (value ...) stmt ...)
      (let* ([name value] ...)
        stmt
        ...)]))

(define indicator #f)

(p-let* (k l m) (3 4 5)
  (assert-equal 5 m)
  (define temp m)
  (set! m (+ l k))
  (set! k (- l temp))
  (set! l (* 6 (+ k m)))
  (rotate k l m)
  (assert-equal 7 l)
  (assert-equal -1 m)
  (assert-equal 36 k)
  (set! indicator #t))

(assert indicator)


; Test keywords

(define-syntax assign
  (syntax-rules (values to)
    [(assign values (value ...) to (name ...))
      (begin
        (define name value)
        ...)]))

(assign values (9 7 6) to (foo bar baz))
(assert-equal 9 foo)
(assert-equal 7 bar)
(assert-equal 6 baz)

(assign stuff (3 2) to (foo bar))
(assert-equal 9 foo)
(assert-equal 7 bar)

