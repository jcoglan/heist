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

(assert-equal 'outer
  (let ((x 'outer))
    (let-syntax ((m (syntax-rules () [(m) x])))
      (let ((x 'inner))
        (m)))))

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

