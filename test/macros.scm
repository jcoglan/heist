(define-syntax while (syntax-rules ()
  ((while condition expression)
    (letrec ((loop (lambda ()
                      (if condition
                          (begin
                            (display expression)
                            (loop))
                          #f))))
      (loop)))))

(define i 5)
(while (> i 0)
  (set! i (- i 1)))

(assert-equal 0 i)

(assert-equal 'now
  (let-syntax ((when (syntax-rules ()
                       ((when test stmt1 stmt2 ...)
                        (if test
                            (begin stmt1
                                   stmt2 ...))))))
    (let ((if #t))
      (when if (set! if 'now))
      if)))

(assert-equal 'outer
  (let ((x 'outer))
    (let-syntax ((m (syntax-rules () ((m) x))))
      (let ((x 'inner))
        (m)))))

