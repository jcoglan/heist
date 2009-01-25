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

