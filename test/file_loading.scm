(define (my-loader)
  (cond ((> 5 2) (load "lib"))))

(my-loader)
(assert-equal 42 secret-rule-you-didnt-know)

(load "vars")
(assert-equal 1/137 alpha)

