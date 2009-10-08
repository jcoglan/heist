(define (my-loader)
  (cond ((> 5 2) (load "../helpers/lib"))))

(my-loader)
(assert-equal 42 secret-rule-you-didnt-know)

(load "../helpers/vars")
(assert-equal 1/137 alpha)

