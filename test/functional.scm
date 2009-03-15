(assert-equal 10 (apply + '(1 2 3 4)))
(assert-equal '(1 4 9 16) (map (lambda (x) (* x x)) '(1 2 3 4)))
(assert-equal '(5 7 9) (map + '(1 2 3) '(4 5 6)))

