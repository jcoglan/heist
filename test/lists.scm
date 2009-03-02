(assert (eqv? '() '()))
(assert (not (eqv? '(5) '(5))))
(assert-raise SyntaxError ())

