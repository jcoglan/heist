(assert-raise SyntaxError ())
(assert (eqv? '() '()))
(assert (not (eqv? '(5) '(5))))

