(assert (equal? (symbol->string 'foo) "foo"))
(assert (eqv? (string->symbol "foo") 'foo))

(assert (char? #\a))
(assert (char? #\ ))
(assert (char? #\)))
(assert (char? #\tab))

