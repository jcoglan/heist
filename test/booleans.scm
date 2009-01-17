(assert (boolean? #t))
(assert (not (boolean? "Hello, World!")))
(assert (not #f))
(assert (not (not #t)))
(assert (not (not "Hello, World!")))

