; if -- only works using normal order (lazy) evaluation
(define (if condition consequence alternative)
  (cond (condition consequence)
        (else alternative)))

