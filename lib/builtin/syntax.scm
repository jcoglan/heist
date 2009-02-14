; Control structures

; (cond) acts like the 'switch' statement in C-style languages.
; Once a matching precondition is found, its consequent is
; tail-called and no further preconditions are evaluated.
(define-syntax cond (syntax-rules (else =>)
  [(cond) #f]
  [(cond (else expr1 expr2 ...))
    (begin expr1 expr2 ...)]
  [(cond (test => function) clause ...)
    (let ([temp test])
      (if temp
          (function temp)
          (cond clause ...)))]
  [(cond (test expression ...) clause ...)
    (if test
        (begin expression ...)
        (cond clause ...))]))

;----------------------------------------------------------------

; Binding constructs

; (let), (let*) and (letrec) each create a new scope and bind
; values to some symbols before executing a series of lists.
; They differ according to how they evaluate the bound values.

; (let) evaluates values in the enclosing scope, so lambdas will
; not be able to refer to other values assigned using the (let).
(define-syntax let (syntax-rules ()
  [(let ([variable init] ...) body ...)
    ((lambda (variable ...)
        body ...)
     init ...)]
  [(let name ([variable init] ...) body ...)
    (letrec ([name (lambda (variable ...)
                  body ...)])
      (name init ...))]))

; (let*) creates a new scope for each variable and evaluates
; each expression in its enclosing scope. Basically a shorthand
; for several nested (let)s. Variables may refer to those that
; preceed them but not vice versa.
(define-syntax let* (syntax-rules ()
  [(let* ([name expression]) body ...)
    (let ([name expression]) body ...)]
  [(let* ([n1 e1] [n2 e2] ...) body ...)
    (let ([n1 e1])
      (let* ([n2 e2] ...) body ...))]))

; (letrec) evaluates values in the inner scope, so lambdas are
; able to refer to other values assigned using the (letrec).
(define-syntax letrec (syntax-rules ()
  [(letrec ([variable init] ...) body ...)
    ((lambda ()
      (define variable init) ...
      body ...))]))

(define let-syntax let)
(define letrec-syntax letrec)

