(define-syntax let (syntax-rules ()
  [(let ([name expression] ...) body ...)
    ((lambda (name ...)
        body ...)
     expression ...)]))

(define-syntax let* (syntax-rules ()
  [(let* ([name expression]) body ...)
    (let ([name expression]) body ...)]
  [(let* ([n1 e1] [n2 e2] ...) body ...)
    (let ([n1 e1])
      (let* ([n2 e2] ...) body ...))]))

(define-syntax letrec (syntax-rules ()
  [(letrec ([name expression] ...) body ...)
    ((lambda ()
      (define name expression) ...
      body ...))]))

(define let-syntax let)
(define letrec-syntax letrec)

