(define-syntax when (syntax-rules ()
  [(when test stmt1 stmt2 ...)
   (if test
       (begin stmt1
              stmt2 ...))]))

(define-syntax swap (syntax-rules ()
  [(swap x y)
    (let ([temp x])
      (set! x y)
      (set! y temp))]))

(define-syntax square-sum (syntax-rules ()
  [(_ x y)
    (let* ([first x]
           [second y]
           [sum (+ first second)])
      (* sum sum))]))

