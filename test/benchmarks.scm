(define-syntax swap
  (syntax-rules ()
    [(swap x y)
      (let ([temp x])
        (set! x y)
        (set! y temp))]))

(define-syntax rotate
  (syntax-rules ()
    [(rotate x y)
      (swap x y)]
    [(rotate x y z ...)
      (begin
        (swap x y)
        (rotate y z ...))]))

(define-syntax p-let
  (syntax-rules ()
    [(p-let (name ...) (value ...) body ...)
      (let ([name value] ...)
        body ...)]))

(p-let (a b c d e) (1 2 3 4 5)
  (benchmark "(rotate speed)" 50 times
    (rotate a b c d e)))
      
