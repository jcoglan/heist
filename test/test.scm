; Really basic test using closures,
; define, lambda, and display

(define (print-doubled x)
  (display
    ( (lambda (y)
        (+ x y))
      x )))

(define (add n)
  (lambda (x)
    (+ n x)))

(print-doubled 5)

(display ((add 4) 7))

(define (f x) x)

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (double x)
  (* x 2))

(define (apply-self f)
  (lambda (x)
    (f (f x))))

(display ((compose (add 5) double) 7))
(display ((apply-self double) 7))

