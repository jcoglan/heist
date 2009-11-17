; Check that user-level redefinitions of core functions does
; not break other core functions implemented in Scheme

(define (check-map-integrity)
  (assert-equal '(1 4 9 16)
                (map (lambda (x) (* x x))
                     '(1 2 3 4))))
(define cons #f)
(check-map-integrity)

(set! null? #f)
(check-map-integrity)

