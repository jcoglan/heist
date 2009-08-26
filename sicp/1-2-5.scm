; Section 1.2.5
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.5

(load "helpers")


(exercise "1.20")
; Euclid's algorithm

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Normal order:
; (gcd 206 40)
; 
; (gcd 40 (remainder 206 40))
;     eval b -> 6, 1 call
; 
; (gcd (remainder 206 40)
;      (remainder 40 (remainder 206 40)))
;     eval b -> 4, 2 calls
; 
; (gcd (remainder 40 (remainder 206 40))
;      (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;     eval b -> 2, 4 calls
; 
; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;      (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;     eval b -> 0, 7 calls
;     eval a -> 2, 4 calls
; 
; Total (remainder) calls for normal order: 1+2+4+7+4 = 18
; 
; Applicative order:
; (gcd 206 40)
; (gcd 40 (remainder 206 40))
; (gcd 40 6)
; (gcd 6 (remainder 40 6))
; (gcd 6 4)
; (gcd 4 (remainder 6 4))
; (gcd 4 2)
; (gcd 2 (remainder 4 2))
; (gcd 2 0)
; 2
; 
; Total (remainder) calls for applicative order: 4

