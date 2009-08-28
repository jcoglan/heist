; Section 1.2.1
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.1

(load "../helpers")


(exercise "1.9")
; This code is slightly different from SICP so as
; not to overwrite functions we need later

(define (add a b)
  (if (= a 0)
      b
      (inc (add (dec a) b))))
; (add 4 5)
; (inc (add 3 5))
; (inc (inc (add 2 5)))
; (inc (inc (inc (add 1 5))))
; (inc (inc (inc (inc (add 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

(define (add a b)
  (if (= a 0)
      b
      (add (dec a) (inc b))))
; (add 4 5)
; (add 3 6)
; (add 2 7)
; (add 1 8)
; (add 0 9)
; 9


(exercise "1.10")

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; (A 1 10)
; (A 0 (A 1 9))
; (A 0 (A 0 (A 1 8)))
; (A 0 (A 0 (A 0 (A 1 7))))
; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
; (A 0 (A 0 (A 0 (A 0 64))))
; (A 0 (A 0 (A 0 128)))
; (A 0 (A 0 256))
; (A 0 512)
; 1024
(output '(A 1 10))

; (A 2 4)
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2)))
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2)))
; (A 1 (A 1 (A 0 (A 1 1))))
; (A 1 (A 1 (A 0 2)))
; (A 1 (A 1 4))
; (A 1 (A 0 (A 1 3)))
; (A 1 (A 0 (A 0 (A 1 2))))
; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
; (A 1 (A 0 (A 0 (A 0 2))))
; (A 1 16)
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))))))))
; 65536
(output '(A 2 4))

; (A 3 3)
; (A 2 (A 3 2))
; (A 2 (A 2 (A 3 1)))
; (A 2 (A 2 2))
; (A 2 (A 1 (A 2 1)))
; (A 2 (A 1 2))
; (A 2 (A 0 (A 1 1)))
; (A 2 (A 0 2))
; (A 2 4)
; 65536
(output '(A 3 3))

