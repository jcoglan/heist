; Section 1.2.4
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.4

(load "../helpers")


(exercise "1.16")
; Compose an iterative fast exponent function using repeated
; squaring. b^n is invariant each time we divide the problem.
; This is divide and conquer: n multiplications can be divided
; into log(n) squarings.

(define (fast-expt b n)
  (define (expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (expt-iter (square b) (/ n 2) a))
          (else (expt-iter b (- n 1) (* b a)))))
  (expt-iter b n 1))

(output '(expt 5 13))
(output '(fast-expt 5 13))


(exercise "1.17")
; Compose a multiplication function that uses addition, double()
; and halve(). Note that some of these operations are extremely
; cheap when you get down to the hardware level: even?() just
; checks if the last bit is zero, double() bit-shifts to the left
; and halve() bit-shifts to the right.

(define (mult x y)
  (cond ((= y 0) 0)
        ((even? y) (double (mult x (halve y))))
        (else (+ x (mult x (- y 1))))))

(output '(mult 3 7))
(output '(mult 5 4))


(exercise "1.18")
; Combine 1.16 and 1.17 to make an iterative multiplication
; function, again using +(), double() and halve()

(define (fast-mult x y)
  (define (mult-iter x y a)
    (cond ((= y 0) a)
          ((even? y) (mult-iter (double x) (halve y) a))
          (else (mult-iter x (- y 1) (+ y x)))))
  (mult-iter x y 1))

(output '(mult 3 7))
(output '(mult 5 4))


(exercise "1.19")
; Let T be the transformation:      a <- a + b,         b <- a
; Let T(pq) be the transformation:  a <- bq + aq + ap,  b <- bp + aq
; => T = T(01)
; Apply T(pq) twice to a,b:
; 
;                                        a                               b
; 
; ->                          bq + aq + ap                         bp + aq
; 
; ->    (bp + aq)q + (bq + aq + ap)(q + p)    (bp + aq)p + (bq + aq + ap)q
; 
; First expression:
;       bpq + aq^2 + bq^2 + bpq + aq^2 + apq + apq + ap^2
;     = b(2pq + q^2) + a(p^2 + 2pq + 2q^2)
; 
; Second expression:
;       bp^2 + apq + bq^2 + aq^2 + apq
;     = b(p^2 + q^2) + a(2pq + q^2)
; 
; => q' = 2pq + q^2
;    p' = p^2 + q^2

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))        ; compute p'
                   (+ (* 2 p q) (* q q))      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(output '(fib 1))
(output '(fib 2))
(output '(fib 3))
(output '(fib 4))
(output '(fib 5))
(output '(fib 6))
(output '(fib 7))
(output '(fib 8))

