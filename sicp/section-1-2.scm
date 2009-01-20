; Section 1.2
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html

(load "helpers")
  

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
(output "(A 1 10)")

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
(output "(A 2 4)")

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
(output "(A 3 3)")


(exercise "1.11")

; f(n) = | n if n < 3
;        | f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3

; Recursive solution
(define (f n)
  (if (< n 3)
      n
      (+
        (f (- n 1))
        (* 2 (f (- n 2)))
        (* 3 (f (- n 3))))))

(output "(f 6)")

; Iterative solution
(define (f n)
  (define (iter count x y z)
    (if (< count 3)
        z
        (iter (- count 1)
              y z
              (+ z (* 2 y) (* 3 x)))))
  (iter n 0 1 2))

(output "(f 6)")


(exercise "1.12")
; Pascal's triangle

(define (pascal line n)
  (if (or (= n 1) (= n line))
      1
      (+ (pascal (- line 1) (- n 1))
         (pascal (- line 1) n))))

(output "(pascal 5 3)")


(exercise "1.14")

(define (count-change amount)
  (cc amount 5))
(define (cc amount coin-type)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= coin-type 0)) 0)
        (else (+ (cc amount
                     (- coin-type 1))
                 (cc (- amount
                        (first-denomination coin-type))
                     coin-type)))))
(define (first-denomination coin-type)
  (cond ((= coin-type 1) 1)
        ((= coin-type 2) 5)
        ((= coin-type 3) 10)
        ((= coin-type 4) 25)
        ((= coin-type 5) 50)))

; (output "(count-change 100)")
(output "(count-change 11)")

; Tree:
;   (count-change 11)
;   (cc 11 5)
;   |-- (cc 11 4)
;   |   |-- (cc 11 3)
;   |   |   |-- (cc 11 2)
;   |   |   |   |-- (cc 11 1)
;   |   |   |   |   |-- (cc 11 0) = 0
;   |   |   |   |   |-- (cc 10 1)
;   |   |   |   |       |-- (cc 10 0) = 0
;   |   |   |   |       |-- (cc 9 1)
;   |   |   |   |           |-- (cc 9 0) = 0
;   |   |   |   |           |-- (cc 8 1)
;   |   |   |   |               |-- (cc 8 0) = 0
;   |   |   |   |               |-- (cc 7 1)
;   |   |   |   |                   |-- (cc 7 0) = 0
;   |   |   |   |                   |-- (cc 6 1)
;   |   |   |   |                       |-- (cc 6 0) = 0
;   |   |   |   |                       |-- (cc 5 1)
;   |   |   |   |                           |-- (cc 5 0) = 0
;   |   |   |   |                           |-- (cc 4 1)
;   |   |   |   |                               |-- (cc 4 0) = 0
;   |   |   |   |                               |-- (cc 3 1)
;   |   |   |   |                                   |-- (cc 3 0) = 0
;   |   |   |   |                                   |-- (cc 2 1)
;   |   |   |   |                                       |-- (cc 2 0) = 0
;   |   |   |   |                                       |-- (cc 1 1)
;   |   |   |   |                                           |-- (cc 1 0) = 0
;   |   |   |   |                                           |-- (cc 0 1) = 1
;   |   |   |   |-- (cc 6 2)
;   |   |   |       |-- (cc 6 1)
;   |   |   |       |   |-- (cc 6 0) = 0
;   |   |   |       |   |-- (cc 5 1)
;   |   |   |       |       |-- (cc 5 0) = 0
;   |   |   |       |       |-- (cc 4 1)
;   |   |   |       |           |-- (cc 4 0) = 0
;   |   |   |       |           |-- (cc 3 1)
;   |   |   |       |               |-- (cc 3 0) = 0
;   |   |   |       |               |-- (cc 2 1)
;   |   |   |       |                   |-- (cc 2 0) = 0
;   |   |   |       |                   |-- (cc 1 1)
;   |   |   |       |                       |-- (cc 1 0) = 0
;   |   |   |       |                       |-- (cc 0 1) = 1
;   |   |   |       |-- (cc 1 2)
;   |   |   |           |-- (cc 1 1)
;   |   |   |           |   |-- (cc 1 0) = 0
;   |   |   |           |   |-- (cc 0 1) = 1
;   |   |   |           |
;   |   |   |           |-- (cc -4 2) = 0
;   |   |   |
;   |   |   |-- (cc 1 3)
;   |   |       |-- (cc 1 2)
;   |   |       |   |-- (cc 1 1)
;   |   |       |   |   |-- (cc 1 0) = 0
;   |   |       |   |   |-- (cc 0 1) = 1
;   |   |       |   |
;   |   |       |   |-- (cc -4 2) = 0
;   |   |       |
;   |   |       |-- (cc -9 3) = 0
;   |   |
;   |   |-- (cc -14 4) = 0
;   |
;   |-- (cc -39 5) = 0


(exercise "1.16")
; Compose an iterative fast exponent function using repeated
; squaring. b^n is invariant each time we divide the problem.
; This is divide and conquer: n multiplications can be divided
; into log(n) squarings.

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(define (fast-expt b n)
  (define (expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (expt-iter (square b) (/ n 2) a))
          (else (expt-iter b (- n 1) (* b a)))))
  (expt-iter b n 1))

(output "(expt 5 13)")
(output "(fast-expt 5 13)")


(exercise "1.17")
; Compose a multiplication function that uses addition, double()
; and halve(). Note that some of these operations are extremely
; cheap when you get down to the hardware level: even?() just
; checks if the last bit is zero, double() bit-shifts to the left
; and halve() bit-shifts to the right.

(define (double x)
  (* 2 x))

(define (halve x)
  (/ x 2))

(define (mult x y)
  (cond ((= y 0) 0)
        ((even? y) (double (mult x (halve y))))
        (else (+ x (mult x (- y 1))))))

(output "(mult 3 7)")
(output "(mult 5 4)")


(exercise "1.18")
; Combine 1.16 and 1.17 to make an iterative multiplication
; function, again using +(), double() and halve()

(define (fast-mult x y)
  (define (mult-iter x y a)
    (cond ((= y 0) a)
          ((even? y) (mult-iter (double x) (halve y) a))
          (else (mult-iter x (- y 1) (+ y x)))))
  (mult-iter x y 1))

(output "(mult 3 7)")
(output "(mult 5 4)")

