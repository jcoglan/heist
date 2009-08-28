; Section 1.2.3
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.3

(load "../helpers")


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

; (output '(count-change 100))
(output '(count-change 11))

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

