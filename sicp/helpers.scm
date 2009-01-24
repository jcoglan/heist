(define (exercise x)
  (newline)
  (display (+ "Exercise " x)))

(define (output expression)
  (display (+ expression " = " (eval expression))))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))


(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
  
(define (prime? n)
  (= n (smallest-divisor n)))

