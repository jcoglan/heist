; Section 1.2.6
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.6

(load "../helpers")


(exercise "1.21")

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(output '(smallest-divisor 199))
(output '(smallest-divisor 1999))
(output '(smallest-divisor 19999))


(exercise "1.22")
; Benchmarking primality tests

(define (timed-prime-test n)
  ; (newline)
  ; (display n) (newline)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))))
(define (report-prime n elapsed-time)
  (display (+ " *** " n)) (newline)
  (display elapsed-time) (newline)
  #t)

; Search for first 3 primes above x
(define (search-for-primes x)
  (define (iter x found)
    (cond ((< found 3)
              (if (timed-prime-test x)
                  (iter (+ x 2) (+ found 1))
                  (iter (+ x 2) found)))))
  (iter (if (even? x) (+ x 1) x) 0))

; (sqrt 10) = 3.16            ; Test time
(search-for-primes 1000)      ; 12ms
(search-for-primes 10000)     ; 50ms
(search-for-primes 100000)    ; 160ms
(search-for-primes 1000000)   ; 530ms


(exercise "1.23")
; Skip even numbers when looking for divisors

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (next x)
  (if (= x 2) 3
              (+ x 2)))

(define (prime-benchmarks)
  (timed-prime-test 1009)       ; 8ms
  (timed-prime-test 1013)
  (timed-prime-test 1019)
  (timed-prime-test 10007)      ; 30ms
  (timed-prime-test 10009)
  (timed-prime-test 10037)
  (timed-prime-test 100003)     ; 107ms
  (timed-prime-test 100019)
  (timed-prime-test 100043)
  (timed-prime-test 1000003)    ; 333ms
  (timed-prime-test 1000033)
  (timed-prime-test 1000037))

(prime-benchmarks)


(exercise "1.24")
; Use Fermat test for prime numbers

(define (start-prime-test n start-time)
  (if (fast-prime? n 3)
      (report-prime n (- (runtime) start-time))))

(prime-benchmarks)
; Times are (using fast-prime? 3 times)
; 25ms, 30ms, 35ms, 40ms (roughly)

