; Section 2.2.1
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-15.html#%_sec_2.2.1

(load "helpers")
(define nil (list))
(define orig-map map)


(exercise "2.17")
; Return the last pair in a list

(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))

(output '(last-pair (list 23 72 149 34)))


(exercise "2.18")
; Reverse a list

(define (reverse seq)
  (if (null? seq)
      nil
      (append (reverse (cdr seq))
              (list (car seq)))))

(output '(reverse (list 1 4 9 16 25)))


(exercise "2.19")
; Change counter parameterized by lists of coins

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (first-denomination coins)
  (car coins))

(define (except-first-denomination coins)
  (cdr coins))

(define (no-more? coins)
  (null? coins))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(output '(cc 11 (reverse us-coins)))


(exercise "2.20")
; Find arguments with the same parity as the first

(define (same-parity x . rest)
  (let ((parity (remainder x 2)))
    (define (iter input)
      (if (null? input)
          nil
          (let ((y (car input))
                (rest (iter (cdr input))))
            (if (= (remainder y 2) parity)
                (cons y rest)
                rest))))
    (cons x (iter rest))))

(output '(same-parity 1 2 3 4 5 6 7))
(output '(same-parity 2 3 4 5 6 7))


(exercise "2.21")
; Completing definitions of square-list

(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items))
            (square-list (cdr items)))))

(output '(square-list (list 1 2 3 4)))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

(output '(square-list (list 1 2 3 4)))


(exercise "2.23")
; Implement for-each

(define (for-each proc list)
  (if (null? list)
      #t
      (begin
        (proc (car list))
        (for-each proc (cdr list)))))

(for-each (lambda (x) (display x) (newline))
          (list 57 321 88))

