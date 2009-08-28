; Section 2.1.3
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-14.html#%_sec_2.1.3

(load "../helpers")


(exercise "2.4")
; Procedural implementation of pairs

(define (p-cons x y)
  (lambda (m) (m x y)))

(define (p-car z)
  (z (lambda (p q) p)))

(define (p-cdr z)
  (z (lambda (p q) q)))


(exercise "2.5")
; Arithmetic implementation of pairs

(define (z-cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (z-car pair)
  (if (odd? pair)
      0
      (+ (z-car (/ pair 2)) 1)))

(define (z-cdr pair)
  (/ (log (/ pair
             (expt 2 (z-car pair))))
     (log 3)))

(define z-pair (z-cons 37 24))
(output '(z-car z-pair))
(output '(z-cdr z-pair))


(exercise "2.6")
; Church numerals

(define zero (lambda (f)
  (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

; Derive one and two by substitution

(define one (add-1 zero))
; (add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
; (lambda (f) (lambda (x) (f x)))
(define one (lambda (f)
  (lambda (x) (f x))))

(define two (add-1 one))
; (add-1 one)
; (lambda (f) (lambda (x) (f ((one f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
; (lambda (f) (lambda (x) (f (f x))))
(define two (lambda (f)
  (lambda (x) (f (f x)))))

; Addition
(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

; e.g. (add one two)
; (lambda (f) (lambda (x) ((one f) ((two f) x))))
; (lambda (f) (lambda (x) ((one f) (f (f x)))))
; (lambda (f) (lambda (x) (f (f (f x)))))
; = three
(define three (add one two))
(output '((three inc) 0))
(define three (add one (add (add one zero) one)))
(output '((three inc) 0))

