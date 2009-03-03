; Section 2.1
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-14.html

(load "helpers")

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

; A better make-rat
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


(exercise "2.1")
; (make-rat) for negative numbers
(define (make-rat n d)
  (let ([g (gcd n d)]
        [s (if (positive? (* n d)) 1 -1)])
    (cons (* s (abs (/ n g))) (abs (/ d g)))))

(print-rat (make-rat 1 2))
(print-rat (make-rat 1 -2))
(print-rat (make-rat -1 2))
(print-rat (make-rat -1 -2))


(exercise "2.2")
; Lines and points

; Initially at least, the constructors just use pairs
; directly, so just write them as aliases
(define make-point cons)
(define x-point car)
(define y-point cdr)
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (midpoint-segment line)
  (make-point (/ (+ (x-point (start-segment line))
                    (x-point (end-segment line)))
                 2)
              (/ (+ (y-point (start-segment line))
                    (y-point (end-segment line)))
                 2)))

(print-point (midpoint-segment (cons (cons 2 3) (cons 4 5))))


(exercise "2.3")
; Rectangles
; All we need is a line and a height

(define (length-segment line)
  (sqrt (+ (expt (- (x-point (end-segment line))
                    (x-point (start-segment line)))
                 2)
           (expt (- (y-point (end-segment line))
                    (y-point (start-segment line)))
                 2))))

(define (make-rect line h)
  (cons line h))

(define (width-rect rect)
  (length-segment (car rect)))

(define (height-rect rect)
  (cdr rect))

(define (perim-rect rect)
  (* 2 (+ (width-rect rect)
          (height-rect rect))))

(define (area-rect rect)
  (* (width-rect rect)
     (height-rect rect)))

(define rect (make-rect (make-segment (make-point 3 1)
                                      (make-point 7 4))
                        9))
(output "(perim-rect rect)")
(output "(area-rect rect)")


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
(output "(z-car z-pair)")
(output "(z-cdr z-pair)")


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
(output "((three inc) 0)")
(define three (add one (add (add one zero) one)))
(output "((three inc) 0)")

