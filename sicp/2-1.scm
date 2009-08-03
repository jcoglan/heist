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
(output '(perim-rect rect))
(output '(area-rect rect))


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


; Functions for extended exercise

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))


(exercise "2.7")
; Interval constructors
; upper-bound and lower-bound ought to return the
; greater and lesser bounds, respectively

(define (make-interval a b) (cons a b))

(define (upper-bound int)
  (max (car int) (cdr int)))

(define (lower-bound int)
  (min (car int) (cdr int)))


(exercise "2.8")
; Interval subtraction

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


(exercise "2.9")
; Widths of intervals

(define (width-interval int)
  (/ (- (upper-bound int)
        (lower-bound int))
     2))

; Expand (w (+ x y)):
; w = width, i = make, l = lower, u = upper
; 
; (w (i (+ (l x) (l y)) (+ (u x) (u y))))
; (/ (- (+ (u x) (u y)) (+ (l x) (l y))) 2)
; 
; Which is equal to
; (+ (/ (- (u x) (l x)) 2) (/ (- (u y) (l y)) 2))
; = (+ (w x) (w y))


(exercise "2.10")
; Check for zero-spanning intervals

(define (spans-zero? int)
  (and (negative? (lower-bound int))
       (not (negative? (upper-bound int)))))

(define (div-interval x y)
  (if (spans-zero? y)
      (error "Cannot divide by an interval that spans zero")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))


(exercise "2.11")
; Handle mul-interval as nine special cases

;               0
; 1.            | lx-----ux
;               | ly-----uy
;               |
; 2.            | lx-----ux
;     ly--------|--------uy
;               |
; 3.            | lx-----ux
;     ly-----uy |
;               |
; 4.  lx--------|--------ux
;               | ly-----uy
;               |
; 5.  lx--------|--------ux
;     ly--------|--------uy
;               |
; 6.  lx--------|--------ux
;     ly-----uy |
;               |
; 7.  lx-----ux |
;               | ly-----uy
;               |
; 8.  lx-----ux |
;     ly--------|--------uy
;               |
; 9.  lx-----ux |
;     ly-----uy |
;
(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((and (not (negative? lx)) (not (negative? ly)))
            (make-interval (* lx ly) (* ux uy)))
          ((and (not (negative? lx)) (negative? ly) (not (negative? uy)))
            (make-interval (* ux ly) (* ux uy)))
          ((and (not (negative? lx)) (negative? uy))
            (make-interval (* ux ly) (* lx uy)))
          ((and (negative? lx) (not (negative? ux)) (not (negative? ly)))
            (make-interval (* lx uy) (* ux uy)))
          ((and (negative? lx) (not (negative? ux)) (negative? ly) (not (negative? uy)))
            (let ((p1 (* ux ly))
                  (p2 (* lx uy))
                  (p3 (* lx ly))
                  (p4 (* ux uy)))
              (make-interval (min p1 p2) (max p3 p4))))
          ((and (negative? lx) (not (negative? ux)) (negative? uy))
            (make-interval (* ux ly) (* lx ly)))
          ((and (negative? ux) (not (negative? ly)))
            (make-interval (* lx uy) (* ux ly)))
          ((and (negative? ux) (negative? ly) (not (negative? uy)))
            (make-interval (* lx uy) (* lx ly)))
          ((and (negative? ux) (negative? uy))
            (make-interval (* ux uy) (* lx ly))))))

(output '(mul-interval (cons 1 3) (cons 2 5)))
(output '(mul-interval (cons 1 3) (cons -2 5)))
(output '(mul-interval (cons 1 3) (cons -2 -5)))
(output '(mul-interval (cons -1 3) (cons 2 5)))
(output '(mul-interval (cons -1 3) (cons -2 5)))
(output '(mul-interval (cons -1 3) (cons -2 -5)))
(output '(mul-interval (cons -1 -3) (cons 2 5)))
(output '(mul-interval (cons -1 -3) (cons -2 5)))
(output '(mul-interval (cons -1 -3) (cons -2 -5)))


(exercise "2.12")
; Allow intervals to be entered as "x +/- y%"

; These are from the text:
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; These are my own:
(define (make-center-percent value tolerance)
  (let ((dx (abs (* (/ tolerance 100) value))))
    (make-interval (- value dx) (+ value dx))))
(define (percent i)
  (let ((l (lower-bound i))
        (u (upper-bound i)))
    (let ((center (/ (+ l u) 2))
          (tolerance (/ (- u l) 2)))
      (* 100 (/ tolerance center)))))

(output '(make-center-percent 10.0 10))
(output '(percent (make-interval 0.9 1.1)))


(exercise "2.13")
; Formula for the tolerance of the product of two intervals,
; assuming positive values

; (percent (mul-interval (make-center-percent x a)
;                        (make-center-percent y b)))
;
; (percent (mul-interval (make-interval (- x (* (/ a 100) x)) (+ x (* (/ a 100) x)))
;                        (make-interval (- y (* (/ y 100) b)) (+ y (* (/ b 100) y)))))
;
; (percent (make-interval (* (- x (* (/ a 100) x)) (- y (* (/ b 100) y)))
;                         (* (+ x (* (/ a 100) x)) (+ y (* (/ b 100) y)))))
;
; (* 100 (/ (/ (- (* (+ x (* (/ a 100) x))
;                    (+ y (* (/ b 100) y)))
;                 (* (- x (* (/ a 100) x))
;                    (- y (* (/ b 100) y))))
;              2)
;           (/ (+ (* (- x (* (/ a 100) x))
;                    (- y (* (/ b 100) y)))
;                 (* (+ x (* (/ a 100) x))
;                    (+ y (* (/ b 100) y))))
;              2)))
;
; (* 100 (/ (- (* (+ x (* (/ a 100) x))
;                 (+ y (* (/ b 100) y)))
;              (* (- x (* (/ a 100) x))
;                 (- y (* (/ b 100) y))))
;           (+ (* (- x (* (/ a 100) x))
;                 (- y (* (/ b 100) y)))
;              (* (+ x (* (/ a 100) x))
;                 (+ y (* (/ b 100) y))))))
;
;         (x + ax/100)(y + by/100) - (x - ax/100)(y - by/100)
; = 100 * ---------------------------------------------------
;         (x - ax/100)(y - by/100) + (x + ax/100)(y + by/100)
;
;         (xy + bxy/100 + axy/100 + abxy/10000) - (xy - bxy/100 - axy/100 + abxy/10000)
; = 100 * -----------------------------------------------------------------------------
;         (xy - bxy/100 - axy/100 + abxy/10000) + (xy + bxy/100 + axy/100 + abxy/10000)
;
;         b/50 + a/50
; = 100 * -----------
;         2 + ab/5000
;
; If a,b are small, ab/5000 << 2 -> ignore ab/5000
;
; -> (percent (mul-interval (make-center-percent x a)
;                           (make-center-percent y b)))
;    =~ a + b

(output '(percent (mul-interval (make-center-percent 56.0 0.5) (make-center-percent 34.7 1.2))))
; => 1.6999 =~ 1.7

