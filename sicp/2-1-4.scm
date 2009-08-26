; Section 2.1.4
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-14.html#%_sec_2.1.4

(load "helpers")


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

