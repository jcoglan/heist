; Section 1.1.7
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1.7

(load "../helpers")


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))


(exercise "1.7")

(define (good-enough? guess last)
  (< (abs (/ (- guess last) last)) 0.0001))

(define (sqrt-iter guess last x)
  (if (good-enough? guess last)
      guess
      (sqrt-iter (improve guess x) guess x)))
(define (sqrt x)
  (sqrt-iter 0.1 1.0 x))

(output '(sqrt 40000))


(exercise "1.8")

(define (improve-cube guess x)
  (/  (+ (/ x (square guess)) (* 2 guess))
      3))

(define (cube-root-iter guess last x)
  (if (good-enough? guess last)
      guess
      (cube-root-iter (improve-cube guess x) guess x)))
      
(define (cube-root x)
  (cube-root-iter 0.1 1.0 x))

(output '(cube-root 27))
(output '(cube-root 64))

