; Section 2.1.2
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-14.html#%_sec_2.1.2

(load "../helpers")


(exercise "2.2")
; Lines and points

; Initially at least, the constructors just use pairs directly, so just write
; them as aliases
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

