; (vector object ...)
; Returns a newly allocated vector from its arguments
(define (vector . args) (list->vector args))

; (list->vector list)
; Returns a newly allocated vector from a list
(define (list->vector list)
  (let* ((size (length list))
         (new-vector (make-vector size)))
    (do ((i 0 (+ i 1))
         (pair list (cdr pair)))
        ((= i size) new-vector)
      (vector-set! new-vector i (car pair)))))

; (vector->list vector)
; Returns a newly allocated proper list from a vector
(define (vector->list vector)
  (do ((i (vector-length vector) (- i 1))
       (pair '() (cons (vector-ref vector (- i 1)) pair)))
      ((zero? i) pair)))

; (vector-fill! vector fill)
; Sets every element of vector to fill
(define (vector-fill! vector fill)
  (do ((i (vector-length vector) (- i 1)))
      ((zero? i) vector)
    (vector-set! vector (- i 1) fill)))

