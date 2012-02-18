; (get-keyword keyword args [default])
; Returns the subsequent element to +keyword+ in +args+ if
; present. Otherwise returns the return value of applying the thunk
; +default+ or, if +default+ is not given, returns +#f+.
(define (get-keyword keyword args . thunk)
  (let ((default (if (pair? thunk) (car thunk) (lambda () #f))))
    (let loop ((args args))
      (cond ((null? args) (default))
            ((eq? keyword (car args))
             (cadr args))
            (else
             (loop (cddr args)))))))
