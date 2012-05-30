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

; (let-keywords args ((keyword default) keyword ...) body ...)
; Convenience syntax for binding a list of keyword arguments to variables.
; Note that this macro is very inefficient at the moment.
(define-syntax let-keywords
  (syntax-rules ()
    ((_ args () body ...) (begin body ...))
    ((_ args ((keyword default) rest ...) body ...)
     (let ((keyword (get-keyword
                     (string->keyword
                      (symbol->string 'keyword))
                     args
                     (lambda () default))))
       (let-keywords args (rest ...)
         body ...)))
    ((_ args (keyword rest ...) body ...)
     (let-keywords args ((keyword #f) rest ...)
       body ...))))
