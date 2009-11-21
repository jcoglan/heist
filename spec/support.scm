(define-syntax describe (syntax-rules (with =>)
  ((describe (single) example ...)
   (describe single example ...))
  
  ((describe (first rest ...) example ...)
   (begin
     (describe first example ...)
     (describe (rest ...) example ...)))
  
  ((describe proc-name
     (with description (argument ...) => result)
     further-clauses ...)
   (begin
     (spec 'run 'proc-name description
                proc-name `(,argument ...)
                result)
     (describe proc-name further-clauses ...)))
  
  ((describe proc-name
     (binding-construct ((name value) ...)
       example ...)
     further-clauses ...)
   (binding-construct ((name value) ...)
     (describe proc-name
       example ...
       further-clauses ...)))
  
  ((describe proc-name)
   '())))

(define-syntax define-object (syntax-rules (var public private)
  ((_ "match?" symbol public (name . args) body ...)
   (eq? symbol 'name))
  
  ((_ "match?" expression ...) #f)
  
  ((_ "lookup" (name . args) body ...) name)
  
  ((_ "lookup" name expression ...) name)
  
  ((define-object name
     (modifier expression ...)
     ...)
   (define name (let ()
     (define expression ...)
     ...
     (lambda (symbol . args)
       (define proc (cond ((define-object "match?" symbol modifier expression ...)
                           (define-object "lookup" expression ...))
                          ...))
       (if (procedure? proc)
           (apply proc args)
           (error "No such method" symbol))))))))

(define-object spec
  (var test-failures '())
  
  (public (run proc-name description proc args expected)
    (let ((actual (apply proc args)))
      (if (equal? expected actual)
          (display ".")
          (begin
            (display "F")
            (set! test-failures
                  (cons `(,proc-name "with" ,description
                            ": expected" ,expected "but got" ,actual)
                        test-failures))))))
  
  (public (summary)
    (define (report failures i)
      (if (not (null? failures))
          (begin
            (report (cdr failures) (- i 1))
            (print "----------------------------------------------------------------")
            (apply print `(,i "|" ,@(car failures)))
            (newline))
          '()))
    (newline)
    (report test-failures (length test-failures)))
  
  (private (print . args)
    (if (null? args)
        (newline)
        (begin
          (display (car args))
          (display " ")
          (apply print (cdr args))))))

