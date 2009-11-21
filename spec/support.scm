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

(define spec (let ()
  (define test-failures '())

  (define (run-test proc-name description proc args expected)
    (let ((actual (apply proc args)))
      (if (equal? expected actual)
          (display ".")
          (begin
            (display "F")
            (set! test-failures
                  (cons `(,proc-name "with" ,description
                            ": expected" ,expected "but got" ,actual)
                        test-failures))))))

  (define (test-summary)
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

  (define (print . args)
    (if (null? args)
        (newline)
        (begin
          (display (car args))
          (display " ")
          (apply print (cdr args)))))
  
  (lambda (symbol . args)
    (define proc (case symbol
                   ((run)     run-test)
                   ((summary) test-summary)))
    (apply proc args))))

