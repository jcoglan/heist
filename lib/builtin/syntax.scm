; Control structures

; (cond) goes through a list of tests, evaluating each one
; in order of appearance. Once a matching precondition is
; found, its consequent is tail-called and no further
; preconditions are evaluated.
(define-syntax cond (syntax-rules (else =>)
  [(cond) #f]
  [(cond (else expr1 expr2 ...))
    (begin expr1 expr2 ...)]
  [(cond (test => function) clause ...)
    (let ([temp test])
      (if temp
          (function temp)
          (cond clause ...)))]
  [(cond (test expression ...) clause ...)
    (if test
        (begin expression ...)
        (cond clause ...))]))

; (case) acts like Ruby's case statement. The value of the
; given expression is compared against a series of lists;
; once a list is found to include the value, the expressions
; following the list are evaluated and no further lists
; are tested.
(define-syntax case (syntax-rules (else)
  [(case key) #f]
  [(case key (else expr1 expr2 ...))
    (begin expr1 expr2 ...)]
  [(case key
         ((cell ...) expr1 expr2 ...)
         clause ...)
    (let ([temp key])
      (if (member temp '(cell ...))
          (begin expr1 expr2 ...)
          (case temp
                clause ...)))]))

;----------------------------------------------------------------

; Binding constructs

; (let), (let*) and (letrec) each create a new scope and bind
; values to some symbols before executing a series of lists.
; They differ according to how they evaluate the bound values.

; (let) evaluates values in the enclosing scope, so lambdas will
; not be able to refer to other values assigned using the (let).
(define-syntax let (syntax-rules ()
  [(let ([variable init] ...) body ...)
    ((lambda (variable ...)
        body ...)
     init ...)]
  [(let name ([variable init] ...) body ...)
    (letrec ([name (lambda (variable ...)
                     body ...)])
      (name init ...))]))

; (let*) creates a new scope for each variable and evaluates
; each expression in its enclosing scope. Basically a shorthand
; for several nested (let)s. Variables may refer to those that
; preceed them but not vice versa.
(define-syntax let* (syntax-rules ()
  [(let* ([name expression]) body ...)
    (let ([name expression]) body ...)]
  [(let* ([n1 e1] [n2 e2] ...) body ...)
    (let ([n1 e1])
      (let* ([n2 e2] ...) body ...))]))

; (letrec) evaluates values in the inner scope, so lambdas are
; able to refer to other values assigned using the (letrec).
(define-syntax letrec (syntax-rules ()
  [(letrec ([variable init] ...) body ...)
    ((lambda ()
      (define variable init) ...
      body ...))]))

(define let-syntax let)
(define letrec-syntax letrec)

;----------------------------------------------------------------

; Iteration

; (do) is similar to the 'while' construct in procedural
; languages. It assigns initial values to a set of variables,
; then performs the list of given commands in a loop. If
; before any iteration the test is found to be false, the
; loop is halted and the value of the expression following
; the test is returned.
(define-syntax do (syntax-rules ()
  [(do ([variable init step ...] ...)   ; Allow 0 or 1 step
       (test expression ...)
       command ...)
    (let loop ([variable init] ...)
      (if test
          (begin expression ...)
          (begin
            command ...
            (loop (do "step" variable step ...) ...))))]
  [(do "step" variable)
    variable]
  [(do "step" variable step)
    step]))

;----------------------------------------------------------------

; Boolean combinators

; (and) returns the first falsey value returned by the list
; of expressions, or returns the value of the last expression
; if all values are truthy.
(define-syntax and (syntax-rules ()
  [(and test) test]
  [(and test1 test2 ...)
    (let ([temp test1])
      (if (not temp)
          temp
          (and test2 ...)))]))

; (or) returns the first truthy value returned by the list
; of expressions, or returns the value of the last expression
; if all values are falsey.
(define-syntax or (syntax-rules ()
  [(or test) test]
  [(or test1 test2 ...)
    (let ([temp test1])
      (if temp
          temp
          (or test2 ...)))]))

;----------------------------------------------------------------

; Delayed evaluation

; (delay) allows the evaluation of an expression to be delayed
; by wrapping it in a promise. Use (force) to evaluate the promise
; at a later time. The expression inside a promise is only
; ever evaluated once, so a promise can be implemented as a
; memoized closure.
(define-syntax delay (syntax-rules ()
  [(delay expression)
    (let ([forced #f]
          [memo #f])
      (lambda ()
        (if forced
            memo
            (begin
              (set! memo expression)
              (set! forced #t)
              memo))))]))

;----------------------------------------------------------------

; Quasiquotation

; (quasiquote) is similar to (quote), except that when it
; encounters an (unquote) or (unquote-splicing) expression
; it will evaluate it and insert the result into the
; surrounding quoted list.
(define-syntax quasiquote (syntax-rules (unquote unquote-splicing)
  [`,expr                 expr                                ]
  [`(,@expr)              expr                                ]
  [`(,@first . rest)      (append first `rest)                ]
  [`(first . rest)        (cons `first `rest)                 ]
  [`#(,@first rest ...)   (list->vector `(,@first rest ...))  ]
  [`#(expr ...)           (list->vector `(expr ...))          ]
  [`expr                  'expr                               ]))

