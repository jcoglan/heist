(load "../helpers/macro-helpers")

; Basic test: no subpatterns or ellipses

(define-syntax while
  (syntax-rules ()
    [(while condition expression)
      (let loop ()
        (if condition
            (begin
              expression
              (loop))))]))

(define i 5)
(while (> i 0)
  (set! i (- i 1)))

(assert-equal 0 i)


; Test keywords

(define-syntax assign
  (syntax-rules (values to)
    [(assign values (value ...) to (name ...))
      (begin
        (define name value)
        ...)]))

(assign values (9 7 6) to (foo bar baz))
(assert-equal 9 foo)
(assert-equal 7 bar)
(assert-equal 6 baz)

(assert-raise SyntaxError (assign stuff (3 2) to (foo bar)))
(assert-equal 9 foo)
(assert-equal 7 bar)

(define-syntax dont-rename-else (syntax-rules ()
  [(foo test cons alt)
    (cond (test cons)
          (else alt))]))

(assert-equal 8 (dont-rename-else #f 6 8))

; Check that empty lists are stored as matches and appear in output
(let-syntax ([quote-match (syntax-rules (quote)
                            [(_ 'arg ...) '(arg ...)])])
  (assert-equal '(4 5 6) (quote-match '4 '5 '6))
  (assert-equal '(4 5 ()) (quote-match '4 '5 '()))
  (assert-equal '(4 () 6) (quote-match '4 '() '6)))

; Check that keywords are ignored if locally bound
; example from R6RS -- http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_11.19
(assert-equal 'ok (let ((=> #f))
                    (cond (#t => 'ok))))

; These tests come from tinkering with MZScheme
(define-syntax keyword-detect (syntax-rules (word)
  [(_ word) 'keyword]
  [(_ data) 'data]))
(assert-equal 'keyword (keyword-detect word))
(assert-equal 'word (let ([word 4]) (keyword-detect word)))
(define word 5)
(assert-equal 'keyword (keyword-detect word))
(define copy word)
(assert-equal 'copy (keyword-detect copy))

(define-syntax bad-keyword (syntax-rules (with)
  [(_ with x)
    `(,with ,x)]))

(let ([with 16])
  (assert-raise SyntaxError (bad-keyword with 1)))

(assert-raise UndefinedVariable (bad-keyword with 2))

(define with 16)
(assert-equal '(16 3) (bad-keyword with 3))


; Test literal matching

(define-syntax iffy
  (syntax-rules ()
    [(iffy x #t y) x]
    [(iffy x #f y) y]))

(assert-equal 7 (iffy 7 #t 3))
(assert-equal 3 (iffy 7 #f 3))


; Test improper patterns
(define-syntax rest (syntax-rules ()
  [(_ foo bar . rest)
    rest]))
(assert-equal 10 (rest 4 5 + 3 7))

(let-syntax ([foo (syntax-rules ()
                    [(_ expr ...)
                      (list expr ...)])])
  (assert-equal '(1 2 3) (foo 1 2 3))
  (assert-raise SyntaxError (foo 1 2 3 . 4)))

(let-syntax ([foo (syntax-rules ()
    [(_ bindings body ...)
      '(defun (proc . bindings) body ...)])])
  
  (assert-equal '(defun (proc x y) (display x) y)
                (foo (x y) (display x) y))
  
  (assert-equal '(defun (proc x y . z) z)
                (foo (x y . z) z))
  
  (assert-equal '(defun (proc . z) z)
                (foo z z)))


; Test input execution - example from R5RS

(define-syntax my-or
  (syntax-rules ()
    ((my-or) #f)
    ((my-or e) e)
    ((my-or e1 e2 ...)
     (let ((temp e1))
       (if temp
           temp
           (my-or e2 ...))))))

(define e 1)
(define (inc)
  (set! e (+ e 1))
  e)
(my-or (> 0 (inc))   ; false
       (> 0 (inc))   ; false
       (> 9 6)       ; true - should not evaluate further
       (> 0 (inc))
       (> 0 (inc)))

(assert-equal 3 e)


; Test ellipses
(when true
  (set! i (+ i 1))
  (set! i (+ i 1))
  (set! i (+ i 1))
  (set! i (+ i 1)))

(assert-equal 4 i)


; Test that ellipses match ZERO or more inputs

(define-syntax one-or-more
  (syntax-rules ()
    [(one-or-more stmt1 stmt2 ...)
      (begin
        stmt1
        stmt2
        ...)]))

(assert-equal 6 (one-or-more (+ 2 4)))
(assert-equal 11 (one-or-more (+ 2 4) (+ 3 8)))
(assert-equal 13 (one-or-more (+ 2 4) (+ 3 8) (+ 7 6)))


; Test that null lists terminators don't count as input

(define-syntax table (syntax-rules ()
  [(_)
    '()]
  [(_ key value rest ...)
    (cons (cons key value) (table rest ...))]))

(assert-equal (list (cons 1 2) (cons 3 4) (cons 5 6))
              (table 1 2 3 4 5 6))

(assert-raise SyntaxError (table 1 2 3))


; Test execution scope using (swap)
(define a 4)
(define b 7)
(swap a b)

(assert-equal 7 a)
(assert-equal 4 b)


; More ellipsis tests from PLT docs

(define-syntax rotate
  (syntax-rules ()
    [(rotate a) a]
    [(rotate a b c ...) (begin
                          (swap a b)
                          (rotate b c ...))]))

(define a 1)  (define d 4)
(define b 2)  (define e 5)
(define c 3)
(rotate a b c d e)

(assert-equal 2 a)  (assert-equal 5 d)
(assert-equal 3 b)  (assert-equal 1 e)
(assert-equal 4 c)


; Check repeated macro use doesn't eat the parse tree
(letrec
  ([loop (lambda (count)
            (rotate a b c d e)
            (if (> count 1) (loop (- count 1))))])
  (loop 3))

(assert-equal 5 a)  (assert-equal 3 d)
(assert-equal 1 b)  (assert-equal 4 e)
(assert-equal 2 c)


; Test subpatterns

(define-syntax p-swap
  (syntax-rules ()
    [(swap (x y))
      (let ([temp x])
        (set! x temp)   ; Force temp in lazy mode
        (set! x y)
        (set! y temp))]))

(define m 3)
(define n 8)
(p-swap (m n))
(assert-equal 8 m)
(assert-equal 3 n)


(define-syntax parallel-set!
  (syntax-rules ()
    [(_ (symbol ...) (value ...))
      (begin
        (set! symbol value)
        ...)]))

(parallel-set! (a b c) (74 56 19))
(assert-equal 74 a)
(assert-equal 56 b)
(assert-equal 19 c)


; Test that ellipses are correctly matched
; to numbers of splices in subexpressions

(define-syntax p-let*
  (syntax-rules ()
    [(_ (name ...) (value ...) stmt ...)
      (let* ([name value] ...)
        stmt
        ...)]))

(define indicator #f)

(p-let* (k l m) (3 4 5)
  (assert-equal 5 m)
  (define temp m)
  (set! m (+ l k))
  (set! k (- l temp))
  (set! l (* 6 (+ k m)))
  (rotate k l m)
  (assert-equal 7 l)
  (assert-equal -1 m)
  (assert-equal 36 k)
  (set! indicator #t))

(assert indicator)


(define-syntax sum-lists
  (syntax-rules ()
    [(_ (value1 ...) (value2 ...))
      (+ value1 ... value2 ...)]))

(assert-equal 21 (sum-lists (1 2) (3 4 5 6)))
(assert-equal 21 (sum-lists (1 2 3 4 5) (6)))


(define-syntax do-this
  (syntax-rules (times)
    [(_ n times body ...)
      (letrec ([loop (lambda (count)
                        body ...
                        (if (> count 1)
                            (loop (- count 1))))])
        (loop n))]))

(define myvar 0)
(do-this 7 times
  (set! myvar (+ myvar 1)))
(assert-equal 7 myvar)


; Test that ellipsis expressions can be reused

(define-syntax weird-add
  (syntax-rules ()
    [(_ (name ...) (value ...))
      (let ([name value] ...)
        (+ name ...))]))

(assert-equal 15 (weird-add (a b c d e) (1 2 3 4 5)))

(define-syntax double-up
  (syntax-rules ()
    [(double-up value ...)
      '((value value) ...)]))

(assert-equal '((5 5)) (double-up 5))
(assert-equal '((3 3) (9 9) (2 2) (7 7)) (double-up 3 9 2 7))
(assert-equal '() (double-up))


; Test that pattern variables may appear at a greater
; repetition depth in the template than they do in the pattern

(let-syntax ((repeater (syntax-rules ()
                         [(_ one (many ...))
                          '((one many) ...)]))
             
             (root-v-2 (syntax-rules ()
                         [(_ one (many ...) ...)
                          '(((one many) ...) ...)]))
             
             (1-v-2 (syntax-rules ()
                      [(_ (one ...) (many ...) ...)
                       '(((one many) ...) ...)])))
  
  (assert-equal '((a 2) (a 3) (a 4))
                (repeater a (2 3 4)))
  
  (assert-equal '(((a 1) (a 2)) ((a 6)) () ((a 3) (a 6) (a 8)))
                (root-v-2 a (1 2) (6) () (3 6 8)))
  
  (assert-equal '(((a 1) (a 2)) ((b 6)) () ((d 3) (d 6) (d 8)))
                (1-v-2 (a b c d) (1 2) (6) () (3 6 8))))


; R5RS version of (let), uses ellipsis after lists in patterns

(define-syntax r5rs-let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...)
                      body1 body2 ...)))
        tag)
      val ...))))

(define let-with-macro #f)

(r5rs-let ([x 45] [y 89])
  (assert-equal 45 x)
  (assert-equal 89 y)
  (set! let-with-macro #t))

(assert let-with-macro)


; Non-standard extension to R5RS, not quite R6RS
; Allow ellipses before the end of a list as long
; as, in the expression (A ... B), B is a less specific
; pattern than A
(define-syntax infix-ellip
  (syntax-rules ()
    [(_ (name value) ... fn)
      (let ([name value] ...)
        (fn name ...))]))

(assert-equal 24 (infix-ellip (a 1) (b 2) (c 3) (d 4) *))


; Test nested splicings

(define-syntax nest1
  (syntax-rules ()
    [(_ (value ...) ... name ...)
      '((name (value) ...) ...)]))

(assert-equal '((foo (1) (2)) (bar (3)) (baz) (whizz (4) (5) (6) (7)))
              (nest1 (1 2) (3) () (4 5 6 7) foo bar baz whizz))


(define-syntax triple-deep
  (syntax-rules ()
    [(_ (((name ...) ...) ...) ((value ...) ...) ...)
      '((((value (name)) ...) ...) ...)]))

(assert-equal '((((5 (foo)) (6 (bar))) ((2 (it)))) (((4 (wont))) ((8 (matter)) (7 (really)) (2 (anyway)))))
    (triple-deep (((foo bar) (it)) ((wont) (matter really anyway)))
                 ((5 6) (2)) ((4) (8 7 2))))


(define-syntax triple-deep2
  (syntax-rules ()
    [(_ (((name ...) ...) ...) ((value ...) ...) ...)
      '(((((value (name)) ...) ((value (name)) ...)) ...) ...)]))

(assert-equal '(((((5 (foo)) (6 (bar))) ((5 (foo)) (6 (bar)))))
               ((((4 (wont))) ((4 (wont))))
               (((8 (matter)) (7 (really)) (2 (anyway))) ((8 (matter)) (7 (really)) (2 (anyway))))))
    (triple-deep2 (((foo bar)) ((wont) (matter really anyway)))
                 ((5 6)) ((4) (8 7 2))))


(define-syntax trial (syntax-rules (with)
  [(_ ((with (value ...) ...) ...) obj ...)
    '((obj ((value ...) (value value) ...) ... (obj obj)) ...)]))

(assert-equal '((bar ((4 2 7) (4 4) (2 2) (7 7)) (bar bar)))
              (trial ((with (4 2 7))) bar))
(assert-equal '((bar (bar bar)))
              (trial ((with)) bar))
(assert-raise MacroTemplateMismatch (trial () bar))


(define-syntax trial2 (syntax-rules (with)
  [(_ (with (value ...) ...) ... obj ...)
    '((obj ((value ...) (value value) ...) ... (obj obj)) ...)]))

(assert-equal '((foo ((a) (a a)) (foo foo))
                (bar (bar bar)))
    (trial2 (with (a)) (with) foo bar))

(assert-equal '((foo ((a) (a a)) (foo foo))
                (bar (bar bar))
                (baz ((1 2 3) (1 1) (2 2) (3 3)) (baz baz)))
    (trial2 (with (a)) (with) (with (1 2 3)) foo bar baz))


; Test nested macros with keywords and nested splices
; http://fabiokung.com/2007/10/24/ruby-dsl-to-describe-automata/

(define-syntax automaton (syntax-rules (:)
  [(_ init-state
      [state : response ...]
      ...)
    (let-syntax ([process-state (syntax-rules (-> accept)
                    [(_ accept)
                      (lambda (stream)
                        (cond [(null? stream) #t]
                              [else #f]))]
                    [(... (_ (label -> target) ...))
                      (lambda (stream)
                        (cond [(null? stream) #f]
                              [else (case (car stream)
                                      [(label) (target (cdr stream))]
                                      (... ...)
                                      [else #f])]))])])
      (letrec ([state (process-state response ...)]
               ...)
        init-state))]))

(define cdar-sequence?
  (automaton init
             [init : (c -> more)]
             [more : (a -> more)
                     (d -> more)
                     (r -> end)]
             [end : accept]))

(assert (cdar-sequence? '(c a d a d r)))
(assert (not (cdar-sequence? '(a c a d r c))))

