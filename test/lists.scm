(assert (null? '()))
(assert (list? '()))
(assert (not (pair? '())))

(assert (not (vector? '(1 2 3))))

(define foo-list (list (+ 3 2) (* 4 5) 6))
(assert (not (eqv? '(5 20 6) foo-list)))
(assert (equal? '(5 20 6) foo-list))

(define bar-list (cons 12 foo-list))
(assert (equal? '(12 5 20 6) bar-list))
(assert (equal? '(5 20 6) foo-list))
(assert-equal 4 (length bar-list))
(assert-equal 3 (length foo-list))

(assert-equal 5 (car foo-list))
(assert-equal '(20 6) (cdr foo-list))
(assert-equal 20 (cadr foo-list))
(assert-equal 6 (caddr foo-list))

(define eggman '(you (walrus (hurt the) one) ((you) love)))
(assert-equal '(hurt the) (cadadr eggman))

(assert (null? (cdddr eggman)))
(assert (null? '()))
(assert (list? eggman))
(assert (pair? eggman))
(assert (not (null? eggman)))

(define my-pair (cons 'foo 'bar))
(assert (pair? my-pair))
(assert (not (list? my-pair)))
(assert (not (null? my-pair)))

(set-car! my-pair 27)
(set-cdr! my-pair (cons 64 '()))
(assert (list? my-pair))
(assert (equal? '(27 64) my-pair))
(assert-equal 2 (length my-pair))

(define (f) (list 'not-a-constant-list))
(define (g) '(constant-list))
(assert-equal 3 (set-car! (f) 3))
(assert-raise ImmutableError (set-car! (g) 3))

(assert-raise SyntaxError ())
(assert-raise SyntaxError (1 2 3))

(assert-equal (cons 1 2) '(1 . 2))
(assert-equal (cons 1 (cons 2 3)) '(1 2 . 3))

(assert-equal '(x y)        (append '(x) '(y)))
(assert-equal '(a b c d)    (append '(a) '(b c d)))
(assert-equal '(a (b) (c))  (append '(a (b)) '((c))))
(assert-equal '(a b c . d)  (append '(a b) '(c . d)))
(assert-equal 'a            (append '() 'a))
(assert-equal '(a b)        (append '(a b) '()))
(assert-equal '(a (b) (c) (1 2) 3)  (append '(a (b)) '((c)) '() '((1 2) 3)))

(let ([base '(a b)])
  (assert (eqv? (cddr (append '(1 2) base)) base)))

(assert-equal '(4 3 2 1) (reverse '(1 2 3 4)))
(assert-equal '(4 (3 5) 2 1) (reverse '(1 2 (3 5) 4)))

(assert-equal '(3 4) (list-tail '(1 2 3 4) 2))

(assert-equal '(a b c) (memq 'a '(a b c)))
(assert-equal '(b c) (memq 'b '(a b c)))
(assert-equal #f (memq 'a '(b c d)))
(assert-equal #f       (memq   (list 'a) '(b (a) c)))
(assert-equal '((a) c) (member (list 'a) '(b (a) c)))

(define e '((a 1) (b 2) (c 3)))
(assert-equal '(a 1) (assq 'a e))
(assert-equal '(b 2) (assq 'b e))
(assert-equal #f (assq 'd e))
(assert-equal #f (assq (list 'a) '(((a)) ((b)) ((c)))))
(assert-equal '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))
(assert-equal '(5 7) (assv 5 '((2 3) (5 7) (11 13))))

