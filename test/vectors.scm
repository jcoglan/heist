(assert (equal? #() #()))
(assert (equal? #(1 8 7) #(1 8 7)))
(assert (equal? #(1 (2 3) 7) #(1 (2 3) 7)))
(assert (not (equal? #(1 (2 3) 7) #(1 (2 4) 7))))

(assert (vector? #(1 2 3)))
(assert (vector? #()))
(assert (vector? #((1 2 3))))

(for-each (lambda (predicate)
            (assert (not (predicate #()))))
          `(,pair? ,list? ,null?))

(assert (vector? (vector 1 3)))
(assert-equal #(1 4) (vector 1 4))
(assert (not (equal? #(2 4) (vector 2 4 5))))

(assert (not (equal? '(1 2) #[1 2])))
(assert (equal? '(1 2) (vector->list #(1 2))))
(assert (equal? (list->vector '(1 2)) #(1 2)))

(assert-equal #(() () ()) (make-vector 3))
(assert-equal #(foo foo foo foo) (make-vector 4 'foo))

(assert-equal 0 (vector-length #()))
(assert-equal 3 (vector-length #(1 8 9)))

(assert-equal 8 (vector-ref '#(1 1 2 3 5 8 13 21) 5))

(assert-equal 13 (vector-ref '#[1 1 2 3 5 8 13 21]
                             (round (* 2 (acos -1)))))

(assert-equal #(0 ("Sue" "Sue") "Anna") (let ((vec (vector 0 '(2 2 2 2) "Anna")))
                                          (vector-set! vec 1 '("Sue" "Sue"))
                                          vec))

(assert-raise ImmutableError (vector-set! '#(0 1 2) 1 "doe"))
(assert-equal #(0 "doe" 2) (let ([v #(0 1 2)])
                             (vector-set! v 1 "doe")
                             v))

(assert-equal #(23 23 23) (let ([v (vector 1 2 3)])
                            (vector-fill! v 23)
                            v))

(define (create-unquoted-vector)
  #(1 2 3))

(let ([a (create-unquoted-vector)])
  (vector-set! a 1 7)
  (assert-equal #(1 2 3) (create-unquoted-vector))
  (assert-equal '#(1 7 3) a))

(define (create-quoted-vector)
  '#(1 2 3))

(let ([a (create-quoted-vector)])
  (assert-raise ImmutableError (vector-set! a 1 7)))

(define-syntax vector-mac (syntax-rules ()
  ((_ #(a (b c) d ...))
    c)
  ((_ #(a b c))
    a)))

(assert-equal 3 (vector-mac #(3 8 7)))
(assert-raise SyntaxError (vector-mac #(1 2)))
(assert-raise SyntaxError (vector-mac #(1 2 3 4)))
(assert-raise SyntaxError (vector-mac (1 2 3)))

(assert-equal 5 (vector-mac #(3 (4 5))))
(assert-equal 5 (vector-mac #(3 (4 5) 6)))
(assert-equal 3 (vector-mac #(3 #(4 5) 6)))

(assert-equal #(1 2 3) '#(1 2 3))
(assert-equal #(1 2 3) `#(1 2 3))
(assert-equal #(1 (* 7 4) 3) `#(1 (* 7 4) 3))
(assert-equal #(1 28 3) `#(1 ,(* 7 4) 3))
(assert-equal #(1 28 9 5) `#(1 ,(* 7 4) ,@'(9 5)))
(assert-equal #(1 28 9 5 8) `#(1 ,(* 7 4) ,@'(9 5) 8))
(assert-equal #(9 5 8) `#(,@'(9 5) 8))
(assert-equal #((9 5) 8) `#(,'(9 5) 8))

