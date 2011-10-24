(assert (equal? (symbol->string 'foo) "foo"))
(assert (eqv? (string->symbol "foo") 'foo))

(assert (char? #\a))
(assert (char? #\ ))
(assert (char? #\)))
(assert (char? #\tab))

(assert (char=? #\F #\F))
(assert (char=? #\space #\space))
(assert (char=? #\space #\ ))
(assert (char=? #\  #\space))
(assert (not (char=? #\F #\f)))
(assert (not (char=? #\8 #\f)))

(assert (char<? #\A #\B))
(assert (char<? #\a #\b))
(assert (char<? #\0 #\9))

(assert (char=? (integer->char 97) #\a))
(assert-equal 100 (char->integer #\d))

(assert (char=? #\A (char-upcase #\a)))
(assert (char=? #\h (char-downcase #\H)))

(assert (char-alphabetic? #\A))
(assert (char-alphabetic? #\Z))
(assert (char-alphabetic? #\a))
(assert (char-alphabetic? #\z))
(assert (not (char-alphabetic? #\0)))
(assert (not (char-alphabetic? #\[)))

(assert (char-numeric? #\0))
(assert (char-numeric? #\9))
(assert (not (char-numeric? #\k)))

(assert (char-whitespace? #\ ))
(assert (char-whitespace? #\tab))
(assert (char-whitespace? #\newline))
(assert (char-whitespace? #\space))
(assert (not (char-whitespace? #\s)))

(assert (char-ci=? #\A #\a))
(assert (not (char-ci=? #\A #\b)))

(assert (string? "foo"))
(assert (not (string? 'foo)))
(assert (not (string? #\a)))
(assert (not (string? 9)))

(assert-equal "    " (make-string 4))
(assert-equal ")))))))" (make-string 7 #\)))

(let ([g (lambda () "***")])
  (assert-raise ImmutableError (string-set! (g) 0 #\?))
  (assert-equal "***" (g)))

(assert-equal 13 (string-length "smoked salmon"))
(assert-equal #\o (string-ref "salmon" 4))
(assert-raise BadIndexError (string-ref "salmon" 7))

(let ([s (string-copy "saLMON")])
  (string-set! s 4 #\k)
  (assert-equal "saLMkN" s)
  (assert (eqv? s s))
  (assert (not (eqv? (string-copy s) s)))
  (assert (equal? (string-copy s) s))
  (string-fill! s #\G)
  (assert-equal "GGGGGG" s))

(assert-equal "alm" (substring "salmon" 1 4))

(assert-equal "foo" (string #\f #\o #\o))
(assert-equal "" (string))

(assert-equal '(#\f #\o #\o) (string->list "foo"))
(assert-equal "foo" (list->string '(#\f #\o #\o)))

(assert-equal "foo bar baz" (string-append "foo " "ba" "r baz" ""))

(assert (string=? "foo" "foo"))
(assert (not (string=? "foo" "Foo")))
(assert (not (string=? "foo" "food")))
(assert (not (string=? "food" "foo")))
(assert (string-ci=? "foo" "Foo"))
(assert (not (string-ci=? "food" "Fool")))

(assert (string<? "abacus" "badger"))
(assert (string<? "badger" "badges"))
(assert (string<? "string" "stringify"))
(assert (not (string<? "stringify" "string")))
(assert (not (string<? "badger" "badger")))
(assert (not (string<? "badges" "badger")))

(assert (not (string>? "abacus" "badger")))
(assert (not (string>? "badger" "badges")))
(assert (not (string>? "string" "stringify")))
(assert (string>? "stringify" "string"))
(assert (not (string>? "badger" "badger")))
(assert (string>? "badges" "badger"))

(assert (string<=? "foo" "foo"))
(assert (string<=? "foo" "goo"))
(assert (string<=? "Foo" "foo"))
(assert (not (string<=? "foo" "Foo")))

(assert (string-ci<=? "foo" "Foo"))
(assert (not (string-ci>=? "abacus" "Badger")))
(assert (string>? "abacus" "Badger"))

