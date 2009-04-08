(assert (eqv? #t #t))
(assert (eqv? #f #f))
(assert (not (eqv? #t #f)))

(assert (symbol? 'foo))
(assert (eqv? 'foo 'foo))
(assert (not (eqv? 'foo 'bar)))

(assert (eqv? 42 42))
(assert (not (eqv? 42 #f)))
(assert (not (eqv? 42 42.0)))

(assert (eqv? '() '()))
(assert (not (eqv? '(1 2) '(1 2))))
(assert (not (eqv? '() '(1 2))))

(assert (not (eqv? #() #())))
(assert (not (eqv? #(4) #(4))))

(assert (eqv? ceil ceiling))


; From http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_idx_210

(define gen-counter
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))

(assert (let ((g (gen-counter)))
          (eqv? g g)))

(assert (not (eqv? (gen-counter) (gen-counter))))

(assert (let ((x '(a)))
          (eqv? x x)))

