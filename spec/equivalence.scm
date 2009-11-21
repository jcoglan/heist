(let ((pair   '(1 . 2))
      (vector '#(1 2))
      (string "scheme")
      
      (gen-counter (lambda ()
                     (let ((n 0))
                       (lambda () (set! n (+ n 1)) n)))))
  
  (describe (eq? eqv? equal?)
    (with "both true"                   (#t #t)         => #t)
    (with "both false"                  (#f #f)         => #t)
    
    (with "identical symbols"           ('foo 'foo)     => #t)
    
    (with "equal integers"              (3 3)           => #t)
    (with "equal integer and rational"  (4 8/2)         => #t)
    (with "equal rational and integer"  (30/6 5)        => #t)
    (with "equal integer and complex"   (6 6+0i)        => #t)
    (with "equal complex and integer"   (9+0i 9)        => #t)
    (with "equal rational and complex"  (12/2 6+0i)     => #t)
    (with "equal complex and rational"  (9+0i 54/6)     => #t)
    
    (with "equal characters"            (#\h #\h)       => #t)
    
    (with "both the empty list"         ('() '())       => #t)
    
    (with "identical pairs"             (pair pair)     => #t)
    (with "identical vectors"           (vector vector) => #t)
    (with "identical strings"           (string string) => #t)
    
    (let ((g (gen-counter)))
      (with "identical procedures" (g g) => #t)
      (with "distinct procedures" ((gen-counter) (gen-counter)) => #f))
    
    (with "a boolean and a number"      (#f 0)          => #f)
    (with "false and the empty list"    (#f '())        => #f)
    (with "a number and a boolean"      (1 #t)          => #f)
    (with "a number and a pair"         (5.6 pair)      => #f)
    (with "a string and a number"       (string 4)      => #f)
    (with "a vector and a pair"         (vector pair)   => #f)
    (with "a pair and a vector"         (pair vector)   => #f)
    (with "a symbol and a string"       ('foo "foo")    => #f)
    (with "a procedure and a list"      (abs '())       => #f)
    
    (with "true and false"              (#t #f)         => #f)
    (with "false and true"              (#f #t)         => #f)
    (with "different symbols"           ('foo 'bar)     => #f)
    (with "unequal numbers"             (4 5)           => #f)
    (with "equal integer and real"      (4 4.0)         => #f)
    (with "exact complex and real"      (4+0i 4.0)      => #f)
    (with "real and rational"           (4.5 9/2)       => #f)
    
    (with "unequal characters"          (#\h #\H)       => #f)
    
    (with "the empty list and a pair"   ('() pair)      => #f)
    (with "a pair and the empty list"   (pair '())      => #f)
    
    (with "unequal lists"               ('(1 2) '(1 3))   => #f)
    (with "unequal vectors"             ('#(1 2) '#(1 3)) => #f)
    (with "unequal strings"             ("foo" "bar")     => #f)
  )
  
  (describe eq?
    (with "equal rationals"             (7/8 14/16)     => #f)
    (with "equal reals"                 (3.14 3.14)     => #f)
    (with "equal complexes"             (8+3i 8+3i)     => #f)
    (with "inexact complex and real"    (9.3+0i 9.3)    => #f)
    (with "real and inexact complex"    (9.3 9.3+0i)    => #f)
  )
  
  (describe (eqv? equal?)
    (with "equal rationals"             (7/8 14/16)     => #t)
    (with "equal reals"                 (3.14 3.14)     => #t)
    (with "equal complexes"             (8+3i 8+3i)     => #t)
    (with "inexact complex and real"    (9.3+0i 9.3)    => #t)
    (with "real and inexact complex"    (9.3 9.3+0i)    => #t)
  )
  
  (describe (eq? eqv?)
    (with "distinct pair objects"       ('(1 2) '(1 2))   => #f)
    (with "distinct vector objects"     ('#(1 2) '#(1 2)) => #f)
    (with "distinct strings"            ("foo" "foo")     => #f)
  )
  
  (describe equal?
    (with "distinct pair objects"       ('(1 2) '(1 2))   => #t)
    (with "distinct vector objects"     ('#(1 2) '#(1 2)) => #t)
    (with "distinct strings"            ("foo" "foo")     => #t)
  ))

