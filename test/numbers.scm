(assert (eqv? 42 42))
(assert (not (eqv? 42 #f)))
(assert (not (eqv? 42 42.0)))
(assert (= 42 42))
(assert (= 42 42.0))

; TODO: allow builtins in Ruby to call each other  
;  assert  @@env.eval("(number? 42)")  #t
;  assert !@@env.eval("(number? #t)")  #f
;  #assert  @@env.eval("(complex? 2+3i)")  #t
;  assert !@@env.eval("(real? 2+3i)")  #f
;  assert  @@env.eval("(real? 3.1416)")  #t
;  assert  @@env.eval("(real? 22/7)")  #t
;  assert  @@env.eval("(real? 42)")  #t
;  #assert !@@env.eval("(rational? 2+3i)")  #f
;  #assert  @@env.eval("(rational? 3.1416)")  #t
;  #assert  @@env.eval("(rational? 22/7)")  #t
;  assert !@@env.eval("(integer? 22/7)")  #f
;  assert  @@env.eval("(integer? 42)")  #t

