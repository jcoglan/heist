(describe (number? complex? real? rational? integer?)
  (with "an integer"            (3)         => #t)
  (with "an negative int"       (-9)        => #t)
  (with "an integral rational"  (6/2)       => #t)
  (with "an integral complex"   (7+0i)      => #t)
  (with "a boolean"             (#t)        => #f)
  (with "a symbol"              ('foo)      => #f)
  (with "a pair"                ('(1 . 2))  => #f)
  (with "the empty list"        ('())       => #f)
  (with "a string"              ("foo")     => #f)
  (with "a vector"              ('#(1 2))   => #f)
)

(describe (number? complex? real? rational?)
  (with "a rational"            (2/5)       => #t)
)

(describe integer?
  (with "a rational"            (2/5)       => #f)
)

(describe (number? complex? real?)
  (with "a real number"         (3.14)      => #t)
  (with "a round real"          (8.0)       => #t)
  (with "a real complex number" (4.5+0i)    => #t)
)

; All these fail in mzscheme for rational?
; "integer? with a round real" also fails.
; @incompat
(describe (rational? integer?)
  (with "a real number"         (3.14)      => #f)
  (with "a round real"          (8.0)       => #f)
  (with "a real complex number" (4.5+0i)    => #f)
)

(describe (number? complex?)
  (with "a complex number"      (6+4i)      => #t)
)

(describe (real? rational? integer?)
  (with "a complex number"      (6+4i)      => #f)
)

(describe exact?
  (with "an integer"            (12)        => #t)
  (with "a rational"            (24/7)      => #t)
  (with "a real number"         (3.14)      => #f)
  (with "an exact complex"      (8+9i)      => #t)
  (with "an inexact complex"    (8+9.2i)    => #f)
)

(describe inexact?
  (with "an integer"            (12)        => #f)
  (with "a rational"            (24/7)      => #f)
  (with "a real number"         (3.14)      => #t)
  (with "an exact complex"      (8+9i)      => #f)
  (with "an inexact complex"    (8+9.2i)    => #t)
)

(describe (+ - * quotient remainder modulo max min gcd lcm expt)
  (with "two integers"          (4 5)       ~> exact?)
)

(describe (abs numerator denominator floor ceiling truncate round)
  (with "an integer"            (17)        ~> exact?)
  (with "an rational number"    (23/6)      ~> exact?)
)

(describe (abs floor ceiling truncate round)
  (with "a real number"         (-34.6)     ~> inexact?)
)

(describe (+ - *)
  (with "the first arg inexact"   (3.6 4)   ~> inexact?)
  (with "the second arg inexact"  (3 4.2)   ~> inexact?)
  (with "both arguments  inexact" (0.1 2.5) ~> inexact?)
)

(describe *
  (with "the first arg as zero"   (0 3.4)   ~> exact?)
  (with "the second arg as zero"  (3.4 0)   ~> exact?)
)

(describe /
  (with "two integers"          (7 4)       ~> exact?)
  (with "two rationals"         (4/5 2/3)   ~> exact?)
  (with "an integer and a real" (9 4.5)     ~> inexact?)
  (with "a real and a rational" (2.3 8/9)   ~> inexact?)
  
  (with "two integers giving an integer"  (20 5)    => 4)
  (with "two integers giving a rational"  (20 6)    => 10/3)
  (with "two rationals giving an integer" (8/2 2/3) => 6)
  (with "two rationals giving a rational" (3/4 2/3) => 9/8)
)

(with "zero"                      0 (~>   zero? even?)
                                    (!~>  positive? negative? odd?))

(with "a positive even integer"   4 (!~>  zero? negative? odd?)
                                    (~>   positive? even?))

(with "a positive odd integer"    5 (!~>  zero? negative? even?)
                                    (~>   positive? odd?))

(with "a negative even integer"  -6 (!~>  zero? positive? odd?)
                                    (~>   negative? even?))

(with "a negative odd integer"   -9 (!~>  zero? positive? even?)
                                    (~>   negative? odd?))

(with "a positive real"         4.5 (!~>  zero? negative? odd? even?)
                                    (~>   positive?))

(with "a negative real"        -6.3 (!~>  zero? positive? odd? even?)
                                    (~>   negative?))

