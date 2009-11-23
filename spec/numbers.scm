(describe (+ - * quotient remainder modulo max min gcd lcm expt)
  (with "two integers"            (4 5)     ~> exact?)
)

(describe (abs numerator denominator floor ceiling truncate round)
  (with "an integer"              (17)      ~> exact?)
  (with "an rational number"      (23/6)    ~> exact?)
)

(describe (abs floor ceiling truncate round)
  (with "a real number"           (-34.6)   ~> inexact?)
)

(describe (+ - *)
  (with "the first arg inexact"   (3.6 4)   ~> inexact?)
  (with "the second arg inexact"  (3 4.2)   ~> inexact?)
  (with "both arguments  inexact" (0.1 2.5) ~> inexact?)
)

(describe /
  (with "two integers"            (7 4)     ~> exact?)
  (with "two rationals"           (4/5 2/3) ~> exact?)
  (with "an integer and a real"   (9 4.5)   ~> inexact?)
  (with "a real and a rational"   (2.3 8/9) ~> inexact?)
)

(describe *
  (with "the first arg as zero"   (0 3.4)   ~> exact?)
  (with "the second arg as zero"  (3.4 0)   ~> exact?)
)

(describe +
  (with "no arguments"            ()          => 0)
  (with "one argument"            (44)        => 44)
  (with "two integers"            (3 -6)      => -3)
  (with "two rationals"           (3/4 5/2)   => 13/4)
  (with "two reals"               (9.2 3.4)   => 12.6)
  (with "two complexes"           (4+5i 9+2i) => 13+7i)
  
  (with "an integer and a rational" (5 7/2)   => 17/2)
  (with "exact and inexact types"   (5 4.0)   => 9.0)
  
  (with "more than two arguments"   (3.4 5 7/2) => 11.9)
)

(describe *
  (with "no arguments"            ()          => 1)
  (with "one argument"            (44)        => 44)
  (with "two integers"            (3 -6)      => -18)
  (with "two rationals"           (3/4 5/2)   => 15/8)
  (with "two reals"               (9.2 3.6)   => 33.12)
  (with "two complexes"           (4+5i 9+2i) => 26+53i)
  
  (with "an integer and a rational" (5 7/2)   => 35/2)
  (with "exact and inexact types"   (5 4.0)   => 20.0)
  
  (with "more than two arguments"   (3.4 5 7/2) => 59.5)
)

(describe -
  (with "one argument"            (44)        => -44)
  (with "two integers"            (3 -6)      => 9)
  (with "two rationals"           (3/4 5/2)   => -7/4)
  (with "two reals"               (9.2 3.6)   => 5.6)
  (with "two complexes"           (4+5i 9+2i) => -5+3i)
  
  (with "an integer and a rational" (5 7/2)   => 3/2)
  (with "exact and inexact types"   (5 4.0)   => 1.0)
  
  (with "more than two arguments"   (3 4 5)   => -6)
)

(describe /
  (with "one argument"                    (5)       => 1/5)
  (with "two integers giving an integer"  (20 5)    => 4)
  (with "two integers giving a rational"  (20 6)    => 10/3)
  (with "two rationals giving an integer" (8/2 2/3) => 6)
  (with "two rationals giving a rational" (3/4 2/3) => 9/8)
  
  (with "more than two arguments"         (3 4 5)   => 3/20)
)

(describe =
  (with "equal integers"          (3 3)             => #t)
  (with "equal rationals"         (4/3 4/3)         => #t)
  (with "equal reals"             (3.14 3.14)       => #t)
  (with "equal complexes"         (3+8i 3.0+8i)     => #t)
  (with "mixed types"             (3 6/2 3.0 3+0i)  => #t)
  (with "unequal complex"         (3 6/2 3.0 3+2i)  => #f)
  (with "unequal real"            (3 6/2 3.1 3+0i)  => #f)
  (with "unequal rational"        (3 6/5 3.0 3+0i)  => #f)
  (with "unequal integer"         (7 6/2 3.0 3+0i)  => #f)
)

(describe <
  (with "increasing arguments"    (3 9/2 5.6)       => #t)
  (with "an equal pair"           (4.5 9/2 5)       => #f)
  (with "a decreasing pair"       (4.5 4.4 5.6)     => #f)
)

(describe <=
  (with "increasing arguments"    (3 9/2 5.6)       => #t)
  (with "an equal pair"           (4.5 9/2 5)       => #t)
  (with "a decreasing pair"       (4.5 4.4 5.6)     => #f)
)

(describe >
  (with "decreasing arguments"    (9 17/2 7.2)      => #t)
  (with "an equal pair"           (9 17/2 8.5)      => #f)
  (with "an increasing pair"      (9 17/2 8.6)      => #f)
)

(describe >=
  (with "decreasing arguments"    (9 17/2 7.2)      => #t)
  (with "an equal pair"           (9 17/2 8.5)      => #t)
  (with "an increasing pair"      (9 17/2 8.6)      => #f)
)

(describe max
  (with "two integers"            (6 3)             => 6)
  (with "two rationals"           (5/2 22/7)        => 22/7)
  (with "two reals"               (6.7 8.3)         => 8.3)
  
  (with "mixed types, integer largest"  (8 5/2 6.7)   => 8.0)
  (with "mixed types, rational largest" (4 13/2 5.9)  => 6.5)
  (with "mixed types, real largest"     (4.6 5/2 6.7) => 6.7)
  
  (with "an inexact argument"     (8 5/2 6.7) ~> inexact?)
)

(describe min
  (with "two integers"            (6 3)             => 3)
  (with "two rationals"           (5/2 22/7)        => 5/2)
  (with "two reals"               (6.7 8.3)         => 6.7)
  
  (with "mixed types, integer smallest"   (2 5/2 6.7)   => 2.0)
  (with "mixed types, rational smallest"  (4 3/4 5.9)   => 0.75)
  (with "mixed types, real smallest"      (4.6 5/2 1.5) => 1.5)
  
  (with "an inexact argument"     (2 5/2 6.7) ~> inexact?)
)

