(describe (+ - * quotient remainder modulo max min gcd lcm expt)
  (with "two integers"            (4 5)       ~> exact?)
)

(describe (abs numerator denominator floor ceiling truncate round)
  (with "an integer"              (17)        ~> exact?)
  (with "an rational number"      (23/6)      ~> exact?)
)

(describe (abs floor ceiling truncate round)
  (with "a real number"           (-34.6)     ~> inexact?)
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
  (with "two integers"            (7 4)       ~> exact?)
  (with "two rationals"           (4/5 2/3)   ~> exact?)
  (with "an integer and a real"   (9 4.5)     ~> inexact?)
  (with "a real and a rational"   (2.3 8/9)   ~> inexact?)
  
  (with "two integers giving an integer"  (20 5)    => 4)
  (with "two integers giving a rational"  (20 6)    => 10/3)
  (with "two rationals giving an integer" (8/2 2/3) => 6)
  (with "two rationals giving a rational" (3/4 2/3) => 9/8)
)

