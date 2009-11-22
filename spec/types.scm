(with "zero" 0
  (~>   integer? rational? real? complex? number? exact? zero? even?)
  (!~>  inexact? positive? negative? odd?))

(with "a positive odd integer" 7
  (~>   integer? rational? real? complex? number? exact? positive? odd?)
  (!~>  inexact? zero? negative? even?))

(with "a positive even integer" 6
  (~>   integer? rational? real? complex? number? exact? positive? even?)
  (!~>  inexact? zero? negative? odd?))

(with "a negative odd integer" -3
  (~>   integer? rational? real? complex? number? exact? negative? odd?)
  (!~>  inexact? zero? positive? even?))

(with "a negative even integer" -8
  (~>   integer? rational? real? complex? number? exact? negative? even?)
  (!~>  inexact? zero? positive? odd?))

(with "a positive integral rational" 12/4
  (~>   integer? rational? real? complex? number? exact? positive? odd?)
  (!~>  inexact? zero? negative? even?))

(with "a negative integral rational" -8/2
  (~>   integer? rational? real? complex? number? exact? negative? even?)
  (!~>  inexact? zero? positive? odd?))

(with "a positive rational" 7/5
  (~>   rational? real? complex? number? exact? positive?)
  (!~>  integer? inexact? zero? negative?))

(with "a negative rational" -3/4
  (~>   rational? real? complex? number? exact? negative?)
  (!~>  integer? inexact? zero? positive?))

(with "a positive integral real" 5.0
  (~>   real? complex? number? inexact? positive?)
  (!~>  integer? rational? exact? zero? negative?))

(with "a negative integral real" -8.0
  (~>   real? complex? number? inexact? negative?)
  (!~>  integer? rational? exact? zero? positive?))

(with "a positive real" 5.4
  (~>   real? complex? number? inexact? positive?)
  (!~>  integer? rational? exact? zero? negative?))

(with "a negative real" -8.9
  (~>   real? complex? number? inexact? negative?)
  (!~>  integer? rational? exact? zero? positive?))

(with "an exact complex number" 4+3i
  (~>   complex? number? exact?)
  (!~>  integer? rational? real? inexact? zero?))

(with "an integral complex number" 7+0i
  (~>   integer? rational? real? complex? number? exact?)
  (!~>  inexact? zero?))

(with "an inexact complex number" 4+3.5i
  (~>   complex? number? inexact?)
  (!~>  integer? rational? real? exact? zero?))

(with "an real complex number" 7.4+0i
  (~>   real? complex? number? inexact?)
  (!~>  integer? rational? exact? zero?))

