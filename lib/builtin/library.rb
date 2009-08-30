# This file encodes the built-in library functions as Ruby
# data. The idea is that these procedures should be written
# in Scheme, but writing them like this means Heist does not
# have to parse the code and therefore starts up faster.

program [
  
  [:define, :quit, :exit],
  
  # (newline)
  # prints a new-line character
  [:define, [:newline],
    [:display, "\n"]],
  
  # (force)
  # Extracts the value of a promise created using (delay)
  [:define, [:force, :promise], [:promise]],
  
  # (call/cc)
  # Alias for (call-with-current-continuation)
  [:define, :'call/cc', :'call-with-current-continuation'],
  
  # (eq? x y)
  # Currently an alias for (eqv? x y). TODO implement properly
  [:define, :eq?, :eqv?],
  
  # (not x)
  # Boolean inverse of x
  [:define, [:not, :x],
    [:if, :x, false, true]],
  
  # Longhand aliases for boolean constants
  [:define, :true, true],
  [:define, :false, false],
  
  # (boolean? x)
  # Returns true iff x is a boolean value
  [:define, [:boolean?, :x],
    [:or, [:eqv?, :x, true], [:eqv?, :x, false]]],
  
  # (number? x)
  # Returns true iff x is any type of number
  [:define, :number?, :complex?],
  
  # (exact? x)
  # Returns true iff the given number is exact i.e. an integer, a
  # rational, or a complex made of integers or rationals
  [:define, [:exact?, :x],
    [:or, [:rational?, :x],
          [:and, [:not, [:zero?, [:'imag-part', :x]]],
                 [:exact?, [:'real-part', :x]],
                 [:exact?, [:'imag-part', :x]]]]],
  
  # (inexact? x)
  # Returns true iff x is not an exact number
  [:define, [:inexact?, :x],
    [:not, [:exact?, :x]]],
  
  # (zero? x)
  # Returns true iff x is zero
  [:define, [:zero?, :x],
    [:eqv?, :x, 0]],
  
  # (positive? x)
  # Returns true iff x > 0
  [:define, [:positive?, :x],
    [:'>', :x, 0]],
  
  # (negative? x)
  # Returns true iff x < 0
  [:define, [:negative?, :x],
    [:'<', :x, 0]],
  
  # (odd? x)
  # Returns true iff x is odd
  [:define, [:odd?, :x],
    [:'=', 1, [:remainder, :x, 2]]],
  
  # (even? x)
  # Returns true iff x is even
  [:define, [:even?, :x],
    [:zero?, [:remainder, :x, 2]]],
  
  # (abs x)
  # Returns the absolute value of a number
  [:define, [:abs, :x],
    [:if, [:negative?, :x],
          [:'-', :x],
          :x]],
  
  # (quotient) and (remainder) satisfy
  # 
  # (= n1 (+ (* n2 (quotient n1 n2))
  #          (remainder n1 n2)))
  
  # (quotient x y)
  # Returns the quotient of two numbers, i.e. performs n1/n2
  # and rounds toward zero.
  [:define, [:quotient, :x, :y],
    [:let, [[:result, [:'/', :x, :y]]],
      [[:if, [:positive?, :result],
             :floor,
             :ceiling],
       :result]]],
  
  # (remainder x y)
  # Returns the remainder after dividing the first operand
  # by the second
  [:define, [:remainder, :x, :y],
    [:'-', [:round, :x],
           [:'*', [:round, :y],
                  [:quotient, :x, :y]]]],
  
  # (modulo x y)
  # Returns the first operand modulo the second
  [:define, [:modulo, :x, :y],
    [:'+', [:remainder, :x, :y],
           [:if, [:negative?, [:'*', :x, :y]],
                 [:round, :y],
                 0]]],
  
  [:define, :ceiling, :ceil],
  
  # (rationalize x tolerance)
  # Returns the simplest rational number that differs from x by
  # no more than tolerance. Here 'simplest' means the smallest
  # possible denominator is found first, and with that set the
  # smallest corresponding numerator is chosen.
  [:define, [:rationalize, :x, :tolerance],
    [:cond, [[:rational?, :x],
              :x],
            [[:not, [:zero?, [:'imag-part', :x]]],
              [:'make-rectangular', [:rationalize, [:'real-part', :x], :tolerance],
                                    [:rationalize, [:'imag-part', :x], :tolerance]]],
            [:else,
              [:'let*', [[:t, [:abs, :tolerance]],
                         [:a, [:'-', :x, :t]],
                         [:b, [:'+', :x, :t]]],
                [:do, [[:i, 1, [:'+', :i, 1]],
                       [:z, false]],
                      [[:number?, :z], :z],
                  [:let, [[:p, [:ceiling, [:'*', :a, :i]]],
                          [:q, [:floor, [:'*', :b, :i]]]],
                    [:if, [:'<=', :p, :q],
                          [:'set!', :z, [:'/', [:if, [:positive?, :p], :p, :q],
                                               :i]]]]]]]]],
  
  # (make-polar magnitude angle)
  # Returns a new complex number with the given
  # magnitude and angle
  [:define, [:'make-polar', :magnitude, :angle],
    [:let, [[:re, [:'*', :magnitude, [:cos, :angle]]],
            [:im, [:'*', :magnitude, [:sin, :angle]]]],
      [:'make-rectangular', :re, :im]]],
  
  # (magnitude z)
  # Returns the magnitude of a complex number
  [:define, [:magnitude, :z],
    [:let, [[:re, [:'real-part', :z]],
            [:im, [:'imag-part', :z]]],
      [:sqrt, [:'+', [:'*', :re, :re], [:'*', :im, :im]]]]],
  
  # (angle z)
  # Returns the angle a complex number makes with the
  # real axis when plotted in the complex plane
  [:define, [:angle, :z],
    [:let, [[:re, [:'real-part', :z]],
            [:im, [:'imag-part', :z]]],
      [:atan, :im, :re]]],
  
  # (factorial x)
  # Returns factorial of x
  [:define, [:factorial, :x],
    [:define, [:iter, :y, :acc],
      [:if, [:zero?, :y],
            :acc,
            [:iter, [:'-', :y, 1], [:'*', :y, :acc]]]],
    [:iter, :x, 1]]
]

