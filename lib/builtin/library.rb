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
  
  #----------------------------------------------------------------
  
  # Numerical functions
  
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
    [:iter, :x, 1]],
  
  #----------------------------------------------------------------
  
  # List/pair functions
  
  # (null? object)
  # Returns true iff object is the empty list
  [:define, [:null?, :object],
    [:eqv?, [:quote, []], :object]],
  
  # (list? object)
  # Returns true iff object is a proper list
  [:define, [:list?, :object],
    [:or, [:null?, :object],
          [:and, [:pair?, :object],
                 [:list?, [:cdr, :object]]]]],
  
  # (length object)
  # Returns the length of a proper list
  [:define, [:length, :object],
    [:define, [:iter, :list, :acc],
      [:if, [:null?, :list],
            :acc,
            [:iter, [:cdr, :list], [:'+', 1, :acc]]]],
    [:iter, :object, 0]],
  
  # (reverse list)
  # Returns a newly allocated list consisting of the
  # elements of list in reverse order.
  [:define, [:reverse, :object],
    [:if, [:null?, :object],
          :object,
          [:append, [:reverse, [:cdr, :object]],
                    [:list, [:car, :object]]]]],
  
  # (list-tail list k)
  # Returns the sublist of list obtained by omitting the
  # first k elements.
  [:define, [:'list-tail', :list, :k],
    [:do, [[:pair, :list, [:cdr, :pair]],
           [:i, :k, [:'-', :i, 1]]],
          [[:zero?, :i], :pair]]],
  
  # (list-ref list k)
  # Returns the kth element of list.
  [:define, [:'list-ref', :list, :k],
    [:car, [:'list-tail', :list, :k]]],
  
  # (memq obj list)
  # (memv obj list)
  # (member obj list)
  # These procedures return the first sublist of list whose
  # car is obj, where the sublists of list are the non-empty
  # lists returned by (list-tail list k) for k less than the
  # length of list. If obj does not occur in list, then #f
  # (not the empty list) is returned. Memq uses eq? to compare
  # obj with the elements of list, while memv uses eqv? and
  # member uses equal?.
  
  [:define, [:'list-transform-search', :transform],
    [:lambda, [:predicate],
      [:lambda, [:object, :list],
        [:do, [[:pair, :list, [:cdr, :pair]]],
              [[:or, [:null?, :pair],
                     [:predicate, [:car, [:transform, :pair]], :object]],
               [:if, [:null?, :pair],
                     false,
                     [:transform, :pair]]]]]]],
  
  [:define, :'list-search', [:'list-transform-search', [:lambda, [:x], :x]]],
  [:define, :memq,   [:'list-search', :eq?]],
  [:define, :memv,   [:'list-search', :eqv?]],
  [:define, :member, [:'list-search', :equal?]],
  
  # (assq obj alist)
  # (assv obj alist)
  # (assoc obj alist)
  # Alist (for "association list") must be a list of pairs.
  # These procedures find the first pair in alist whose car
  # field is obj, and returns that pair. If no pair in alist
  # has obj as its car, then #f (not the empty list) is
  # returned. Assq uses eq? to compare obj with the car fields
  # of the pairs in alist, while assv uses eqv? and assoc
  # uses equal?.
  
  [:define, :'assoc-list-search', [:'list-transform-search', :car]],
  [:define, :assq,  [:'assoc-list-search', :eq?]],
  [:define, :assv,  [:'assoc-list-search', :eqv?]],
  [:define, :assoc, [:'assoc-list-search', :equal?]],
  
  # (foldr proc value list)
  [:define, [:foldr, :proc, :value, :list],
    [:if, [:null?, :list],
          :value,
          [:proc, [:car, :list],
                  [:foldr, :proc, :value, [:cdr, :list]]]]],
  
  # (sublist list start end)
  [:define, [:sublist, :list, :start, :end],
    [:cond, [[:null?, :list], [:quote, []]],
            [[:'>', :start, 0], [:sublist, [:cdr, :list], [:'-', :start, 1], [:'-', :end, 1]]],
            [[:'<=', :end, 0], [:quote, []]],
            [:else, [:cons, [:car, :list],
                            [:sublist, [:cdr, :list], 0, [:'-', :end, 1]]]]]]
]

