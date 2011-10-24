(load "../helpers/macro-helpers")

(assert-equal 4 (let ([first 9])
                  (square-sum 1 first)))

(let ([temp 5]
      [other 6])
  (swap temp other)         ; (let ([temp temp])
  (assert-equal 5 temp)     ;   (set! temp other)
  (assert-equal 6 other))   ;   (set! other temp))

