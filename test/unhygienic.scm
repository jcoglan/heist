(load "macro-helpers")

(assert-equal 4 (let ([first 9])
                  (square-sum 1 first)))

