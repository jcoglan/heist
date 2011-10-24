(load "../helpers/macro-helpers")

(assert-equal 100 (let ([first 9])
                  (square-sum 1 first)))

(assert-equal 'outer
  (let ((x 'outer))
    (let-syntax ((m (syntax-rules () [(m) x])))
      (let ((x 'inner))
        (m)))))

(assert-equal 'now
 (let ((if #t))
      (when if (set! if 'now))
      if))

(let ([temp 5]
      [other 6])
  (swap temp other)
  (assert-equal 6 temp)
  (assert-equal 5 other))

(let ([set! 5]
      [other 6])
  (swap set! other)
  (assert-equal 6 set!)
  (assert-equal 5 other))

(define plus +)
(define-syntax dyn-plus (syntax-rules ()
  [(_ x y)
    (plus x y)]))
(define (dyn-plus-call)
  (dyn-plus 7 8))

(assert-equal 15 (dyn-plus-call))
(set! plus -)
(assert-equal -1 (dyn-plus-call))

(let-syntax ([ok? (syntax-rules ()
                    [(_ expr)
                     (cond [expr 'ok]
                           [else 'no])])])
  (let ([else #t])
    (assert-equal 'no (ok? #f))))

