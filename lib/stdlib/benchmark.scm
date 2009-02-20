(define benchmark (lambda (n fn)
  (begin
    (define start (runtime))
    (define iter (lambda (n)
      (begin
        (if (> n 0)
            (begin (fn)
                   (iter (- n 1)))))))
    (iter n)
    (display (- (runtime) start)))))

