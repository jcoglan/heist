; Section 2.3.1
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-16.html#%_sec_2.3.1

(load "../helpers")


(exercise "2.53")

(output '(list 'a 'b 'c))                           ; => (a b c)

(output '(list (list 'george)))                     ; => ((george))
(output '(cdr '((x1 x2) (y1 y2))))                  ; => ((y1 y2))

(output '(cadr '((x1 x2) (y1 y2))))                 ; => (y1 y2)
(output '(pair? (car '(a short list))))             ; => #f
(output '(memq 'red '((red shoes) (blue socks))))   ; => #f

(output '(memq 'red '(red shoes blue socks)))       ; => (red shoes blue socks)


(exercise "2.54")
; List equality

(define (equal? a b)
  (or (and (symbol? a)
           (symbol? b)
           (eq? a b))
      (and (null? a)
           (null? b))
      (and (pair? a)
           (pair? b)
           (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))))

(output '(equal? '(this is a list) '(this is a list)))
(output '(equal? '(this is a list) '(this (is a) list)))


(exercise "2.55")

(output '(car ''abracadabra)) ; = (car (quote (quote abracadabra)))
                              ; = (car (list 'quote 'abracadabra))
                              ; = quote

