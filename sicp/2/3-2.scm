; Section 2.3.2
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-16.html#%_sec_2.3.2

(load "../helpers")


; Symbolic differentiator

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (list '+ a1 a2))

(define (make-product m1 m2)
  (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))


(newline) (display "Derivatives") (newline)
(output '(deriv '(+ x 3) 'x))
(output '(deriv '(* x y) 'x))
(output '(deriv '(* (* x y) (+ x 3)) 'x))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(output '(deriv '(+ x 3) 'x))
(output '(deriv '(* x y) 'x))
(output '(deriv '(* (* x y) (+ x 3)) 'x))


(exercise "2.56")
; Implement exponentiation in the differentiator

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

(define (make-exponentiation x1 x2)
  (cond ((and (number? x1) (number? x2)) (expt x1 x2))
        ((=number? x2 0) 1)
        ((=number? x2 1) x1)
        (else (list '** x1 x2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(output '(deriv '(** (+ (* 10 x) 5) 4) 'x))
(output '(deriv '(** x 2) 'x))


(exercise "2.57")
; Extend the differentiator to handle n-ary sums and products

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((null? m2) m1)
        ((and (pair? m2) (not (symbol? (car m2))))
          (make-product m1 (make-product (car m2) (cdr m2))))
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (make-product (caddr p) (cdddr p)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((null? a2) a1)
        ((and (pair? a2) (not (symbol? (car a2))))
          (make-sum a1 (make-sum (car a2) (cdr a2))))
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (addend s)
  (cadr s))

(define (augend s)
  (make-sum (caddr s) (cdddr s)))

(output '(deriv '(* (* x y) (+ x 3)) 'x))
(output '(deriv '(* x y (+ x 3)) 'x))


(exercise "2.58.a")
; Redesign the data representations for symbolic expressions to
; use infix notation. Assume operators take two arguments, and
; assume fully parenthesised expressions.

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (make-exponentiation x1 x2)
  (cond ((and (number? x1) (number? x2)) (expt x1 x2))
        ((=number? x2 0) 1)
        ((=number? x2 1) x1)
        (else (list x1 '** x2))))

(define base car)
(define exponent caddr)

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define multiplier car)
(define multiplicand caddr)

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define addend car)
(define augend caddr)

(output '(deriv '(x + (3 * (x + (y + 2)))) 'x))
(output '(deriv '(((10 * x) + 5) ** 4) 'x))


(exercise "2.58.b")
; Abandon assumption that everything is fully parenthesised and
; handle infix expressions with operator precedence (+ and * only)
;
; Note + and * are associative -- (a + b) + c == a + (b + c)
; so order does not matter when parsing an n-ary expression.

; If the expression contains a '+', it's a sum (lowest binding)
(define (sum? x)
  (and (pair? x) (memq '+ x)))
; If it is not a sum but contains a '*', it's a product
(define (product? x)
  (and (pair? x) (not (sum? x)) (memq '* x)))

(define addend car)

; The augend is simply the rest of the expression after the operator.
; It will either contain a single item (a number or a parenthesised
; product or sum) or at least three items (two args and an operator).
; If it's one item, we remove the surrounding list.
(define (augend s)
  (let ((rest (cddr s)))
    (if (=number? (length rest) 1)
        (car rest)
        rest)))

(define multiplier addend)
(define multiplicand augend)

(output '(deriv '(x + 3 * (x + y + 2)) 'x))

