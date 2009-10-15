; Section 2.4.3
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-17.html#%_sec_2.4.3

(load "../helpers")


; Symbolic differentiation using generics

; Background: assume we have
; (put <op> <types> <function>)
; (get <op> <types>)

; Looks up a concrete implementation based on the type tags of the
; arguments, then if a suitable function is found it is applied to
; the values of the arguments and the result is returned.
(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let [(proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC"
                 (list op type-tags))))))

; (deriv exp var) using hard-coding type dispatch
(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp) (if (same-variable? exp var) 1 0)]
        [(sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))]
        [(product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp)))]
        ; <more rules can be added here>
        (else (error "unknown expression type -- DERIV" exp))))

; (deriv exp var) using data-driven design
(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp) (if (same-variable? exp var) 1 0)]
        [else
          ((get 'deriv (operator exp)) (operands exp)
                                       var)]))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


(exercise "2.73.a")

; Deriv now does simple data-driven dispatch based on the type of
; expression handed to it. If the expression is a number, we return
; 0. If the expression is a symbol, we differentiate it w.r.t. to
; var giving either 0 or 1. For all other types we dispatch using
; the type tag i.e. the operator symbol at the start of the expression.
; 
; Numbers and variables cannot be assimilated as they are atomic
; pieces of data without type tags, so they have to be checked directly.


(exercise "2.73.b")
; Register derivative functions for sums and products

(define (install-eriv-package)
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  
  (define (deriv-product exp var)
    (make-sum
      (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
                    (multiplicand exp))))
  
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  'done)


(exercise "2.73.c")
; Add a derivative function for exponentiations (see ex. 2.56)

(define (deriv-expt exp var)
  (make-product
    (make-product (exponent exp)
                  (make-exponentiation (base exp)
                                       (- (exponent exp) 1)))
    (deriv (base exp) var)))

(put 'deriv '** deriv-expt)


(exercise "2.73.d")
; Switch the indexing from operation/type to type/operation
; You simply need to switch the ordering of the arguments to
; put() in the above examples, assuming the index table is
; agnostic concerning the types used to navigate the index.


(exercise "2.75")
; Implement make-from-mag-ang in message-passing style

(define (make-from-mag-ang mag ang)
  (lambda (op)
    (cond ((eq? op 'real-part)
            (* mag (cos ang)))
          ((eq? op 'imag-part)
            (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else
            (error "Unknown op -- MAKE-FROM-MAG-ANG")))))


(exercise "2.76")
; Explicit dispatch: When new types are added, all existing operations
; must be modified to dispatch to a new concrete implementation for the
; new type. When new operations are added, implementations are required
; for all applicable types and a generic wrapper listing the types to
; dispatch on must be written.
; 
; Data-driven dispatch: When new types are added, all applicable operations
; require implementations for the new type and these implementations must
; be indexed. Generic wrappers remain untouched. When new operations are
; added, they must be implemented across all types and similarly registered
; so that generics may dispatch to them.
; 
; Message-passing. When a new type is added, it must contain concrete
; implementations for all required operations but no existing code need
; change. For a new operation, all existing types must add support for
; the operation but again no other code needs to change.

