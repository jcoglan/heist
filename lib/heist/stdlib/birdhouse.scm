; A library of combinators, partly inspired by Raymond Smullyan's "To Mock A
; Mockingbird".
; 
; http://en.wikipedia.org/wiki/To_Mock_a_Mockingbird
; 
; Each bird is a higher-order function that takes a single function as input and
; returns a function.
; 
; A bird A 'is fond of' bird B if AB = B, that is to say that B is the fixed
; point of A such that YA = B.
; 
; Rule C1: for any pair of birds (A,B) there is some bird C that composes them
; such that Cf = A(Bf) for all f.
; 
; Rule C2: there exists a Mockingbird M where Mf = ff.
; 
; These can be used to show every higher-order function has at least one fixed
; point:
; 
;   * For any A there is a C where Cf = A(Mf) for all f
; 
;     Cf = A(Mf) for all f. Let f = C
;     -> CC = A(MC) = A(CC) -> A is fond of CC
; 
; If C composes A and M, CC is a fixed point of A and therefore YA = CC = MC
; where Cf = A(Mf) for all f.
; 
; Instead of rule C2, assume this:
; 
; Rule C3: a bird is said to be 'agreeable' if, for every other bird B, there
; exists an f such that Af = Bf. We are given that there exists an agreeable
; bird A.
; 
;   * C1: For any y there exists H such that Hf = y(Af) for all f
;   * C3: A must agree with H for some input x
;   * Let f = x
; 
;   -> Hx = y(Ax), and Hx = Ax -> y(Ax) = Ax
; 
; So any bird y is fond of Ax where A is the agreeable bird, Ax = Hx and H
; composes y with A.

; B combinator (Bluebird) -- Bfgh = f(gh)
; Returns a function that composes two others (rule C1)
(define (B f)
  (lambda (g)
    (lambda (h)
      (f (g h)))))

; M combinator (Mockingbird) -- Mf = ff
; Returns a function's response to itself (rule C2)
(define (M f) (f f))

; K combinator (Kestrel) -- Kfg = f
; Returns its first input
(define (K f)
  (lambda (g)
    (begin g f)))

; Y combinator -- Yf = MC = M(BfM)
; Returns fixed points of higher order functions, that is to say Yf = f(Yf).
; It's often used to implement anonymous recursion.
; 
; Interestingly, using a lazy evaluator you can write this simply as
; 
;     (define (Y f)
;       (f (Y f)))
; 
; and it will work correctly. The following form assumes lazy evaluation but is
; expressed in terms of results derived above. If using applicative order, the
; following form should be used:
; 
;     (define (Y f)
;       (M (lambda (g)
;           (lambda (h)
;             ((((B f) M) g) h)))))
(define (Y f)
  (M ((B f) M)))

