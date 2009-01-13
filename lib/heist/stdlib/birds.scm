; A library of combinators, partly inspired by Raymond
; Smullyan's "To Mock A Mockingbird".
; 
; http://en.wikipedia.org/wiki/To_Mock_a_Mockingbird
; 
; Each bird is a high-order function that takes a single
; function as input and returns a function.
; 
; A bird A 'is fond of' bird B if AB = B, that is to say
; that B is the fixed point of A such that YA = B.
; 
; Rule C1: for any pair of birds (A,B) there is some bird
; C that composes them such that Cf = A(Bf) for all f.
; 
; Rule C2: there exists a Mockingbird M where Mf = ff.
; 
; These can be used to show every higher-order function
; has at least one fixed point:
; 
;     For any A there is a C where Cf = A(Mf) for all f
; 
;     Cf = A(Mf) for all f. Let f = C
;     -> CC = A(MC) = A(CC) -> A is fond of CC
; 
; If C composes A and M, CC is a fixed point of A and
; therefore YA = CC = MC where Cf = A(Mf).

; M combinator (Mockingbird)
; Returns a function's response to itself
(define (M f) (f f))

; K combinator (Kestrel)
; Returns its input
(define (K f) f)

; Y combinator
; Returns fixed points of higher order functions, that
; is to say Yf = f(Yf). It's often used to implement
; anonymous recursion.
(define (Y f)
  (M (lambda (g)
    (lambda (x)
      ((f (M g)) x)))))

; E combinator
; This combinator is its own fixed point, according to
; the following logic:
; 
;     All combinators have fixed points
;     Let ME = E -> EE = E
; 
; So E is its own fixed point, as well as that of M.
(define E (Y M))

