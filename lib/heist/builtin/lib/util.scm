; Any built-in functions that we can implement directly
; in Scheme should go here. If at all possible, write
; builtins in Scheme rather than Ruby.

(define quit exit)

; (newline)
; prints a new-line character
(define (newline)
  (display "\n"))

; (force)
; Extracts the value of a promise created using (delay)
(define (force promise) (promise))

; (call/cc)
; Alias for (call-with-current-continuation)
(define call/cc call-with-current-continuation)
