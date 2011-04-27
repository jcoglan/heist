; (char string)
; Returns a character from a single-character string. Mostly
; useful for succinct representation of characters in hand-
; written Ruby code.
(define (char string)
  (if (and (string? string) (= (string-length string) 1))
      (string-ref string 0)
      '()))

; (char-upper-case? letter)
; Returns true iff letter is an uppercase letter
(define (char-upper-case? letter)
  (and (char? letter)
       (let ((code (char->integer letter)))
         (and (>= code 65)
              (<= code 90)))))

; (char-lower-case? letter)
; Returns true iff letter is a lowercase letter
(define (char-lower-case? letter)
  (and (char? letter)
       (let ((code (char->integer letter)))
         (and (>= code 97)
              (<= code 122)))))

; (char-alphabetic? char)
; Returns true iff char is an alphabetic character
(define (char-alphabetic? char)
  (or (char-upper-case? char)
      (char-lower-case? char)))

; (char-numeric? char)
; Returns true iff char is a numeric character
(define (char-numeric? char)
  (and (char? char)
       (let ((code (char->integer char)))
         (and (>= code 48)
              (<= code 57)))))

; (char-whitespace? char)
; Returns true iff char is a whitespace character
(define (char-whitespace? char)
  (and (char? char)
       (if (member (char->integer char)
                  '(9 10 32))
           #t
           #f)))

; (char-upcase char)
; Returns an uppercase copy of char
(define (char-upcase char)
  (let ((code (char->integer char)))
    (if (and (>= code 97) (<= code 122))
        (integer->char (- code 32))
        (integer->char code))))

; (char-downcase char)
; Returns a lowercase copy of char
(define (char-downcase char)
  (let ((code (char->integer char)))
    (if (and (>= code 65) (<= code 90))
        (integer->char (+ code 32))
        (integer->char code))))

(define (char-compare-ci operator)
  (lambda (x y)
    (operator (char-downcase x)
              (char-downcase y))))

(define char-ci=?  (char-compare-ci char=?))
(define char-ci<?  (char-compare-ci char<?))
(define char-ci>?  (char-compare-ci char>?))
(define char-ci<=? (char-compare-ci char<=?))
(define char-ci>=? (char-compare-ci char>=?))
