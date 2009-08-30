; Any built-in functions that we can implement directly
; in Scheme should go here. If at all possible, write
; builtins in Scheme rather than Ruby.

;----------------------------------------------------------------

; Numerical functions

; Returns true iff all arguments are numerically equal
(define (= . args)
  (define (iter x rest)
    (if (null? rest)
        #t
        (let ([y (car rest)])
          (if (or (not (number? x))
                  (not (number? y))
                  (not (equal? x y)))
              #f
              (iter x (cdr rest))))))
  (iter (car args) (cdr args)))

; (max arg1 arg2 ...)
; Returns the maximum value in the list of arguments
(define (max . values)
  (foldr (lambda (a b) (if (>= a b) a b))
         (car values)
         (cdr values)))

; (min arg1 arg2 ...)
; Returns the minimum value in the list of arguments
(define (min . values)
  (foldr (lambda (a b) (if (<= a b) a b))
         (car values)
         (cdr values)))

; (gcd x y)
; Returns the greatest common divisor of two numbers
; http://en.wikipedia.org/wiki/Euclidean_algorithm
(define (gcd x y . rest)
  (if (null? rest)
      (if (zero? y)
          (abs x)
          (gcd y (remainder x y)))
      (apply gcd (cons (gcd x y) rest))))

; (lcm x y)
; Returns the lowest common multiple of two numbers
; http://en.wikipedia.org/wiki/Least_common_multiple
(define (lcm x y . rest)
  (if (null? rest)
      (/ (abs (* x y))
         (gcd x y))
      (apply lcm (cons (lcm x y) rest))))

;----------------------------------------------------------------

; List/pair functions

; (list arg ...)
; Allocates and returns a new list from its arguments
(define (list . args) args)

; (append list ...)
; Returns a new list formed by concatenating the arguments.
; The final argument is not copied and the return value of
; (append) shares structure with it.
(define (append first . rest)
  (cond [(null? rest) first]
        [(null? first) (apply append rest)]
        [else
          (cons (car first)
                (append (cdr first)
                        (apply append rest)))]))

; (map proc list1 list2 ...)
; Returns a new list formed by applying proc to each member
; (or set of members) of the given list(s).
(define (map proc list1 . list2)
  (if (null? list1)
      list1
      (if (null? list2)
          (cons (proc (car list1))
                (map proc (cdr list1)))
          (let* ([all (cons list1 list2)]
                 [args (map car all)]
                 [rest (map cdr all)])
            (cons (apply proc args)
                  (apply map (cons proc rest)))))))

; (for-each proc list1 list2 ...)
; Calls proc once for each member of list1, passing each
; member (or set of members if more than one list given)
; as arguments to proc.
(define (for-each proc list1 . list2)
  (do ([pair list1 (cdr pair)]
       [others list2 (map cdr others)])
      ((null? pair) '())
    (apply proc (cons (car pair)
                      (map car others)))))

;----------------------------------------------------------------

; Character functions

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
       (let ([code (char->integer letter)])
         (and (>= code 65)
              (<= code 90)))))

; (char-lower-case? letter)
; Returns true iff letter is a lowercase letter
(define (char-lower-case? letter)
  (and (char? letter)
       (let ([code (char->integer letter)])
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
       (let ([code (char->integer char)])
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
  (let ([code (char->integer char)])
    (if (and (>= code 97) (<= code 122))
        (integer->char (- code 32))
        (integer->char code))))

; (char-downcase char)
; Returns a lowercase copy of char
(define (char-downcase char)
  (let ([code (char->integer char)])
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

;----------------------------------------------------------------

; String functions

; (string char ...)
; Returns a new string formed by combining the given characters
(define (string . chars) (list->string chars))

(define (string-compare string1 string2 char-less? char-greater?)
  (if (or (not (string? string1))
          (not (string? string2)))
      (error "Expected two strings as arguments")
      (do ([pair1 (string->list string1) (cdr pair1)]
           [pair2 (string->list string2) (cdr pair2)]
           [diff '()])
          ((integer? diff) diff)
        (set! diff (cond [(null? pair1) (if (null? pair2) 0 -1)]
                         [(null? pair2) 1]
                         [else (let ([char1 (car pair1)]
                                     [char2 (car pair2)])
                                 (cond [(char-less?    char1 char2) -1]
                                       [(char-greater? char1 char2)  1]
                                       [else '()]))])))))

; (string=? string1 string2)
; Returns true iff string1 and string2 are equal strings
(define (string=? string1 string2)
  (zero? (string-compare string1 string2 char<? char>?)))

; (string-ci=? string1 string2)
; Returns true iff string1 and string2 are equal strings, ignoring case
(define (string-ci=? string1 string2)
  (zero? (string-compare string1 string2 char-ci<? char-ci>?)))

; (string<? string1 string2)
; Returns true iff string1 is lexicographically less than string2
(define (string<? string1 string2)
  (= (string-compare string1 string2 char<? char>?) -1))

; (string>? string1 string2)
; Returns true iff string1 is lexicographically greater than string2
(define (string>? string1 string2)
  (= (string-compare string1 string2 char<? char>?) 1))

; (string<=? string1 string2)
; Returns true iff string1 is lexicographically less than or equal
; to string2
(define (string<=? string1 string2)
  (not (string>? string1 string2)))

; (string>=? string1 string2)
; Returns true iff string1 is lexicographically greater than or equal
; to string2
(define (string>=? string1 string2)
  (not (string<? string1 string2)))

; (string-ci<? string1 string2)
; Returns true iff string1 is lexicographically less than string2,
; ignoring differences in case
(define (string-ci<? string1 string2)
  (= (string-compare string1 string2 char-ci<? char-ci>?) -1))

; (string-ci>? string1 string2)
; Returns true iff string1 is lexicographically greater than string2,
; ignoring differences in case
(define (string-ci>? string1 string2)
  (= (string-compare string1 string2 char-ci<? char-ci>?) 1))

; (string-ci<=? string1 string2)
; Returns true iff string1 is lexicographically less than or equal
; to string2, ignoring differences in case
(define (string-ci<=? string1 string2)
  (not (string-ci>? string1 string2)))

; (string-ci>=? string1 string2)
; Returns true iff string1 is lexicographically greater than or equal
; to string2, ignoring differences in case
(define (string-ci>=? string1 string2)
  (not (string-ci<? string1 string2)))

; (substring string start end)
; Returns a string composed of the characters from start (inclusive)
; to end (exclusive) in string
(define (substring string start end)
  (list->string (sublist (string->list string) start end)))

; (list->string chars)
; Returns a new string formed by combining the list
(define (list->string chars)
  (let* ([size (length chars)]
         [str (make-string size)])
    (do ([list chars (cdr list)]
         [i 0 (+ i 1)])
        ((= i size) str)
      (string-set! str i (car list)))))

; (string->list string)
; Returns a newly allocated list of the characters in the string
(define (string->list string)
  (let ([size (string-length string)])
    (do ([i size (- i 1)]
         [list '() (cons (string-ref string (- i 1)) list)])
        ((zero? i) list))))

; (string-copy string)
; Returns a newly allocated copy of the string
(define (string-copy string)
  (list->string (string->list string)))

; (string-fill! string char)
; Replaces every character of string with char
(define (string-fill! string char)
  (let ([size (string-length string)])
    (do ([i size (- i 1)])
        ((zero? i) string)
      (string-set! string (- i 1) char))))

; (string-append string ...)
; Returns a new string formed by concatenating the arguments
(define (string-append . strings)
  (list->string (apply append (map string->list strings))))

;----------------------------------------------------------------

; Vector functions

; (vector object ...)
; Returns a newly allocated vector from its arguments
(define (vector . args) (list->vector args))

; (list->vector list)
; Returns a newly allocated vector from a list
(define (list->vector list)
  (let* ([size (length list)]
         [new-vector (make-vector size)])
    (do ([i 0 (+ i 1)]
         [pair list (cdr pair)])
        ((= i size) new-vector)
      (vector-set! new-vector i (car pair)))))

; (vector->list vector)
; Returns a newly allocated proper list from a vector
(define (vector->list vector)
  (do ([i (vector-length vector) (- i 1)]
       [pair '() (cons (vector-ref vector (- i 1)) pair)])
      ((zero? i) pair)))

; (vector-fill! vector fill)
; Sets every element of vector to fill
(define (vector-fill! vector fill)
  (do ([i (vector-length vector) (- i 1)])
      ((zero? i) vector)
    (vector-set! vector (- i 1) fill)))

