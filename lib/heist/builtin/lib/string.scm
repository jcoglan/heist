; (string char ...)
; Returns a new string formed by combining the given characters
(define (string . chars) (list->string chars))

(define (string-compare string1 string2 char-less? char-greater?)
  (if (or (not (string? string1))
          (not (string? string2)))
      (error "Expected two strings as arguments")
      (do ((pair1 (string->list string1) (cdr pair1))
           (pair2 (string->list string2) (cdr pair2))
           (diff '()))
          ((integer? diff) diff)
        (set! diff (cond ((null? pair1) (if (null? pair2) 0 -1))
                         ((null? pair2) 1)
                         (else (let ((char1 (car pair1))
                                     (char2 (car pair2)))
                                 (cond ((char-less?    char1 char2) -1)
                                       ((char-greater? char1 char2)  1)
                                       (else '())))))))))

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
  (let* ((size (length chars))
         (str (make-string size)))
    (do ((list chars (cdr list))
         (i 0 (+ i 1)))
        ((= i size) str)
      (string-set! str i (car list)))))

; (string->list string)
; Returns a newly allocated list of the characters in the string
(define (string->list string)
  (let ((size (string-length string)))
    (do ((i size (- i 1))
         (list '() (cons (string-ref string (- i 1)) list)))
        ((zero? i) list))))

; (string-copy string)
; Returns a newly allocated copy of the string
(define (string-copy string)
  (list->string (string->list string)))

; (string-fill! string char)
; Replaces every character of string with char
(define (string-fill! string char)
  (let ((size (string-length string)))
    (do ((i size (- i 1)))
        ((zero? i) string)
      (string-set! string (- i 1) char))))

; (string-append string ...)
; Returns a new string formed by concatenating the arguments
(define (string-append . strings)
  (list->string (apply append (map string->list strings))))
