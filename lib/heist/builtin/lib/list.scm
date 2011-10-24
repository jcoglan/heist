; (null? object)
; Returns true iff object is the empty list
(define (null? object)
  (eqv? '() object))

; (list? object)
; Returns true iff object is a proper list
(define (list? object)
  (or (null? object)
      (and (pair? object)
           (list? (cdr object)))))

; (list arg ...)
; Allocates and returns a new list from its arguments
(define (list . args) args)

; (length object)
; Returns the length of a proper list
(define (length object)
  (define (iter list acc)
    (if (null? list)
        acc
        (iter (cdr list) (+ 1 acc))))
  (iter object 0))

; (append list ...)
; Returns a new list formed by concatenating the arguments.
; The final argument is not copied and the return value of
; (append) shares structure with it.
(define (append first . rest)
  (cond ((null? rest) first)
        ((null? first) (apply append rest))
        (else
          (cons (car first)
                (append (cdr first)
                        (apply append rest))))))

; (reverse list)
; Returns a newly allocated list consisting of the
; elements of list in reverse order.
(define (reverse object)
  (if (null? object)
      object
      (append (reverse (cdr object))
              (list (car object)))))

; (list-tail list k)
; Returns the sublist of list obtained by omitting the
; first k elements.
(define (list-tail list k)
  (do ((pair list (cdr pair))
       (i k (- i 1)))
      ((zero? i) pair)))

; (list-ref list k)
; Returns the kth element of list.
(define (list-ref list k)
  (car (list-tail list k)))

; (memq obj list)
; (memv obj list)
; (member obj list)
; These procedures return the first sublist of list whose
; car is obj, where the sublists of list are the non-empty
; lists returned by (list-tail list k) for k less than the
; length of list. If obj does not occur in list, then #f
; (not the empty list) is returned. Memq uses eq? to compare
; obj with the elements of list, while memv uses eqv? and
; member uses equal?.

(define (list-transform-search transform)
  (lambda (predicate)
    (lambda (object list)
      (do ((pair list (cdr pair)))
          ((or (null? pair)
               (predicate (car (transform pair)) object))
           (if (null? pair)
               #f
               (transform pair)))))))

(define list-search (list-transform-search (lambda (x) x)))
(define memq   (list-search eq?))
(define memv   (list-search eqv?))
(define member (list-search equal?))

; (assq obj alist)
; (assv obj alist)
; (assoc obj alist)
; Alist (for "association list") must be a list of pairs.
; These procedures find the first pair in alist whose car
; field is obj, and returns that pair. If no pair in alist
; has obj as its car, then #f (not the empty list) is
; returned. Assq uses eq? to compare obj with the car fields
; of the pairs in alist, while assv uses eqv? and assoc
; uses equal?.

(define assoc-list-search (list-transform-search car))
(define assq  (assoc-list-search eq?))
(define assv  (assoc-list-search eqv?))
(define assoc (assoc-list-search equal?))

; (map proc list1 list2 ...)
; Returns a new list formed by applying proc to each member
; (or set of members) of the given list(s).
(define (map proc list1 . list2)
  (if (null? list1)
      list1
      (if (null? list2)
          (cons (proc (car list1))
                (map proc (cdr list1)))
          (let* ((all (cons list1 list2))
                 (args (map car all))
                 (rest (map cdr all)))
            (cons (apply proc args)
                  (apply map (cons proc rest)))))))

; (for-each proc list1 list2 ...)
; Calls proc once for each member of list1, passing each
; member (or set of members if more than one list given)
; as arguments to proc.
(define (for-each proc list1 . list2)
  (do ((pair list1 (cdr pair))
       (others list2 (map cdr others)))
      ((null? pair) '())
    (apply proc (cons (car pair)
                      (map car others)))))

; (fold-right proc value list)
(define (fold-right proc value list)
  (if (null? list)
      value
      (proc (car list)
            (fold-right proc value (cdr list)))))

; (fold-left proc value list)
(define (fold-left proc value list)
  (if (null? list)
      value
      (fold-left proc
                 (proc value (car list))
                 (cdr list))))

; (sublist list start end)
(define (sublist list start end)
  (cond ((null? list) '())
        ((> start 0) (sublist (cdr list) (- start 1) (- end 1)))
        ((<= end 0) '())
        (else (cons (car list)
                    (sublist (cdr list) 0 (- end 1))))))
