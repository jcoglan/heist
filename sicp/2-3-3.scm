; Section 2.3.3
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-16.html#%_sec_2.3.3

(load "helpers")


; Set procedures

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


(exercise "2.59")
; union-set for unordered list representation

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
          (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (cons (car set1) set2)))))

(output '(union-set '(1 2 6 8 3) '(3 8 5 6 9)))


(exercise "2.60")
; Procedures allowing duplicates within the representation

; Since we don't care about duplicates, we don't have to
; call element-of-set? quite so much.

; Leave element-of-set? as is -> O(n)

; No membership test -> O(1)
(define (adjoin-set x set)
  (cons x set))

; No membership checks, single loop -> O(n)
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1)
                 (adjoin-set (car set1) set2))))

; Leave intersection-set as is. This by definition requires
; membership checks over both sets -> O(n^2)


; Ordered list representation

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))


(exercise "2.61")
; adjoin-set for ordered lists

(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (adjoin-set (car set)
                          (adjoin-set x (cdr set))))))

(output '(adjoin-set 5 '(2 3 7 9)))


(exercise "2.62")
; O(n) union-set for ordered lists

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1 (union-set (cdr set1) (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set (cdr set1) set2)))
                  ((> x1 x2)
                   (cons x2 (union-set set1 (cdr set2)))))))))

(output '(union-set '(1 2 3 6 8 11) '(3 5 6 8 9)))


; Binary tree representation

(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define make-tree list)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


(exercise "2.63")
; Converting trees to lists

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define tree-1 '(7 (3 (1 () ())
                      (5 () ()))
                   (9 ()
                      (11 () ()))))

(define tree-2 '(3 (1 () ())
                   (7 (5 () ())
                      (9 ()
                         (11 () ())))))

(define tree-3 '(5 (3 (1 () ())
                      ())
                   (9 (7 () ())
                      (11 () ()))))

(output '(tree->list-1 tree-1))
(output '(tree->list-2 tree-1))
(output '(tree->list-1 tree-2))
(output '(tree->list-2 tree-2))
(output '(tree->list-1 tree-3))
(output '(tree->list-2 tree-3))

; a. Both produce the same result: the first produces
;    (append left-side entry right-side), where left-side and
;    right-side are the flattened portions of each side of
;    the tree. Thus the output is an ordered list.
;    
;    The second simply conses items from the tree onto
;    a list starting with '() and iterating from rightmost
;    to leftmost leaf, again producing a correctly ordered
;    list as output.
;
; b. tree->list-2 grows more slowly since it requires a
;    single `cons` for each entry in the tree and tree->list-1
;    requires an `append` for each entry. `Append` is more
;    expensive than `cons` since it copies all its arguments
;    by consing their elements together.


(exercise "2.64")

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      ; If `n` is zero, return an empty tree and the whole of `elts`
      (cons '() elts)
      ; We need to make a tree out of the first `n` elements of
      ; `elts`, whose root node is the median value in this range.
      ; Pick the first half of the list (n=6 -> left-size=2,
      ; n=7 -> left-size=3) and use `partial-tree` recursively
      ; to process it. We get back the left-hand subtree and
      ; the remainder of elts.
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          ; Extract the tree and remaining list from the result.
          ; The right-size of the tree is `(- n left-size 1)`,
          ; since `n` is the size of the tree, we've consumed
          ; `left-size` elements already and we need one element
          ; for the root node.
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            ; Get the root node value from the front of the
            ; remainder list, and use another recursive call
            ; to get the right-hand-side of the tree and a list
            ; of remaining unprocessed values.
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                ; Return a tree node using the root value and the
                ; two subtrees, and a list of the unprocessed
                ; elements from the original elts list.
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(output '(list->tree '(1 3 5 7 9 11)))
; => (5 (1 ()
;          (3 () ()))
;       (9 (7 () ())
;          (11 () ())))

; `Partial-tree` returns once for every element in the input list
; -- the `this-entry` term in the return expression takes every value
; in the list once and only once -- so grows as O(n). (There is
; an O(log n) overhead of making empty subtrees for the leaves, but
; the O(n) term will dominate.)


(exercise "2.65")
; O(n) union-set and intersection-set for balanced binary trees.
; Use the list-based procedures with O(n) input/output
; conversion filters.

(define tree->list tree->list-2)

(define (intersection-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2) (cons x1 (intersection-list (cdr set1) (cdr set2))))
              ((< x1 x2) (intersection-list (cdr set1) set2))
              ((< x2 x1) (intersection-list set1 (cdr set2)))))))

(define (intersection-set set1 set2)
  (list->tree (intersection-list (tree->list set1)
                                 (tree->list set2))))

(define (union-list set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2) (cons x1 (union-list (cdr set1) (cdr set2))))
                  ((< x1 x2) (cons x1 (union-list (cdr set1) set2)))
                  ((> x1 x2) (cons x2 (union-list set1 (cdr set2)))))))))

(define (union-set set1 set2)
  (list->tree (union-list (tree->list set1)
                          (tree->list set2))))

(define A (list->tree '(1 3 5 7 8)))
(define B (list->tree '(2 3 5 6 9)))

(output '(intersection-set A B))
(output '(union-set A B))

