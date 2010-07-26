; Section 2.2.2
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-15.html#%_sec_2.2.2

(load "../helpers")
(define nil (list))
(define orig-map map)


(exercise "2.24")
(output '(list 1 (list 2 (list 3 4))))
; ;=> (1 (2 (3 4)))
; 
;     ---------    ---------
;     | o | o-|--->| o | / |
;     --|------    --|------
;       |            |
;       V            V
;       1          ---------    ---------
;                  | o | o-|--->| o | / |
;                  --|------    --|------
;                    |            |
;                    V            V
;                    2          ---------    ---------
;                               | o | o-|--->| o | / |
;                               --|------    --|------
;                                 |            |
;                                 V            V
;                                 3            4
;
;         (1 (2 (3 4)))
;            /  \
;           /    \
;          1   (2 (3 4))
;               /  \
;              /    \
;             2    (3 4)
;                   /  \
;                  /    \
;                 3      4


(exercise "2.25")
; Picking elements with car and cdr

(output '(car (cdr (car (cdr (cdr '(1 3 (5 7) 9)))))))
(output '(car (car '((7)))))
(output '(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7)))))))))))))))))))


(exercise "2.26")
(define x (list 1 2 3))
(define y (list 4 5 6))
(output '(append x y))
(output '(cons x y))
(output '(list x y))


(exercise "2.27")
; Deep reversal

(define x (list (list 1 2) (list 3 4)))
(output '(reverse x))

(define (deep-reverse x)
  (reverse (map (lambda (o)
                  (if (pair? o)
                      (deep-reverse o)
                      o))
                x)))

(output '(deep-reverse x))


(exercise "2.28")
; Find the fringe of a tree

(define x (list (list 1 2) (list 3 4)))

(define (fringe x)
  (if (pair? x)
      (let* ((left  (fringe (car x)))
             (right (fringe (cdr x)))
             (join  (if (pair? left) append cons)))
        (join left right))
      x))

(output '(fringe x))
(output '(fringe (list x x)))


(exercise "2.29")
; Binary mobile

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (if (number? mobile)
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

(output '(total-weight (make-mobile (make-branch 4 6)
                                    (make-branch 7 3))))

(define mobile-x (make-mobile (make-branch 4 (make-mobile (make-branch 6 2)
                                                                (make-branch 12 5)))
                              (make-branch 7 3)))

(output '(total-weight mobile-x))

(define (mobile-balanced? mobile)
  (if (number? mobile)
      #t
      (let ((l (left-branch mobile))
            (r (right-branch mobile)))
        (and (= (* (total-weight (branch-structure l))
                   (branch-length l))
                (* (total-weight (branch-structure r))
                   (branch-length r)))
             (mobile-balanced? (branch-structure l))
             (mobile-balanced? (branch-structure r))))))

(output '(mobile-balanced? mobile-x))

(output '(mobile-balanced? (make-mobile (make-branch 4 6)
                                        (make-branch 2 (make-mobile (make-branch 2 4)
                                                                    (make-branch 1 8))))))

; Redefine constructor
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

; New accessors:
(define left-branch car)
(define right-branch cdr)
(define branch-length car)
(define branch-structure cdr)

(output '(mobile-balanced? (make-mobile (make-branch 4 6)
                                        (make-branch 2 (make-mobile (make-branch 2 4)
                                                                    (make-branch 1 8))))))


(exercise "2.30")
; Square the elements of a tree with and without using map

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(output '(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

(output '(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))


(exercise "2.31")
; Abstract previous solution to get tree-map

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))
(output '(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))


(exercise "2.32")
; Determine all the subsets of a set

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (set)
                            (cons (car s) set))
                          rest)))))

(output '(subsets (list 1 2 3)))

