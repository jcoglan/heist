; Section 2.2
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-15.html

(load "helpers")
(define nil (list))
(define orig-map map)


(exercise "2.17")
; Return the last pair in a list

(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))

(output '(last-pair (list 23 72 149 34)))


(exercise "2.18")
; Reverse a list

(define (reverse seq)
  (if (null? seq)
      nil
      (append (reverse (cdr seq))
              (list (car seq)))))

(output '(reverse (list 1 4 9 16 25)))


(exercise "2.19")
; Change counter parameterized by lists of coins

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (first-denomination coins)
  (car coins))

(define (except-first-denomination coins)
  (cdr coins))

(define (no-more? coins)
  (null? coins))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(output '(cc 11 (reverse us-coins)))


(exercise "2.20")
; Find arguments with the same parity as the first

(define (same-parity x . rest)
  (let ((parity (remainder x 2)))
    (define (iter input)
      (if (null? input)
          nil
          (let ((y (car input))
                (rest (iter (cdr input))))
            (if (= (remainder y 2) parity)
                (cons y rest)
                rest))))
    (cons x (iter rest))))

(output '(same-parity 1 2 3 4 5 6 7))
(output '(same-parity 2 3 4 5 6 7))


(exercise "2.21")
; Completing definitions of square-list

(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items))
            (square-list (cdr items)))))

(output '(square-list (list 1 2 3 4)))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

(output '(square-list (list 1 2 3 4)))


(exercise "2.23")
; Implement for-each

(define (for-each proc list)
  (if (null? list)
      #t
      (begin
        (proc (car list))
        (for-each proc (cdr list)))))

(for-each (lambda (x) (display x) (newline))
          (list 57 321 88))


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
      (append (fringe (car x))
              (fringe (cdr x)))
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


; Sequence operations

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(exercise "2.33")
; Basic accumulations

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x acc) (+ 1 acc)) 0 sequence))

(output '(map square (list 1 2 3 4)))
(output '(append (list 9 8 7) (list 4 5 6)))
(output '(length (list 1 2 3)))


(exercise "2.34")
; Horner's rule

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(output '(horner-eval 2 (list 1 3 0 5 0 1)))


(exercise "2.35")
; Write count-leaves as an accumulation

(define (count-leaves t)
  (accumulate + 0 (map (lambda (branch)
                         (if (pair? branch)
                             (count-leaves branch)
                             1))
                       t)))

(output '(count-leaves (list 1 2 (list 3 4 5) 6 (list (list 7 8) 9))))


(exercise "2.36")
; Accumulation using zip-like list of lists

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(output '(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))) ; => (22 26 30)


(exercise "2.37")
; Matrix operations

(define map orig-map)
(define Mx '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define Vx '(7 3 2 1))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row)
         (dot-product row v))
       m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col)
                  (dot-product row col))
                cols))
         m)))

(output '(dot-product Vx Vx))
(output '(matrix-*-vector Mx Vx))
(output '(transpose Mx))
(output '(matrix-*-matrix Mx (transpose Mx)))
(output '(matrix-*-matrix (transpose Mx) Mx))


(exercise "2.38")
; Right- and left-fold

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(output '(fold-right / 1 (list 1 2 3)))       ; => 1,5
(output '(fold-left / 1 (list 1 2 3)))        ; => 0.166666
(output '(fold-right list nil (list 1 2 3)))  ; => (1 (2 (3 ())))
(output '(fold-left list nil (list 1 2 3)))   ; => (((() 1) 2) 3)


(exercise "2.39")
; Reverse a list using folds

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(output '(reverse (list 1 2 3)))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(output '(reverse (list 1 2 3)))

