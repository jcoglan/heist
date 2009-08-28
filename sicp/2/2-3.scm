; Section 2.2.3
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-15.html#%_sec_2.2.3

(load "../helpers")
(define nil (list))
(define orig-map map)


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


; Procedures for nested mappings

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))


(exercise "2.40")
; Generate lists of unique pairs

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(output '(unique-pairs 5))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(output '(prime-sum-pairs 5))


(exercise "2.41")
; Write a procedure to find all ordered triples of distinct
; positive integers i, j, and k less than or equal to a given
; integer n that sum to a given integer s.

(define (list-sum list)
  (accumulate + 0 list))

(define (ordered-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list k j i))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(output '(ordered-triples 6))

(define (triples-with-sum s n)
  (filter (lambda (triple) (= (list-sum triple) s))
          (ordered-triples n)))

(output '(triples-with-sum 9 6))


(exercise "2.42")
; The Eight Queens puzzle

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k positions)
  (append positions (list new-row)))

(define empty-board nil)

(define (safe? k positions)
  (if (> k (length positions))
      #t
      (let ((q (list-ref positions (- k 1))))
        (define (iter rest i safe)
          (if (or (null? rest) (not safe))
              safe
              (let ((p (list-ref positions (- i 1))))
                (iter (cdr rest)
                      (+ i 1)
                      (or (not (or (= p q)
                                   (= (abs (- p q)) (abs (- i k)))))
                          (= i k))))))
        (iter positions 1 #t))))

(output '(queens 1))
(output '(queens 2))
(output '(queens 3))
(output '(queens 4))

