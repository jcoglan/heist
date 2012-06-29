; Section 2.3.4
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-16.html#%_sec_2.3.4

(load "../helpers")


; Huffman encoding trees

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))


(exercise "2.67")
; Decoding a message

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(output '(decode sample-message sample-tree))


(exercise "2.68")
; Encoding a message

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree)
         (if (eq? (symbol-leaf tree) symbol)
             '()
             (error "Symbol not in tree" symbol)))
        ((memq symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((memq symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))))

(output '(encode '(A D A B B C A) sample-tree))


(exercise "2.69")
; Generate a Huffman tree from a list of weighted symbols

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define sample-symbols '((D 1) (B 2) (A 4) (C 1)))
(output '(make-leaf-set sample-symbols))

(define (successive-merge set)
  (cond ((null? set) '())
        ((= (length set) 1) (car set))
        (else (successive-merge
                (adjoin-set (make-code-tree (car set) (cadr set))
                            (cddr set))))))

(output 'sample-tree)
(output '(generate-huffman-tree sample-symbols))
      
      
(exercise "2.70")
; Huffman tree for 1950s rock songs

(define rock-symbols '((A 2) (BOOM 1) (GET 2) (JOB 2)
                       (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define rock-tree (generate-huffman-tree rock-symbols))

(define rock-song '(Get a job
                    Sha na na na na na na na na
                    Get a job
                    Sha na na na na na na na na
                    Wah yip yip yip yip yip yip yip yip yip
                    Sha boom))

(define rock-encoding (encode rock-song rock-tree))

(output 'rock-encoding)
(output '(length rock-encoding))

(define fixed-length-bits (ceiling (/ (log (length rock-symbols)) (log 2))))
(define fixed-length-encoding-size (* (length rock-song) fixed-length-bits))
(output 'fixed-length-encoding-size)


(exercise "2.71")

; n=5 => A:1, B:2, C:4, D:8, E:16
; 
;         /\
;        /  \
;       /    \
;     E:16   (A B C D):15
;             /\
;            /  \
;           /    \
;         D:8    (A B C):7
;                 /\
;                /  \
;               /    \
;             C:4    (A B):3
;                     /\
;                    /  \
;                   /    \
;                 B:2    A:1
; 
; These trees will always follow this structure, since the following is true:
; 
;     sum(0,n-1) 2^i = 2^n - 1
; 
; i.e. the sum of all the weights below the current maximum is always less than
; that maximum.
; 
; 1 bit is needed for the most frequent symbol, and n-1 are needed for the least
; frequent (A = 1111 above).

