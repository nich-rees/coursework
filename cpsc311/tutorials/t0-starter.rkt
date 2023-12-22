#lang plai
(define (... . args) (cons '... args))
(print-only-errors #t)

;; Tutorial 0: Processing Binary Trees in PLAI


(define-type NumberTree
  [mt] ;; get it? em-tee, empty? :-/
  [node (num number?) (left NumberTree?) (right NumberTree?)])
;; interp. a binary tree of numbers

(define MT (mt))
(define MT2 (make-mt))
(define FOUR (node 4 (mt) (mt)))
(define FIVE (node 5 (mt) (mt)))
(define N1 (node 6 FOUR FIVE))
(define N2 (node 7
                 (node 8 (mt) (mt))
                 (node 9 (mt) (mt))))

(define N3 (node 10 N1 N2))

#;
(define (fn-for-ntree nt)
  (type-case NumberTree nt
    [mt () (...)]
    [node (n l r)
          (... n
               (fn-for-ntree l)
               (fn-for-ntree r))]))

;; Number -> NumberTree
;; produce a leaf node containing the given number
(define (leaf n) (node n (mt) (mt)))

(test (leaf 4) FOUR)
(test (leaf 5) FIVE)
(test (leaf 0) (node 0 (mt) (mt)))


;; NumberTree -> NumberTree
;; produce a tree with the same shape but doubled numbers
(define (double-tree nt)
  (type-case NumberTree nt
    [mt () (mt)]
    [node (n l r)
          (node (* 2 n)
                (double-tree l)
                (double-tree r))]))

(test (double-tree (mt)) (mt))
(test (double-tree FIVE) (node 10 (mt) (mt)))
(test (double-tree N2)
      (node 14
            (node 16 (mt) (mt))
            (node 18 (mt) (mt))))


;; Number Number -> Boolean
;; produces true if first number factors second number, else false
(define (factors f n) (= (modulo n f) 0))


;; NumberTree Number -> Number or false
;; produce a number from the tree for which the given number is a factor
;; or false if none
(define (has-factor nt factor)
  (type-case NumberTree nt
    [mt () false]
    [node (n l r)
          (cond [(factors factor n) n]
                [(has-factor l factor) => (λ (x) x)]
                [else (has-factor r factor)])]))

(test (has-factor MT 1) false)
(test (has-factor MT 0) false)
(test (has-factor FOUR 5) false)
(test (has-factor FOUR 4) 4)
(test (has-factor FOUR 2) 4)
(test (has-factor N1 4) 4)
(test (has-factor N1 5) 5)
(test (has-factor N3 2) 10)
(test (has-factor N3 4) 4)