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
  (type-case number-tree nt
    [mt () (...)]
    [node (n l r)
          (... n
               (fn-for-ntree l)
               (fn-for-ntree r))]))

;; type-case on NumberTree compiles to this.
#;
(define (fn-for-ntree2 nt)
  (cond [(mt? nt)
         (local []
           (...))]         
        [(node? nt)
         (local [(define n (node-num nt))
                 (define l (node-left nt))
                 (define r (node-right nt))]
           (... n
                (fn-for-ntree2 l)
                (fn-for-ntree2 r)))]))

#;
(define (fn-for-ntree3 nt)
  (cond [(mt? nt)
         (let ()
           (...))]         
        [(node? nt)
         (let ([n (node-num nt)]
               [l (node-left nt)]
               [r (node-right nt)])
           (... n
                (fn-for-ntree3 l)
                (fn-for-ntree3 r)))]))



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
(define (has-factor nt factor)
  (type-case NumberTree nt
    [mt () false]
    [node (n l r)
          (if (factors factor n)              
              n
              (local [(define l-result (has-factor l factor))]
                (if l-result ;; PLAI does not need "not false?" trick
                    l-result
                    (has-factor r factor))))]))

(test (has-factor (mt) 7) false)  ; empty trees got nothin'
(test (has-factor (node 16 (mt) (mt)) 4) 16) ; simple tree has the factor
(test (has-factor (node 16 (mt) (mt)) 5) false) ; simple tree doesn't have it
(test (has-factor (node 16 FIVE (mt)) 5) 5) ; tree has it buried left
(test (has-factor (node 16 (mt) FIVE) 5) 5) ; tree has it buried right
(test (has-factor (node 15 (mt) FIVE) 5) 15) ; tree has two winners
;; NOTE, choosing the highest match in the tree isn't enough
(test (has-factor (node 16
                        (node 10 (mt) (mt))
                        FIVE) 5) 10)
;; go for height, then left, then right.


;; (listof (listof Number)) -> (listof Number)
;; combine a list of list of numbers into one list of numbers
#;
(define (merge lolon)
  (cond [(empty? lolon) empty]
        [else ;; cons
         (append  (first lolon)
                  (merge (rest lolon)))]))

(define (merge lolon)
  (foldr append empty lolon))

(test (merge empty) empty)
(test (merge (list empty empty)) empty)
(test (merge (list (list 1 2 3) empty (list 4 5 6))) (list 1 2 3 4 5 6))




