#lang plai
(print-only-errors)

;; Continuation is one of:
;; - `empty-k
;; - `(extend-k ,Continuation ,Natural)
;; interp.  representation of the rest of a computation


;; Continuation Natural -> Natural
(define (apply-k k n)
  (match k
    [`empty-k n] 
    [`(extend-k ,k^ ,n^)
     (apply-k k^ (* n^ n))]))

(define empty-k `empty-k)
(define (extend-k k n)
  `(extend-k ,k ,n))

;; Natural -> Natural
;; compute the result of factorial on n
(define (fact n) (fact-cps n empty-k))


;; Natural Continuation -> Natural
;; Accumulator: k is Continuation 
;; Invariant: k encodes the rest of the computation as a procedure.
;;   applying k to a value passes the value to the rest of the computation
(define (fact-cps n k)
  (cond
    [(zero? n) (apply-k k 1)]
    [else (fact-cps (- n 1) (extend-k k n))]))


(test (fact 5) 120)

