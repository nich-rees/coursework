#lang plai
(print-only-errors)

;; Continuation is Natural -> Natural
;; interp.  representation of the rest of a computation

;; Natural -> Natural
;; compute the result of factorial on n
(define (fact n) (fact-cps n (λ (v) v)))


;; Natural Continuation -> Natural
;; Accumulator: k is Continuation 
;; Invariant: k encodes the rest of the computation as a procedure.
;;   applying k to a value passes the value to the rest of the computation
(define (fact-cps n k)
  (cond
    [(zero? n) (k 1)]
    [else (fact-cps (- n 1) (λ (v) (k (* n v))))]))


(test (fact 5) 120)

         