#lang plai
(print-only-errors)
(define ... list)

;; Natural -> Natural
;; compute the result of factorial on n
(define (fact n0)
  ;; Accumulator: k is Natural -> Natural
  ;; Invariant: represents the rest of the computation from here
  ;;            until (fact n0) is computed.
  (local [(define (fact/cps n k)
            (cond
              [(zero? n) (k 1)]              
              [else
               (let ([k^ (λ (v) (k (* n v)))])
                 (fact/cps (sub1 n) k^))]))]
    (fact/cps n0 (λ (v) v))))

(test (fact 5) 120)