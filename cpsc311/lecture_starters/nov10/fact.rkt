#lang plai

;; Natural -> Natural
;; compute the result of factorial on n
(define (fact n)
  (cond
    [(zero? n) 1]
    [else (* n (fact (sub1 n)))]))

(test (fact 5) 120)