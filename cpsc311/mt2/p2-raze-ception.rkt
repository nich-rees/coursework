#lang plai

;; Problem 2: RazeCeption

;; Consider the following program at STEP0.  This program doesn't terminate.
;; Step it until it arrives at a step that you've seen before.


(define STEP0
  '{with {f {fun {g}
                 {match/handle {g 9}
                               [y y]
                               [{raze boing h} {h g}]}}}
         {f {fun {x} {raze boing f}}}})

(define STEP1 (error "REPLACE error WITH A STEP"))

(define STEP2 (error "REPLACE error WITH A STEP"))

(define STEP3 (error "REPLACE error WITH A STEP"))
