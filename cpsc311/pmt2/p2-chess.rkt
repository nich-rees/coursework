#lang plai

;;
;; The Mystery of (Chess) Boxing
;;

;; PROBLEM 2.

;; For the following program assume a strict/call-by-value language like Hannah
;; that:
;; 1) *SUPPORTS* mutable boxes;
;; 2) *DOES NOT SUPPORT* mutable variables

;; Consider the following program at STEP0. The program takes five steps to run.
;; We have provided the first step and the last step.
;; Fill in the remaining three steps.


;; Each step should be a two-element list. The first element
;; should be the expression that the previous expression steps to.
;; The second element should be the new store resulting from that step.
;; Represent the store as a list of two-element lists,
;; each containing a symbol (denoting a location) and the value
;; for that location, e.g.
;; '((loc1 3) (loc2 9) (loc3 7))

  
(define STEP0
  (list
   '{with {x {pair 36 {newbox 0}}}
          {seqn {setbox {second x} x}
                x}}
   '()))

(define STEP1
  (list
   '{with {x {pair 36 {boxV loc1}}}
          {seqn {setbox {second x} x}
                x}}
   '((loc1 0))))

(define STEP2
  (list
   '{seqn {setbox {second {pair 36 {boxV loc1}}} {pair 36 {boxV loc1}}}
          {pair 36 {boxV loc1}}}
   '((loc1 0))))

(define STEP3
  (list
   '{seqn {setbox {boxV loc1}} {pair 36 {boxV loc1}}
          {pair 36 {boxV loc1}}}
   '((loc1 0))))

(define STEP4
  (list
   '{pair 36 {boxV loc1}} ;; should be the returned value
   '(loc1 {pair 36 {boxV loc1}})))

(define STEP5
  (list
   '{pair 36 {boxV loc1}}
   '((loc1 {pair 36 {boxV loc1}}))))
