#lang plai


;; PROBLEM 1:  Let's Have Some Fun Throwing

;; Consider the following program at STEP0.  This program terminates.
;; Step this program until it produces an answer.

;; here is a bullet you can cut and paste if necessary: •
;; or you can write \bullet and hit Ctrl-\ after the 't'.


(define STEP0
  '{with {x {letcc {k}
                   {fun {v} {throwcc k v}}}}
         {x {fun {q} 7}}})

(define STEP1 (error "REPLACE error WITH A STEP"))

(define STEP2 (error "REPLACE error WITH A STEP"))

(define STEP3 (error "REPLACE error WITH A STEP"))
