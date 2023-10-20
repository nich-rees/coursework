#lang plai
(define (... . args) (cons '... args))
(print-only-errors)

;; Problem 2 - Step Down



;; Consider the following FWAE program:
;;'{with {down {fun {x}
;;                  {if0 x
;;                       0
;;                       {down {- x 1}}}}}
;;       {down 1}}


;; You are to hand step this program using two different scoping disciplines.
;; If the program terminates in a value, then that value should be the
;; last step.  If the program terminates in an error, define the last step
;; to be the string "error".


;; Problem 2a: hand step the program using lexical scoping semantics.
;; Write each step as a definition of P2A-STEP-N, incrementing
;; N by one at each step until the program terminates.
;; PROCEED FOR AT MOST 10 STEPS (i.e. up to P2A-STEP-10).  Full evaluation may
;; take more or fewer than 10 steps.

(define P2A-STEP-0
  '{with {down {fun {x}
                    {if0 x
                         0
                         {down {- x 1}}}}}
         {down 1}})

(define P2A-STEP-1
  '{{fun {x}
         {if0 x
              0
              {down {- x 1}}}}
    1})

(define P2A-STEP-2
  '{if0 1
        0
        {down {- 1 1}}})

(define P2A-STEP-3
  '{down {- 1 1}})

(define P2A-STEP-4 "error")


;; Problem 2b: hand step the program using *dynamic* scoping semantics.
;; Write each step as a definition of P2B-STEP-N, incrementing
;; N by one at each step until the program terminates.
;; PROCEED FOR AT MOST 10 STEPS (i.e. up to P2B-STEP-10).  Full evaluation may
;; take more or fewer than 10 steps.

(define P2B-STEP-0
  '{with {down {fun {x}
                    {if0 x
                         0
                         {down {- x 1}}}}}
         {down 1}})

(define P2B-STEP-1
  '{bind {down {fun {x}
                    {if0 x
                         0
                         {down {- x 1}}}}}
         {down 1}})

(define P2B-STEP-2
  '{bind {down {fun {x}
                    {if0 x
                         0
                         {down {- x 1}}}}}
         {{fun {x}
               {if0 x
                    0
                    {down {- x 1}}}} 1}})


(define P2B-STEP-3
  '{bind {down {fun {x}
                    {if0 x
                         0
                         {down {- x 1}}}}}
         {if0 1
              0
              {down {- 1 1}}}})

(define P2B-STEP-4
  '{bind {down {fun {x}
                    {if0 x
                         0
                         {down {- x 1}}}}}
         {down {- 1 1}}})

(define P2B-STEP-5
  '{bind {down {fun {x}
                    {if0 x
                         0
                         {down {- x 1}}}}}
         {down 0}})

(define P2B-STEP-6
  '{bind {down {fun {x}
                    {if0 x
                         0
                         {down {- x 1}}}}}
         {{fun {x}
               {if0 x
                    0
                    {down {- x 1}}}} 0}})

(define P2B-STEP-7
  '{bind {down {fun {x}
                    {if0 x
                         0
                         {down {- x 1}}}}}
         {{if0 0
               0
               {down {- 0 1}}}} 0})

(define P2B-STEP-9
  '{bind {down {fun {x}
                    {if0 x
                         0
                         {down {- x 1}}}}}
         0})

(define P2B-STEP-10 '0)

;; NOOO I forgot that function calls are binds too