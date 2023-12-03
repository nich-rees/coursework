#lang plai

;;
;; Hold on a sec, I think I might throw up...
;;

;; PROBLEM 1.
;; Consider the following program at STEP0.  The program takes three steps to
;; run.  Step this program for three steps.

;; here is a bullet you can cut and paste if necessary: •
;; or you can write \bullet and hit Ctrl-\ after the 't'.

  
(define STEP0
  '{letcc {k}
          {throwcc {letcc {q}
                          {throwcc k q}}
                   4}})

(define STEP1 '{throwcc {letcc {q}
                               {throwcc {kont •} q}}
                        4})

(define STEP2 '{throwcc {letcc {q}
                               {throwcc {kont •} q}}
                        4})

(define STEP3 '{letcc {q}
                               {throwcc 4 q}})
