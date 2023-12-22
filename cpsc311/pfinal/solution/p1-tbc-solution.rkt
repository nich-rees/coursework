#lang plai

;;
;; To Be Continued, Continued
;;

;; PROBLEM 1.
;; Consider the following Li'l Dan program.  The program takes four steps to
;; run.  Step this program for four steps.

;; here is a bullet you can cut and paste if necessary: •
;; or you can write \bullet and hit Ctrl-\ after the 't'.

(define STEP0
  '{if0 {if0 {letcc {k}
               {if0 {throwcc k 0}
                    unbound
                    wow-also-unbound}}
             10
             11}
        17
        42})

#;
(define STEP1 (error "REPLACE error WITH A STEP"))
#;
(define STEP2 (error "REPLACE error WITH A STEP"))
#;
(define STEP3 (error "REPLACE error WITH A STEP"))
#;
(define STEP4 (error "REPLACE error WITH A STEP"))


(define STEP1
  '{if0 {if0 {if0 {throwcc {kont {if0 {if0 •
                                           10
                                           11}
                                      17
                                      42}}
                           0}
                  unbound
                  wow-also-unbound}
             10
             11}
        17
        42})


(define STEP2
  '{if0 {if0 0
             10
             11}
        17
        42})


(define STEP3
  '{if0 10
        17
        42})


(define STEP4
  '42)
