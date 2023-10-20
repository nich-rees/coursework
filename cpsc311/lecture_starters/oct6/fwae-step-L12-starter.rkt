#lang plai

;; Stepping FWAE programs, with lexical scope and dynamic scope



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 1
'{with {double {fun {x} {+ x x}}}
       {with {x 5} {double x}}}1

'{with {x 5} {{fun {x} {+ x x}} x}}
'{{fun {x} {+ x x}} 5} ;; the x in the fun is not free, binding pos., like with
'{+ 5 5}
       
;; NOW WITH DYNAMIC SCOPE!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 1b

'{7 9}
;; Error: 7 is not a procedure (legal program in FWAE, not useful)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 2 
'{with {k {fun {x} {fun {y} x}}}
      {{k 7} 9}}

;; NOW WITH DYNAMIC SCOPE!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 3 - Room Warmer, Revisited
'{{fun {y} {y y}} {fun {x} {x x}}}


;; NOW WITH DYNAMIC SCOPE!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 4 - scope tester
'{with {y 9}
       {{with {y 7}
              {fun {x} y}} 8}}


;; NOW WITH DYNAMIC SCOPE!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 5 : Yes this is a legal program 
'{+ {fun {x} x} 9}

;; Error: {fun {x} x} is not a number

;; THIS IS NOT A LEGAL FWEA PROGRAM:
'{with {+ {fun {y} y}}
       {+ {fun {x} x} 9}} ;; would our parser work, I think that's the question?

;; NOW WITH DYNAMIC SCOPE!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 6 : Another legal program
'{if0 {fun {x} x}
      8
      0}
'0

;; NOW WITH DYNAMIC SCOPE! 



