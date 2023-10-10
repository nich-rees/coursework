#lang plai

;; Stepping F1WAE-Dyn programs (F1WAE with Dynamic Scope)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 1 -  same as F1WAE
'{if0 {+ 1 -1} {+ 9 1} 7}
'{if0 0 {+ 9 1} 7}
'{+ 9 1}
'10

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 2 - same as F1WAE
'{- {if0 {+ 1 -2} 9 7} 2}
'{- {if0 -1 9 7} 2}
'{- 7 2}
'5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 3
'{define-fn {double x} {+ x x}}
'{with {x 5} {double x}}

'{bind {x 5} {double x}} ;; to step a bind, go inside the body of the bind
'{bind {x 5} {double 5}} ;; find the innermost binding of x surrounding it.
                         ;; and replace x with the bound value
;; procedure call: bind the argument to the call to the formal parameter of the
;;                 procedure in the body of the procedure
'{bind {x 5}
       {bind {x 5}
             {+ x x}}}
'{bind {x 5}
       {bind {x 5}
             {+ 5 x}}}
'{bind {x 5}
       {bind {x 5}
             {+ 5 5}}}
'{bind {x 5}
       {bind {x 5}
             10}}
;; if the body of a bind is a value, then hoist it out of the enclosing bind
'{bind {x 5}
       10}
'10

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 4 
'{define-fn {even x} {if0 x 1 {odd {- x 1}}}}
'{define-fn {odd x} {if0 x 0 {even {- x 1}}}}
'{even 3}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 5 - whoops
'{define-fn {room-warmer x} {room-warmer x}}
'{room-warmer 7}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 6 - separate name spaces ***
'{define-fn {f x} x}
'{with {f 7}
       {f f}}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 7 - scope tester ***
'{define-fn {f x} y}
'{with {y 7}
       {f 6}}

'{bind {y 7}
       {f 6}}
'{bind {y 7}
       {bind {x 6} y}}
'{bind {y 7}
       {bind {x 6} 7}} ;; found the innermost binding of y, and that's 7
'{bind {y 7}
       7}
'7