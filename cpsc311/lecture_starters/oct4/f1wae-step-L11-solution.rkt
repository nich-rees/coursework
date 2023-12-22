#lang plai

;; Stepping F1WAE programs (F1WAE with Lexical Scope)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 1 
'{if0 {+ 1 -1} {+ 9 1} 7}
'{if0 0 {+ 9 1} 7}
'{+ 9 1}
'10

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 2
'{- {if0 {+ 1 -2} 9 7} 2}
'{- {if0 -1 9 7} 2}
'{- 7 2}
'5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 3
'{define-fn {double x} {+ x x}}
'{with {x 5} {double x}}
'{double 5}
'{+ 5 5}
'10

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 4 
'{define-fn {even x} {if0 x 1 {odd {- x 1}}}}
'{define-fn {odd x} {if0 x 0 {even {- x 1}}}}
'{even 3}
'{if0 3 1 {odd {- 3 1}}}
'{odd {- 3 1}}
'{odd 2}
'{if0 2 0 {even {- 2 1}}}
'{even {- 2 1}}
'{even 1}
'{if0 1 1 {odd {- 1 1}}}
'{odd {- 1 1}}
'{odd 0}
'{if0 0 0 {even {- 0 1}}}
'0



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 5 - whoops
'{define-fn {room-warmer x} {room-warmer x}}
'{room-warmer 7}
'{room-warmer 7}
'{room-warmer 7}
'{room-warmer 7}
'{room-warmer 7}
'{room-warmer 7}
'{room-warmer 7}
;; repeats forever


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 6 - separate name spaces ***
'{define-fn {f x} x}
'{with {f 7}
       {f f}}
'{f 7}
'7



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 7 - scope tester ***
'{define-fn {f x} y}
'{with {y 7}
       {f 6}}
'{f 6}
'y
;; Error: Unbound identifier


