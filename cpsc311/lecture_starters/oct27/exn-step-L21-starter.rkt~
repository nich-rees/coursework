#lang plai

;;
;; Stepping Programs with Exceptions
;;

;; New features

;; Pattern matching-style exception handler:
;; {match/handle <TEL>
;;  [<id> <TEL>]
;;  [{raze <tag> <id>} <TEL>]}

;; Raise an exception:
;; {raze <tag> <TEL>}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 1
'{raze oops 5}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 2
'5


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 3
'{match/handle {+ 7 3}
  [x {+ x 9}]
  [{raze oops x} {+ x 12}]}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 4
'{match/handle {+ {raze oops 7} 3}
  [x {+ x 9}]
  [{raze oops x} {+ x 12}]}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 5
'{match/handle {+ {+ 3 4} {+ {raze oops 7} 3}}
  [x {+ x 9}]
  [{raze oops x} {+ x 12}]}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 6
'{match/handle 
  {match/handle {+ {raze oops 7} 3}
   [x {+ x 9}]
   [{raze oops x} {+ x 12}]}
  [y {+ -5 y}]
  [{raze doh y} y]}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 7
'{match/handle 
  {match/handle {+ {raze doh 7} 3}
   [x {+ x 9}]
   [{raze oops x} {+ x 12}]}
  [y {+ -5 y}]
  [{raze doh y} y]}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 8
'{raze POW! {raze 'WHAMMO! 9}}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 9
'{match/handle 
  {match/handle {raze oops 7}
         [x {+ x 9}]
         [{raze oops x} {raze doh 2}]}
  [y {+ -5 y}]
  [{raze doh y} y]}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 10
'{if0 9
      {raze 'yup {fun {z} z}}
      12}

