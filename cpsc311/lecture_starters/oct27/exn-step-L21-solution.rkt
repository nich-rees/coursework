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
'{raze oops 5}  ;; does not step, *but* this is NOT a value
                ;; we call this (and values) "canonical forms"  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 2
'5  ;; is a value, and is also a canonical form


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 3
'{match/handle {+ 7 3}
  [x {+ x 9}]
  [{raze oops x} {+ x 12}]}

;; to step a match/handle expression, first step, if possible, the argument
;; expression
'{match/handle 10
  [x {+ x 9}]
  [{raze oops x} {+ x 12}]}

;; ... if the argument expression is canonical, and is a value, step to the
;; result of substituting the value for the bound identifier in
;; the first (i.e. value) branch of the handler

'{+ 10 9}
'19



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 4

'{match/handle {+ {raze oops 7} {+ 3 3}}
  [x {+ x 9}]
  [{raze oops x} {+ x 12}]}

;; if a raze expression is in step position, and its payload is a value,
;; then so long as the next case doesn't apply, the raze expression eats its
;; immediately surrounding expression.

'{match/handle {raze oops 7}
  [x {+ x 9}]
  [{raze oops x} {+ x 12}]}

;; if the surroundings of a raze expression is a match/handle expression, whose
;; raze branch has a tag that matches the raze expression, then
;; substitute the "payload" (i.e. the value inside the raze expression) for
;; free instances of the identifier bound by the raze pattern
'{+ 7 12}
'19

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 5
'{match/handle {+ {+ 3 4} {+ {raze oops 7} 3}}
  [x {+ x 9}]
  [{raze oops x} {+ x 12}]}

'{match/handle {+ 7 {+ {raze oops 7} 3}}
  [x {+ x 9}]
  [{raze oops x} {+ x 12}]}

'{match/handle {+ 7 {raze oops 7}}
  [x {+ x 9}]
  [{raze oops x} {+ x 12}]}

'{match/handle {raze oops 7}
  [x {+ x 9}]
  [{raze oops x} {+ x 12}]}

'{+ 7 12}

'19

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 6
'{match/handle 
  {match/handle {+ {raze oops 7} 3}
   [x {+ x 9}]
   [{raze oops x} {+ x 12}]}
  [y {+ -5 y}]
  [{raze doh y} y]}

'{match/handle 
  {match/handle {raze oops 7}
   [x {+ x 9}]
   [{raze oops x} {+ x 12}]}
  [y {+ -5 y}]
  [{raze doh y} y]}

'{match/handle 
  {+ 7 12}
  [y {+ -5 y}]
  [{raze doh y} y]}

'{match/handle 19
  [y {+ -5 y}]
  [{raze doh y} y]}

'{+ -5 19}

'14

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 7
'{match/handle 
  {match/handle {+ {raze doh 7} 3}
   [x {+ x 9}]
   [{raze oops x} {+ x 12}]}
  [y {+ -5 y}]
  [{raze doh y} y]}

'{match/handle 
  {match/handle {raze doh 7}
   [x {+ x 9}]
   [{raze oops x} {+ x 12}]}
  [y {+ -5 y}]
  [{raze doh y} y]}

'{match/handle {raze doh 7}
  [y {+ -5 y}]
  [{raze doh y} y]}

'7


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 8
'{raze POW! {raze 'WHAMMO! 9}}
;; stepping the inner raze eats the outer one.
'{raze 'WHAMMO! 9}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 9
'{match/handle 
  {match/handle {raze oops 7}
         [x {+ x 9}]
         [{raze oops x} {raze doh 2}]}
  [y {+ -5 y}]
  [{raze doh y} y]}


'{match/handle {raze doh 2}
  [y {+ -5 y}]
  [{raze doh y} y]}

'2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 10
'{if0 9
      {raze 'yup {fun {z} z}}
      12}

'12
