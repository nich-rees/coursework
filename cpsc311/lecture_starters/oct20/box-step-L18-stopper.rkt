#lang plai


;;
;; Stepping Mutable Boxes
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1
'{newbox 9} ;; Program

'{newbox 9} ;; Initial Step of Stepping
'()         ;; the empty store

'{boxV loc0} ;; a box value representing a new location in the store
'((loc0 9))  ;; the store, which binds locations to storable objects (values)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2
'{openbox {newbox 9}} ;; Program

'{openbox {newbox 9}}
'()

'{openbox {boxV loc0}}
'((loc0 9))

;; openbox evaluates its argument
;; if the result is not a boxV signal an error
;; if it is a boxV, produce the value its location points to in the store
'9
'((loc0 9))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3
'{with {x {newbox 9}}      ;; Program
       {seqn {setbox x 12}
             {openbox x}}}
       
'{with {x {newbox 9}}      ;; Program
       {seqn {setbox x 12}
             {openbox x}}}

'()

'{with {x {boxV loc0}}      ;; Program
       {seqn {setbox x 12}
             {openbox x}}}

'((loc0 9))


'{seqn {setbox {boxV loc0} 12}
      {openbox {boxV loc0}}}
'((loc0 9))

;; seqn evaluates the first position to a value
;; and then throws it away and runs the second position

'{seqn {setbox {boxV loc0} 12}
       {openbox {boxV loc0}}}
'((loc0 9))

;; setbox evaluates its first argument, and expects a boxV back
;; then evaluates its second argument to any value
;; then replaces the value in the store with the second argument's value
;; and produces the previous value from the store

'{seqn 9
       {openbox {boxV loc0}}}
'((loc0 12))

'{openbox {boxV loc0}}
'((loc0 12))
;; openbox evaluates its argument and expects a boxV back
;; then produces the value in the store

'12
'((loc0 12))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 4
'{with {x {newbox 5}}  ;; Program
       {with {y {newbox 6}}
             {with {temp {+ {openbox x} {openbox y}}}
                   {seqn {setbox x temp}
                         {setbox y temp}}}}}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 5
'{seqn {newbox 2}
       {newbox 3}}



