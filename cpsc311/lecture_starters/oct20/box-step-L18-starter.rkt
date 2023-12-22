#lang plai


;;
;; Stepping Mutable Boxes
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1
'{newbox 9} ;; Program

'{newbox 9}
'()

'{boxV loc0}
'((loc0 9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2
'{openbox {newbox 9}} ;; Program

'{openbox {newbox 9}}
'()

'{openbox {boxV loc0}}
'((loc0 9))

'9
'((loc0 9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3
'{with {x {newbox 9}}      ;; Program
       {seqn {setbox x 12}
             {openbox x}}}
       
'{with {x {newbox 9}}
       {seqn {setbox x 12}
             {openbox x}}}
'()

'{with {x {boxV loc0}}
       {seqn {setbox x 12}
             {openbox x}}}
'((loc0 9))

'{seqn {setbox {boxV loc0} 12}
       {openbox {boxV loc0}}}
'((loc0 9))

'{seqn 9
       {openbox {boxV loc0}}}
'((loc0 12))

'{openbox {boxV loc0}}
'((loc0 12))

'12
'((loc0 12))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 4
'{with {x {newbox 5}}  ;; Program
       {with {y {newbox 6}}
             {with {temp {+ {openbox x} {openbox y}}}
                   {seqn {setbox x temp}
                         {setbox y temp}}}}}

'{with {x {newbox 5}}
       {with {y {newbox 6}}
             {with {temp {+ {openbox x} {openbox y}}}
                   {seqn {setbox x temp}
                         {setbox y temp}}}}}
'()

'{with {x {boxV loc0}}
       {with {y {newbox 6}}
             {with {temp {+ {openbox x} {openbox y}}}
                   {seqn {setbox x temp}
                         {setbox y temp}}}}}
'((loc0 5))

'{with {y {newbox 6}}
       {with {temp {+ {openbox {boxV loc0}} {openbox y}}}
             {seqn {setbox {boxV loc0} temp}
                   {setbox y temp}}}}
'((loc0 5))

'{with {y {boxV loc1}}
       {with {temp {+ {openbox {boxV loc0}} {openbox y}}}
             {seqn {setbox {boxV loc0} temp}
                   {setbox y temp}}}}
'((loc1 6) (loc0 5))

'{with {temp {+ {openbox {boxV loc0}} {openbox {boxV loc1}}}}
       {seqn {setbox {boxV loc0} temp}
             {setbox {boxV loc1} temp}}}
'((loc1 6) (loc0 5))

'{with {temp {+ 5 {openbox {boxV loc1}}}}
       {seqn {setbox {boxV loc0} temp}
             {setbox {boxV loc1} temp}}}
'((loc1 6) (loc0 5))

'{with {temp {+ 5 6}}
       {seqn {setbox {boxV loc0} temp}
             {setbox {boxV loc1} temp}}}
'((loc1 6) (loc0 5))

'{with {temp 11}
       {seqn {setbox {boxV loc0} temp}
             {setbox {boxV loc1} temp}}}
'((loc1 6) (loc0 5))

'{seqn {setbox {boxV loc0} 11}
       {setbox {boxV loc1} 11}}
'((loc1 6) (loc0 5))

'{seqn 5
       {setbox {boxV loc1} 11}}
'((loc1 6) (loc0 11))

'{setbox {boxV loc1} 11}
'((loc1 6) (loc0 11))

'{setbox {boxV loc1} 11}
'((loc1 6) (loc0 11))

'6
'((loc1 11) (loc0 11))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 5
'{seqn {newbox 2}
       {newbox 3}}

'{seqn {newbox 2}
       {newbox 3}}
'()

'{seqn {boxV loc0}
       {newbox 3}}
'((loc0 2))

'{newbox 3}
'((loc0 2))

'{boxV loc1}
'((loc1 3) (loc0 2))