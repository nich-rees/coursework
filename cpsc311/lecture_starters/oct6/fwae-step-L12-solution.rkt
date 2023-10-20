#lang plai

;; Stepping FWAE programs, with lexical scope and dynamic scope



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 1
'{with {double {fun {x} {+ x x}}}
       {with {x 5} {double x}}}
'{with {x 5} {{fun {x} {+ x x}} x}}
'{{fun {x} {+ x x}} 5}
'{+ 5 5}
'10

;; NOW WITH DYNAMIC SCOPE!
'{with {double {fun {x} {+ x x}}}
       {with {x 5}
             {double x}}}
'{bind {double {fun {x} {+ x x}}}
       {with {x 5}
             {double x}}}
'{bind {double {fun {x} {+ x x}}}
       {bind {x 5}
             {double x}}}
'{bind {double {fun {x} {+ x x}}}
       {bind {x 5}
             {{fun {x} {+ x x}} x}}}
'{bind {double {fun {x} {+ x x}}}
       {bind {x 5}
             {{fun {x} {+ x x}} 5}}}
'{bind {double {fun {x} {+ x x}}}
       {bind {x 5}
             {bind {x 5}
                   {+ x x}}}}
'{bind {double {fun {x} {+ x x}}}
       {bind {x 5}
             {bind {x 5}
                   {+ 5 x}}}}
'{bind {double {fun {x} {+ x x}}}
       {bind {x 5}
             {bind {x 5}
                   {+ 5 5}}}}
'{bind {double {fun {x} {+ x x}}}
       {bind {x 5}
             {bind {x 5}
                   10}}}
'{bind {double {fun {x} {+ x x}}}
       {bind {x 5}
             10}}
'{bind {double {fun {x} {+ x x}}}
       10}
'10


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 1b

'{7 9}
;; Error: 7 is not a procedure

;; SAME WITH DYNAMIC SCOPE!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 2 
'{with {k {fun {x} {fun {y} x}}}
       {{k 7} 9}}
'{{{fun {x} {fun {y} x}} 7} 9}
'{{fun {y} 7} 9}
'7

;; NOW WITH DYNAMIC SCOPE!
'{with {k {fun {x} {fun {y} x}}}
       {{k 7} 9}}
'{bind {k {fun {x} {fun {y} x}}}
       {{k 7} 9}}
'{bind {k {fun {x} {fun {y} x}}}
       {{{fun {x} {fun {y} x}} 7} 9}}
'{bind {k {fun {x} {fun {y} x}}}
       {{bind {x 7}
              {fun {y} x}} 9}}
'{bind {k {fun {x} {fun {y} x}}}
       {{fun {y} x} 9}}
'{bind {k {fun {x} {fun {y} x}}}
       {bind {y 9}
             x}}
;; ERROR: Unbound identifier: x

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 3 - Room Warmer, Revisited
'{{fun {y} {y y}} {fun {x} {x x}}}
'{{fun {x} {x x}} {fun {x} {x x}}}
'{{fun {x} {x x}} {fun {x} {x x}}}
'{{fun {x} {x x}} {fun {x} {x x}}}
'{{fun {x} {x x}} {fun {x} {x x}}}
;; ...


;; NOW WITH DYNAMIC SCOPE!
'{{fun {y} {y y}} {fun {x} {x x}}}
'{bind {y {fun {x} {x x}}}
       {y y}}
'{bind {y {fun {x} {x x}}}
       {{fun {x} {x x}} y}}
'{bind {y {fun {x} {x x}}}
       {{fun {x} {x x}} {fun {x} {x x}}}}
'{bind {y {fun {x} {x x}}}
       {bind {x {fun {x} {x x}}}
             {x x}}}
'{bind {y {fun {x} {x x}}}
       {bind {x {fun {x} {x x}}}
             {{fun {x} {x x}} x}}}
'{bind {y {fun {x} {x x}}}
       {bind {x {fun {x} {x x}}}
             {{fun {x} {x x}} {fun {x} {x x}}}}}
;; ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 4 - scope tester
'{with {y 9}
       {{with {y 7}
              {fun {x} y}} 8}}
'{{with {y 7}
        {fun {x} y}} 8}

'{{fun {x} 7} 8}
'7


;; NOW WITH DYNAMIC SCOPE!
'{with {y 9}
       {{with {y 7}
              {fun {x} y}} 8}}
'{with {y 9}
       {{with {y 7}
              {fun {x} y}} 8}}

'{bind {y 9}
       {{with {y 7}
              {fun {x} y}} 8}}

'{bind {y 9}
       {{bind {y 7}
              {fun {x} y}} 8}}

'{bind {y 9}
       {{fun {x} y} 8}}  ;; Uh-Oh!

'{bind {y 9}
       {bind {x 8}
             y}}


'{bind {y 9}
       {bind {x 8}
             9}}

'{bind {y 9}
       9}

'9

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 5 : Yes this is a legal program 
'{+ {fun {x} x} 9}
;; Error: {fun {x} x} is not a number

;; SAME WITH DYNAMIC SCOPE! 


;; THIS IS NOT A LEGAL FWAE PROGRAM
'{with {+ {fun {y} y}}
       {+ {fun {x} x} 9}}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program 6 : Another legal program
'{if0 {fun {x} x}
      8
      0}
'0

;; SAME WITH DYNAMIC SCOPE! 



