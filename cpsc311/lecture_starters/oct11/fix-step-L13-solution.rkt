#lang plai


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Recursion via fix

;; <FFWAE> ::= ...
;;          | {fix <identifier> <FFWAE>}
;;          | {fixFun <identifier> {<identifier>} <FFWAE>}

;; {fix x fwae} steps by (naively) substituting {fix x ffwae} for x in fwae

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1: Warmup

;; Part a
'{with {x 7} 4}
'4

;; Part b
'{fix f 4}
'4



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2: Room Warmer
'{fix f f}
'{fix f f}
'{fix f f}
'{fix f f}
'...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3: Getting Warmer and Alternating
'{fix a {fix b a}}
'{fix b {fix a {fix b a}}}
'{fix a {fix b a}}
'{fix b {fix a {fix b a}}}
'{fix a {fix b a}}
'...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4: Recursive Anonymous Function
'{fix f {fun {x} {if0 x 9 {f {- x 1}}}}}
'{fun {x} {if0 x 9 {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- x 1}}}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 5: Recursive Function Call
;; "Others call me 'down', but *I* call myself 'f'!"
'{with {down {fix f {fun {x} {if0 x 9 {f {- x 1}}}}}}
       {down 1}}
'{with {down {fun {x}
                  {if0 x 9 {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- x 1}}}}}
       {down 1}}
'{{fun {x} {if0 x 9 {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- x 1}}}} 1}
'{if0 1 9 {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- 1 1}}}
'{{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- 1 1}}
'{{fun {x} {if0 x 9 {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- x 1}}}}
  {- 1 1}}
'{{fun {x} {if0 x 9 {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- x 1}}}}
  0}
'{if0 0 9 {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- 0 1}}}
'9


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 6:  For Your Chewing Enjoyment
'{with {down {fix f {fun {x} {if0 x 9 {f {- x 1}}}}}}
       {+ {down 1} {down 0}}}

'{with {down {fun {x}
                  {if0 x
                       9
                       {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- x 1}}}}}
       {+ {down 1} {down 0}}}

'{+ {{fun {x}
          {if0 x
               9
               {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- x 1}}}} 1}
    {{fun {x}
          {if0 x
               9
               {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- x 1}}}} 0}}

'{+ {if0 1
         9
         {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- 1 1}}}
    {{fun {x}
          {if0 x
               9
               {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- x 1}}}} 0}}

'{+ {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- 1 1}}
    {{fun {x}
          {if0 x
               9
               {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- x 1}}}} 0}}

'{+ {{fun {x} {if0 x
                   9
                   {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- x 1}}}} {- 1 1}}
    {{fun {x}
          {if0 x
               9
               {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- x 1}}}} 0}}

'{+ {{fun {x} {if0 x
                   9
                   {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- x 1}}}} 0}
    {{fun {x}
          {if0 x
               9
               {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- x 1}}}} 0}}

'{+ {if0 0
         9
         {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- 0 1}}}
    {{fun {x}
          {if0 x
               9
               {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- x 1}}}} 0}}

'{+ 9
    {{fun {x}
          {if0 x
               9
               {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- x 1}}}} 0}}

'{+ 9
    {if0 0
         9
         {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- 0 1}}}}
'{+ 9
    9}

'18