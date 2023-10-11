#lang plai


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Recursion via fix

;; <FFWAE> ::= ...
;;          | {fix <identifier> <FFWAE>}
;;          | {fixFun <identifier> {<identifier>} <FFWAE>}

;; {fix x fwae} steps by (naively) substituting {fix x fwae} for x in fwae

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1: Warmup

;; Part a
'{with {x 7} 4}


;; Part b
'{fix f 4}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2: Room Warmer
'{fix f f}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3: Getting Warmer and Wider!
'{fix a {fix b a}}
;; came in late

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4: Recursive Anonymous Function
'{fix f {fun {x} {if0 x 9 {f {- x 1}}}}}

'{fun {x} {if0 x 9 {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} {- x 1}}}}
;; fix more fundamental concept than rec, which is what the textbook has
;; self reference more important than recursion

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 5: Recursive Function Call
;; "Others call me 'down', but *I* call myself 'f'!"
'{with {down {fix f {fun {x} {if0 x 9 {f {- x 1}}}}}}
       {down 1}}

'{{fix f {fun {x} {if0 x 9 {f {- x 1}}}}} 1}
'{{fun {x}
       {if0 x
            9
            {{fix f {fun {x} {if0 x 9 {f {- x 1}}}}}} {- x 1}}}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 6:  For Your Chewing Enjoyment
'{with {down {fix f {fun {x} {if0 x 9 {f {- x 1}}}}}}
      {+ {down 1} {down 0}}}

