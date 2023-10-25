#lang plai




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mutable Variables (Binding "by-value")

'{with {x 5}
       {with {y x}
             {seqn {setvar y 7}
                   x}}}

'{with {x 5}
       {with {y x}
             {seqn {setvar y 7}
                   x}}}
'()

;; substitute a "variable location" for all free references to the bound
;; variable in the body of the the with expression
'{with {y {varL loc0}}
       {seqn {setvar y 7}
             {varL loc0}}}
'((loc0 5))
;; identifiers now denote location instead of values

;; to step a varL (i.e. a variable location), replace it with the current
;; value assigned to it in the store
'{with {y 5}
       {seqn {setvar y 7} ; setvar must be followed by a variable name
             {varL loc0}}}
'((loc0 5))

;; replace the current value assigned to the relevant location in the store
;; with the given value, and produce the previously stored value
'{seqn {setvar {vaL loc1} 7}
       {varL loc0}}
'((loc1 7) (loc0 5))

'{seqn 5
       {varL loc0}}}
'((loc1 7) (loc0 5))

'{varL loc0}
'((loc1 7) (loc0 5))

'5
'((loc1 7) (loc0 5))

;; if you change y, don't change x: no aliasing like before
;; ... but will see maybe later with lambdas can get with first class fns

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mutable Variables (Binding "by-reference")

'{with {x 5}
       {with {y x}
             {seqn {setvar y 7}
                   x}}}

;; same as before:

'{with {x 5}
       {with {y x}
             {seqn {setvar y 7}
                   x}}}
'()

;; substitute a "variable location" for all free references to the bound
;; variable in the body of the the with expression
'{with {y {varL loc0}}
       {seqn {setvar y 7}
             {varL loc0}}}
'((loc0 5))

;; different:

'{seqn {setvar {varL loc0} 7}
       {varL loc0}}
'((loc0 5))
;; new rule for with: if next expression is a location?? that already have
;; just put location, otherwise new location??

'{seqn 5
       {varL loc0}}
'((loc0 7))
;; returns current loc0, plai actually returns (void)
;; > (define x 7)
;; > (set! x 10)
;; would be (void) ???
;; he actually prefers this I think? something about functional vs imperative
;; and handling side effects

'{varL loc0}
'((loc0 7))

'7
'((loc0 7))


;; odd that we say "call" when there is no function, but because with
;; typically has similar semantics as function call... except
;; for scripting languages, but bc they're too old