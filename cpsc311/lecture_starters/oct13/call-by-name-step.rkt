#lang plai


;; Example 1: Call-by-value
'{with {x {+ 5 7}}
       {- x x}}

;; evaluate the bound expression...
'{with {x 12}
       {- x x}}

;;.. then perform substitution
'{- 12 12}

0


;; Example 2: Call-by-name evaluation (substitute the entire named expression!)

'{with {x {+ 5 7}}
       {- x x}}

;; IMMEDIATELY substitthe named expression into the body of the with for
;; every free instance of the bound identifier
'{- {+ 5 7} {+ 5 7}}
;; continue stepping as usual
'{-12 {+ 7}}
'{- 12 12}
0

;; Example 3: substitution blows away the bound expression without ever
;; evaluating it
'{with {x {+ 5 7}}
       {- 9 2}}

'{- 9 2}
'7


;; Example 4: Wait a minute, that means...
'{with {room-warmer {fix f f}}
       {- 9 2}}

;; call by name produces different result than call by value
'{- 9 2}
'7
; hmm different than our previous languages... how we do sub changes language


;; Call-by-name generalizes to *non-strict* evaluation semantics,
;; wherein in addition to binding expressions to identifiers,
;; constructor expressions do not evaluate their arguments: they are values!

'{pair {fix f f} {fix f f}}  ;; <-- this is a value (a pair of expressions)
;; pair is non-strict in *both* of its arguments
;; COULD HAVE SAID that pair is strict in its first argument,
;; and non-strict in its second argument (but we didn't)


;; this program, unlike the previous, *diverges* (runs forever)
'{left {pair {fix f f} {fix f f}}}
'{fix f f}

;;
;; Let's talk about our good friends fix and pairs and see how
;; call-by-name  (i.e. non-strict evaluation) treats them
;;

;; Here's call-by-value (aka strict aka eager) evaluation
'{fix f {pair 1 f}}

'{pair 1 {fix f {pair 1 f}}}
'{pair 1 {pair 1 {fix f {pair 1 f}}}}
'{pair 1 {pair 1 {pair 1 {fix f {pair 1 f}}}}}


;;...  and it keeps going.  This represents an *infinite list* of 1's!
;; Sometimes these infinite lists are called *streams*

;; Under call-by-name (non-strict) evaluation pairs do not evaluate their
;; arguments until needed
'{fix f {pair 1 f}}

'{pair 1 {fix f {pair 1 f}}}
;; and we're done


;; here's where it gets interesting
;; the following would diverge in call-by-value, but in call-by-name it
;; produces a value!
'{with {inf {fix f {pair 1 f}}}
       {left inf}}

'{left {fix f {pair 1 f}}}
;; while pair has non-strict semantics,left has *strict* semantics,
;; which means it evlautes its argument
'{left {pair 1 {fix f {pair 1 f}}}} ;; but once you get to pair, we're done
'1
;; see how pair helps a strict semantic in producing a value


;; let's look at with again
'{with {x {+ 5 7}}
       {- x x}}

;; The above with expression is equivalent to the following!
'{{fun {x} {- x x}} {+ 5 7}}

;; step the operator position until you get a value
;; if the value is not a fun, signal an error
;; otherwise substitute (without evaluating first) the argument into the body
;; for the function parameter
'{- {+ 5 7} {+ 5 7}}



;; Two nested withs:
'{with {y {- 10 55}}
       {with {x y}
             x}}

'{with {x {- 10 55}}
       x}
'{- 10 55}
'-45

;; As per our previous languages, a legal program can have free identifiers
'{with {y {- 10 x}}
       {with {x 7}
             y}}

;; NOOO
'{with {x 7}
       {- 10 x}}
'{- 10 7}
'3

;;YES:
'{with {g 7} ;; rename x to g
       {- 10 x}}
'{- 10 x}

;; ERROR unbound x.

;; OPTION 1: capture-avoiding substitution (not just for refactoring)
;; OPTION 2: restrict what counts as a program to programs that
;;           have no free identifiers ("closed).
;; an expression is called "closed" if it has no free identifiers

;; Most functional languages takes option 2,
;; if you just put a random identifer in plai, compile error not runtime error
;; Scheme did not have this feature