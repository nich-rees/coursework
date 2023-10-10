#lang plai

;;
;; Hand-Stepping WAE programs
;;



'x  ;; x occurs free in this WAE expression

'{with {y 7} x} ;; x occurs free in this WAE expression

'{with {y x} y} ;; x occurs free in this WAE expression

'{with {x 7} {with {y x} z}} ;; x occurs bound, z occurs free, y is bound
;; but does not occur


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define WAES1 '{with {x 4} x})
'{with {x 4} x}
;; if the expression to be bound to the identifier can step, step it;
;; if not, then substitute the value (to be bound) for every free occurence of
;; the identifier (that binds the value) in the body of the with
'4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define WAES2 '{with {x 4} {+ x x}})
'{with {x 4} {+ x x}}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define WAES3 '{with {x {+ 5 5}} {+ x x}})
'{with {x {+ 5 5}} {+ x x}}
'{with {x 10} {+ x x}}
'{+ 10 10}
'20

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define WAES4 '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})
'{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define WAES5 '{with {x 5} {+ x {with {x 3} 10}}})
'{with {x 5} {+ x {with {x 3} 10}}}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define WAES6 '{with {x 5} {+ x {with {x 3} x}}})
'{with {x 5} {+ x {with {x 3} x}}}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define WAES7 '{with {x 5} {+ x {with {y 3} x}}})
'{with {x 5} {+ x {with {y 3} x}}}
'{+ 5 {with {y 3} 5}} ; the second x was free because the with was not binding x
'{5 5}
'10

;; NEW PROGRAM, NEW BEHAVIOUR
'{with {x 5} {+ x {with {x 3} x}}}
'{+ 5 {with {x 3} x}} ; the x in reference pos is already bound, but will change
'{+ 5 3}
'8

;; TWO MORE PROGRAMS
'x
;; ERROR: x is unbound

;; ONE MORE PROGRAM
'{+ x y}
;; ERROR: x is unbound