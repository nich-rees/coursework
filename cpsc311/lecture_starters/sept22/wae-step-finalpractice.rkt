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
'4


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define WAES2 '{with {x 4} {+ x x}})
'{with {x 4} {+ x x}}
'{+ 4 4}
'8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define WAES3 '{with {x {+ 5 5}} {+ x x}})
'{with {x {+ 5 5}} {+ x x}}
'{with {x 10} {+ x x}}
'{+ 10 10}
'20

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define WAES4 '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})
'{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}
'{with {x 10} {with {y {- x 3}} {+ y y}}}
'{with {y {- 10 3}} {+ y y}}
'{with {y 7} {+ y y}}
'{+ 7 7}
'14

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define WAES5 '{with {x 5} {+ x {with {x 3} 10}}})
'{with {x 5} {+ x {with {x 3} 10}}}
'{+ 5 {with {x 3} 10}}
'{+ 5 10}
'15

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define WAES6 '{with {x 5} {+ x {with {x 3} x}}})
'{with {x 5} {+ x {with {x 3} x}}}
'{+ 5 {with {x 3} x}}
'{+ 5 3}
'8


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define WAES7 '{with {x 5} {+ x {with {y 3} x}}})
'{with {x 5} {+ x {with {y 3} x}}}
'{+ 5 {with {y 3} 5}}
'{+ 5 5}
'10