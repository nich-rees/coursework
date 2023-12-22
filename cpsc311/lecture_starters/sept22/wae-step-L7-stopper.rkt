#lang plai

;;
;; Hand-Stepping WAE programs
;;

;; Identifier Concepts:
;; 1) each identifier in an expression occurs "in binding position"
;;    or "in reference position"

;; (occurrences are described left to right)

'x  ;; x occurs in reference position in this WAE expression

'{with {y 7} x} ;; y occurs in binding position
                ;; x occurs in reference position

'{with {y x} y} ;; y occurs in binding position
                ;; x occurs in reference position
                ;; y occurs in reference position

'{with {x 7} {with {y x} z}} ;; x occurs in binding position
                             ;; y occurs in binding position
                             ;; x occurs in reference position
                             ;; z occurs in reference position

;; 2) of the identifiers that occur in reference position, each occurs
;;    either "free" or "bound" in the surrounding expression

'x  ;; x occurs free in this WAE expression

'{with {y 7} x} ;; x occurs free 
                ;; y occurs neither free nor bound 

'{with {y x} y} ;; x occurs free 
                ;; y occurs bound


'{with {x 7} {with {y x} z}} ;; x occurs bound
                             ;; z occurs free,
                             ;; y occurs neither free nor bound


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define WAES1 '{with {x 4} x})
'{with {x 4} x}
;; if the expression to be bound to the identifier can step, step it;
;; if not, then substitute the value (to be bound) for every free occurrence of
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
20

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define WAES4 '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})
'{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define WAES5 '{with {x 5} {+ x {with {x 3} 10}}})
'{with {x 5} {+ x {with {x 3} 10}}}


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
10


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define WAES8 'x)
'x
;; ERROR: x is unbound

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define WAES9 '{+ x y})
'{+ x y}
;; ERROR: x is unbound





