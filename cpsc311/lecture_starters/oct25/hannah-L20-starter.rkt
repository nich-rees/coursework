#lang plai
(require "../util/parsing.rkt")
(require "../util/test-match.rkt")
(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

;; Hannah -
;; A functional language with:
;; - arithmetic
;; - conditional expressions
;; - pairs/lists
;; - first-class functions
;; - ** mutable boxes **
;; - AND NOW ** mutable variables **

;; Named in honour of Canadian (formerly Vancouverite) musician Hannah Georgas

(define-type HG    
  [num (n number?)]
  [isnumz (e HG?)]
  [add (lhs HG?) (rhs HG?)]
  [sub (lhs HG?) (rhs HG?)]
  [id (name symbol?)]
  [setvar (name symbol?) (e HG?)] 
  [fun (param symbol?) (body HG?)]
  [isfunz (e HG?)]
  [app (rator HG?) (arg HG?)]
  [if0 (predicate HG?) (consequent HG?) (alternative HG?)]
  [fix (name symbol?) (body HG?)]
  [pair (left HG?) (right HG?)]
  [ispairz (e HG?)]
  [left (e HG?)]
  [right (e HG?)]
  [mt]
  [ismtz (e HG?)]
  [newbox (e HG?)]
  [isboxz (e HG?)]
  [setbox (e1 HG?) (e2 HG?)]
  [openbox (e HG?)])
;; interp. expressions in a language that supports applying first-class
;; functions. Its syntax is defined by the following BNF:
;; <HG> ::=
;; (ARITHMETIC)
;;          <num>
;;        | {isnumz <HG>}
;;        | {+ <HG> <HG>}
;;        | {- <HG> <HG>}
;; (IDENTIFIERS)
;;        | {with {<id> <HG>} <HG>}
;;        | <id>
;;        | {setvar <id> HG}     (NEW!)
;; (CONDITIONALS)
;;        | {if0 <HG> <HG> <HG>}
;; (FUNCTIONS)
;;        | {<HG> <HG>}
;;        | {fun {<id>} <HG>}
;;        | {isfunz <HG>}
;; (RECURSION)
;;        | {fix <id> <HG>}
;;        | {fixFun <id> <id> <HG>}
;; (PAIRS/LISTS)
;;        | {pair <HG> <HG>}
;;        | {ispairz <HG>}
;;        | {left <HG>}
;;        | {right <HG>}
;;        | {mt}
;;        | {ismtz}
;; (BOXES)
;;        | {newbox <HG>}
;;        | {isboxz <HG>}
;;        | {setbox <HG> <HG>}
;;        | {openbox <HG>}
;;        | {seqn <HG> <HG>}
;;
;; {fixFun f {x} body} ≡ {fix f {fun {x} body}})
;; {with {x named} body} ≡ {{fun {x} body} named}
;; {seqn expr1 expr2} ≡ {with {x0 expr1} expr2}
;;                where x0 does not occur free in expr2

;; with, fixFun, and seqn are syntactic sugar
(define (with x named body) (app (fun x body) named))
(define (fixFun f x body) (fix f (fun x body)))
(define (seqn expr1 expr2) (with (gensym) expr1 expr2))

;; Every AE program is a Hannah program
(define AE1 (num 4))
(define AE2 (add AE1 (num 5)))
(define AE3 (sub (num 6) (num 3)))


;; Every WAE program is a Hannah program
(define WAES4 '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})
(define WAE4 (with 'x (add (num 5) (num 5))
                   (with 'y (sub (id 'x) (num 3))
                         (add (id 'y) (id 'y)))))

(define WAES5 '{with {x 5} {+ x {with {x 3} 10}}})
(define WAE5 (with 'x (num 5) (add (id 'x) (with 'x (num 3) (num 10)))))

(define WAES6 '{with {x 5} {+ x {with {x 3} x}}})
(define WAE6 (with 'x (num 5) (add (id 'x) (with 'x (num 3) (id 'x)))))

(define WAES7 '{with {x 5} {+ x {with {y 3} x}}})
(define WAE7 (with 'x (num 5) (add (id 'x) (with 'y (num 3) (id 'x)))))


;; Every FWAE program is a Hannah program
(define FWAES1 '{fun {x} {+ x x}})
(define FWAE1 (fun 'x (add (id 'x) (id 'x))))

(define FWAES2 '{fun {x} {+ x 1}})
(define FWAE2 (fun 'x (add (id 'x) (num 1))))

(define FWAES3 '{fun {x} {+ x y}})
(define FWAE3 (fun 'x (add (id 'x) (id 'y))))

;; Every FFWAE program is a Hannah program
(define FFWAES1 '{fix f 4})
(define FFWAE1 (fix 'f (num 4)))
(define FFWAES2 '{fix f f})
(define FFWAE2 (fix 'f (id 'f)))
(define FFWAES3 '{fix a {fix b a}})
(define FFWAE3 (fix 'a (fix 'b (id 'a))))
(define FFWAES4
  '{with {down {fix f {fun {x} {if0 x 9 {f {- x 1}}}}}}
         {down 1}})
(define FFWAE4
  (app (fun 'down (app (id 'down) (num 1)))
       (fix 'f (fun 'x (if0 (id 'x)
                            (num 9)
                            (app (id 'f) (sub (id 'x) (num 1))))))))

(define FFWAES5
  '{rec {down {fun {x} {if0 x
                            9
                            {down {- x 1}}}}}
     {down 1}})
(define FFWAE5
  (app (fun 'down (app (id 'down) (num 1)))
       (fix 'down (fun 'x (if0 (id 'x)
                               (num 9)
                               (app (id 'down) (sub (id 'x) (num 1))))))))

(define FFWAES6
  '{with {down {fixFun f {x} {if0 x 9 {f {- x 1}}}}}
         {down 1}})

(define FFWAE6
  (app (fun 'down (app (id 'down) (num 1)))
       (fixFun 'f 'x (if0 (id 'x)
                          (num 9)
                          (app (id 'f) (sub (id 'x) (num 1)))))))


;; Some Hannah-only programs
(define HG1 (ismtz (mt)))
(define HG2 (ismtz (num 0)))
(define HG3 (isnumz (num 0)))
(define HG4 (isnumz (mt)))
(define HG5 (ispairz (pair (mt) (mt))))
(define HG6 (ispairz (num 0)))

(define HG7 (left (pair (mt) (num 7))))
(define HG8 (right (pair (mt) (num 7))))
(define HG9 (openbox (newbox (num 7))))



#;
(define (fn-for-hg f)
  (type-case HG f
    [num (n) (... n)]
    [isnumz (e) (... (fn-for-hg e))]
    [add (l r) (... (fn-for-hg l)
                    (fn-for-hg r))]
    [sub (l r) (... (fn-for-hg l)
                    (fn-for-hg r))]
    [id (x) (... x)]
    [setvar (x e) (... x (fn-for-hg e))]
    [fun (x body) (... x
                       (fn-for-hg body))]
    [isfunz (e) (... (fn-for-hg e))]
    [app (rator rand) (... (fn-for-hg rator)
                           (fn-for-hg rand))]
    [if0 (p c a)
         (... (fn-for-hg p)
              (fn-for-hg c)
              (fn-for-hg a))]
    [fix (x body) (... x
                       (fn-for-hg body))]
    [pair (l r) (... (fn-for-hg l)
                     (fn-for-hg r))]   
    [ispairz (e) (... (fn-for-hg e))]
    [left (e) (... (fn-for-hg e))]
    [right (e) (... (fn-for-hg e))]
    [mt () (...)]
    [ismtz (e) (... (fn-for-hg e))]
    [newbox (e) (... (fn-for-hg e))]
    [isboxz (e) (... (fn-for-hg e))]
    [setbox (e1 e2) (... (fn-for-hg e1)
                         (fn-for-hg e2))]
    [openbox (e) (... (fn-for-hg e))]))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Procedural Symbol Tables
;;   (formerly "environments", but now used more broadly)
;;

;; (symtabof X) is symbol -> X
;; interp.  bindings of identifiers to objects of type X
;; Effect: signals an error if given symbol has no binding


;; extend-symtab : (symtabof X) symbol X -> (symtabof X)
(define (extend-symtab symtab x0 v)
  (λ (x)
    (if (symbol=? x x0)
        v
        (symtab x))))

;; lookup: (symtabof X) symbol -> X
;; produce the binding associated with the given symbol
;; Effect: Signal an error if the given symbol has no binding
(define (lookup-symtab symtab x) (symtab x))


;;
;; Environments as symbol tables (just a wrapper)
;;

;; (envof X) is (symtabof X)
;; interp.  environment representation for lexically scoped identifiers

;; empty-env : (envof X)
;; an empty environment for lexical scoping (note the custom error msg)
;; Effect: signals an error if variable is not there
(define empty-env
  (λ (x) (error 'lookup-env "Unbound variable: ~a" x)))

;; (envof X) symbol X -> (envOf X)
(define extend-env extend-symtab)

;; (envof X) symbol -> X
;; produce the binding associated with the given symbol
;; Effect: Signal an error if the given symbol has no binding
(define lookup-env lookup-symtab)


;;
;; Stores as symbol tables (just a wrapper)
;;

;; (storeof X) is (symtabof X)
;; interp.  stores represent bindings of locations to mutable values 

;; (storeof X)
;; an empty store for mutable locations (note the custom error msg)
;; Effect: signals an error if location is not there
(define empty-store
  (λ (x) (error 'lookup-store "Unbound store location: ~a" x)))

;; (storeof X) symbol X -> (envOf X)
(define update-store extend-symtab)

;; (storeof X) symbol -> X
;; produce the binding associated with the given symbol
;; Effect: Signal an error if the given symbol has no binding
(define lookup-store lookup-symtab)


;;
;; Interpreter Values
;;

;; Location is Symbol
(define (location? x) (symbol? x))

(define-type Value
  [numV (n number?)]
  [funV (param symbol?) (body HG?) (env procedure?)]
  [pairV (left Value?) (right Value?)]
  [mtV]
  [boxV (l location?)])

;; interp.  Represents a potential value of HG language.
(define V1 (numV 7))
(define V2 (funV 'x (id 'x) empty-env))
(define V3 (funV 'x (id 'y) (extend-env empty-env 'y (numV 7))))
(define V4 (pairV V2 V3))
(define V5 (pairV V2 (pairV V3 (mtV))))

#;
(define (fn-for-value v)
  (type-case Value v
    [numV (n) (... n)]
    [funV (x body env) (... x
                            (fn-for-hg body)
                            env)]
    [pairV (l r) (... (fn-for-value l)
                      (fn-for-value r))]
    [mtV () (...)]
    [boxV (loc) (... loc)]))
          

;;
;; Self-reference Support
;;

(define-type BlackHole
  [blackHole])
;; interp. a sentinel value for fix expressions for detecting premature
;;         self-reference
;; <examples are redundant for enumerations>

; Changed possible type Value to Location:
;; Binding is one of:
;; - (boxof Value)
;; - (boxof BlackHole)
;; - Location
;; interp. possibly mutable environment binding (to support fix)
(define B1 (numV 7))
(define B2 (box (numV 7)))
(define B3 (box (blackHole)))

#;
(define (fn-for-binding b)
  (match b
    [(box v) #:when (Value? v) (fn-for-value v)]
    [(box bh) #:when (BlackHole? bh) (...)]
    [v (fn-for-value v)]))

;;
;; Concrete Environment and Store Datatypes
;;

;; Env is (envof Binding)
;; Store is (storeof Value)



;;
;; Interpretation Functions
;;

;; Binding Store -> Value
;; produce the value denoted by the binding
;; Effect: signal an error in case of a black hole
(define (binding->value b)
  (match b
    [(box l) #:when (location? l) (lookup-store store l)]
    [(box bh) #:when (BlackHole? bh) (error 'divergence "Nooooooooooooo.....!")]
    [l (lookup-store store l)]))

;(test (binding->value (numV 3)) (numV 3))
;(test (binding->value (box (numV 3))) (numV 3))
(test (binding->value 'loc0 (update-store empty-store 'loc0 (numV 3))) (numV 3))
(test (binding->value (box 'loc0)
                      (update-store empty-store 'loc0 (numV 3))) (numV 3))
(test/exn (binding->value (box (blackHole))) "No")
; Needs more example to show interaction of something and store?

;; Value -> number
;; produce the number represented by the given value
;; Effect: signal an error if the value does not represent a number
(define (value->num v)
  (type-case Value v
    [numV (n) n]
    [else (error 'value->num "Bad number: ~a" v)]))

(test (value->num (numV 6)) 6)
(test (value->num (numV 9)) 9)
(test/exn (value->num (funV 'x (id 'x) empty-env)) "Bad number")


;; Value -> pairV
;; produce the same v if it is a pairV
;; Effect: signal an error if the value does not represent a pair
(define (value->pair v)
  (type-case Value v
    [pairV (l r) v]
    [else (error 'value->num "Bad pair: ~a" v)]))

(test (value->pair (pairV (numV 0) (numV 1))) (pairV (numV 0) (numV 1)))
(test/exn (value->pair (numV 6)) "Bad pair")      


;; (Number Number -> Number) Value Value  -> Value
;; apply num-op to the numbers represented by v1 and v2
;; Effect: signal an error if either argument does not represent a number
(define (num-binop-value num-op v1 v2)
  (let ([n1 (value->num v1)]
        [n2 (value->num v2)])
    (numV (num-op n1 n2))))

(test (num-binop-value * (numV 5) (numV 6)) (numV 30))
(test/exn (num-binop-value * (numV 5) (funV 'x (id 'x) empty-env))
          "Bad number")
(test/exn (num-binop-value * (funV 'x (id 'x) empty-env) (numV 6))
          "Bad number")
(test/exn (num-binop-value * (funV 'x (id 'x) empty-env)
                           (funV 'x (id 'x) empty-env)) "Bad number")

  
;; Value Value -> Value
;; produce the sum of two numbers
;; Effect: signal an error if either argument does not represent a number
(define (add-value v1 v2)
  (num-binop-value + v1 v2))

(test (add-value (numV 5) (numV 6)) (numV 11))
(test/exn (add-value (numV 5) (funV 'x (id 'x) empty-env)) "Bad number")
(test/exn (add-value (funV 'x (id 'x) empty-env) (numV 6)) "Bad number")
(test/exn (add-value (funV 'x (id 'x) empty-env)
                     (funV 'x (id 'x) empty-env)) "Bad number")


;; Value Value -> Value
;; produce the difference of two numbers
;; Effect: signal an error if either argument does not represent a number
(define (sub-value v1 v2)
  (num-binop-value - v1 v2))

(test (sub-value (numV 5) (numV 6)) (numV -1))
(test/exn (sub-value (numV 5) (funV 'x (id 'x) empty-env)) "Bad number")
(test/exn (sub-value (funV 'x (id 'x) empty-env) (numV 6)) "Bad number")
(test/exn (sub-value (funV 'x (id 'x) empty-env)
                     (funV 'x (id 'x) empty-env)) "Bad number")


;; Value -> Boolean
;; produce true if v1 represents the number zero, else false
(define (zero-value? v)
  (type-case Value v
    [numV (n) (zero? n)]    
    [else #f]))

(test (zero-value? (numV 7)) #f)
(test (zero-value? (numV 0)) #t)
(test (zero-value? (funV 'x (id 'x) empty-env)) #f)



;; Value Value Store -> (list Value Store)
;; produce the result of applying v1 to v2
;; Effect: signal an error if v1 does not represent a function
(define (apply-value v1 v2 store)
  (type-case Value v1
    [funV (x body env)
          (let ([l (gensym)]) ;; I missed something here
            (interp-hg-acc body (extend-env env x v2) store))]
    [else (error 'apply-value "Bad function: ~a" v1)]))


;; Symbol HG Env Store -> (list Value Store)
(define (interp-fix f body env store)
  (let ([b (box (blackHole))])
    (let ([env^ (extend-env env f b)])
      (match-let ([`(,v1 ,store1) (interp-hg-acc body env^ store)])
        (let ([l (gensym)])
          
          (begin
            ;; replace blackHole with val
            (set-box! b l)
            (list v1 (update-store store1 l v1))))))))


;; Symbol Env -> Value
;; produce the value bound to a given identifier, unboxing bindings as needed
;; Effect: signal an error in case of a black hole
(define (interp-id x env store)
  (binding->value (lookup-env env x) store))

;(test (interp-id 'x (extend-env empty-env 'x (numV 7))) (numV 7))
;(test/exn (interp-id 'x empty-env) "Unbound")
;(test/exn (interp-id 'x (extend-env empty-env 'x (box (blackHole)))) "No")


;; Value -> Value
;; produce the left member of a pair
;; Effect: Signal an error if the given value is not a pair
(define (left-value v)
  (match-let ([(pairV v1 v2) (value->pair v)])
    v1))

(test (left-value (pairV (numV 0) (numV 1))) (numV 0))
(test/exn (left-value (numV 6)) "Bad pair")      


;; Value -> Value
;; produce the right member of a pair
;; Effect: Signal an error if the given value is not a pair
(define (right-value v)
  (match-let ([(pairV v1 v2) (value->pair v)])
    v2))

(test (right-value (pairV (numV 0) (numV 1))) (numV 1))
(test/exn (right-value (numV 6)) "Bad pair")      


;; Value -> Value
;; produce a 0 if the value is a number, otherwise 1
(define (isnumz-value v)
  (if (numV? v) (numV 0) (numV 1)))

;; Value -> Value
;; produce a 0 if the value is a function, otherwise 1
(define (isfunz-value v)
  (if (funV? v) (numV 0) (numV 1)))

;; Value -> Value
;; produce a 0 if the value is a pair, otherwise 1
(define (ispairz-value v)
  (if (pairV? v) (numV 0) (numV 1)))

;; Value -> Value
;; produce a 0 if the value is mt, otherwise 1
(define (ismtz-value v)
  (if (mtV? v) (numV 0) (numV 1)))

;; Value -> Value
;; produce a 0 if the value is a box, otherwise 1
(define (isboxz-value v)
  (if (boxV? v) (numV 0) (numV 1)))


;; Value Store -> (list Value Store)
;; allocate a new box containing v and return it
(define (newbox-value v store)
  ;; allocate a new box
  (let ([l (gensym)])
    ;; update the store to contain the intial value of the box
    (list (boxV l)
          (update-store store l v)))) ; stub



;; Value Store -> Value
;; look up the value stored in the given box (if given a box)
;; Effect: signals an error if v is not a box.
(define (openbox-value v store)
  (type-case Value v
    [boxV (loc) (lookup-store store loc)]
    [else (error 'apply-value "Bad box: ~a" v)]))


;; Value Value Store -> (list Value Store)
;; assign v2 to the contents of the box represented by v1 (if it is one)
;; Effect: signals an error if v1 is not a box.
(define (setbox-value v1 v2 store)
  (type-case Value v1
    [boxV (loc)
          ;; what value should we return?  Let's go with the old value!
          (list (lookup-store store loc) 
                (update-store store loc v2))]
    [else (error 'apply-value "Bad box: ~a" v1)]))

;; !!!
;; Symbol Env Value Store -> (list Value Store)
;; assign the variable x to the value v
;; Effect: signals an error if x is unbound.
; Really symbol here is Hvar: difference between identifiers and locations
(define (setvar-value x env v store) ; need env bc looking up location
  (let* ([l (lookup-env env x) x] ; let* allows nested lets?
         [v-old (lookup-store store l)])
    (list v-old ;; old value in the store
          (update-store store l v)))) ; needed l instead of x
                                      ; x is id, l is location


;; interp-hg-acc : HG Env Store -> (list Value Store)
;; Accumulator: env is Env
;; Invariant: env represents the bindings (in inside-out order)
;;            of identifiers to values due to deferred substitutions

;; Accumulator: store is Store
;; Invariant: store represents the present contents of boxes as they evolve
;;            during interpretation
(define (interp-hg-acc hg env store)
  (type-case HG hg
    [num (n) (list (numV n) store)]
    [isnumz (e)
            (match-let ([`(,v1 ,store1) (interp-hg-acc e env store)])
              (list (isnumz-value v1) store1))]
    [add (l r)
         (match-let ([`(,v1 ,store1) (interp-hg-acc l env store)])
           (match-let ([`(,v2 ,store2) (interp-hg-acc r env store1)])
             (list (add-value v1 v2) store2)))]
    [sub (l r)
         (match-let ([`(,v1 ,store1) (interp-hg-acc l env store)])
           (match-let ([`(,v2 ,store2) (interp-hg-acc r env store1)])
             (list (sub-value v1 v2) store2)))]
    [id (x) (list (interp-id x env store) store)]
    [setvar (x e) (match-let ([`(,v1 ,store1) (interp-hg-acc e env store)])
                    (setvar-value x env v1 store1))]
    [fun (x body)
         (list (funV x body env) store)] ;; this is a closure!
    [isfunz (e)
            (match-let ([`(,v1 ,store1) (interp-hg-acc e env store)])
              (list (isfunz-value v1) store1))]
    [app (rator rand)
         (match-let ([`(,v1 ,store1) (interp-hg-acc rator env store)])
           (match-let ([`(,v2 ,store2) (interp-hg-acc rand env store1)])
             (apply-value v1 v2 store2)))]
    [if0 (p c a)
         (match-let ([`(,v1 ,store1) (interp-hg-acc p env store)])
           (if (zero-value? v1)
               (interp-hg-acc c env store1)
               (interp-hg-acc a env store1)))]
    [fix (f body)
         (interp-fix f body env store)]
    [pair (l r)
          (match-let ([`(,v1 ,store1) (interp-hg-acc l env store)])
            (match-let ([`(,v2 ,store2) (interp-hg-acc r env store1)])
              (list (pairV v1 v2) store2)))]   
    [ispairz (e)
             (match-let ([`(,v1 ,store1) (interp-hg-acc e env store)])
               (list (ispairz-value v1) store1))]
    [left (e)
          (match-let ([`(,v1 ,store1) (interp-hg-acc e env store)])
            (list (left-value v1) store1))]
    [right (e)
           (match-let ([`(,v1 ,store1) (interp-hg-acc e env store)])
             (list (right-value v1) store1))]
    [mt () (list (mtV) store)]
    [ismtz (e)
           (match-let ([`(,v1 ,store1) (interp-hg-acc e env store)])
             (list (ismtz-value v1) store1))]

    ;; IMPLEMENT BOXES!
    [newbox (e)
            (match-let ([`(,v1 ,store1) (interp-hg-acc e env store)])
              (newbox-value v1 store1))] 
    [isboxz (e)
            (match-let ([`(,v1 ,store1) (interp-hg-acc e env store)])
              (list (isboxz-value v1) store1))]
    [setbox (e1 e2)
            (match-let ([`(,v1 ,store1) (interp-hg-acc e1 env store)])
              (match-let ([`(,v2 ,store2) (interp-hg-acc e2 env store1)])
                (list (setbox-value v1 v2) store2)))]
    [openbox (e)
             (match-let ([`(,v1 ,store1) (interp-hg-acc e env store)])
               (list (openbox-value v1 store1) store1))]))

  


;; interp-hg : HG -> Value
;; interpret the given hg expression
;; EFFECTS: Signals an error in case of runtime type error.
(define (interp-hg hg)
  (match-let ([`(,v1 ,store1) (interp-hg-acc hg empty-env empty-store)])
    v1))



(test/match (apply-value (funV 'x (add (id 'x) (num 7)) empty-env)
                         (numV 12)
                         empty-store)
            (list (numV 19) store))

(test/exn (apply-value (numV 12)
                       (funV 'x (add (id 'x) (num 7)) empty-env)
                       empty-store)
          "Bad function")

(test (interp-hg (ismtz (mt))) (numV 0))
(test (interp-hg (ismtz (num 0))) (numV 1))
(test (interp-hg (isnumz (num 0))) (numV 0))
(test (interp-hg (isnumz (mt))) (numV 1))
(test (interp-hg (ispairz (pair (mt) (mt)))) (numV 0))
(test (interp-hg (ispairz (num 0))) (numV 1))

(test (interp-hg (left (pair (mt) (num 7)))) (mtV))
(test (interp-hg (right (pair (mt) (num 7)))) (numV 7))

;; Fun with boxes
(test (interp-hg (openbox (newbox (num 7)))) (numV 7))


;; Using the built-in recursion
(test
 (interp-hg
  (with '* (fixFun 'mult 'lhs
                   (fun 'rhs
                        (if0 (id 'rhs)
                             (num 0)
                             (add (id 'lhs) (app (app (id 'mult)
                                                      (id 'lhs))
                                                 (sub (id 'rhs)
                                                      (num 1)))))))
        (app (app (id '*)
                  (num 20))
             (num 3))))
 (numV 60))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hg-focused-sexp (hgfs) is one of:
;; - number
;; - `{isnumz ,hgfs}
;; - `{+ ,hgfs ,hgfs}
;; - `{- ,hgfs ,hgfs}
;; - `{with ,identifier ,hgfs ,hgfs
;; -  identifier
;; - `{setvar ,identifier ,hgfs}
;; - `{fun ,identifier ,hgfs}
;; - `{isfunz ,hgfs}
;; - `{,hgfs ,hgfs}
;; - `{fix ,identifier ,hgfs}
;; - `{fixFun ,identifier {,identifier} ,hgfs}
;; - `{if0 ,hgfs ,hgfs ,hgfs}
;; - `{pair ,hgfs ,hgfs}
;; - `{ispairz ,hgfs}
;; - `{left ,hgfs}
;; - `{right ,hgfs}
;; - `{mt}
;; - `{ismtz ,hgfs}
;; - `{newbox ,hgfs}
;; - `{isboxz ,hgfs}
;; - `{setbox ,hgfs ,hgfs}
;; - `{openbox ,hgfs}
;; - `{seqn ,hgfs ,hgfs}
;; - <any other s-expression>
;; where identifier is any symbol except +, -, with, if0, or fun
;; interp.  any s-expression, but with a focus on those that represent
;; HG expressions.


(define (identifier? x)
  (and (symbol? x)
       (not (member x '(+ - with if0 fun fix fixFun)))))

#;
(define (fn-for-hgfs sexp)
  (match sexp
    [`,n
     #:when (number? n)
     (... n)]
    [`{isnumz ,sexp1} (... (fn-for-hgfs sexp1))]
    [`{+ ,sexp1 ,sexp2}
     (... (fn-for-hgfs sexp1)
          (fn-for-hgfs sexp2))]
    [`{- ,sexp1 ,sexp2}
     (... (fn-for-hgfs sexp1)
          (fn-for-hgfs sexp2))]
    [`{with {,x ,sexp1} ,sexp2}
     #:when (identifier? x)
     (... x
          (fn-for-hgfs sexp1)
          (fn-for-hgfs sexp2))]
    [`,x
     #:when (identifier? x)
     (... x)]
    [`{setvar ,x ,sexp1}
     #:when (identifier? x)
     (... x
          (fn-for-hgfs sexp1))]
    [`{if0 ,sexp1 ,sexp2 ,sexp3}
     (... (fn-for-hgfs sexp1)
          (fn-for-hgfs sexp2)
          (fn-for-hgfs sexp3))]
    [`{fun {,x} ,sexp1}
     #:when (identifier? x)
     (... x
          (fn-for-hgfs sexp1))]
    [`{isfunz ,sexp1} (... (fn-for-hgfs sexp1))]
    [`{fix ,f ,sexp1}
     #:when (identifier? f)
     (... f
          (fn-for-hgfs sexp1))]
    [`{fixFun ,f {,x} ,sexp1}
     #:when (and (identifier? f) (identifier? x))
     (... f
          x
          (fn-for-hgfs sexp1))]
    [`{pair ,sexp1 ,sexp2}
     (... (fn-for-hgfs sexp1)
          (fn-for-hgfs sexp2))]
    [`{ispairz ,sexp1} (... (fn-for-hgfs sexp1))]
    [`{left ,sexp1} (... (fn-for-hgfs sexp1))]
    [`{right ,sexp1} (... (fn-for-hgfs sexp1))]
    [`{mt} (...)]
    [`{ismtz ,sexp1} (... (fn-for-hgfs sexp1))]
    [`{newbox ,sexp1} (... (fn-for-hgfs sexp1))]
    [`{isboxz ,sexp1} (... (fn-for-hgfs sexp1))]
    [`{setbox ,sexp1 ,sexp2}
     (... (fn-for-hgfs sexp1)
          (fn-for-hgfs sexp2))]
    [`{openbox ,sexp1} (... (fn-for-hgfs sexp1))]
    [`{seqn ,sexp1 ,sexp2}
     (... (fn-for-hgfs sexp1)
          (fn-for-hgfs sexp2))]
    ;; Notice that application is now the last focused case...
    [`{,sexp1 ,sexp2}
     (... (fn-for-hgfs sexp1)
          (fn-for-hgfs sexp1))]    
    [else (... sexp)] ))



;; parse-expr : s-expression -> HG
;; parse the given s-expression into a HG expression
;; EFFECT: signals an error on failure
(define (parse-expr sexp)
  (match sexp
    [`,n #:when (number? n) (num n)]
    [`{isnumz ,sexp1} (isnumz (parse-expr sexp1))]
    [`{+ ,lhs ,rhs} (add (parse-expr lhs) (parse-expr rhs))]
    [`{- ,lhs ,rhs} (sub (parse-expr lhs) (parse-expr rhs))]
    [`{with {,id ,named-exp} ,body}
     #:when (identifier? id)
     ;; desugar with into anonymous function application
     (with id (parse-expr named-exp) (parse-expr body))]
    [`,x #:when (identifier? x) (id x)]
    [`{setvar ,x ,sexp1}
     #:when (identifier? x)
     (setvar x (parse-expr sexp1))]
    [`{if0 ,pred ,conseq ,altern}
     (if0 (parse-expr pred) (parse-expr conseq) (parse-expr altern))]
    ;; Notice that application is now last...
    [`{fun {,x} ,sexp1}
     #:when (identifier? x)
     (fun x (parse-expr sexp1))]
    [`{isfunz ,sexp1} (isfunz (parse-expr sexp1))]    
    [`{fix ,f ,sexp1}
     #:when (identifier? f)
     (fix f (parse-expr sexp1))]
    [`{fixFun ,f {,x} ,sexp1}
     #:when (and (identifier? f) (identifier? x))
     ;; desugar to fix and fun
     (fixFun f x (parse-expr sexp1))]
    [`{pair ,sexp1 ,sexp2}
     (pair (parse-expr sexp1)
           (parse-expr sexp2))]
    [`{ispairz ,sexp1} (ispairz (parse-expr sexp1))]
    [`{left ,sexp1} (left (parse-expr sexp1))]
    [`{right ,sexp1} (right (parse-expr sexp1))]
    [`{mt} (mt)]
    [`{ismtz ,sexp1} (ismtz (parse-expr sexp1))]
    [`{newbox ,sexp1} (newbox (parse-expr sexp1))]
    [`{isboxz ,sexp1} (isboxz (parse-expr sexp1))]
    [`{setbox ,sexp1 ,sexp2}
     (setbox (parse-expr sexp1)
             (parse-expr sexp2))]
    [`{openbox ,sexp1} (openbox (parse-expr sexp1))]
    [`{seqn ,sexp1 ,sexp2}
     (seqn (parse-expr sexp1)
           (parse-expr sexp2))]
    [`{,rator-exp ,arg-exp} 
     (app (parse-expr rator-exp) (parse-expr arg-exp))]
    [_ (error 'parse-expr "bad expression ~a" sexp)]))


(test (parse-expr WAES4) WAE4)
(test (parse-expr WAES5) WAE5)
(test (parse-expr WAES6) WAE6)
(test (parse-expr WAES7) WAE7)

;;
;; PUTTING IT ALL TOGETHER  - an interpreter of files on disk
;;

;; String -> Value
;; produce the result of interpreting the HG program in input
;; EFFECT: signals an error if interpretation signals a runtime error.
(define (interp-string pgm)
  (interp-hg
   (parse-expr
    (read-from-string pgm))))


;; String -> Value
;; produce the result of interpreting the HG stored in the file fname
;; EFFECT: signals an error if no file fname contains an AE representation
;;         or if interpretation signals a runtime error.
(define (interp-file fname)
  (interp-hg
   (parse-expr
    (read-from-file fname))))

(test (with-temporary-data-file "{+ 3 7}\n"
        (λ (fname) (interp-file fname)))
      (numV 10))

(test (with-temporary-data-file "{+ {- 3 4} 7}\n"
        (λ (fname) (interp-file fname)))
      (numV 6))

(test (with-temporary-data-file "{with {x 5} {+ x {with {y 3} x}}}"
        (λ (fname) (interp-file fname)))
      (numV 10))


(test (with-temporary-data-file
          "{with {double {fun {x} {+ x x}}} {with {x 5} {double x}}}"
        (λ (fname) (interp-file fname)))
      (numV 10))


(test (with-temporary-data-file
          "{with {* {fixFun mult {lhs} {fun {rhs}
                                {if0 rhs
                                     0
                                     {+ lhs {{mult lhs} {- rhs 1}}}}}}}
             {{* 20} 3}}"
        (λ (fname) (interp-file fname)))
      (numV 60))

(test (with-temporary-data-file
          "{with {* {fix mult {fun {lhs} {fun {rhs}
                                {if0 rhs
                                     0
                                     {+ lhs {{mult lhs} {- rhs 1}}}}}}}}
             {{* 20} 3}}"
        (λ (fname) (interp-file fname)))
      (numV 60))


(define DOUBLE-LIST
  "{with {dbl
          {fixFun recur {l} {if0 {ismtz l}
                               {mt}
                               {pair {+ {left l} {left l}}
                                     {recur {right l}}}}}}
    {dbl {pair 5 {pair 4 {pair 3 {pair 2 {pair 1 {mt}}}}}}}}
")

(test (with-temporary-data-file
          DOUBLE-LIST
        (λ (fname) (interp-file fname)))
      (pairV (numV 10)
             (pairV (numV 8)
                    (pairV (numV 6)
                           (pairV (numV 4)
                                  (pairV (numV 2)
                                         (mtV)))))))

(test (with-temporary-data-file
          "{with {x 5}
       {with {y x}
             {seqn {setvar y 7}
                   x}}}
"
        (λ (fname) (interp-file fname)))
      (numV 5)) ;; Call-by-reference: 7
