#lang plai
(require "parsing.rkt")
(require "test-match.rkt")
(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates


;; Assignment 4: Not To Be Confused With Two-Factor Authentication

;; Truth!Falsehood!Assignment! (TFA):
;; A *by-reference* functional language with:
;; - numbers
;; - booleans
;; - conditional expressions
;; - first-class (recursive) functions
;; - mutable variables


;; The TFA language is an effectful strict language with mutable variables
;; and by-reference semantics.  Here are some salient aspects of its design


;; 1) BOOLEANS
;; Finally some Booleans!  The ifB language feature lets us branch on Booleans,
;; e.g.:
#;
'{ifB #t 7 9}
;; evaluates to 7

;; ifB is insistent about branching on Booleans:  any other kind of value in
;; predicate position signals an error, like:
#;
'{ifB 7 #t 9}

;; Naturally the language has identifier binding, and an expression like
#;
'{with {a #t}
       {ifB a 7 9}}
;; also evaluates to 7 while

#;
'{with {a 7}
       {ifB a #t 9}}
;; also signals an error

;; The language has short-circuiting conjunction (andB) and negation (notB),
;; which can be understood in terms of ifB.

;; One can also convert a number to a Boolean using nought?, which is TFA's
;; analogue to PLAI's zero?.  However, like ifB, nought? only produces a value
;; if its argument evaluates to a number, so:
#;
'{with {a {fun {b} b}}
       {nought? a}}
;; yields a runtime error.


;; BY-REFERENCE SEMANTICS:

;; As regards variables and mutation, TFA features call-by-reference semantics.
;; For example, the expression (where setobj in this example acts like Hannah's
;; setvar):
#;
'{with {a 5}
       {with {b a}
             {seqn {setobj b 7}
                   a}}}

;; produces 7 because b and a are bound to the same location.
;; translating the inner with into the equivalent function application
;; yields the same result:
#;
'{with {a 5}
       {{fun {b}
             {seqn {setobj b 7}
                   a}}
        a}}
;; since with expressions and function application have similar semantics.

;; TFA is rather comprehensive in its by-reference semantics.
;; A with expression need not be *immediately* bound to an identifier:
;; any with expression whose named expression ultimately evaluates a
;; variable to produce its "result" will bind the identifier by-reference.
;; Here is an example using a conditional expression:
#;
'{with {a 5}
       {with {b 6}
             {with {c {ifB #t a b}}
                   {seqn {setobj c 7}
                         a}}}}
;; The ifB (boolean if) expression ultimately binds c by-reference to a,
;; so so alias, and the expression yields 7.


;; This behaviour even applies to function applications where the function body
;; ultimately yields a variable (i.e. "return-by-reference"):
#;
'{with {a 5}
       {with {f {fun {b} a}}
             {with {c {f 6}}
                   {seqn {setobj c 7}
                         a}}}}

;; Since the call to f in the with expression yields a, it is bound by-reference
;; to c and, yet again, our expression yields 7.

;; This even applies to a function that yields a locally-bound variable, though
;; this takes a little more work to interrogate
#;
'{with {f {fun {a} {fun {b} a}}}
       {with {g {f 9}}
             {with {d {g 2}}
                   {with {c {g 3}}
                         {seqn {setobj c 7}
                               d}}}}}

;; You guessed it: lucky number 7!


;; FLEXIBLE ASSIGNMENT: L-VALUES AND R-VALUES

;; In Hannah, the setvar operation requires that its first argument immediately
;; be an identifier.  TFA is far more flexible:  any expression whose final
;; result yields an identifier is a legal first argument to setobj.
;; For instance:
#;
'{with {a 5}
       {with {b 6}
             {seqn {setobj {ifB #t a b} 7}
                   a}}}
;; produces 7!

;; Even function applications are fair game:
#;
'{with {f {fun {a} {fun {b} a}}}
       {with {g {f 9}}
             {with {d {g 2}}
                   {seqn {setobj {g 3} 7}
                         d}}}}
;; So much 7!

;; Of course attempting to set something that is not a variable
;; will produce an error, e.g.:
#;
'{with {f {fun {a} 6}}
       {seqn {setobj {f 2} 7}
             12}}

;; WHAT IS THIS WITCHCRAFT?!?

;; The TFA language distinguishes between two kinds of values and
;; three kinds of evaluation contexts.

;; R-Values are the values that we are used to, like numbers and functions:
;; they are called R-Values because they appear as the "right-hand"
;; (i.e., second) argument to a setobj operation.

;; L-Values are locations, which are what one expects to appear as the
;; "left-hand" (i.e. first) argument to setobj.

;; Each of these kinds of values describes an *evaluation context*, a way
;; of telling our interpreter that the expression waiting for a value is
;; expecting one or the other kind of value.

;; If an interpreter in L-Value context is to interpret an identifier, it
;; should simply produce the location that the identifier is bound to.
;; This way our complex setobj gets the proper location to assign a value
;; to.  However, if in L-Value context the interpreter produces an R-Value
;; like a number, Boolean, or function, then the interpreter should signal
;; an error (because you cannot assign these things).

;; If an interpreter in R-Value context is to interpret an identifier,
;; it should produce the current R-value stored in the location to which
;; the identifier is bound.  Any other expression produces its value in
;; a normal fashion.

;; In addition to L-Value context and R-Value Context, there is a
;; third context called Binding context.  This refers to when the
;; result of interpreter's current evaluation is going to be bound to a
;; variable, either because of a with expression or a function application.

;; If an interpreter in Binding context is to interpret an identifier, then
;; like L-Value context it should produce the relevant location.  Unlike
;; L-Value context, evaluating to an R-Value is fine, but the interpreter
;; must store that R-Value in a fresh location and return that location.
;; So interpretation of a Binding context uniformly produces an L-Value,
;; without error, since any result can be bound to an identifier.

;; Implementing this involves passing a lost-context accumulator to every
;; recursive call to the interpreter.  The key is determining what context
;; applies for each call, based on the outer expression making the recursive
;; calls, as well as that outer expression's context itself, when relevant.


;; NOTE ON SELF-REFERENCE:

;; This interpreter implements fixFun directly, and then implements
;; fun in terms of it. 
;; Notice that Env just binds identifiers to locations and
;; Store just binds locations to R-Values.  This is because
;; interp-fixFun will not need anything like a blackhole to be implemented.
;; The implementation is compact and clean.


;; YOUR MISSION:

;; Design an interpreter for the TFA language!  We are providing just a few
;; examples, the names of helpers for the various operations, and parsing
;; infrastructure.  This is an opportunity for you to design much of the
;; interpreter yourself.



;; TID is Symbol
;; INVARIANT: a TID cannot be a TFA keyword
;; interp.  an identifier in the TFA language
(define (tid? x)
  (let ([keywords
         '(+ nought? ifB andB notB with fun fixFun array aref setobj seqn)])
    (and (symbol? x)
         (not
          (member x keywords)))))

(define TID0 'a)
(define TID1 'b)

;; No template: atomic data



(define-type TFA    
  [num (n number?)]
  [add (lhs TFA?) (rhs TFA?)]
  [nought? (arg TFA?)]
  [bool (b boolean?)]
  [ifB (pred TFA?) (conseq TFA?) (altern TFA?)]
  [id (name tid?)]
  [setobj (l TFA?) (r TFA?)] 
  [fixFun (self tid?) (param tid?) (body TFA?)]
  [app (rator TFA?) (rand TFA?)])
;; interp. expressions in the TFA language.
;; Its syntax is defined by the following BNF:
;; <TFA> ::=
;; (ARITHMETIC)
;;          <num>
;;        | {+ <TFA> <TFA>}
;;        | {nought? <TFA>}
;; (LOGIC)
;;        | <bool>
;;        | {ifB <TFA> <TFA> <TFA>}
;;        | {andB <TFA> <TFA>}
;;        | {notB <TFA> <TFA>}
;; (IDENTIFIERS)
;;        | <id>
;;        | {with {<id> <HG>} <HG>}
;; (FUNCTIONS)
;;        | {<TFA> <TFA>}
;;        | {fun {<id>} <TFA>}
;;        | {fixFun <id> {<id>} <TFA>}
;; (ARRAYS)
;;        | {array <TFA>*}
;;        | {aref <TFA>}
;; (ASSIGNMENT)
;;        | {setobj <TFA> <TFA>}
;;        | {seqn <TFA> <TFA>}
;; {andB expr1 expr2} ≡ {with {x0 expr1} {ifB x0 expr2 #f}}
;;   where x0 does not occur free in expr2
;; {notB expr} ≡ {ifB expr #f #t}
;; {fun {x} body} ≡ {fixFun f0 {x} body} where f0 does not occur free in body
;; {with {x named} body} ≡ {{fun {x} body} named}
;; {seqn expr1 expr2} ≡ {with {x0 expr1} expr2}
;;                where x0 does not occur free in expr2

;; Syntactic sugars
(define (andB expr1 expr2)
  (let ([x0 (gensym)])
    (with x0 expr1 (ifB (id x0) expr2 (bool #f)))))
(define (notB expr) (ifB expr (bool #f) (bool #t)))
(define (fun x body) (fixFun (gensym) x body))
(define (with x named body) (app (fun x body) named))
(define (seqn expr1 expr2) (with (gensym) expr1 expr2))


(define AE1 (num 4))
(define AE2 (add AE1 (num 5)))

(define WAES4 '{with {x {+ 5 5}} {with {y {+ x -3}} {+ y y}}})
(define WAE4 (with 'x (add (num 5) (num 5))
                   (with 'y (add (id 'x) (num -3))
                         (add (id 'y) (id 'y)))))

(define WAES5 '{with {x 5} {+ x {with {x 3} 10}}})
(define WAE5 (with 'x (num 5) (add (id 'x) (with 'x (num 3) (num 10)))))

(define WAES6 '{with {x 5} {+ x {with {x 3} x}}})
(define WAE6 (with 'x (num 5) (add (id 'x) (with 'x (num 3) (id 'x)))))

(define WAES7 '{with {x 5} {+ x {with {y 3} x}}})
(define WAE7 (with 'x (num 5) (add (id 'x) (with 'y (num 3) (id 'x)))))

(define FWAES1 '{fun {x} {+ x x}})
(define FWAE1 (fun 'x (add (id 'x) (id 'x))))

(define FWAES2 '{fun {x} {+ x 1}})
(define FWAE2 (fun 'x (add (id 'x) (num 1))))

(define FWAES3 '{fun {x} {+ x y}})
(define FWAE3 (fun 'x (add (id 'x) (id 'y))))

(define FFWAES6
  '{with {down {fixFun f {x} {if0 x 9 {f {+ x -1}}}}}
         {down 1}})

(define FFWAE6
  (app (fun 'down (app (id 'down) (num 1)))
       (fixFun 'f 'x (ifB (nought? (id 'x))
                          (num 9)
                          (app (id 'f) (add (id 'x) (num -1)))))))


#;
(define (fn-for-tfa f)
  (type-case TFA f
    [num (n) (... n)]
    [add (l r) (... (fn-for-tfa l)
                    (fn-for-tfa r))]
    [nought? (e) (... (fn-for-tfa e))]
    [bool (b) (... b)]
    [ifB (p c a)
         (... (fn-for-tfa p)
              (fn-for-tfa c)
              (fn-for-tfa a))]
    [id (x) (... x)]
    [setobj (l r) (... (fn-for-tfa l)
                       (fn-for-tfa r))]
    [fixFun (f x body) (... f x (fn-for-tfa body))]
    [app (rator rand) (... (fn-for-tfa rator)
                           (fn-for-tfa rand))]))

#;
(define (fn-for-lotfa elems)
  (local [(define for-empty (...))
          (define (for-cons tfa rlotfa) (... tfa rlotfa))]          
    (foldr for-empty for-cons elems)))


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


(define-type EvalContext
  [lValueCtxt]
  [bindingCtxt]
  [rValueCtxt])
;; interp. the kind of context in which an expression is being evaluated.
;; lValueCtxt means the result of evaluating the current expression will
;;   be the first argument to a surrounding setobj expression;
;; bindingCtxt means the result of evaluating the current expression will be:
;;   1) the operand position of a function application, or
;;   2) the named expression position of a with expression;
;; rValueCtxt means the result of the evaluating the current expression is
;;   neither lValueCtxt nor bindingCtxt; 

#;
(define (fn-for-ectxt ectxt)
  (type-case EvalContext
    [lValueCtxt () (...)]
    [bindingCtxt () (...)]
    [rValueCtxt () (...)]))

;;
;; Interpreter Values
;;


;; Location is Symbol
(define (location? x) (symbol? x))

;; LValue is Location
;; interp.  Represents a potential "left side of set-obj" value 

(define LV1 'l1)
(define LV2 'l2)


(define-type RValue
  [numV (n number?)]
  [boolV (b boolean?)]
  [funV (param symbol?) (body TFA?) (env procedure?)])
;; interp.  Represents a potential "right side of set-obj" value 

(define RV1 (numV 7))
(define RV2 (funV 'x (id 'x) empty-env))
(define RV3 (funV 'x (id 'y) (extend-env empty-env 'y (numV 7))))
(define RV4 (boolV #f))
(define RV5 (boolV #t))

#;
(define (fn-for-rvalue v)
  (type-case RValue v
    [numV (n) (... n)]
    [boolV (b) (... b)]
    [funV (x body env) (... x
                            (fn-for-tfa body)
                            env)]))
          



;;
;; Concrete Environment and Store Datatypes
;;


;; Env is (envof Location)
;; Store is (storeof RValue)



;;
;; Interpretation Functions
;;

;; lValueCtxt RValue Store -> (list LValue Store)
;; and
;; bindingCtxt RValue Store -> (list LValue Store)
;; and
;; rValueCtxt RValue Store -> (list RValue Store)
;; produce the kind of value appropriate for the given evaluation context
;; Effect: signal an error in case of context mismatch
(define (coerce-rvalue ectxt rv store)
  (type-case EvalContext ectxt
    [lValueCtxt () (error 'interp/tfa "Bad L-Value: ~a" rv)]
    [bindingCtxt () (let ([l (gensym)])
                      (list l (update-store store l rv)))]
    [rValueCtxt () (list rv store)]))


;; helper function names for your design:

;; add/tfa
;; nought?/tfa
;; apply/tfa
;; fixFun/tfa
;; id/tfa
;; setobj/tfa
;; interp/tfa-acc
;; interp/tfa

;; You are encouraged to design additional helpers as needed.

;; RValue -> number
;; produce the number represented by the given value
;; Effect: signal an error if the value does not represent a number
(define (value->num v)
  (type-case RValue v
    [numV (n) n]
    [else (error 'value->num "Bad number: ~a" v)]))

(test (value->num (numV 6)) 6)
(test (value->num (numV 9)) 9)
(test/exn (value->num (boolV #t)) "Bad number")
(test/exn (value->num (funV 'x (id 'x) empty-env)) "Bad number")

;; RValue RValue -> RValue
;; produce the sum of two numbers
;; Effect: signal an error if either argument does not represent a number
(define (add/tfa v1 v2)
  (let ([n1 (value->num v1)]
        [n2 (value->num v2)])
    (numV (+ n1 n2))))

(test (add/tfa (numV 5) (numV 6)) (numV 11))
(test/exn (add/tfa (numV 5) (funV 'x (id 'x) empty-env)) "Bad number")
(test/exn (add/tfa (numV 5) (boolV #t)) "Bad number")
(test/exn (add/tfa (funV 'x (id 'x) empty-env) (numV 6)) "Bad number")
(test/exn (add/tfa (funV 'x (id 'x) empty-env)
                   (funV 'x (id 'x) empty-env)) "Bad number")



;; RValue -> Boolean
;; produce true if v represents the number zero, false if another number
;; Effect: signal an error if v does not represent a number
(define (nought?/tfa v)
  (let ([n (value->num v)])
    (zero? n)))

(test (nought?/tfa (numV 7)) #f)
(test (nought?/tfa (numV 0)) #t)
(test/exn (nought?/tfa (funV 'x (id 'x) empty-env)) "Bad number")
(test/exn (nought?/tfa (boolV #t)) "Bad number")


;; RValue -> Boolean
;; produce if v represents a boolean, produce its value
;; Effect: signal an error if v does not represent a boolean
(define (true-value? v)
  (type-case RValue v
    [boolV (b) b]
    [else (error 'value->num "Bad boolean: ~a" v)]))

(test (true-value? (boolV #t)) #t)
(test (true-value? (boolV #f)) #f)
(test/exn (true-value? (funV 'x (id 'x) empty-env)) "Bad boolean")
(test/exn (true-value? (numV 23)) "Bad boolean")


;; Note: id/tfa has *three signatures*, one for each possible
;; evaluation context value, since the result type may vary
;; depending on the input.  This conjunction of signatures is called
;; an "intersection type".  Other functions you design may also
;; have an analogous signature.  This one is provided as an
;; example

;; Symbol Env Store lValueCtxt  -> (list LValue Store)
;; and
;; Symbol Env Store bindingCtxt -> (list LValue Store)
;; and
;; Symbol Env Store rValueCtxt  -> (list RValue Store)
;; produce the value currently associated with the variable x and the context
;(define (id/tfa x env store ectxt) (list (...) empty-store)) ; stub
(define (id/tfa x env store ectxt)
  (let ([l (lookup-env env x)])
    (type-case EvalContext ectxt
      [lValueCtxt () (list l store)]
      [bindingCtxt () (list l store)]
      [rValueCtxt () (list (lookup-store store l) store)])))



;; LValue RValue Env Store -> (list RValue Store)
;; assign the location l to the value v
;; Effect: signals an error if l is unbound.
(define (setobj/tfa l v env store)
  (let ([v-old (lookup-store store l)])
    (list v-old 
          (update-store store l v))))


;; Symbol Symbol TFA env -> RValue
;; produce a recursive function resulting from (fixFun f x body) in env
(define (fixFun/tfa f x body env)
  (letrec ([env^ (λ (x0)
                   (if (symbol=? x0 f)
                       (funV x body env^)
                       (env x0)))])
    (funV x body env^)))


;; RValue RValue Store EvalContext -> (list RValue Store)
;; produce the result of applying v1 to v2
;; Effect: signal an error if v1 does not represent a function
(define (apply/tfa v1 v2 store ectxt)
  (type-case RValue v1
    [funV (x body env)
          (let ([l (gensym)])
            (interp/tfa-acc body
                            (extend-env env x l)
                            (update-store store l v2)
                            ectxt))]
    [else (error 'apply-value "Bad function: ~a" v1)]))


;; interp/tfa-acc : TFA Env Store -> (list RValue Store)
;; Accumulator: env is Env
;; Invariant: env represents the bindings (in inside-out order)
;;            of identifiers to values due to deferred substitutions

;; Accumulator: store is Store
;; Invariant: store represents the present values of variables as they evolve
;;            during interpretation

;; Accumulator: ectxt is EvalContext
;; Invariant: ectxt represents the context the interpretation is taking place in
#;(define (interp/tfa-acc f env store)
    (list (numV 0) store))

(define (interp/tfa-acc f env store ectxt)
  (type-case TFA f
    [num (n) (list (numV n) store)]
    [add (l r)
         (match-let ([`(,v1 ,store1) (interp/tfa-acc l env store (rValueCtxt))])
           (match-let ([`(,v2 ,store2)
                        (interp/tfa-acc r env store1 (rValueCtxt))])
             (coerce-rvalue ectxt (add/tfa v1 v2) store2)))]
    [nought? (e) (match-let ([`(,v1 ,store1)
                              (interp/tfa-acc e env store (rValueCtxt))])
                   (coerce-rvalue ectxt (nought?/tfa v1) store1))]
    [bool (b) (list (boolV b) store)]
    [ifB (p c a)
         (match-let ([`(,v1 ,store1) (interp/tfa-acc p env store (rValueCtxt))])
           (if (true-value? v1)
               (interp/tfa-acc c env store1 ectxt)
               (interp/tfa-acc a env store1 ectxt)))]
    [id (x) (id/tfa x env store ectxt)]
    [setobj (l r) (match-let ([`(,v1 ,store1)
                               (interp/tfa-acc l env store (lValueCtxt))])
                    (match-let ([`(,v2 ,store2)
                                 (interp/tfa-acc r env store1 (rValueCtxt))])
                      (match-let ([`(,v3 ,store3) (setobj/tfa v1 v2 env)])
                        (coerce-rvalue ectxt v3 store3))))]
    [fixFun (f x body) (list (fixFun/tfa f x body env) store)]
    [app (rator rand)
         (match-let ([`(,v1 ,store1)
                      (interp/tfa-acc rator env store (rValueCtxt))])
           (match-let ([`(,v2 ,store2)
                        (interp/tfa-acc rand env store1 (bindingCtxt))])
             (apply/tfa v1 v2 store2 ectxt)))]))

;; TFA -> RValue
;; interpret the given TFA expression
;; EFFECTS: Signals an error in case of runtime type error.
;(define (interp/tfa tfa) (numV 9)) ; stub for the examples
(define (interp/tfa tfa)
  (match-let ([`(,v1 ,store1)
               (interp/tfa-acc tfa empty-env empty-store (rValueCtxt))])
    v1))


(test (interp/tfa AE1) (numV 4))
(test (interp/tfa AE2) (numV 9))

(test (interp/tfa WAE4) (numV 14))
(test (interp/tfa WAE5) (numV 15))
(test (interp/tfa WAE6) (numV 8))
(test (interp/tfa WAE7) (numV 10))

(test (interp/tfa FWAE1) (funV 'x (add (id 'x) (id 'x)) empty-env))
(test (interp/tfa FWAE2) (funV 'x (add (id 'x) (num 1)) empty-env))
(test (interp/tfa FWAE3) (funV 'x (add (id 'x) (id 'y)) empty-env))

(test (interp/tfa (bool #t)) (boolV #t))
(test (interp/tfa (ifB (bool #t) (num 7) (num 9))) (numV 7))
(test (interp/tfa (ifB (bool #f) (num 7) (num 9))) (numV 9))
(test/exn (interp/tfa (ifB (num 7) (bool #t) (num 9))) "Bad boolean")
(test (interp/tfa (with 'a (bool #t) (ifB (id 'a) (num 7) (num 9))))
      (numV 7))
(test/exn (interp/tfa (with 'a (num 7) (ifB (id 'a) (bool #t) (num 9))))
          "Bad boolean")
(test (interp/tfa (with 'a (num 7) (nought? (id 'a)))) (boolV #f))
(test/exn (interp/tfa (with 'a (fun 'x (id 'x)) (nought? (id 'a))))
          "Bad number")

(test (interp/tfa FFWAE6) (numV 9))

(test (with 'a (num 5) (with 'a (id 'b) (seqn (setobj 'b (num 7)) (id 'a)))) 7)
(test (with 'a (num 5) (fun 'b (seqn (setobj 'b (num 7)) (id 'a)))) 7)

(test (with 'a (num 5) (with 'b (num 6)
                             (with 'c (ifB (bool #t) (id 'a) (id 'b))
                                   (seqn (setobj 'c (num 7)) (id 'a)))))
      7)
(test (with 'a (num 5)
            (with 'f (fun 'b (id 'a))
                  (with 'c (app (id 'f) (num 6))
                        (seqn (setobj 'c (num 7)) (id 'a)))))
      7)
(test (with 'f (fun 'a (fun 'b (id 'a)))
            (with 'g (app (id 'f) (num 9))
                  (with 'd (app (id 'g) (num 2))
                        (with 'c (app (id 'g) (num 3))
                              (seqn (setobj 'c (num 7)) (id 'a))))))
      7)

;; L-VALUES AND R-VALUES
(test (with 'a (num 5)
            (with 'b (num 6)
                  (seqn (setobj (ifB (bool #t) (id 'a) (id 'b)) (num 7))
                        (id 'a))))
      7)
(test (with 'f (fun 'a (fun 'b (id 'a)))
            (with 'g (app (id 'f) (num 9))
                  (with 'd (app (id 'g) (num 2))
                        (seqn (setobj (app (id 'g) (num 3)) (num 7))
                              (id 'd)))))
      7)
(test/exn (with 'f (fun 'a (num 6))
                (seqn (setobj (app (id 'f) (num 2)) (num 7))
                      (num 12)))
          "Bad L-Value")


(test
 (interp/tfa
  (with '* (fixFun 'mult 'lhs
                   (fun 'rhs
                        (ifB  (nought? (id 'rhs))
                              (num 0)
                              (add (id 'lhs) (app (app (id 'mult)
                                                       (id 'lhs))
                                                  (add (id 'rhs)
                                                       (num -1)))))))
        (app (app (id '*)
                  (num 20))
             (num 3))))
 (numV 60))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TFAFS is one of:
;; - Number
;; - `{+ ,TFAFS ,TFAFS}
;; - `{nought? ,TFAFS}
;; - Boolean
;; - `{ifB ,TFAFS ,TFAFS ,TFAFS}
;; - `{andB ,TFAFS ,TFAFS}
;; - `{notB ,TFAFS}
;; -  TID
;; - `{with {,TID ,TFAFS} ,TFAFS}
;; - `{,TFAFS ,TFAFS}
;; - `{fun {,TID} ,TFAFS}
;; - `{fixFun ,TID {,TID} ,TFAFS}
;; - `{setobj ,TFAFS ,TFAFS}
;; - `{array . ,LOTFAFS}
;; - `{aref ,TFAFS ,TFSFS}
;; - `{seqn ,TFAFS ,TFAFS}
;; - <any other s-expression>
;; interp.  any s-expression, focusing on those that represent TFA expressions.

#;
(define (fn-for-tfafs tfafs)
  (match tfafs
    [`,n
     #:when (number? n)
     (... n)]
    [`{+ ,tfafs1 ,tfafs2}
     (... (fn-for-tfafs tfafs1)
          (fn-for-tfafs tfafs2))]
    [`{nought? ,tfafs1}
     (... (fn-for-tfafs tfafs1))]
    [`,b
     #:when (boolean? b)
     (... b)]
    [`{ifB ,tfafs1 ,tfafs2 ,tfafs3}
     (... (fn-for-tfafs tfafs1)
          (fn-for-tfafs tfafs2)
          (fn-for-tfafs tfafs3))]
    [`{andB ,tfafs1 ,tfafs2}
     (... (fn-for-tfafs tfafs1)
          (fn-for-tfafs tfafs2))]     
    [`{notB ,tfafs1}
     (... (fn-for-tfafs tfafs1))]
    [`,x
     #:when (tid? x)
     (... x)]
    [`{with {,x ,tfafs1} ,tfafs2}
     (... x
          (fn-for-tfafs tfafs1)
          (fn-for-tfafs tfafs2))]
    [`{,tfafs1 ,tfafs2}
     (... (fn-for-tfafs tfafs1)
          (fn-for-tfafs tfafs2))]
    [`{fun {,x} ,tfafs1}
     (... x
          (fn-for-tfafs tfafs1))]
    [`{fixFun ,f {,x} ,tfafs1}
     (... f
          x
          (fn-for-tfafs tfafs1))]
    [`{setobj ,tfafs1 ,tfafs2}
     (... (fn-for-tfafs tfafs1)
          (fn-for-tfafs tfafs2))]     
    [`{array ,lotfafs}
     (... (fn-for-lotfafs lotfafs))]
    [`{aref ,tfafs1 ,tfafs2}
     (... (fn-for-tfafs tfafs1)
          (fn-for-tfafs tfafs2))]     
    [`{seqn ,tfafs1 ,tfafs2}
     (... (fn-for-tfafs tfafs1)
          (fn-for-tfafs tfafs2))]     
    [otherwise (...)]))



;; LOTFAFS is one of;
;; - '{}
;; - `{,TFAFS . ,LOTFAFS}
;; - <any other s-expression>
;; interp.  Any s-expression, focused on lists of TFAFS

#;
(define (fn-for-lotfafs lotfafs)
  (match lotfafs
    [`{} (...)]
    [`{,tfafs . ,lotfafs} (... (fn-for-tfafs tfafs)
                               (fn-for-lotfafs lotfafs))]
    [otherwise (...)]))



;; TFAFS -> TFA
;; parse the given s-expression into a TFA expression
;; EFFECT: signals an error on failure
(define (parse/tfa tfafs)
  (match tfafs
    [`,n
     #:when (number? n)
     (num n)]
    [`{+ ,tfafs1 ,tfafs2}
     (add (parse/tfa tfafs1)
          (parse/tfa tfafs2))]
    [`{nought? ,tfafs1}
     (nought? (parse/tfa tfafs1))]
    [`,b
     #:when (boolean? b)
     (bool b)]
    [`{ifB ,tfafs1 ,tfafs2 ,tfafs3}
     (ifB (parse/tfa tfafs1)
          (parse/tfa tfafs2)
          (parse/tfa tfafs3))]
    [`{andB ,tfafs1 ,tfafs2}
     (andB (parse/tfa tfafs1)
           (parse/tfa tfafs2))]     
    [`{notB ,tfafs1}
     (notB (parse/tfa tfafs1))]
    [`,x
     #:when (tid? x)
     (id x)]
    [`{with {,x ,tfafs1} ,tfafs2}
     (with x
           (parse/tfa tfafs1)
           (parse/tfa tfafs2))]
    [`{,tfafs1 ,tfafs2}
     (app (parse/tfa tfafs1)
          (parse/tfa tfafs2))]
    [`{fun {,x} ,tfafs1}
     (fun x
          (parse/tfa tfafs1))]
    [`{fixFun ,f {,x} ,tfafs1}
     (fixFun f
             x
             (parse/tfa tfafs1))]
    [`{setobj ,tfafs1 ,tfafs2}
     (setobj (parse/tfa tfafs1)
             (parse/tfa tfafs2))]     
    [`{seqn ,tfafs1 ,tfafs2}
     (seqn (parse/tfa tfafs1)
           (parse/tfa tfafs2))]     
    [otherwise (error 'parse/tfa "Bad TFA expression: ~a" tfafs)]))


;; LOTFAFS -> (listof TFA)
;; parse lotfafs as a list of TFA expressions
;; Effect: signasl an error in case of parse failure
(define (parse-lotfa lotfafs)
  (match lotfafs
    [`{} '()]
    [`{,tfafs . ,lotfafs} (cons (parse/tfa tfafs) (parse-lotfa lotfafs))]
    [otherwise (error 'parse/tfa "Bad list of TFA expressions: ~a" lotfafs)]))


(test/match
 (parse/tfa WAES4)
 (app (fixFun g1 'x (app (fixFun g2 'y (add (id 'y) (id 'y)))
                         (add (id 'x) (num -3))))
      (add (num 5) (num 5))))

(test/match
 (parse/tfa WAES5)
 (app (fixFun g1 'x (add (id 'x)
                         (app (fixFun g2 'x (num 10)) (num 3))))
      (num 5)))

(test/match
 (parse/tfa WAES6)
 (app (fixFun g1 'x (add (id 'x) (app (fixFun g2 'x (id 'x))
                                      (num 3))))
      (num 5)))

(test/match
 (parse/tfa WAES7)
 (app (fixFun g1 'x (add (id 'x) (app (fixFun g2 'y (id 'x))
                                      (num 3))))
      (num 5)))


;;
;; PUTTING IT ALL TOGETHER  - an interpreter of files on disk
;;

;; S-expression -> Value
;; produce the result of interpreting the HG program in input
;; EFFECT: signals an error if interpretation signals a runtime error.
(define (interp-sexp pgm)
  (interp/tfa
   (parse/tfa
    pgm)))

(test (interp-sexp '{with {f {fixFun x {y} {seqn {setobj x 7} x}}}
                          {f 2}})
      (numV 7))


(test (interp-sexp '{ifB #t 7 9}) (numV 7))

(test/exn (interp-sexp '{ifB 7 #t 9}) "Bad Boolean")

(test (interp-sexp
       '{with {x #t}
              {ifB x 7 9}})
      (numV 7))

(test/exn (interp-sexp
           '{with {x 7}
                  {ifB x #t 9}})
          "Bad Boolean")

(test/exn (interp-sexp
           '{with {a {fun {b} b}}
                  {nought? a}})
          "Bad number")

(test (interp-sexp
       '{with {a 5}
              {with {b 6}
                    {with {c {ifB #t a b}}
                          {seqn {setobj c 7}
                                a}}}})
      (numV 7))


(test (interp-sexp
       '{with {a 5}
              {with {f {fun {b} a}}
                    {with {c {f 6}}
                          {seqn {setobj c 7}
                                a}}}})
      (numV 7))

(test (interp-sexp
       '{with {f {fun {a} {fun {b} a}}}
              {with {g {f 9}}
                    {with {d {g 2}}
                          {with {c {g 3}}
                                {seqn {setobj c 7}
                                      d}}}}})
      (numV 7))

(test (interp-sexp
       '{with {a 5}
              {with {b 6}
                    {seqn {setobj {ifB #t a b} 7}
                          a}}})
      (numV 7))

(test (interp-sexp
       '{with {f {fun {a} {fun {b} a}}}
              {with {g {f 9}}
                    {with {d {g 2}}
                          {seqn {setobj {g 3} 7}
                                d}}}})
      (numV 7))

(test/exn (interp-sexp
           '{with {f {fun {a} 6}}
                  {seqn {setobj {f 2} 7}
                        12}})
          "Bad L-Value")



;; String -> Value
;; produce the result of interpreting the HG program in input
;; EFFECT: signals an error if interpretation signals a runtime error.
(define (interp-string pgm)
  (interp/tfa
   (parse/tfa
    (read-from-string pgm))))


;; String -> Value
;; produce the result of interpreting the HG stored in the file fname
;; EFFECT: signals an error if no file fname contains an AE representation
;;         or if interpretation signals a runtime error.
(define (interp-file fname)
  (interp/tfa
   (parse/tfa
    (read-from-file fname))))

(test (with-temporary-data-file "{+ 3 7}\n"
        (λ (fname) (interp-file fname)))
      (numV 10))

(test (with-temporary-data-file "{+ {+ 3 -4} 7}\n"
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
                                {ifB {nought? rhs}
                                     0
                                     {+ lhs {{mult lhs} {+ rhs -1}}}}}}}
             {{* 20} 3}}"
        (λ (fname) (interp-file fname)))
      (numV 60))

(test (with-temporary-data-file
          "{with {* {fixFun mult {lhs} {fun {rhs}
                                {ifB {nought? rhs}
                                     0
                                     {+ lhs {{mult lhs} {+ rhs -1}}}}}}}
             {{* 20} 3}}"
        (λ (fname) (interp-file fname)))
      (numV 60))


(test (with-temporary-data-file
          "{with {x 5}
       {with {y x}
             {seqn {setobj y 7}
                   x}}}
"
        (λ (fname) (interp-file fname)))
      (numV 7)) ;; Call-by-reference: 7
