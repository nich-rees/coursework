#lang plai

(require racket/stream)
;(require "parsing.rkt")
(define ... list) ;; enables us to use ... in templates

(print-only-errors #t)

;;
;; mca.rkt - a language with support for first-class continuations
;;  Named in honour of MCA (1964-2012) https://en.wikipedia.org/wiki/Adam_Yauch
;;  'cause you can't, you won't, and you don't stop 


;; Syntax for MCA:
;; <MCA> ::=
;; (ARITHMETIC)
;;          <num>
;;        | {+ <MCA> <MCA>}
;;        | {- <MCA> <MCA>}
;; (IDENTIFIERS)
;;        | {with {<id> <MCA>} <MCA>}
;;        | <id>
;; (CONDITIONALS)
;;        | {if0 <MCA> <MCA> <MCA>}
;; (FUNCTIONS)
;;        | {<MCA> <MCA>}
;;        | {fun {<id>} <MCA>}
;; (RECURSION)
;;        | {fix <id> <MCA>}
;;        | {fixFun <id> {<id>} <MCA>}
;; (SEQUENCING)
;;        | {seqn <MCA> <MCA>}
;; (CONTINUATIONS)
;;        | {letcc {<id>} <MCA>}
;;        | {throwcc <MCA> <MCA>}

;; Unlike previous intepreters, we will *NOT* use this syntax as the direct
;; basis for our interpreter: we'll do something a bit different, which will
;; lead to different tradeoffs.

;; 1) We will define a datatype for an "internal language" (language used by
;; our interpreter, for which code is not written by a person).  This
;; internal language intentionally looks *a lot* like the implementations of
;; our previous interpreters, in that it has features corresponding to our
;; effect abstraction:
;;   a) let/eff ≡ do
;;   b) return/eff ≡ return
;; 
;; also the language will explicitly be split into two parts: Pure MCA
;; Expressions (PMCA) e and
;; MCA Computations (CMCA) c.  So our language will be defined using two
;; datatypes
;;
;; 2) To define our interpreter for our whole language, we write a parser
;;    that translates our "external language" (the language of programs written
;;    by people) into our "internal language" (the language consumed by our
;;    interpreter).  In short, we are writing a small compiler.
;;
;; This approach amounts to refactoring some of the complexity of our
;; interpreter in interesting ways (naturally with tradeoffs):
;;
;; 1) Our front-end compiler is now responsible for describing the dependency
;;    order of our source computations:  "do", like let/eff, determines which
;;    computations happen in which order, and "return" exposes the
;;    distinction between where values and computations are created and
;;    produced.
;;
;; 2) Our substitution function subst gets more complicated.  Now we have to
;;    deal with mutual references, so mutual recursion comes into play. :(
;;
;; 3) Our interpreter gets *even simpler*.  Now there's a case for do and for
;;    return, but in trade every other case has gotten super-simple because
;;    the compilation phase has turned all the necessary subcomputations into
;;    values already.  This style (compile then interpret) is how Eugenio Moggi
;;    originally presented these ideas in a purely mathematical context.
;;    It turns out that they also lead to an interesting and useful
;;    design pattern for writing interpreters.

(define-type CMCA    
  [return/fx (v PMCA?)]
  [let/fx (x symbol?) (named CMCA?) (body CMCA?)]
  [app (rator PMCA?) (arg PMCA?)]
  [if0 (predicate PMCA?) (consequent CMCA?) (alternative CMCA?)]
  [fix (name symbol?) (body CMCA?)]
  [cid (name symbol?)] ;; computation identifiers are for fix expressions
  ;; *NEW*
  [letcc (name symbol?) (body CMCA?)]
  [throwcc (kont PMCA?) (arg PMCA?)])


(define-type PMCA
  [id (name symbol?)] ;; MCA identifiers always denote (get replaced by) values
  [num (n number?)]
  [add (lhs PMCA?) (rhs PMCA?)]
  [sub (lhs PMCA?) (rhs PMCA?)]
  [fun (param symbol?) (body CMCA?)]
  [kont (k Kont?)])
;; interp. an internal, core, "computational" language for MCA, which
;; explicitly distinguishes between computations (CMCA) and values (Value).

(define (fn-for-cmca f)
  (type-case CMCA f
    [return/fx (e) (fn-for-pmca e)]
    [let/fx (x c1 c2) (... x
                           (fn-for-cmca c1)
                           (fn-for-cmca c2))]
    [app (vrator vrand) (... (fn-for-pmca vrator)
                             (fn-for-pmca vrand))]
    [if0 (ep cc ca) (... (fn-for-pmca ep)
                         (fn-for-cmca cc)
                         (fn-for-cmca ca))]
    [fix (x cbody) (... x
                        (fn-for-cmca cbody))]
    [cid (f) (... f)]
    [letcc (k cbody) (... k (fn-for-cmca cbody))]
    [throwcc (k e) (... k (fn-for-pmca e))]))


(define (fn-for-pmca e)
  (type-case PMCA e
    [id (x) (... x)]
    [num (n) (... n)]
    [add (el er) (... (fn-for-pmca el)
                      (fn-for-pmca er))]
    [sub (el er) (... (fn-for-pmca el)
                      (fn-for-pmca er))]
    [fun (id body) (... id (fn-for-cmca body))]
    [kont (k) (... k)]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Value is one of
;; - (num number)
;; - (fun symbol CMCA)
;; - (kont Kont)
;; interp. a runtime value
#;
(define (fn-for-value v)
  (match v
    [(num n) (... n)]
    [(fun id body) (... id (fn-for-lola body))]
    [(kont k) (... k)]))


;; any -> boolean
;; produce true if x is a value, otherwise false
(define (Value? x)
  (match x
    [(num n) #t]
    [(fun id body) #t]
    [(kont k) #t]
    [else #f]))


;; Identifier is one of:
;; - (id symbol)
;; - (cid symbol)
;; interp. value and computation identifier references
(define (ident->symbol ident)
  (match ident
    [(id x) x]
    [(cid x) x]))


;; NOTE: the following signature is a "union type" = two distinct signatures!
;; (CMCA id Value -> CMCA) ∪ (CMCA cid CMCA -> CMCA)
;; substitute v0 for references to ident0 in mca
;; NOTE: it's not a well-formed operation to substitute computations for vars!
(define (subst mca ident0 v0)
  (local [;; CMCA symbol -> CMCA
          ;; substitute mca0 for x0 in cmca if binder ident is not ident0
          (define (maybe-subst-in cmca ident)
            (if (equal? ident ident0)
                cmca
                (recur-cmca cmca)))

          ;; Value -> Value
          ;; substitute into the given value
          (define (recur-pmca v)
            (type-case PMCA v
              [id (x) (if (equal? (id x) ident0)
                          v0
                          (id x))]
              [add (el er) (add (recur-pmca el)
                                (recur-pmca er))]
              [sub (el er) (sub (recur-pmca el)
                                (recur-pmca er))]
              [num (n) (num n)]
              [fun (x body) (fun x (maybe-subst-in body (id x)))]
              [kont (k) (kont k)]))
         
          ;; CMCA -> CMCA
          ;; substitute into the given computation
          (define (recur-cmca expr1)
            (type-case CMCA expr1
              [return/fx (v) (return/fx (recur-pmca v))]
              [let/fx (x c1 c2) (let/fx x (recur-cmca c1)
                                        (maybe-subst-in c2 (id x)))]
              [app (vrator vrand) (app (recur-pmca vrator)
                                       (recur-pmca vrand))]
              [if0 (vp c a)
                   (if0 (recur-pmca vp)
                        (recur-cmca c)
                        (recur-cmca a))]
              [fix (x body) (fix x (maybe-subst-in body (cid x)))]
              [cid (f) (if (equal? (cid f) ident0)
                           v0
                           (cid f))]
              [letcc (k body) (letcc k (maybe-subst-in body (cid k)))]
              [throwcc (vkont varg) (throwcc (recur-pmca vkont)
                                             (recur-pmca varg))]))]
    (recur-cmca mca)))


(test (subst (return/fx (num 5)) (id 'x) (num 7)) (return/fx (num 5)))
(test (subst (return/fx (id 'x)) (id 'x) (num 7)) (return/fx (num 7)))
(test (subst (return/fx (id 'y)) (id 'x) (num 7)) (return/fx (id 'y)))
(test (subst (return/fx (fun 'y (return/fx (id 'x)))) (id 'x) (num 7))
      (return/fx (fun 'y (return/fx (num 7)))))
(test (subst (return/fx (fun 'x (return/fx (id 'x)))) (id 'x) (num 7))
      (return/fx (fun 'x (return/fx (id 'x)))))

(test (subst (return/fx (fun 'y (cid 'f))) (cid 'f) (return/fx (num 7)))
      (return/fx (fun 'y (return/fx (num 7)))))

(test (subst (fix 'f (cid 'g)) (cid 'g) (return/fx (num 7)))
      (fix 'f (return/fx (num 7))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effect Abstraction

;;
;; Effect Interface
;;

;; (letcc/eff ([k]) c1) - execute c1, with k bound to the current continuation
;; (throwcc/eff e1 e2) - reinstate the continuation resulting from e1, and
;;                       proceed with the result of e2

;; The Generic Interface Components:
;; (return/eff e) - return the value of e
;; (run/eff c) - run an effectful computation
;; (let/eff ([x c1] c2)) - bind x to the next successful value of c1 in c2
;; (let/eff* ([x* c1*] ...) c2) - sequentialize instances of let/eff

;;
;; Effect Implementation
;;

;; Kont is (Value -> Value)
;; interp.  a procedural representation of "the rest of the computation".
(define Kont? procedure?)

;;
;; Computation is (Kont -> Value) (i.e. ((Value -> Value) -> Value) )
;; interp. a partial computation that when given "the rest of the computation"
;; produces the final result of the entire computation

(define-syntax letcc/eff
  (syntax-rules ()
    [(_ ([k]) c1)     
     (λ (kont)
       (let ([k kont])
         (c1 kont)))]))

;; Kont
;; ignore the current continuation, use the provided one.
(define (throwcc/eff override-kont v)
  (λ (kont) (override-kont v)))


;; Value -> Computation
;; "return" a successful value (by embedding it in a Computation)
(define (return/eff v)
  (λ (kont) (kont v)))


;; Computation -> Value
;; produce a value corresponding to the given Computation
(define (run/eff c0)
  ;; after c0 completes, there is nothing more to do, so just force c0 to
  ;; produce its value.
  (c0 identity))

;; Compose two computations
;; If  (c1 : Computation) and (x : Value implies c2 : Computation) then 
;; (let/eff ([x c1]) c2) : Computation 
(define-syntax let/eff
  (syntax-rules ()
    [(_ ([x c1]) c2)
     (λ (kont0)
       (let ([kont-c1 (λ (x) (c2 kont0))])
         (c1 kont-c1)))]))


;; Compose many computations
(define-syntax let/eff*
  (syntax-rules ()
    [(_ () e) e]
    [(_ ([x e1] [x* e1*] ...) e2)
     (let/eff ([x e1])
              (let/eff* ([x* e1*] ...) e2))]))

;; End of Effect Abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Value -> number
;; produce the number corresponding to the given Value
;; Effect: signal an error if the value does not represent a number
(define (value->num v)
  (match v
    [(num n) n]
    [else (error 'interp "bad number: ~a" v)]))


;; Value Value -> Value
;; produce the result of adding v1 to v2
;; Effect: signal an error if the values do not represent numbers
(define (add-value v1 v2)
  (let ([n1 (value->num v1)]
        [n2 (value->num v2)])
    (num (+ n1 n2))))


;; Value Value -> Value
;; produce the result of subtracting v2 from v1
;; Effect: signal an error if the values do not represent numbers
(define (sub-value v1 v2)
  (let ([n1 (value->num v1)]
        [n2 (value->num v2)])
    (num (- n1 n2))))


;; Value -> boolean
;; produce true if the v is zero, otherwise false
(define (zero?-value v)
  (match v
    [(num n) (= n 0)]
    [else #f]))


;; Value Value -> Computation
;; Effect: signal an error if v1 is not a function or any other runtime errors.
(define (apply-value v1 v2)
  (match v1
    [(fun x body) (interp-cmca/eff (subst body (id x) v2))]
    [else (error 'interp "bad function: ~a" v1)]))


;; symbol CMCA -> Computation
;; interpret the self-referential expression (fix f mca)
;; Effect: signal an error in case of runtime error
(define (interp-fix f mca)
  (interp-cmca/eff (subst mca (cid f) (fix f mca))))

;; Examples follow interp-cmca/eff

;; symbol CMCA CMCA -> Computation
;; interpret the given let/fx expression
(define (interp-let/fx x c1 c2)
  (let/eff ([v (interp-cmca/eff c1)])
           (interp-cmca/eff (subst c2 (id x) v))))


;; Value -> Kont
;; produce the continuation corresponding to the given Value
;; Effect: signal an error if the value does not represent a kont
(define (value->kont v)
  (match v
    [(kont k) k]
    [else (error 'interp "bad continuation: ~a" v)]))


;; symbol CMCA -> Computation
;; interpret the given letcc expression
(define (interp-letcc k cbody)
  (letcc/eff ([k-capture])
             (interp-cmca/eff (subst cbody (id k) (kont k-capture)))))


;; Value Value -> Computation
;; interpret the given throwcc expression
(define (interp-throwcc kont varg)
  (let ([k (value->kont kont)])
    (throwcc/eff k varg)))


;; PMCA -> Value
;; produce the result of returning the given value
;; Effect: signal an error in case of unbound identifier
(define (interp-pmca/eff e)
  (type-case PMCA e
    [id (x) (error 'interp "Unbound identifier: ~s" x)]
    [add (el er) (add-value el er)]
    [sub (el er) (sub-value el er)]
    [else e]))


;; CMCA -> Computation
;; produce the result of interpreting the given MCA computation
;; Effect: signal an error in case of runtime error
(define (interp-cmca/eff cmca)
  (type-case CMCA cmca
    [return/fx (e) (return/eff (interp-pmca/eff e))]
    [let/fx (x c1 c2) (interp-let/fx x c1 c2)]
    [app (vrator vrand) (apply-value vrator vrand)]         
    [if0 (vp cc ca)
         (if (zero?-value vp)
             (interp-cmca/eff cc)
             (interp-cmca/eff ca))]
    [fix (x cbody) (interp-fix x cbody)]
    [cid (f) (error 'interp "Unbound computation identifier: ~s" f)]
    [letcc (k cbody) (interp-letcc k cbody)]
    [throwcc (kont varg) (interp-throwcc kont varg)]))


;; Examples for interp-mca/eff
(test ((interp-cmca/eff (return/fx (num 5))) identity) (num 5))

;; CMCA -> Value
;; interpret the given expression, producing n results (or all if n=false)
;; Effect: Signal an exception for runtime errors *or uncaught user exceptions*
(define (interp-cmca cmca)
  (run/eff (interp-cmca/eff cmca)))


(test (interp-cmca (return/fx (num 5)))  (num 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mca-focused-sexp (mfs) is one of:
;; - number
;; - `{+ ,hgfs ,hgfs}
;; - `{- ,hgfs ,hgfs}
;; - `{with ,identifier ,hgfs ,hgfs
;; -  identifier
;; - `{fun ,identifier hgfs}
;; - `{,hgfs ,hgfs}
;; - `{fix ,identifier ,hgfs}
;; - `{fixFun ,identifier {,identifier} ,hgfs}
;; - `{if0 ,hgfs ,hgfs ,hgfs}
;; - `{seqn ,hgfs ,hgfs}
;; - "any other s-expression"
;; where identifier is any symbol except +, -, with, if0, fix, or fun
;; interp.  any s-expression, but with a focus on those that represent
;; MCA expressions.

;; Every AE program is an MCA program
(define AE1-sexp `4)
(define AE2-sexp `{+ ,AE1-sexp 5})
(define AE3-sexp `{- 6 3})

;; Every WAE program is an MCA program
(define WAE1-sexp'{with {x {+ 5 5}} {+ x x}})

(define WAE2-sexp '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})

(define WAE3-sexp '{with {x 5} {+ x {with {x 3} 10}}})

(define WAE4-sexp '{with {x 5} {+ x {with {x 3} x}}})

(define WAE5-sexp '{with {x 5} {+ x {with {y 3} x}}})

;; FWAE *expressions* are also MCA expressions
;; (but represented using a different data type!)
(define FWAE1-sexp '{with {double {fun {n} {+ n n}}}
                          {with {x 5} {double x}}})

(define FWAE2-sexp '{with {one-plus {fun {n} {+ n 1}}}
                          {with {x 5} {one-plus x}}})

;; Thanks to fix and fun we can encode simple recursive functions
(define HG4-sexp '{fix f {fun {x} {if0 x 9 {f {- x 1}}}}})

(define HG4b-sexp '{fixFun f {x} {if0 x 9 {f {- x 1}}}})

(define HG5-sexp `{,HG4-sexp 1})

(define HG5b-sexp `{,HG4b-sexp 1})


(define (identifier? x)
  (and (symbol? x)
       (not (member x '(+ - with if0 fun fix fixFun)))))


(define (fn-for-mca-focused-sexp sexp)
  (match sexp
    [`,n
     #:when (number? n)
     (... n)]
    [`{+ ,sexp1 ,sexp2}
     (... (fn-for-mca-focused-sexp sexp1)
          (fn-for-mca-focused-sexp sexp2))]
    [`{- ,sexp1 ,sexp2}
     (... (fn-for-mca-focused-sexp sexp1)
          (fn-for-mca-focused-sexp sexp2))]
    [`{with {,x ,sexp1} ,sexp2}
     #:when (identifier? x)
     (... x
          (fn-for-mca-focused-sexp sexp1)
          (fn-for-mca-focused-sexp sexp2))]
    [`,x
     #:when (identifier? x)
     (... x)]
    [`{if0 ,sexp1 ,sexp2 ,sexp3}
     (... (fn-for-mca-focused-sexp sexp1)
          (fn-for-mca-focused-sexp sexp2)
          (fn-for-mca-focused-sexp sexp3))]
    [`{fun {,x} ,sexp1}
     #:when (identifier? x)
     (... x
          (fn-for-mca-focused-sexp sexp1))]
    [`{fix ,f ,sexp1}
     #:when (identifier? f)
     (... f
          (fn-for-mca-focused-sexp sexp1))]
    [`{fixFun ,f {,x} ,sexp1}
     #:when (and (identifier? f) (identifier? x))
     (... f
          x
          (fn-for-mca-focused-sexp sexp1))]
    [`{seqn ,sexp1 ,sexp2}
     ;; Notice that application is now the last focused case...
     [`{,sexp1 ,sexp2}
      (... (fn-for-mca-focused-sexp sexp1)
           (fn-for-mca-focused-sexp sexp2))]
     (... (fn-for-mca-focused-sexp sexp1)
          (fn-for-mca-focused-sexp sexp1))]    
    [else (... sexp)] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments

;; Env is (listof (list symbol CMCA))
;;

;; Env
;; an empty env (initial environment)
(define empty-env empty)

;; Env symbol -> CMCA
;; look up x in env
;; Effect: signal an error if x is not bound in env
(define (lookup-env env x)
  (cond
    [(assoc x env) => cadr] ;; consult cond docs if this looks crazy :)
    [else (error 'interp "Unbound Identifier: ~a" x)]))


;; Env symbol CMCA -> Env
;; produce a new environment that is updated with the new binding
(define (update-env env x cmca)
  (cond
    [(empty? env) (list (list x cmca))]
    [else ;; cond
     (if (symbol=? (first (first env)) x)
         (cons (list x cmca) (rest env))
         (cons (first env)
               (update-env (rest env) x cmca)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; s-expression -> CMCA
;; Compile the given MCA program into its corresponding CMCA program
;; EFFECT: signals an error on failure
(define (compile-expr sexp)
  (local
    ;; Accumulator env is (envof CMCA)
    ;; Invariant: associates identifiers bound in the surrounding context with
    ;;   the appropriate reference expression.
    [(define (compile-acc sexp env)
       (let recur ([sexp sexp]) ;; common case
         (match sexp
           [`,n #:when (number? n) (return/fx (num n))]
           [`{+ ,lhs ,rhs}
            (let ([g1 (gensym)] [g2 (gensym)])
              (let/fx g1 (recur lhs)
                      (let/fx g2 (recur rhs)
                              (return/fx (add (id g1) (id g2))))))]
           [`{- ,lhs ,rhs}
            (let ([g1 (gensym)] [g2 (gensym)])
              (let/fx g1 (recur lhs)
                      (let/fx g2 (recur rhs)
                              (return/fx (sub (id g1) (id g2))))))]
           [`{with {,id ,named-exp} ,body}
            #:when (identifier? id)
            ;; desugar with into anonymous function application
            (recur `{{fun {,id} ,body} ,named-exp})]
           [`,x #:when (identifier? x) (lookup-env env x)]
           [`{if0 ,pred ,conseq ,altern}
            (let ([g1 (gensym)])
              (let/fx g1 (recur pred)
                      (if0  (id g1) (recur conseq)
                            (recur altern))))]
           ;; Notice that application is now last...
           [`{fun {,x} ,sexp1}
            #:when (identifier? x)
            (return/fx
             (fun x (compile-acc sexp1 (update-env env x (return/fx (id x))))))]
           [`{fix ,f ,sexp1}
            #:when (identifier? f)
            (fix f (compile-acc sexp1 (update-env env f (cid f))))]
           [`{fixFun ,f {,x} ,sexp1}
            #:when (and (identifier? f) (identifier? x))
            ;; desugar to fix and fun
            (recur `{fix ,f {fun {,x} ,sexp1}})]
           [`{seqn ,sexp1 ,sexp2} (recur `{with {,(gensym) ,sexp1} ,sexp2})]
           [`{letcc {,k} ,sexp1}
            #:when (identifier? k)
            (letcc k (compile-acc sexp1 (update-env env k (return/fx (id k)))))]
           [`{throwcc ,sexp1 ,sexp2}
            (let ([g1 (gensym)] [g2 (gensym)])
              (let/fx g1 (recur sexp1)
                      (let/fx g2 (recur sexp2)
                              (throwcc (id g1) (id g2)))))]
           [`{,rator-exp ,rand-exp} 
            (let ([g1 (gensym)] [g2 (gensym)])
              (let/fx g1 (recur rator-exp)
                      (let/fx g2 (recur rand-exp)
                              (app (id g1) (id g2)))))]
           [_ (error 'parse-expr "bad expression ~a" sexp)])))]
    (compile-acc sexp empty-env)))


(test (compile-expr '5) (return/fx (num 5)))


;(define AE1-sexp `4)
(test (compile-expr AE1-sexp) (return/fx (num 4)))
;(define AE2-sexp `{+ ,AE1-sexp 5})
(test (match (compile-expr AE2-sexp)
        [(let/fx g1 (return/fx (num 4))
                 (let/fx g2 (return/fx (num 5))
                         (return/fx (add (id g1) (id g2))))) #t]
        [else #f])
      #t)

;(define AE3-sexp `{- 6 3})
(test (match (compile-expr AE3-sexp)
        [(let/fx g1 (return/fx (num 6))
                 (let/fx g2 (return/fx (num 3))
                         (return/fx (sub (id g1) (id g2))))) #t]
        [else #f])
      #t)

;(define WAE1-sexp'{with {x {+ 5 5}} {+ x x}})
;(define WAE2-sexp '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})
;(define WAE3-sexp '{with {x 5} {+ x {with {x 3} 10}}})
;(define WAE4-sexp '{with {x 5} {+ x {with {x 3} x}}})
;(define WAE5-sexp '{with {x 5} {+ x {with {y 3} x}}})

;(define FWAE1-sexp '{with {double {fun {n} {+ n n}}}
;                          {with {x 5} {double x}}})
;
;(define FWAE2-sexp '{with {one-plus {fun {n} {+ n 1}}}
;                          {with {x 5} {one-plus x}}})


;(test (parse-expr HG4-sexp) HG4)
;(test (parse-expr HG4b-sexp) HG4b)
;(test (parse-expr HG5-sexp) HG5)
;(test (parse-expr HG5b-sexp) HG5b)

;;
;; PUTTING IT ALL TOGETHER  - an interpreter of files on disk
;;

;; sexpr -> Value
;; produce the result of interpreting the provided MCA program in input
;; EFFECT: signals an error if interpretation signals a runtime error.
(define (interp-sexpr pgm)
  (interp-cmca
   (compile-expr pgm)))


(test (interp-sexpr '{+ 3 7}) (num 10))
(test (interp-sexpr '{+ {- 3 4} 7}) (num 6))
(test (interp-sexpr '{with {x 5} {+ x {with {y 3} x}}}) (num 10))
(test (interp-sexpr '{with {double {fun {x} {+ x x}}} {with {x 5} {double x}}})
      (num 10))
(test (interp-sexpr
       '{with {* {fix mult {fun {lhs}
                                {fun {rhs}
                                     {if0 rhs
                                          0
                                          {+ lhs {{mult lhs} {- rhs 1}}}}}}}}
              {{* 20} 3}})
      (num 60))

;; Continuation examples

(define MCA1 '{letcc {k}
                {+ 5 3}})
(test (interp-sexpr MCA1) (num 8))

(define MCA2 '{letcc {k}
                {+ 5 {throwcc k 3}}})
(test (interp-sexpr MCA2) (num 3))

(define MCA3 '{letcc {k}
                {throwcc k {+ 5 3}}})
(test (interp-sexpr MCA3) (num 8))

(define MCA4 '{+ 5
                 {letcc {k}
                   {+ {throwcc k 3} {- 9 6}}}})
(test (interp-sexpr MCA4) (num 8))

(define MCA5 '{with {x {letcc {k} k}}
                    {if0 x
                         22
                         {+ x {throwcc x 0}}}})
(test (interp-sexpr MCA5) (num 22))



(define CALLCC '{fun {p} {letcc {k}
                           {p {fun {v} {throwcc k v}}}}})

(define HOWTF `{{{,CALLCC {fun {k} k}} {fun {x} x}} 9})
(test (interp-sexpr HOWTF) (num 9))

