#lang plai
;(define ... (λ args (cons '... args))) 
(print-only-errors #t) ;; enables us to use ... in templates
(require "datatype-311.rkt")

;; For Problems 4 and 5: Type Checking and Type Inference

;; Implicit Typing for Tuplely Language (ITTL)

(require "solver.rkt") ;; For TypeExpr, Type, and unify

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relevant declarations from solver.rkt repeated here for reference

;; Variable is Symbol
;; interp. a unification variable

;; variable? : Any -> Boolean
;; produce true if the argument is a variable, otherwise false

#;
(define-type TypeExpr
  [emptyType]     ;; NO OPERATIONS, NO VALUES!
  [numType]
  [funType (arg TypeExpr?) (result TypeExpr?)]
  [tupleType (elems (listof TypeExpr?))]
  [unifVar (id variable?)])
;; interp. an expression of a type that has 0 or more as yet unknown components,
;;         denoted by variables X.  Corresponds to the following BNF:
;; <TypeExpr> ::= empty
;;              | number
;;              | <TypeExpr> -> <TypeExpr>
;;              | (tupleType <TypeExpr>*)
;;              | X

;; Type is one of
;; - (emptyType)
;; - (numType)
;; - (funType Type Type)
;; - (tupleType (listof Type))
;; interp. a TypeExpr containing no metavariables
;; <Type> ::= empty | number | <Type> -> <Type> | (tupleType <TypeExpr>*)

;; Type? : Any -> Boolean
;; produce true if the argument is a type, otherwise false

;; (substof X) is Variable -> X

;; empty-subst : (substof X)
;; a substitution that maps all variables to their corresponding type expression

;; make-subst : Variable TypeExpr -> (substof TypeExpr)
;; produce a subst that replaces X with texpr, otherwise acts like empty-subst

;; apply-subst : (substof TypeExpr) TypeExpr -> TypeExpr
;; replace all type variables in texpr with their mapping in subst

#;
(define-type Equation
  [assert-equal (lhs TypeExpr?) (rhs TypeExpr?)])
;; interp.  An equation over type expressions.

;; SystemOfEqns is (listof Equation)
;; interp. a system of equations over types.

;; unify : SystemOfEqns -> (substof Type)
;; produce a substitution that solves the given system of equations


;; end of declarations from solver.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The syntax of TL and ITTL

;; Any -> Boolean
;; produce true if the given object is an TL identifier, otherwise false 
(define (identifier? x)
  (local [(define RESERVED '(+ fixFun tuple match-with with))]
    (and (symbol? x)
         (not (memq x RESERVED)))))

(define-type TL
  [num (n number?)]
  [add (lhs TL?) (rhs TL?)]
  [id (x symbol?)]
  [fixFun (f symbol?) (x symbol?) (body TL?)]
  [app (rator TL?) (rand TL?)]
  [tuple (args (listof TL?))]
  [match-with (ids (listof identifier?)) (named TL?) (body TL?)])
;; interp. An AST for TL programs, matching the following BNF:
;; <TL> ::= <number>
;;         | {+ <TL> <TL>}
;;         | <id>
;;         | {fixFun <id> {<id>} <TL>}
;;         | {<TL> <TL>}
;;         | {tuple <TL>*}
;;         | {match-with {{tuple <id>*} <TL>}
;;             <TL>}
;;            where ids in <id*> are unique

;; Examples
(define TL1 (num 7))
;; Unbound identifier
(define TL2 (add (num 6) (id 'g)))
(define TL3 (tuple (list)))
(define TL4 (fixFun 'f 'x (id 'x)))
(define TL5 (app (fixFun 'add1 'x (add (id 'x) (num 1)))
                 (num 2)))
(define TL6 (tuple (list (add (num 1) (num 2)) (num 3))))

(define TL7 (tuple (list (num 1) (num 2) (num 3))))

;; {match-with {{tuple x y} {tuple 5 6}}
;;    {+ x y}}
;; produces 11
(define TL8
  (match-with (list 'x 'y) (tuple (list (num 5) (num 6)))
              (add (id 'x) (id 'y))))

;; {match-with {{tuple} {tuple}}
;;    {+ 2 3}}
;; produces 5
(define TL9
  (match-with (list) (tuple (list))
              (add (num 2) (num 3))))

;; Unbound identifier
(define TL10
  (match-with (list) (tuple (list (num 5) (num 6)))
              (add (id 'x) (id 'y))))

;; {match-with {{tuple x} {tuple 5 6}}
;;    {+ x 7}}
;; static type error
(define TL11
  (app (fixFun 'two-args 'args
               (match-with '(x y) (id 'args)
                           (add (id 'x) (id 'y))))
       (tuple (list (num 9) (num 7)))))

;; TL dynamic error or ITTL type error
(define TL1e
  (match-with (list 'x) (tuple (list (num 5) (num 6)))
              (add (id 'x) (num 7))))

#;
(define (fn-for-tl tl)
  (type-case TL tl
    [num (n) (... n)]
    [add (lhs rhs) (... (fn-for-tl lhs) (fn-for-tl rhs))]
    [id (x) (... x)]
    [fixFun (f x body) (... f x (fn-for-tl body))]
    [app (rator rand) (... (fn-for-tl rator) (fn-for-tl rand))]
    [tuple (args) (... (fn-for-lotl args))] ;; fn-for-lotl is for lists of tl
    [match-with (ids named body)
                (... ids (fn-for-tl named) (fn-for-tl body))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments

;; (envof X) is (listof (list symbol X))

;; (Any -> Boolean) -> (Any -> Boolean)
;; produce true if the given object is an (envof X)
(define ((envof? X?) x)
  (match x
    [(list `(,x ,v) ...) #:when (and (andmap symbol? x)
                                     (andmap X? v)) #t]
    [else #f]))

;; (listof Symbol) -> Boolean
;; produce true if each symbol in x* appears only once
(define (unique? lox)
  (cond
    [(empty? lox) #t]
    [else ;; (cons? lox)
     (and (not (member (first lox) (rest lox)))
          (unique? (rest lox)))]))

(test (unique? '()) #t)
(test (unique? '(a)) #t)
(test (unique? '(a b c d)) #t)
(test (unique? '(a b c b)) #f)
(test (unique? '(a a c d)) #f)

;; empty-env : (envof X)
(define empty-env '())

;; extend-env : (envof X) (listof symbol) (listof X) -> (envof X)
;; Produce an environment that binds distinct symbols in x* to values in v*.
;; ASSUME: (= (length x*) (length v*))
;; ASSUME: (unique? x*)
(define (extend-env env x* v*)
  (append (map list x* v*) env))

(test (extend-env empty-env '(x y z) '(5 6 7)) '((x 5)
                                                 (y 6)
                                                 (z 7)))
(test (extend-env
       (extend-env empty-env '(x y z) '(5 6 7))
       '(a x c) '(5 6 7))
      '((a 5)
        (x 6)
        (c 7)
        (x 5)
        (y 6)
        (z 7)))

;; lookup-env : (envof X) symbol -> X
;; Produce the binding for the given symbol.
;; Effect: Signals an error if no binding is found.
(define (lookup-env env x)
  (cond [(empty? env) (error 'lookup-env "unbound identifier: ~a" x)]
        [else (if (symbol=? x (car (first env)))
                  (cadr (first env))
                  (lookup-env (rest env) x))]))

(test (lookup-env (extend-env
                   (extend-env empty-env '(x y z) '(5 6 7))
                   '(a x c) '(5 6 7))
                  'z)
      7)

;; (envof X) symbol -> boolean
;; produce true if the identifier is bound in the environment, otherwise false
(define (in-env? env x)
  (if (assoc x env)
      #t
      #f))

;; (envof X) (listof identifier) -> (envof X)
;; remove the initial bindings of env indicated by loid
(define (shrink-env env loid)
  (cond
    [(empty? loid) env]
    [else ; (cons? loid)
     (if (symbol=? (caar env) (first loid))
         (shrink-env (rest env) (rest loid))
         (error 'shrink-env "Binding mismatch: ~a ∈ ~a"  (first loid) env))]))


(test (shrink-env empty-env empty) empty-env)

(test (shrink-env (extend-env empty-env (list 'x 'y) (list 7 8))
                  (list 'x))
      (extend-env empty-env (list 'y) (list 8)))

(test (shrink-env (extend-env empty-env (list 'x 'y) (list 7 8))
                  (list 'x 'y)) empty-env)

(test/exn (shrink-env (extend-env empty-env (list 'x 'y) (list 7 8))
                      (list 'y)) "mismatch")


;; (envof X) (listof Symbol) (listof X) -> Boolean
;; produce true if env0 = (extend-env env^ x*0 v*0) for some env^, else false
;; ASSUME: (= (length x*) (length v*))
;; ASSUME: (unique? x*)
(define (extends-env? env0 x*0 v*0)
  (let loop ([x* x*0] [v* v*0] [env env0])
    (cond [(empty? x*) #t]
          [else (match env0
                  [(cons (list x^ v^)  env^)
                   (and (equal? (first x*) x^)
                        (equal? (first v*) v^)
                        (loop (rest x*) (rest v*) (rest env)))]
                  [else #f])])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Checking 

;; Tenv is (envof Type)
;; interp. The type associated to each identifier in the surrounding context

;; Any -> Boolean
;; produce true if the argument is a Tenv, else false
(define Tenv? (envof? Type?))


;; Stubs for back-patching side-conditions with exam code
(define CHECK-TUPLE-REF (box (void)))
(define CHECK-MATCH-WITH-REF (box (void)))

(define (set-check-tuple/wt! fn)
  (set-box! CHECK-TUPLE-REF fn))

(define (set-check-match-with/wt! fn)
  (set-box! CHECK-MATCH-WITH-REF fn))

;; Tenv TL/wt -> Boolean
;; produce true if arguments satisfy tuple/wt side-conditions else false
(define (chk-tuple/wt tenv args/wt)
  ((unbox CHECK-TUPLE-REF) tenv args/wt)) 

;; (listof identifier?) (listof Type) TL/wt TL/wt -> Boolean
;; produce true if arguments satisfy match-with/wt side-conditions else false
(define (chk-match-with/wt ids types named/wt body/wt)
  ((unbox CHECK-MATCH-WITH-REF) ids types named/wt body/wt)) 


;; tenv ⊢ tl : type  judgment.
(define-type TL/wt

  ;; [num/wt]
  ;; ----------------------
  ;;    tenv ⊢ n : number
  [num/wt (tenv Tenv?) (n number?)]

  ;; [add/wt]  tenv1 ⊢ tl1 : type1   tenv2 ⊢ tl2 : type2
  ;; ------------------------------------------------------------
  ;;            tenv1 ⊢ {+ tl1 tl2} : number
  ;; CHECK: type1 = type2 = number
  ;; CHECK: tenv1 = tenv2
  [add/wt (tl/wt1 TL/wt?) (tl/wt2 TL/wt?)
          #:when (and
                  (equal? (tl/wt-type tl/wt1) (numType))
                  (equal? (tl/wt-type tl/wt2) (numType))
                  (equal? (tl/wt-tenv tl/wt1)
                          (tl/wt-tenv tl/wt2)))]

  ;; [id/wt]
  ;; ------------------------
  ;;     tenv ⊢ x : type
  ;; CHECK: (lookup-env tenv x) = type
  [id/wt (tenv Tenv?) (x identifier?)
          #:when (in-env? tenv x)]

  ;; [fixFun/wt]  tenv2 ⊢ tl : type1
  ;; ------------------------------------------------------------
  ;;            tenv1 ⊢ {fixFun {f} {x} tl} : f-type
  ;; CHECK: tenv2 = (extend-env tenv1
  ;;                            (list f x)
  ;;                            (list f-type type0))
  ;; CHECK: f-type = type0 -> type1
  [fixFun/wt (f identifier?) (f-type Type?) (x identifier?) (tl/wt TL/wt?)
             #:when (and (funType? f-type)
                       (match-let ([(funType param-type result-type) f-type]
                                   [tenv1 (tl/wt-tenv tl/wt)])
                         (extends-env? tenv1
                                       (list f x)
                                       (list f-type param-type))
                         (equal? (tl/wt-type tl/wt) result-type)))]

  ;; [app/wt]  tenv1 ⊢ tl1 : type1   tenv2 ⊢ tl2 : type2
  ;; ------------------------------------------------------------
  ;;            tenv1 ⊢ {tl1 tl2} : type3
  ;; CHECK: tenv1 = tenv2
  ;; CHECK: type1 = type2 -> type3
  [app/wt (tl/wt1 TL/wt?) (tl/wt2 TL/wt?)
        #:when (and (equal? (tl/wt-tenv tl/wt1) (tl/wt-tenv tl/wt2))
                    (funType? (tl/wt-type tl/wt1))
                    (match-let ([(funType param-type result-type)
                                 (tl/wt-type tl/wt1)])
                      (equal? (tl/wt-type tl/wt2) param-type)))]


  ;; [tuple/wt] tenv1 ⊢ tl1 : type1  tenv2 ⊢ tl2 : type2 ... tenvN ⊢ tlN : typeN
  ;; ---------------------------------------------------------------------------
  ;;         tenv ⊢ {tuple tl1 tl2 ... tlN} : (tupleType type1 type2 ... typeN)
  ;; CHECK: tenv = tenv1 = tenv2 = ... = tenvN
  [tuple/wt (tenv Tenv?) (args/wt (listof TL/wt?))
            #:when (chk-tuple/wt tenv args/wt)]


  ;;                 tenv1 ⊢ tl1 : typeA 
  ;; [with-tuple/wt] tenv2 ⊢ tl2 : typeB
  ;; ----------------------------------------------------------------------
  ;;         tenv1 ⊢ {match-with {{tuple x1 x2 ... xN} tl1}
  ;;                   tl2} : typeB
  ;; CHECK: (length (list type1 type2 ... typeN)) = (length (list x1 x2 ... xN))
  ;; CHECK: (unique? (list x1 x2 ... xN))
  ;; CHECK: typeA = (tupleType type1 type2 ... typeN)
  ;; CHECK: tenv2 = (extend-env tenv1
  ;;                            (list x1 x2 ... xN)
  ;;                            (list type1 type2 ... typeN))
  [match-with/wt (ids (listof identifier?)) (types (listof Type?))
                 (named/wt TL/wt?) (body/wt TL/wt?)
                 #:when (chk-match-with/wt ids types named/wt body/wt)])

; template
#;
(define (fn-for-tl/wt tl/wt)
  (type-case TL/wt tl/wt
    [num/wt (tenv n) (... tenv n)]
    [add/wt (tl/wt1 tl/wt2)
            (... (fn-for-tl/wt tl/wt1)
                 (fn-for-tl/wt tl/wt2))]
    [id/wt (tenv x) (... tenv x)]
    [fixFun/wt (f type x tl/wt)
               (... f type x (fn-for-tl/wt tl/wt))]
    [app/wt (tl/wt1 tl/wt2)
            (... (fn-for-tl/wt tl/wt1)
                 (fn-for-tl/wt tl/wt2))]
    [tuple/wt (tenv args/wt) (... tenv (fn-for-listof-tl/wt args/wt))]
    [match-with/wt (ids types named/wt body/wt)
                   (... ids
                        types
                        (fn-for-tl/wt named/wt)
                        (fn-for-tl/wt body/wt))]))
#;
(define (fn-for-listof-tl/wt lotl/wt) (... lotl/wt))




;; Selector functions:
;; * Produce the type environment of the judgment
;; * Produce the expression of the judgment
;; * Produce the type component of the judgment


;; TL/wt -> Tenv
;; produce the type environment associated with the given derivation
(define (tl/wt-tenv tl/wt)
  (type-case TL/wt tl/wt
    [num/wt (tenv n) tenv]
    [add/wt (tl/wt1 tl/wt2)
            (tl/wt-tenv tl/wt1)] ; arbitrarily choose left
    [id/wt (tenv x) tenv]
    [fixFun/wt (f type x tl/wt^)
               (let ([tenv (tl/wt-tenv tl/wt^)])
                 (shrink-env tenv (list f x)))]
    [app/wt (tl/wt1 tl/wt2)
            (tl/wt-tenv tl/wt1)]
    [tuple/wt (tenv args/wt) tenv] ;; because args/wt may be empty
    [match-with/wt (ids types named/wt body/wt)
                   (tl/wt-tenv named/wt)]))


;; TL/wt -> TL
;; produce the tl expression associated with the given derivation
(define (tl/wt-tl tl/wt)
  (type-case TL/wt tl/wt
    [num/wt (tenv n) (num n)]
    [add/wt (tl/wt1 tl/wt2)
            (add (tl/wt-tl tl/wt1)
                 (tl/wt-tl tl/wt2))]
    [id/wt (tenv x) (id x)]
    [fixFun/wt (f f-type x tl/wt)
               (fixFun f x (tl/wt-tl tl/wt))]
    [app/wt (tl/wt1 tl/wt2)
            (app (tl/wt-tl tl/wt1)
                 (tl/wt-tl tl/wt2))]
    [tuple/wt (tenv args/wt) (tuple (map tl/wt-tl args/wt))]
    [match-with/wt (ids types named/wt body/wt)
                   (match-with ids
                               (tl/wt-tl named/wt)
                               (tl/wt-tl body/wt))]))


;; TL/wt -> Type
;; produce the type associated with the given derivation
(define (tl/wt-type tl/wt)
  (type-case TL/wt tl/wt
    [num/wt (tenv n) (numType)]
    [add/wt (tl/wt1 tl/wt2)
            (numType)]
    [id/wt (tenv x) (lookup-env tenv x)]
    [fixFun/wt (f f-type x tl/wt)
               f-type] ; return self-reference's type
    [app/wt (tl/wt1 tl/wt2)
            (funType-result (tl/wt-type tl/wt1))]
    [tuple/wt (tenv args/wt) (tupleType (map tl/wt-type args/wt))]
    [match-with/wt (ids types named/wt body/wt)
                   (tl/wt-type body/wt)]))


;; Type -> string
;; produce a string corresponding to a given type
(define (type-string type)
  (match type
    [(emptyType) "(empty)"]
    [(numType) "(number)"]
    [(funType dom rng) (string-append "("
                                      (type-string dom)
                                      " -> "
                                      (type-string rng)
                                      ")")]
    [(tupleType types)
     (string-append "(tupleType "
                    (foldr (λ (t rlot)
                             (string-append (type-string t) " " rlot))
                           ""
                           types)
                    ")")]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Inference

;; Data Type Expressions (X-Types): Data Types with Metavariables where we
;;   want types

;; TenvX is (envof TypeExpr)
;; interp. The type *expression* associated to each identifier
(define TenvX? (envof? TypeExpr?))

;; TL/wtX - Type Derivation *Expressions*
(define-type TL/wtX
  [num/wtX (tenvX TenvX?) (n number?)]
  [add/wtX (tl/wt1 TL/wtX?) (tl/wt2 TL/wtX?)]
  [id/wtX (tenvX TenvX?) (x identifier?)]
  [fixFun/wtX (f identifier?) (f-typeX TypeExpr?) (x identifier?)
              (tl/wtX TL/wtX?)]
  [app/wtX (tl/wtX1 TL/wtX?) (tl/wtX2 TL/wtX?)]
  [tuple/wtX (tenvX TenvX?) (args/wtX (listof TL/wtX?))]
  [match-with/wtX (ids (listof identifier?)) (typeXs (listof TypeExpr?))
                  (named/wtX TL/wtX?) (body/wt TL/wtX?)])

;; (substof Type) TenvX -> Tenv
;; produce a type environment from the given substitution and tenv expression
(define (apply-subst-tenvX subst tenvX)
  (for/list ([binding tenvX])
    (match binding
      [(list x typeX) (list x (apply-subst subst typeX))])))


;; (substof Type) TL/wtX -> TL/wt
;; produce a derivation from the given subsitution and derivation expression
(define (apply-subst-tl/wtX subst tl/wt)
  (type-case TL/wtX tl/wt
    [num/wtX (tenvX n) (num/wt (apply-subst-tenvX subst tenvX)
                               n)]
    [add/wtX (tl/wtX1 tl/wtX2)
             (add/wt (apply-subst-tl/wtX subst tl/wtX1)
                     (apply-subst-tl/wtX subst tl/wtX2))]
    [id/wtX (tenvX x) (id/wt (apply-subst-tenvX subst tenvX)
                             x)]
    [fixFun/wtX (f typeX x tl/wtX)
                (fixFun/wt f
                           (apply-subst subst typeX)
                           x
                           (apply-subst-tl/wtX subst tl/wtX))]
    [app/wtX (tl/wtX1 tl/wtX2)
             (app/wt (apply-subst-tl/wtX subst tl/wtX1)
                     (apply-subst-tl/wtX subst tl/wtX2))]
    [tuple/wtX (tenvX args/wtX)
               (tuple/wt (apply-subst-tenvX subst tenvX)
                         (for/list ([arg/wtX args/wtX])
                           (apply-subst-tl/wtX subst arg/wtX)))]
    [match-with/wtX (ids typeXs named/wtX body/wtX)
                    (match-with/wt ids
                                   (map (λ (typeX) (apply-subst subst typeX))
                                        typeXs)
                                   (apply-subst-tl/wtX subst named/wtX)
                                   (apply-subst-tl/wtX subst body/wtX))]))


;; ITTLPgm is TL
;; Invariant: '() ⊢ tl : type for some type.
;; interp. Implicitly Typed TL program
