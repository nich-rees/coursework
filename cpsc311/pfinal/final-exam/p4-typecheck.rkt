#lang plai
;(define (... . args) (cons '... args))
(print-only-errors) ;; enables us to use ... in templates
(require rackunit)
(require "appendix/datatype-311.rkt")

;;
;; Problem 4: Type Checking (line 280)
;; Problem 5: Type Inference (line 370)
;;

;; Implicit Typing for Tuplely Language (TL)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "appendix/solver.rkt") ;; For TypeExpr, Type, Equation, and unify
(require "appendix/ittl-typing.rkt") ;; for Typechecking desiderata

;; Utility
;; A testing helper: use match in PLAI tests
;; WARNING: failures yield messy messages!
(define-syntax test/match
  (syntax-rules ()
    [(_ expr pat)
     (test (match expr
             [pat #t]
             [else #f #;(error 'test/match
                               "failed:\nexpected pattern: ~a\ngot ~a"
                               'pat expr)])
           #t)]))

;; Ensure that a derivation can be constructed
(define-syntax-rule (test/data try)
  (check-not-exn (λ () try)))

;; Detect errors during construction of a type derivation
(define-syntax-rule (test/data-exn try)
  (check-exn exn:fail? (λ () try)))


;;
;; Implicitly Typed Tupley Language (ITTL)
;;

;; The word "tuple" is a generalization of "couple, triple, quadruple, etc."
;; It's a language feature that lets you create unnamed compound data.  You
;; can think of it as a generalization of pairs, including even "zero-uples"

;; Rather than introduce selectors like "first" and "second", we introduce
;; a version of Racket's match-let mechanism.
;; NOTE THAT THE SYNTAX FOLLOWS match-let, NOT match.  Hence, we call this
;; feature match-with.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Syntax
;;

;; identifier? : Any -> Boolean
;; produce true if the given object is an identifier, otherwise false

#;
(define-type TL
  [num (n number?)]
  [add (lhs TL?) (rhs TL?)]
  [id (x symbol?)]
  [fixFun (f symbol?) (x symbol?) (body TL?)]
  [app (rator TL?) (rand TL?)]
  [tuple (args (listof TL?))]
  [match-with (ids (listof identifier?)) (named TL?)
              (body TL?)])
;; interp. An AST for TL
;; <TL> ::= <number>
;;         | {+ <TL> <TL>}
;;         | <id>
;;         | {fixFun <id> {<id>} <TL>}
;;         | {<TL> <TL>}
;;         | {mt}
;;         | {tuple <TL>*}
;;         | {match-with {{tuple <id>*} <TL>}
;;             <TL>}
;;            where ids in <id*> are unique

;; Examples
#;(define TL1 (num 7))
;; Unbound identifier
#;(define TL2 (add (num 6) (id 'g)))
#;(define TL3 (tuple (list)))
#;(define TL4 (fixFun 'f 'x (id 'x)))
#;(define TL5 (app (fixFun 'add1 'x (add (id 'x) (num 1)))
                   (num 2)))
#;(define TL6 (tuple (list (add (num 1) (num 2)) (num 3))))

#;(define TL7 (tuple (list (num 1) (num 2) (num 3))))

;; {match-with {{tuple x y} {tuple 5 6}}
;;    {+ x y}}
;; produces 11
#;(define TL8
    (match-with (list 'x 'y) (tuple (list (num 5) (num 6)))
                (add (id 'x) (id 'y))))

;; {match-with {{tuple} {tuple}}
;;    {+ 2 3}}
;; produces 5
#;(define TL9
    (match-with (list) (tuple (list))
                (add (num 2) (num 3))))

;; Unbound identifier
#;(define TL10
    (match-with (list) (tuple (list (num 5) (num 6)))
                (add (id 'x) (id 'y))))

#;(define TL11
    (app (fixFun 'two-args 'args
                 (match-with '(x y) (id 'args)
                             (add (id 'x) (id 'y))))
         (tuple (list (num 9) (num 7)))))

;; {match-with {{tuple x} {tuple 5 6}}
;;    {+ x 7}}
;; static type error
#;(define TL1e
    (match-with (list 'x) (tuple (list (num 5) (num 6)))
                (add (id 'x) (num 7))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types, Type Expressions, and Equations

;; Variable is Symbol

;; variable? Any -> Boolean
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
;; produce true if x is a type, otherwise false

;; (substof X) is Variable -> X

;; empty-subst : (substof X)
;; a substitution that maps all variables to their corresponding type expression

;; make-subst : Variable TypeExpr -> (substof TypeExpr)
;; produce a subst that replaces X with typeX, otherwise acts like empty-subst

;; apply-subst : (substof TypeExpr) TypeExpr -> TypeExpr
;; replace all type variables in typeX with their mapping in subst

#;
(define-type Equation
  [assert-equal (lhs TypeExpr?) (rhs TypeExpr?)])
;; interp.  An equation over type expressions.

;; SystemOfEqns is (listof Equation)
;; interp. a system of equations over types.

;; unify : SystemOfEqns -> (substof Type)
;; produce a substitution that solves the given system of equations


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments

;; (envof X) is (listof (list symbol X))

;; unique? : (listof Symbol) -> Boolean
;; produce true if each symbol in x* appears only once

;; empty-env : (envof X)
;; an empty environment

;; extend-env : (envof X) (listof symbol) (listof X) -> (envof X)
;; Produce an environment that binds distinct symbols in x* to values in v*.
;; ASSUME: (= (length x*) (length v*))
;; ASSUME: (unique? x*)

;; lookup-env : (envof X) symbol -> X
;; Produce the binding for the given symbol.
;; Effect: Signals an error if no binding is found.

;; in-env? : (envof X) symbol -> boolean
;; produce true if the identifier is bound in the environment, otherwise false

;; extends-env? : (envof X) (listof Symbol) (listof X) -> Boolean
;; produce true if env0 = (extend-env* env^ x*0 v*0) for some env^, else false
;; ASSUME: (= (length x*) (length v*))
;; ASSUME: (unique? x*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Checking 


;; Tenv is (envof Type)
;; interp. The type associated to each identifier in the surrounding context

;; Tenv? : Any -> Boolean
;; produce true if the argument is a Tenv, else false


;; tenv ⊢ tl : type  judgment.
;; (irrelevant side-conditions suppressed)
#;
(define-type TL/wt
  [num/wt (tenv Tenv?) (n number?) #:when ...]
  [add/wt (tl/wt1 TL/wt?) (tl/wt2 TL/wt?) #:when ...]
  [id/wt (tenv Tenv?) (x identifier?) #:when ...]
  [fixFun/wt (f identifier?) (f-type Type?) (x identifier?) (tl/wt TL/wt?)
             #:when ...]
  [app/wt (tl/wt1 TL/wt?) (tl/wt2 TL/wt?)
          #:when ...]

  ;; THE RELEVANT TYPING RULES
  
  ;; [tuple/wt] tenv1 ⊢ tl1 : type1  tenv2 ⊢ tl2 : type2 ... tenvN ⊢ tlN : typeN
  ;; ---------------------------------------------------------------------------
  ;;         tenv ⊢ {tuple tl1 tl2 ... tlN} : (tupleType type1 type2 ... typeN)
  ;; CHECK: tenv = tenv1 = tenv2 = ... = tenvN
  [tuple/wt (tenv Tenv?) (args/wt (listof TL/wt?))
            #:when (check-tuple/wt tenv args/wt)]

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
                 #:when (check-match-with/wt ids types named/wt body/wt)])


;; tl/wt-tenv : TL/wt -> Tenv
;; produce the type environment associated with the given derivation
;; Assume: given derivation is wetl-formed

;; tl/wt-tl : TL/wt -> TL
;; produce the tl expression associated with the given derivation
;; Assume: given derivation is wetl-formed

;; tl/wt-type : TL/wt -> Type
;; produce the type associated with the given derivation
;; Assume: given derivation is wetl-formed

;; type-string : Type -> string
;; produce a string corresponding to a given type


;;
;; PROBLEM 4: Enforce the side-condition checks for the tuple/wt and
;;  match-with/wt cases by implementing the two functions
;;  check-tuple/wt and check-match-with/wt below.  The rules have been
;;  reproduce below for you.
;;
;; HINT: You would be wise to start by writing some example (good and bad)
;;   derivations: 
;;   place them AFTER the line indicated below, or they probably won't work.
;;


;; [tuple/wt] tenv1 ⊢ tl1 : type1  tenv2 ⊢ tl2 : type2 ... tenvN ⊢ tlN : typeN
;; ---------------------------------------------------------------------------
;;         tenv ⊢ {tuple tl1 tl2 ... tlN} : (tupleType type1 type2 ... typeN)
;; CHECK: tenv = tenv1 = tenv2 = ... = tenvN
#;[tuple/wt (tenv Tenv?) (args/wt (listof TL/wt?))
            #:when (check-tuple/wt tenv args/wt)]


;; Tenv (listof TL/wt) -> Boolean
;; produce true if arguments satisfy tuple/wt side-conditions else false
(define (check-tuple/wt tenv args/wt)
  (... tenv args/wt))


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
#;[match-with/wt (ids (listof identifier?)) (types (listof Type?))
                 (named/wt TL/wt?) (body/wt TL/wt?)
                 #:when (check-match-with/wt ids types named/wt body/wt)]


;; (listof Identifier?) (listof Type) TL/wt TL/wt -> Boolean
;; produce true if arguments satisfy match-with/wt side-conditions else false
(define (check-match-with/wt ids types named/wt body/wt)
  (... ids types named/wt body/wt)) 


;; NOTE: The next two lines connect your functions to the side conditions
;;  in the TL/wt data definition in ittl-typing.rkt
(set-check-tuple/wt! check-tuple/wt)
(set-check-match-with/wt! check-match-with/wt)

;; PLACE (MORE) PROBLEM 4 EXAMPLES AFTER THIS LINE
(test/data (tuple/wt empty empty))
(test/data-exn (tuple/wt empty
                         (list (num/wt (list (list 'x (numType))) 7))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Inference

;; TenvX is (envof TypeExpr)
;; interp. The type *expression* associated to each identifier

;; TenvX? : any -> Boolean
;; produce true if the given object is a TenvX

;; TL/wtX - Type Derivation *Expressions*
;; Same as TL/wt otherwise
#;
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

;; apply-subst-tenvX : (substof Type) TenvX -> Tenv
;; produce a type environment from the given substitution and tenv expression

;; apply-subst-tl/wtX : (substof Type) TL/wtX -> TL/wt
;; produce a derivation from the given substitution and derivation expression


;;
;; PROBLEM 5: Complete the design of tl->tl/wtX,
;;            in particular the tuple/wt and match-with/wt cases.
;; HINT: You would be wise to start by writing examples


;; TL -> (list TL/wtX SystemOfEqns)
;; produce a TL/wtX derivation *expression* of tenv ⊢ tl : typeX and a
;; system of equations that must be satisfied for a proper TL/wt to exist
;; Effect: signal an error if no such derivation expression exists.
(define (tl->tl/wtX tl0)
  (local
    ;; Accumulator: soeqns is SystemOfEqns
    ;; Invariant: Contains all equations deemed necessary so far.
    ;; Accumulator: tenv is Tenv
    ;; Invariant: tenv associates identifiers with the types of tl expressions
    ;;            to which they are bound in the surrounding context.
    [(define soeqns (void))

     ;; TypeExpr TypeExpr -> Void
     ;; produces void
     ;; Effect: Adds the equation typeX1 = typeX2 to soeqns
     (define (assert-equal! typeX1 typeX2)
       (set! soeqns (cons (assert-equal typeX1 typeX2) soeqns)))

     ;; -> TypeExpr
     ;; produce some type expression
     ;; Effect: generates a fresh unification variable
     (define (some-type!) (unifVar (gensym 'X)))

     ;; -> TypeExpr
     ;; produce some function type expression
     ;; Effect: generates a fresh unification variable
     (define (some-function-type!) (funType (some-type!) (some-type!)))

     ;; Type -> funType
     ;; produce type if type is a function type
     ;; Effect: signal an error if type is not a function type
     (define (assert-funType! typeX)
       (let ([f-typeX (some-function-type!)])
         (begin
           (assert-equal! f-typeX typeX)
           f-typeX)))

     ;; TL/wtX -> TypeX
     ;; produce a type expression that represents the type of the judgment
     (define (tl/wtX-typeX tl/wtX)
       (type-case TL/wtX tl/wtX
         [num/wtX (tenvX n) (numType)]
         [add/wtX (tl/wt1X tl/wt2X) (numType)]
         [id/wtX (tenvX x) (lookup-env tenvX x)]
         [fixFun/wtX (f f-typeX x tl/wtX) f-typeX]
         [app/wtX (tl/wtX1 tl/wtX2)
                  (funType-result
                   (assert-funType! (tl/wtX-typeX tl/wtX1)))]
         [tuple/wtX (tenvX args/wtX)
                    (tupleType
                     (for/list ([arg/wtX args/wtX])
                       (tl/wtX-typeX arg/wtX)))]
         [match-with/wtX
          (ids typeXs named/wtX body/wtX)
          (tl/wtX-typeX body/wtX)]))

     ;; TL TenvX TypeX -> TL/wtX
     ;; produce a derivation expression for tl and assert its type is typeX
     (define (synth&assert tl tenvX typeX)
       (let ([tl/wtX (tl->tl/wtX--tenvX tl tenvX)])
         (begin
           (assert-equal! (tl/wtX-typeX tl/wtX) typeX)
           tl/wtX)))

     ;; TL TenvX -> TL/wtX
     (define (tl->tl/wtX--tenvX tl tenvX)
       (type-case TL tl
         [num (n) (num/wtX tenvX n)]
         [add (lhs rhs)
              (let ([lhs/wtX (synth&assert lhs tenvX (numType))]
                    [rhs/wtX (synth&assert rhs tenvX (numType))])
                (add/wtX lhs/wtX rhs/wtX))]
         [id (x)
             (if (in-env? tenvX x)
                 (id/wtX tenvX x)
                 (error 'tl->tl/wt
                        "id ~a not bound in tenv ~a." x tenvX))]
         [fixFun
          (f x body)
          (let ([f-typeX (some-function-type!)])
            (match-let ([(funType arg-typeX result-typeX) f-typeX])
              (let ([body/wtX (synth&assert
                               body
                               (extend-env tenvX
                                           (list f x)
                                           (list f-typeX arg-typeX))
                               result-typeX)])
                (fixFun/wtX f f-typeX x body/wtX))))]
         [app
          (rator rand)
          (let* ([rator/wtX (tl->tl/wtX--tenvX rator tenvX)]
                 [rator-type (tl/wtX-typeX rator/wtX)])
            (match-let ([(funType arg-type result-type)
                         (assert-funType! rator-type)])
              (let ([rand/wtX (synth&assert rand tenvX arg-type)])
                (app/wtX rator/wtX rand/wtX))))]
         ;;;;;; PROBLEM 5 WORK BEGINS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         [tuple (args)
                (... args)
                ]
         [match-with (ids named body)
                     (... ids named body)
                     ]           
         ;;;;;; PROBLEM 5 WORK ENDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ))]

    (begin (set! soeqns '())
           (let ([tl/wtX (tl->tl/wtX--tenvX tl0 '())])
             (list tl/wtX soeqns)))))

;; YOU SHOULD WRITE (MORE) EXAMPLES FOR PROBLEM 5 (FOR YOUR OWN SAKE)

(test/match (tl->tl/wtX (tuple (list)))
            (list (tuple/wtX '() (list)) '()))



;; TL -> TL/wt
;; produce a TL/wt derivation of '() ⊢ tl : type
;; Effect: signal an error if no such derivation exists.
(define (tl->tl/wt tl)
  (match-let ([(list tl/wtX soeqns) (tl->tl/wtX tl)])
    (let ([subst (unify soeqns)])
      (apply-subst-tl/wtX subst tl/wtX))))


(test (tl->tl/wt (num 7))
      (num/wt '() 7))

(test (tl->tl/wt (fixFun 'f 'x (id 'x)))
      (fixFun/wt 'f (funType (emptyType) (emptyType))
                 'x (id/wt (list (list 'f (funType (emptyType) (emptyType)))
                                 (list 'x (emptyType))) 'x)))


(test (tl->tl/wt (fixFun 'f 
                         'x
                         (app (id 'f) (id 'x))))
      (fixFun/wt
       'f
       (funType (emptyType) (emptyType))
       'x
       (app/wt
        (id/wt (list (list 'f (funType (emptyType) (emptyType)))
                     (list 'x (emptyType)))
               'f)
        (id/wt (list (list 'f (funType (emptyType) (emptyType)))
                     (list 'x (emptyType)))
               'x))))

(test
 (tl->tl/wt (fixFun 'f 
                    'x
                    (app (id 'f)
                         (app (id 'f) (id 'x)))))
 (fixFun/wt
  'f
  (funType (emptyType) (emptyType))
  'x
  (app/wt
   (id/wt (list (list 'f (funType (emptyType) (emptyType)))
                (list 'x (emptyType))) 'f)
   (app/wt
    (id/wt (list (list 'f (funType (emptyType) (emptyType)))
                 (list 'x (emptyType))) 'f)
    (id/wt (list (list 'f (funType (emptyType) (emptyType)))
                 (list 'x (emptyType))) 'x)))))

(test (tl->tl/wt (fixFun 'f 'x (add (id 'x) (id 'x))))
      (fixFun/wt
       'f
       (funType (numType) (numType))
       'x
       (add/wt
        (id/wt (list (list 'f (funType (numType) (numType)))
                     (list 'x (numType)))
               'x)
        (id/wt (list (list 'f (funType (numType) (numType)))
                     (list 'x (numType)))
               'x))))

(test (tl->tl/wt (fixFun 'f 'x (app (id 'x) (num 2))))
      (fixFun/wt
       'f
       (funType (funType (numType) (emptyType))
                (emptyType))
       'x
       (app/wt
        (id/wt (list
                (list 'f (funType (funType (numType) (emptyType)) (emptyType)))
                (list 'x (funType (numType) (emptyType))))
               'x)
        (num/wt (list
                 (list 'f (funType (funType (numType) (emptyType)) (emptyType)))
                 (list 'x (funType (numType) (emptyType))))
                2))))

(test (tl->tl/wt (app (fixFun 'f 'x (id 'x)) (num 7)))
      (app/wt (fixFun/wt 'f (funType (numType) (numType))
                         'x
                         (id/wt (list (list 'f (funType (numType) (numType)))
                                      (list 'x (numType))) 'x))
              (num/wt '() 7)))


;; Examples
(test (tl->tl/wt (tuple (list)))
      (tuple/wt '() (list)))



;; ITTLPgm is TL
;; Invariant: '() ⊢ tl : type for some type.
;; interp. Implicitly Typed TL program

;; TL -> ITTLPgm
;; reproduce tl if tl is a well-typed ITTL program (i.e. '() ⊢ tl : type)
;; Effect: signal an error if not
(define (typecheck-tl tl)
  (let ([tl/wt (tl->tl/wt tl)])
    (begin
      ;; Trust But Verify!
      (unless (and (equal? (tl/wt-tenv tl/wt) '())
                   (equal? (tl/wt-tl tl/wt) tl))
        (error 'typecheck
               "Liar Liar Pants on Fire!\n I wanted: ~a\nYou gave me: ~a.\n"
               (format "'() ⊢ ~s : type for some type" tl)
               (format "~a ⊢ ~a : ~a"
                       (tl/wt-tenv tl/wt)
                       (tl/wt-tl tl/wt)
                       (type-string (tl/wt-type tl/wt)))))
      tl)))

(test (typecheck-tl TL1) TL1)
(test/exn (typecheck-tl TL2) "not bound")
(test (typecheck-tl TL3) TL3)
(test (typecheck-tl TL4) TL4)
(test (typecheck-tl TL5) TL5)
(test (typecheck-tl TL6) TL6)
(test (typecheck-tl TL7) TL7)
(test (typecheck-tl TL8) TL8)
(test (typecheck-tl TL9) TL9)
(test/exn (typecheck-tl TL10) "not bound")
(test (typecheck-tl TL11) TL11)

(test/exn (typecheck-tl TL1e) "Cannot solve")
