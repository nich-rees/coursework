#lang plai
(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

(require "appendix.rkt")  ;; environments and effects

;;
;; C'mon, Gimme Some Props!
;;


;; tl;dr - Problem 3: Using *only* the generic part of an effect abstraction
;;   interface, implement the core of an environment-passing interpreter for
;;   propositional expressions. (line 98)

;; Propositions are Boolean Expressions that can be evaluated to #t or #f,
;; once their identifiers are assigned Boolean values.

;; A full-fledged language for propositions would bind identifiers to truth
;; values, but for this problem, we will merely implement an interpreter for
;; Props that takes an environment from which identifiers are looked up.
;; (Problem 4 will use this interpreter as part of a more interesting one)


(define-type Prop
  [id/P (x symbol?)]
  [bool/P (b boolean?)]
  [not/P (p Prop?)]
  [and/P (p1 Prop?) (p2 Prop?)]
  [or/P (p1 Prop?) (p2 Prop?)]
  [implies/P (p1 Prop?) (p2 Prop?)])
;; interp.  SATLang proposition according to the following BNF:
;; <Prop> ::=
;; (Booleans)
;;          | <id>
;;            #t
;;          | #f
;;          | {not <Prop>}
;;          | {and <Prop> <Prop>}
;;          | {or <Prop> <Prop>}
;;          | {implies <Prop> <Prop>}
(define P1 (id/P 'x))
(define P2 (not/P (id/P 'x)))
(define P3 (implies/P (id/P 'x) (id/P 'y)))
(define P4 (or/P (not/P (id/P 'x)) (id/P 'y)))
(define P5 (not/P (bool/P #t)))
(define P6 (and/P (id/P 'x) (not/P (id/P 'x))))

#;
(define (fn-for-prop p)
  (type-case Prop p
    [id/P (x) (... x)]
    [bool/P (b) (... b)]
    [not/P (p) (... (fn-for-prop p))]
    [and/P (p1 p2) (... (fn-for-prop p1) (fn-for-prop p2))]
    [or/P (p1 p2) (... (fn-for-prop p1) (fn-for-prop p2))]
    [implies/P (p1 p2) (... (fn-for-prop p1) (fn-for-prop p2))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments (defined in appendix.rkt)

;; Env is <implementation details suppressed>
;; interp.  bindings of identifiers to logical values

;; empty-env : Env
;; extend-env : Env Symbol Boolean -> Env
;; lookup-env : Env Symbol -> Boolean

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Generic Effect Abstraction API: (defined in appendix.rkt)

;; Computation is <implementation details suppressed>

;; Boolean -> Computation
;; (return/eff e) - produce a computation that returns the value of e

;; Computation (Natural or false) -> (listof Boolean)
;; (run/eff c n) - run an effectful computation, producing a list of at most n
;;                  results, or as many as possible if n is #f

;; x : Boolean, c1 : Computation, c2 : Computation (may reference x),
;; produces a Computation
;; (let/eff ([x c1] c2)) - bind x to the next successful value of c1 in c2

;; (let/eff* ([x1 c1] [x2 c2] ...) cn) - sequentialize instances of let/eff
;;  equivalent to:
;; (let/eff ([x1 c1]) (let/eff ([x2 c2]) ... cn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Boolean Boolean -> Boolean
;; produce the truth value of propositional implication ("if-then" in logic)
(define (implies b1 b2)
  (or (not b1) b2))

(test (implies #t #t) #t)
(test (implies #f #f) #t)
(test (implies #f #t) #t)
(test (implies #t #f) #f)


;; Problem 3:  Finish the implementation of interp-prop, a function that
;; **uses the generic effect abstraction API** to interpret
;; propositional expressions.  Adding examples is not required, but encouraged.
;; (in Problem 4 (next file) you will introduce effects to this language).

;; Each operator behaves as is usual in Propositional Logic.
;; Implication has been provided as a helper function called implies.
;; PLAI provides the rest (and, or, not).

;; NOTE: in interp-prop, env is just an extra parameter,
;; **not an accumulator**, hence the simple template and no accumulator comment.

;; Prop Env -> Computation
;; produce a computation that, when run, yields the Boolean value of p in env
;; Effect: signal an error in case of an unbound identifier
#;
(define (interp-prop p env)
  (type-case Prop p
    [id/P (x) (... x)]
    [bool/P (b) (... b)]
    [not/P (p) (... (interp-prop p env))]
    [and/P (p1 p2) (... (interp-prop p1 env) (interp-prop p2 env))]
    [or/P (p1 p2) (... (interp-prop p1 env) (interp-prop p2 env))]
    [implies/P (p1 p2) (... (interp-prop p1 env) (interp-prop p2 env))]))



(define (interp-prop p env)
  (type-case Prop p
    [id/P (x) (return/eff (lookup-env env x))]
    [bool/P (b) (return/eff b)]
    [not/P (p) (let/eff ([b (interp-prop p env)])
                         (return/eff (not b)))]
    [and/P (p1 p2) (let/eff* ([b1 (interp-prop p1 env)]
                               [b2 (interp-prop p2 env)])
                              (return/eff (and b1 b2)))]
    [or/P (p1 p2) (let/eff* ([b1 (interp-prop p1 env)]
                               [b2 (interp-prop p2 env)])
                              (return/eff (or b1 b2)))]
    [implies/P (p1 p2) (let/eff* ([b1 (interp-prop p1 env)]
                               [b2 (interp-prop p2 env)])
                              (return/eff (implies b1 b2)))]))

;; Env
;; a simple environment for examples: x = #t and y = #f
(define EXAMPLE-ENV
  (foldr (λ (x b env) (extend-env env x b))
         empty-env
         (list 'x 'y)
         (list #t #f)))

(test/exn (run/eff (interp-prop (id/P 'x) empty-env)
                   #f)
      "Unbound")

(test (run/eff (interp-prop (id/P 'x) EXAMPLE-ENV)
               #f)
      (list #t))

(test (run/eff (interp-prop (not/P (id/P 'x)) EXAMPLE-ENV)
               #f)
      (list #f))

(test (run/eff (interp-prop (implies/P (id/P 'x) (id/P 'y))
                            EXAMPLE-ENV)
                            #f)
      (list #f))


