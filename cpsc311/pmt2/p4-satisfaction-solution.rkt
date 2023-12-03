#lang plai
(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

(require "appendix.rkt")  ;; environments and effects
;(require "p3-props.rkt") ;; propositions
(require "p3-props-solution.rkt")


;;
;; [I Can't Get No] Satisfaction!
;; 

;; NOTE:  THIS PROBLEM IS WORTH ONLY ~10% OF YOUR MARKS.  MAKE SURE YOU FOCUS
;; MOST OF YOUR EFFORTS ON THE REST OF THE EXAM!

;; tl;dr - Problem 4: Use the backtracking effect abstraction to extend
;;   Problem 3's proposition interpreter into a Satisfiability (SAT) solver.
;; (line 170)


;; A satisfiability (SAT) solver takes a proposition and determines whether it
;; can be *satisfied*: whether Boolean values can be assigned to its identifiers
;; such that it evaluates to #t.

;; For example, the proposition (and x y) has one satisfying assignment:
;; 1) x = #t, y = #t
;;  while (or x y) has 3:
;; 1) x = #t, y = #f,
;; 2) x = #f, y = #t,
;; 3) x = #t, y = #t.


;; SATelLite is a language for solving satisfiability problems.  It extends the
;; Prop language with declarations of those identifiers to which Boolean values
;; should be assigned for a satisfying assignment.  For example, our first SAT
;; problem above can be written:
;;
;; {declare-var {x}
;;   {declare-var {y}
;;     {and x y}}}
;;
;; or equivalently
;;
;; {declare-var* {x y}
;;   {and x y}}

;; When evaluated, a SATelLite program yields
;; ***the number of satisfying assignments** to the identifiers.
;; So both of the above programs (line 36 and 42) produce 1 when run.
;; (we could have produced assignments, but for simplicity, we just count)

;; Note that SATelLite lets you have unused identifiers, like:
;; {declare-var {z}
;;   {declare-var {x}
;;      {declare-var {y}
;;        {and x y}}}
;; which has *2* satisfying assignments:
;; 1) x = #t, y = #t, z = #f,
;; 2) x = #t, y = #t, z = #t

;; duplicate identifiers are forbidden, so *the parser* would reject this:

;; {declare-var {x}
;;   {declare-var {y}
;;      {declare-var {x}
;;        {and x y}}}
;;
;; and its equivalent:
;; {declare-var* {x y x} {and x y}}

;; A SATelLite program need not have identifiers: the program
;; {not #f}
;; has one satisfying assignment:
;; 1)  <the empty assignment>

;; However an unbound identifier produces a runtime error instead of a result:
;; {not x}


;; Prop is defined in p3-props.rkt
;(define-type Prop
;  [id/P (x symbol?)]
;  [bool/P (b boolean?)]
;  [not/P (p Prop?)]
;  [and/P (p1 Prop?) (p2 Prop?)]
;  [or/P (p1 Prop?) (p2 Prop?)]
;  [implies/P (p1 Prop?) (p2 Prop?)])


(define-type SL
  [prop/SL (p Prop?)]
  [declare-var/SL (x symbol?) (sl SL?)])
;; interp. SatelLite problem instance according to the following BNF
;; <SL> ::= <Prop>
;;        | {declare-var {<id>} <Prop>}
;;        | {declare-var* { <id>* } <Prop>}
;; where {declare-var* { x1 x2 ...} <Prop>} ≡
;;       {declare-var { x1 }
;;          {declare-var { x2 }
;;             ...
;;             <SLProp>}}
;; INVARIANT: An identifier can be declared only once

;; (listof Symbol) SL -> SL
;; syntactic sugar: wrap body in decls for x*
(define (declare-var*/SL x* body)
  (foldr (λ (x body^) (declare-var/SL x body^)) body x*))

(define SL1 (prop/SL (not/P (bool/P #f))))
(define SL2 (declare-var*/SL '(x y) (prop/SL (and/P (id/P 'x) (id/P 'y)))))
(define SL3 (declare-var*/SL '(x y) (prop/SL (or/P (id/P 'x) (id/P 'y)))))
(define SL4 (declare-var*/SL '(z x y) (prop/SL (and/P (id/P 'x) (id/P 'y)))))

;; (x => y) <=> ((not y) => (not x)) (a tautology)
(define SL5 (declare-var*/SL
             '(x y)
             (prop/SL (and/P (implies/P
                              (implies/P (id/P 'x) (id/P 'y))
                              (implies/P (not/P (id/P 'y)) (not/P (id/P 'x))))
                             (implies/P
                              (implies/P (not/P (id/P 'y)) (not/P (id/P 'x)))
                              (implies/P (id/P 'x) (id/P 'y)))))))
(define SL1e (prop/SL (not/P (id/P 'x))))
(define SL2e (declare-var*/SL
             '(x z)
             (prop/SL (implies/P (id/P 'x) (id/P 'y)))))
#;
(define (fn-for-sl sl)
  (type-case SL sl
    [prop/SL (p) (... (fn-for-prop p))]
    [declare-var/SL (x body) (... x (fn-for-sl body))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments (defined in appendix.rkt)

;; Env is <implementation details suppressed>
;; interp.  bindings of identifiers to logical values

;; empty-env : Env
;; extend-env : Env Symbol Boolean -> Env
;; lookup-env : Env Symbol -> Boolean

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effect Abstraction (defined in appendix.rkt)

;;
;; Effect Interface
;;

;; Computation is <implementation details suppressed>

;; Computation Computation -> Computation
;; (amb/eff c1 c2) - produce the result of c1 unless it (or its continuation)
;;                   fails, in which case produce the result of c2

;;   -> Computation
;; (fail/eff) - fail the current computation

;; The Generic Effect Abstraction API: (defined in stream-abstraction.rkt)
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


;; Problem 4: Complete the design of the interp-sl--env function.
;; this function uses the backtracking effect abstraction to interpret
;; the given SATelLite program with each possible assignment to identifiers.
;; If an assignment yields #f, evaluation backtracks and tries another
;; assignment.  If it yields #t, then the present evaluation produces #t.

;; NOTE 1: As the template shows, this function uses interp-prop from Problem 3.
;; NOTE 2: Our solution to this problem is ~10 lines total (and not fancy).

;; SL Env -> Computation
;; produce a computation that when run yields #t for each satisfying assignment

;; Accumulator: env is Env
;; Invariant: represents the current surrounding assignment of id's to Booleans
#;
(define (interp-sl--env sl env)
  (type-case SL sl
    [prop/SL (p) (... p (interp-prop p (... env)))]
    [declare-var/SL (x body)
                    (... x (interp-sl--env body (... env)))]))


(define (interp-sl--env sl env)
  (type-case SL sl
    [prop/SL (p) (let/eff ([b (interp-prop p env)])
                          (if b
                              (return/eff b)
                              (fail/eff)))]
    [declare-var/SL (x body)
                    (amb/eff (interp-sl--env body (extend-env env x #t))
                             (interp-sl--env body (extend-env env x #f)))]))

(test/exn (run/eff (interp-sl--env SL1e empty-env) #f) "Unbound")
(test/exn (run/eff (interp-sl--env SL2e empty-env) #f) "Unbound")
(test (run/eff (interp-sl--env SL1 empty-env) #f) (list #t))
(test (run/eff (interp-sl--env SL2 empty-env) #f) (list #t))
(test (run/eff (interp-sl--env SL3 empty-env) #f) (list #t #t #t))
(test (run/eff (interp-sl--env SL4 empty-env) #f) (list #t #t))
(test (run/eff (interp-sl--env SL5 empty-env) #f) (list #t #t #t #t))

;; SL -> Natural
;; produce the number of satisfying assignments to sl identifiers
(define (interp-sl sl)
  (length (run/eff (interp-sl--env sl empty-env) #f)))

(test/exn (interp-sl (prop/SL (id/P 'x))) "Unbound")
(test (interp-sl (prop/SL (bool/P #t))) 1)
(test (interp-sl (declare-var/SL
                  'x
                  (prop/SL (id/P 'x)))) 1)

(test (interp-sl (declare-var/SL
                  'x
                  (prop/SL (or/P (id/P 'x) (id/P 'x))))) 1)

(test (interp-sl (declare-var/SL
                  'x
                  (prop/SL (or/P (id/P 'x) (not/P (id/P 'x)))))) 2)

(test (interp-sl (declare-var/SL
                  'x
                  (prop/SL (and/P (id/P 'x) (not/P (id/P 'x)))))) 0)

