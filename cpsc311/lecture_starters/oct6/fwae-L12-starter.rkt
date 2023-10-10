#lang plai
(require "../util/parsing.rkt")
(require "../util/env.rkt")
(require "../util/test-diverge.rkt")

(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

;; FWAE - Expressions for a language with first-class (higher-order) functions
;; Augmented with "if zero" operator


(define-type FWAE    
  [num (n number?)] 
  [add (lhs FWAE?) (rhs FWAE?)]
  [sub (lhs FWAE?) (rhs FWAE?)]
  [with (name symbol?) (named-expr FWAE?) (body FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FWAE?)]
  [app (rator FWAE?) (arg FWAE?)]
  [if0 (predicate FWAE?) (consequent FWAE?) (alternative FWAE?)])
;; interp. expressions in a language that supports applying first-class
;; functions. Its syntax is defined by the following BNF:
;; <FWAE> ::= <num>
;;           | {+ <FWAE> <FWAE>}
;;           | {- <FWAE> <FWAE>}
;;           | {with {<id> <FWAE>} <FWAE>}
;;           | <id>
;;           | {<id> <FWAE>}
;;           | {if0 <FWAE> <FWAE> <FWAE>}
;;           | {fun {<id>} <FWAE>}
;; Every AE program is a FWAE program
(define AE1 (num 4))
(define AE2 (add AE1 (num 5)))
(define AE3 (sub (num 6) (num 3)))

;; Every WAE program is an FWAE program
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

(define FWAES1 '{fun {x} {+ x x}})
(define FWAE1 (fun 'x (add (id 'x) (id 'x))))

(define FWAES2 '{fun {x} {+ x 1}})
(define FWAE2 (fun 'x (add (id 'x) (num 1))))

(define FWAES3 '{fun {x} {+ x y}})
(define FWAE3 (fun 'x (add (id 'x) (id 'y))))


#;
(define (fn-for-fwae f)
  (type-case FWAE f
    [num (n) (... n)]
    [add (l r) (... (fn-for-fwae l)
                    (fn-for-fwae r))]
    [sub (l r) (... (fn-for-fwae l)
                    (fn-for-fwae r))]
    [with (x named body)
          (... x
               (fn-for-fwae named)
               (fn-for-fwae body))]
    [id (x) (... x)]
    [fun (x body) (... x
                       (fn-for-fwae body))]
    [app (rator rand) (... (fn-for-fwae rator)
                           (fn-for-fwae rand))]
    [if0 (p c a)
         (... (fn-for-fwae p)
              (fn-for-fwae c)
              (fn-for-fwae a))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Substitution-based version
;;


;; FWAE identifier FWAE -> FWAE
;; naively substitute fwae2 for free instances of 'x in fwae1
(define (subst/fwae  fwae1 x fwae2)
  (type-case FWAE fwae1
    [num (n) (num n)]
    [add (l r) (add (subst/fwae l x fwae2)
                    (subst/fwae r x fwae2))]
    [sub (l r) (sub (subst/fwae l x fwae2)
                    (subst/fwae r x fwae2))]
    [with (x0 named body)
          (let ([subst-named (subst/fwae named x fwae2)])
            (if (symbol=? x0 x)
                (with x0 subst-named body)
                (with x0 subst-named (subst/fwae body x fwae2))))]
    [id (x0) (if (symbol=? x0 x)
                 fwae2
                 (id x0))]
    [fun (x0 body)
         (if (symbol=? x0 x)
             (fun x0 body)
             (fun x0 (subst/fwae body x fwae2)))]
    [app (rator rand) (app (subst/fwae rator x fwae2)
                           (subst/fwae rand x fwae2))]
    [if0 (p c a)
         (if0 (subst/fwae p x fwae2)
              (subst/fwae c x fwae2)
              (subst/fwae a x fwae2))]))

(test (subst/fwae (id 'x) 'x (num 1)) (num 1))
(test (subst/fwae (id 'x) 'x (num 2)) (num 2))

(test (subst/fwae (id 'y) 'x (num 1)) (id 'y))
(test (subst/fwae (num 10) 'x (num 1)) (num 10))
(test (subst/fwae (add (id 'x) (num 10)) 'x (num 2)) (add (num 2) (num 10)))


(test (subst/fwae (with 'x (num 2) (num 3)) 'x (num 1))
      (with 'x (num 2) (num 3)))
(test (subst/fwae (with 'y (num 2) (num 3)) 'x (num 1))
      (with 'y (num 2) (num 3)))

(test (subst/fwae (with 'x (num 2) (id 'x)) 'x (num 1))
      (with 'x (num 2) (id 'x)))

(test (subst/fwae (with 'y (num 2) (id 'x)) 'x (num 1))
      (with 'y (num 2) (num 1)))


(test (subst/fwae (with 'y (id 'x) (id 'x)) 'x (num 1))
      (with 'y (num 1) (num 1)))

(test (subst/fwae (with 'x (id 'x) (id 'x)) 'x (num 1))
      (with 'x (num 1) (id 'x)))

(test (subst/fwae (fun 'y (id 'x)) 'x (num 1))
      (fun 'y (num 1)))

(test (subst/fwae (app (fun 'y (id 'x)) (id 'x)) 'x (num 1))
      (app (fun 'y (num 1)) (num 1)))


;; Fun is (fun Symbol FWAE)
;; interp. a FWAE function expression
(define F1 (fun 'x (id 'x)))
(define (fn-for-fun f)
  (... (fun-param f)
       (fun-body f)))


;; SubstValue is one of
;; - Number
;; - (fun Symbol FWAE)
;; interp.  possible results of evaluating an FWAE expression.
(define SV1 7)
(define SV2 (fun 'x (add (num 9) (id 'x))))

;; TEMPLATE
#;
(define (fn-for-subst-value sv)
  (cond [(number? sv) (... sv)]
        [(fun? sv) (... (fun-param sv)
                        (fn-for-fwae (fun-body sv)))]))

;;
;; SubstValue Operators
;;

;; SubstValue -> FWAE
;; produce an FWAE expression corresponding to the given value
(define (svalue->fwae sv)
  (cond [(number? sv) (num sv)]
        [(fun? sv) sv]))

(test (svalue->fwae 9) (num 9))
(test (svalue->fwae (fun 'x (num 9))) (fun 'x (num 9)))


;; SubstValue -> Number
;; coerce the given value to a number
;; Effect: signals an error if the value does not represent a number
(define (svalue->number sv)
  (cond [(number? sv) sv]
        [(fun? sv) (error 'svalue->number "Not a number: ~s" sv)]))

(test (svalue->number 9) 9)
(test/exn (svalue->number (fun 'x (num 9))) "Not a number")


;; SubstValue -> Fun
;; coerce the given value to a function
;; Effect: signals an error if the value does not represent a function
(define (svalue->fun sv)
  (cond [(number? sv) (error 'svalue->fun "Not a function: ~s" sv)]
        [(fun? sv) sv]))


;; SubstValue -> Boolean
;; produce true if the given value represents 0, else false
(define (zero-svalue? sv)
  (cond [(number? sv) (zero? sv)]
        [(fun? sv) #f]))

(test (zero-svalue? 0) #t)
(test (zero-svalue? 9) #f)
(test (zero-svalue? (fun 'x (num 9))) #f)


;; SubstValue SubstValue -> SubstValue
;; apply the first (function) value to the second
;; Effect: Signals an error if the first value is not a function or if
;;         subsequent interpretation signals an error
;(define (apply-svalue svrator svrand) 0)
(define (apply-svalue svrator svrand)
  (let ([rator (svalue->fun svrator)]
        [rand (svalue->fwae svrand)])
    (interp/fwae-subst 
     (subst/fwae (fun-body rator)
                 (fun-param rator)
                 rand))))

;; SubstValue SubstValue -> SubstValue
;; produce the sum of the given values
;; Effect: signals an error if either does not represent a number
(define (add-svalue sv1 sv2)
  (+ (svalue->number sv1)
     (svalue->number sv2)))

;; SubstValue SubstValue -> SubstValue
;; produce the difference of the given values
;; Effect: signals an error if either does not represent a number
(define (sub-svalue sv1 sv2)
  (- (svalue->number sv1)
     (svalue->number sv2)))


;; FWAE -> SubstValue
;; interpret the expression fwae to a value
;; Effect: Signals an error on runtime type mismatch
(define (interp/fwae-subst fwae)
  (type-case FWAE fwae
    [num (n) n]
    [add (l r) (add-svalue (interp/fwae-subst l)
                           (interp/fwae-subst r))]
    [sub (l r) (sub-svalue (interp/fwae-subst l)
                           (interp/fwae-subst r))]
    [with (x named body)
          (let ([val (interp/fwae-subst named)])
            (let ([subst-body (subst/fwae body x (svalue->fwae val))])
              (interp/fwae-subst subst-body)))]
    [id (x) (error 'interp/fwae-subst "Unbound variable: ~a" x)]
    [fun (x0 fwae1) (fun x0 fwae1)]
    [app (rator rand)
         (let ([vrator (interp/fwae-subst rator)]
               [vrand (interp/fwae-subst rand)])
           (apply-svalue vrator vrand))]
    [if0 (p c a)
         (if (zero-svalue? (interp/fwae-subst p))
             (interp/fwae-subst c)
             (interp/fwae-subst a))]))


(test/exn (apply-svalue 9 10) "Not a function")
(test (apply-svalue (fun 'x (id 'x)) 12) 12)
(test (apply-svalue (fun 'x (add (id 'x) (id 'x))) 12) 24)


(test (interp/fwae-subst FWAE1) FWAE1)

(test (interp/fwae-subst (app FWAE1 (num 7)))
      14)
(test (interp/fwae-subst (app FWAE2 (num 7)))
      8)
(test/exn (interp/fwae-subst (app FWAE3 (num 7)))
          "Unbound")

;; RECURSION, WAT?!?
;; perplexing even-odd implementation
(define EVEN-PART (fun 'even-part
                       (fun 'odd-part
                            (fun 'x
                                 (if0 (id 'x)
                                      (num 1)
                                      (with 'odd
                                            (app (app (id 'odd-part)
                                                      (id 'even-part))
                                                 (id 'odd-part))
                                            (app (id 'odd)
                                                 (sub (id 'x) (num 1)))))))))

(define ODD-PART (fun 'even-part
                      (fun 'odd-part
                           (fun 'x
                                (if0 (id 'x)
                                     (num 0)
                                     (with 'even
                                           (app (app (id 'even-part)
                                                     (id 'even-part))
                                                (id 'odd-part))
                                           (app (id 'even)
                                                (sub (id 'x) (num 1)))))))))

(define (make-even n)
  (with 'even-part EVEN-PART
        (with 'even
              (app (app (id 'even-part) (id 'even-part)) ODD-PART)
              (app (id 'even) (num n)))))


(test (interp/fwae-subst (make-even 6)) 1)
(test (interp/fwae-subst (make-even 9)) 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment-Based Version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-type EnvValue
  [numV (n number?)]
  [funV (param symbol?) (body FWAE?) (env procedure?)])
;; interp.  Represents a potential value of FWAE language.
(define EV1 (numV 7))
(define EV2 (funV 'x (id 'x) empty-env))

#;
(define (fn-for-env-value ev)
  (type-case EnvValue ev
    [numV (n) (... n)]
    [funV (x body env) (... x
                            (fn-for-fwae body)
                            env)]))
          

;; EnvValue -> number
;; produce the number represented by the given value
;; Effect: signal an error if the value does not represent a number
(define (evalue->num ev)
  (type-case EnvValue ev
    [numV (n) n]
    [funV (x body env) (error 'evalue->num "Bad number: ~a" ev)]))

(test (evalue->num (numV 6)) 6)
(test (evalue->num (numV 9)) 9)
(test/exn (evalue->num (funV 'x (id 'x) empty-env)) "Bad number")


;; EnvValue (Symbol FWAE Env -> X) -> X
;; extract the fields of ev as a funV and apply them to fn
;; Effect: signal an error if ev does not represent a funv
(define (with-evalue-as-funv ev fn)
  (type-case EnvValue ev
    [funV (x body env) (fn x body env)]
    [numV (n) (error 'evalue->num "Bad function: ~a" ev)]))

(test (with-evalue-as-funv (funV 'x (id 'y) empty-env)
        (λ (x body env) (fun x body)))
      (fun 'x (id 'y))) 
(test/exn (with-evalue-as-funv (numV 9) (λ (x body env) (fun x body))) "Bad")
  
;; (number number -> number) EnvValue EnvValue  -> EnvValue
;; apply num-op to the numbers represented by ev1 and ev2
;; Effect: signal an error if either argument does not represent a number
(define (op-evalue num-op ev1 ev2)
  (let ([n1 (evalue->num ev1)]
        [n2 (evalue->num ev2)])
    (numV (num-op n1 n2))))

(test (op-evalue * (numV 5) (numV 6)) (numV 30))
(test/exn (op-evalue * (numV 5) (funV 'x (id 'x) empty-env)) "Bad number")
(test/exn (op-evalue * (funV 'x (id 'x) empty-env) (numV 6)) "Bad number")
(test/exn (op-evalue * (funV 'x (id 'x) empty-env)
                     (funV 'x (id 'x) empty-env)) "Bad number")

  
;; EnvValue EnvValue -> EnvValue
;; produce the sum of two numbers
;; Effect: signal an error if either argument does not represent a number
(define (add-evalue ev1 ev2)
  (op-evalue + ev1 ev2))

(test (add-evalue (numV 5) (numV 6)) (numV 11))
(test/exn (add-evalue (numV 5) (funV 'x (id 'x) empty-env)) "Bad number")
(test/exn (add-evalue (funV 'x (id 'x) empty-env) (numV 6)) "Bad number")
(test/exn (add-evalue (funV 'x (id 'x) empty-env)
                      (funV 'x (id 'x) empty-env)) "Bad number")


;; EnvValue EnvValue -> EnvValue
;; produce the difference of two numbers
;; Effect: signal an error if either argument does not represent a number
(define (sub-evalue ev1 ev2)
  (op-evalue - ev1 ev2))

(test (sub-evalue (numV 5) (numV 6)) (numV -1))
(test/exn (sub-evalue (numV 5) (funV 'x (id 'x) empty-env)) "Bad number")
(test/exn (sub-evalue (funV 'x (id 'x) empty-env) (numV 6)) "Bad number")
(test/exn (sub-evalue (funV 'x (id 'x) empty-env)
                      (funV 'x (id 'x) empty-env)) "Bad number")


;; EnvValue -> boolean
;; produce true if ev1 represents the number zero, else false
(define (zero-evalue? ev)
  (type-case EnvValue ev
    [numV (n) (zero? n)]
    [funV (x body env) #f]))

(test (zero-evalue? (numV 7)) #f)
(test (zero-evalue? (numV 0)) #t)
(test (zero-evalue? (funV 'x (id 'x) empty-env)) #f)



;; FWAE -> EnvValue
;; interpret the given fwae expression
;; EFFECTS: Signals an error in case of runtime type error.
(define (interp/fwae-env fwae0)
  ;; Accumulator: env is (envof EnvValue)
  ;; Invariant: env represents the bindings (in inside-out order)
  ;;            of identifiers to values due to deferred substitutions
  (local [;; EnvValue EnvValue -> EnvValue
          (define (apply-evalue ev-rator ev-rand)
            (with-evalue-as-funv ev-rator
              (λ (x ev-body env) ;; ev-rator = (funV x ev-body env)
                (fn-for-fwae ev-body (extend-env env x ev-rand)))))

          ;; FWAE Env -> EnvValue
          (define (fn-for-fwae f env)
            (type-case FWAE f
              [num (n) (numV n)]
              [add (l r) (add-evalue (fn-for-fwae l env)
                                     (fn-for-fwae r env))]
              [sub (l r) (sub-evalue (fn-for-fwae l env)
                                     (fn-for-fwae r env))]
              [with (x named body)
                    (fn-for-fwae body
                                 (extend-env env x (fn-for-fwae named env)))]
              [id (x) (lookup-env env x)]
              [fun (x body)
                   (funV x body env) ;; THIS is what is called a "closure"
                   ]
              [app (rator rand)
                   (apply-evalue (fn-for-fwae rator env)
                                 (fn-for-fwae rand env))]
              [if0 (p c a)
                   (if (zero-evalue? (fn-for-fwae p env))
                       (fn-for-fwae c env)
                       (fn-for-fwae a env))]))]
    (fn-for-fwae fwae0 empty-env)))


;; A little home-grown recursion
(test (interp/fwae-env (make-even 6)) (numV 1))
(test (interp/fwae-env (make-even 9)) (numV 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fwae-focused-sexp (fwaefs) is one of:
;; - number
;; - `{+ ,fwaefs ,fwaefs}
;; - `{- ,fwaefs ,fwaefs}
;; - `{with ,identifier ,fwaefs ,fwaefs
;; -  identifier
;; -  {fun ,identifier fwaefs}
;; - `{,fwaefs ,fwaefs}
;; - "any other s-expression"
;; where identifier is any symbol except +, -, with, if0, or fun
;; interp.  any s-expression, but with a focus on those that represent
;; FWAE expressions.

(define (identifier? x)
  (and (symbol? x)
       (not (member x '(+ - with if0 fun)))))

#;
(define (fn-for-fwae-focused-sexp sexp)
  (match sexp
    [`,n
     #:when (number? n)
     (... n)]
    [`{+ ,sexp1 ,sexp2}
     (... (fn-for-fwae-focused-sexp sexp1)
          (fn-for-fwae-focused-sexp sexp2))]
    [`{- ,sexp1 ,sexp2}
     (... (fn-for-fwae-focused-sexp sexp1)
          (fn-for-fwae-focused-sexp sexp2))]
    [`{with {,x ,sexp1} ,sexp2}
     #:when (identifier? x)
     (... x
          (fn-for-fwae-focused-sexp sexp1)
          (fn-for-fwae-focused-sexp sexp2))]
    [`,x
     #:when (identifier? x)
     (... x)]

    [`{if0 ,sexp1 ,sexp2 ,sexp3}
     (... (fn-for-fwae-focused-sexp sexp1)
          (fn-for-fwae-focused-sexp sexp2)
          (fn-for-fwae-focused-sexp sexp3))]
    [`{fun {,x} ,sexp1}
     #:when (identifier? x)
     (... x
          (fn-for-fwae-focused-sexp sexp1))]
    ;; Notice that application is now the last focused case...
    [`{,sexp1 ,sexp2}
     (... (fn-for-fwae-focused-sexp sexp1)
          (fn-for-fwae-focused-sexp sexp1))]    
    [else (... sexp)] ))



;; parse-expr : s-expression -> FWAE
;; parse the given s-expression into a FWAE expression
;; EFFECT: signals an error on failure
(define (parse-expr sexp)
  (match sexp
    [`,n #:when (number? n) (num n)]
    [`{+ ,lhs ,rhs} (add (parse-expr lhs) (parse-expr rhs))]
    [`{- ,lhs ,rhs} (sub (parse-expr lhs) (parse-expr rhs))]
    [`{with {,id ,named-exp} ,body}
     #:when (identifier? id)
     (with id (parse-expr named-exp)
           (parse-expr body))]
    [`,x #:when (identifier? x) (id x)]
    [`{if0 ,pred ,conseq ,altern}
     (if0 (parse-expr pred) (parse-expr conseq) (parse-expr altern))]
    ;; Notice that application is now last...
    [`{fun {,x} ,sexp1}
     #:when (identifier? x)
     (fun x
          (parse-expr sexp1))]
    [`{,rator-exp ,arg-exp} 
     (app (parse-expr rator-exp) (parse-expr arg-exp))]
    [_ (error 'parse "bad expression ~a" sexp)]))


(test (parse-expr WAES4) WAE4)
(test (parse-expr WAES5) WAE5)

(test (parse-expr FWAES1) FWAE1)
(test (parse-expr FWAES2) FWAE2)
(test (parse-expr FWAES3) FWAE3)

;;
;; PUTTING IT ALL TOGETHER  - an interpreter of files on disk
;;

;; String -> Number
;; produce the result of interpreting the FWAE stored in the file fname
;; EFFECT: signals an error if no file fname contains an AE representation
;;         or if the program does not evaluate to a number
;;         or if interpretation signals a runtime error.
(define (interp-file fname)
  (evalue->num
   (interp/fwae-env
    (parse-expr
     (read-from-file fname)))))

(test (with-temporary-data-file "{+ 3 7}\n"
        (λ (fname) (interp-file fname)))
      10)

(test (with-temporary-data-file "{+ {- 3 4} 7}\n"
        (λ (fname) (interp-file fname)))
      6)

(test (with-temporary-data-file "{with {x 5} {+ x {with {y 3} x}}}"
        (λ (fname) (interp-file fname)))
      10)

(test (with-temporary-data-file
          "{with {double {fun {x} {+ x x}}} {with {x 5} {double x}}}"
        (λ (fname) (interp-file fname)))
      10)


(test/diverge (with-temporary-data-file
                  "{{fun {y} {y y}} {fun {x} {x x}}}"
                (λ (fname) (interp-file fname)))
              5)


