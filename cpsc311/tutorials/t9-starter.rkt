#lang plai
(print-only-errors)
(define ... (λ args (cons '... args))) ;; enables us to use ... in templates


;; t9-starter - tutorial on well-typing


;; Tiny mostly useless language (TMU), but typed!

(define-type Type
  [numType]
  [funType (domain Type?) (range Type?)])
;; <Type> ::= number | <Type> -> <Type>


(define-type TMU
  [num (n number?)]
  [add (lhs TMU?) (rhs TMU?)]
  [id (x symbol?)]
  [fixFun (f symbol?) (f-type Type?) (x symbol?) (body TMU?)]
  [app (rator TMU?) (rand TMU?)])

;; * Note: the type annotation in fixFun (called f-type in the datatype) refers
;;   to the type of the function *not* the type of the parameter x.

;; <TMU> ::= <number>
;;         | {+ <TMU> <TMU>}
;;         | <id>
;;         | {fixFun {<id> : <Type>} {<id>} <TMU>}
;;         | {<TMU> <TMU>}

;; TODO: Add interp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments

;; (envof X) is (listof (list symbol X))

;; any -> boolean
;; produce true if the given object is an (envof X)
(define ((envof? X?) x)
  (match x
    [(list `(,x ,v) ...) #:when (and (andmap symbol? x)
                                     (andmap X? v)) #t]
    [else #f]))

;; (listof symbol) -> boolean
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

;; empty-env : Env
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Checking

;; Tenv is (envof Type)
;; interp. The type associated to each identifier in the surrounding context
(define Tenv? (envof? Type?))

(define (identifier? x)
  (local [(define RESERVED '(number + fixFun :))]
    (and (symbol? x)
         (not (memq x RESERVED)))))



;; PROBLEM 1: Define a well-typing datatype for the
;; tenv ⊢ tmu : type judgment.  Call it TMU/wt.


(define-type TMU/wt

;; [num/wt]
;; ----------------------
;;    tenv ⊢ n : number
  [num/wt (tenv Tenv?) (n number?)]

;; [add/wt]  tenv1 ⊢ tmu1 : number   tenv2 ⊢ tmu2 : number
;; ------------------------------------------------------------
;;            tenv1 ⊢ {+ tmu1 tmu2} : number
;; CHECK: tenv1 = tenv2
  [add/wt (tmu/wt1 TMU/wt?) (tmu/wt2 TMU/wt?)
          #:when (equal? (tmu/wt-tenv tmu/wt1)
                         (tmu/wt-tenv tmu/wt2))]

;; [id/wt]
;; ------------------------
;;     tenv ⊢ x : type 
;; CHECK: (lookup-env tenv x) = type
  [id/wt (tenv Tenv?) (x identifier?)
         #:when (member x tenv)]

;; [fixFun/wt]  tenv2 ⊢ tmu : type1  
;; ------------------------------------------------------------
;;            tenv1 ⊢ {fixFun {f : type3} {x} tmu} : type3
;; CHECK: tenv2 = (extend-env tenv1
;;                            (list f x)
;;                            (list type3 type0))
;; CHECK: type3 = type0 -> type1
  #;[fixFun/wt (f symbol?) (f-type Type?) (x symbol?) (tmu/wt TMU/wt?)
             #:when (and (funType? f-type)
                         ())];; I will study other things

;; * Notice that x has type type0 in the above rule, which matches the domain
;;   type of the function (type3).
;; * The type annotation for f, i.e. type3, matches the type of the overall
;;   term.
  

;; [app/wt]  tenv1 ⊢ tmu1 : type1   tenv2 ⊢ tmu2 : type2
;; ------------------------------------------------------------
;;            tenv1 ⊢ {tmu1 tmu2} : type3
;; CHECK: tenv1 = tenv2
;; CHECK: type1 = type2 -> type3
)

;; PROBLEM 2: Define selector functions for the TMU/wt datatype.

;; Selector functions:
;; * Produce the type environment in which the well-typing datatype holds
;; * Produce the TMU expression that the well-typing datatype is about
;; * Produce the type of the above expression


;; TMU/wt -> Tenv
;; produce the type environment associated with the given derivation
(define (tmu/wt-tenv tmu/wt) empty-env) ; stub

;; TMU/wt -> TMU
;; produce the tmu expression associated with the given derivation
(define (tmu/wt-tmu tmu/wt) (num 0)) ; stub

;; TMU/wt -> Type
;; produce the type associated with the given derivation
(define (tmu/wt-type tmu/wt) (numType)) ; stub


;; PROBLEM 3: Define a side-condition check function for the datatype.
;; Call it check-TMU/wt. NOTE: we use the term "ill-formed" to refer to a
;; TMU/wt that does not satisfy some side conditions.

;; TMU/wt -> void
;; given a well-formed TMU/wt, produce void
;; Effect: signal an error if the given TMU/wt derivation is ill-formed.
(define (check-tmu/wt tmu/wt) (void)) ; stub