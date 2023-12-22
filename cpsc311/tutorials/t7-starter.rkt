#lang plai

;(require "parsing.rkt")

;;
;; WAE - a language of Arithmetic Expressions with "with"
;;
(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

;; S-expression -> Boolean
;; produce true if the given value is a gensym, otherwise false.
(define (gensym? s)
  (and (symbol? s)
       (not (symbol-interned? s))
       (not (symbol-unreadable? s))))

(test (gensym? 's) #f)
(test (gensym? (gensym)) #t)
(test (gensym? (string->unreadable-symbol "hello")) #f)

;; WID is Symbol
;; INVARIANT: a WID cannot be equal to '+, '-, or 'with
;; interp.  an identifier in the WAE language
(define (wid? x)
  (and (symbol? x)
       (not (member x '(+ - with)))))

(define WID0 'a)
(define WID1 'b)

;; No template: atomic data



(define-type WAE     
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (id wid?) (named-expr WAE?) (body WAE?)]
  [id (name wid?)])
;; interp.  program in the WAE language, corresponding to the following
;; Backus-Naur Form (BNF) specification 
;;   <WAE> ::= <num>
;;          | { + <WAE> <WAE> }
;;          | { - <WAE> <WAE> }
;;          | { with {<id> <WAE>} <WAE>}
;;          | <id>

;; Every AE program is a WAE program
(define AE1 (num 4))
(define AE2 (add AE1 (num 5)))
(define AE3 (sub (num 6) (num 3)))

;; WAE can associate identifiers with expressions
(define WAE1
  (with 'a (num 4)
        (id 'a)))

(define WAE2
  (with 'a (num 4)
        (add (id 'a) (id 'a))))

(define WAE3
  (with 'a (add (num 5) (num 5))
        (add (id 'a) (id 'a))))

(define WAE4
  (with 'a (num 5)
        (add (id 'a)
             (with 'a (num 3)
                   (id 'a)))))

(define WAE5
  (with 'a (num 5)
        (add (id 'a)
             (with 'b (num 3)
                   (id 'a)))))

#;
(define (fn-for-wae wae)
  (type-case WAE wae
    [num (n) (... n)]
    [add (l r) (... (fn-for-wae l)
                    (fn-for-wae r))]
    [sub (l r) (... (fn-for-wae l)
                    (fn-for-wae r))]
    [with (id named body) (... id
                               (fn-for-wae named)
                               (fn-for-wae body))]
    [id (x) (... x)]))


;; Value is Number
;; interp. a value in the WAE language



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Substitution
;;


;; WAE WID WAE -> WAE
;; naïvely substitute wae0 for free instances of x in wae
(define (subst wae x0 wae0)
  (type-case WAE wae
    [num (n) (num n)]
    [add (lhs rhs) (add (subst lhs x0 wae0)
                        (subst rhs x0 wae0))]
    [sub (lhs rhs) (sub (subst lhs x0 wae0)
                        (subst rhs x0 wae0))]
    [with (bound-id named-expr bound-body)
          (if (symbol=? bound-id x0)
              (with bound-id
                    (subst named-expr x0 wae0)
                    bound-body)
              (with bound-id
                    (subst named-expr x0 wae0)
                    (subst bound-body x0 wae0)))]
    [id  (name) (if (symbol=? x0 name)
                    wae0
                    (id name))]) )


(test (subst (id 'a) 'a (num 1)) (num 1))
(test (subst (id 'a) 'a (num 2)) (num 2))

(test (subst (id 'b) 'a (num 1)) (id 'b))
(test (subst (num 10) 'a (num 1)) (num 10))
(test (subst (add (id 'a) (num 10)) 'a (num 2)) (add (num 2) (num 10)))


(test (subst (with 'a (num 2) (num 3)) 'a (num 1))
      (with 'a (num 2) (num 3)))
(test (subst (with 'b (num 2) (num 3)) 'a (num 1))
      (with 'b (num 2) (num 3)))

(test (subst (with 'a (num 2) (id 'a)) 'a (num 1))
      (with 'a (num 2) (id 'a)))

(test (subst (with 'b (num 2) (id 'a)) 'a (num 1))
      (with 'b (num 2) (num 1)))


(test (subst (with 'b (id 'a) (id 'a)) 'a (num 1))
      (with 'b (num 1) (num 1)))

(test (subst (with 'a (id 'a) (id 'a)) 'a (num 1))
      (with 'a (num 1) (id 'a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments

;; Env is (listof (list Symbol Value))
;;

;; Env
;; an empty env (initial environment)
(define empty-env empty)

;; Store 
(define (lookup-env env x)
  (cond
    [(assoc x env) => cadr] ;; consult cond docs if this looks crazy :)
    [else (error 'interp "Unbound Identifier: ~a" x)]))


;; Env Symbol Value -> Env
;; produce a new environment that is updated with the new binding
(define (extend-env env x value)
  (cond
    [(empty? env) (list (list x value))]
    [else ;; cond
     (if (symbol=? (first (first env)) x)
         (cons (list x value) (rest env))
         (cons (first env)
               (extend-env (rest env) x value)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effect Abstraction 1: NOFX

;; Conveniently enough, we can still use the effect interface even if we have
;; no effects at all!  Below we will use this to implement a substitution-based
;; interpreter.

;;
;; Effect Interface
;;

;; No Special Purpose Effects

;; The Generic Interface Components:
;; (return/eff e) - return the value of e
;; (run/eff e) - run an "effectful" computation
;; (let/eff ([x e1] e2)) - bind x to the value of e1 in e2
;; (let/eff* ([x* e1*] ...) e2) - sequentialize instances of let/eff

;;
;; Effect Implementation
;;

;; Computation is Value
;; interp.  a function that awaits a threaded accumulator value and produces a
;; number and an updated accumulated value.

;;; BEGIN COMMENT OF A BLOCK OF CODE NEXT:
#;{

;; Value -> Computation
;; create a computation that yields the given value
(define return/eff identity)

;; Computation -> Value
;; run the given computation and produce its result
(define run/eff identity)

;; Compose two computations
(define-syntax let/eff
  (syntax-rules ()
    [(_ ([x c1]) c2)
     (let ([x c1]) c2)]))

;; Compose many computations
(define-syntax let/eff*
  (syntax-rules ()
    [(_ ([x* c1*] ...) c2)
     (let* ([x* c1*] ...) c2)]))

}  ;; END OF CODE COMMENT

;; End of Effect Abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effect Abstraction 2: Standard Accumulator (Environments)
;;
;; It may seem strange to think of a standard accumulator as an "effect".
;; Instead it may be more useful to observe that an accumulator introduces its
;; own form of boilerplate to our code, and it might be nice to abstract that
;; away.  In the case of a threaded accumulator, we could replace it with a
;; mutable accumulator, but that's not a good abstraction for a standard
;; accumulator.  (EXERCISE:  Can you explain why mutable accumulators are not
;; good *in general* as substitutes for standard accumulators?)

;;
;; Effect Interface
;;

;; (let-env/eff ([env]) e) - bind f to the current accumulator and evalute e
;; (with-env/eff env e)  - run e with the accumulator set to env for its dynamic
;;                         extent

;; The Generic Interface Components:
;; (return/eff e) - return the value of e
;; (run/eff e env0) - run computation e, starting with accumulator env0
;; (let/eff ([x e1] e2)) - bind x to the value of e1 in e2, threading factor
;; (let/eff* ([x* e1*] ...) e2) - sequentialize instances of let/eff

;;
;; Effect Implementation
;;

;; Computation is Env -> Value
;; interp.  a function that awaits a standard accumulator and produces a value

;; BEGIN COMMENT OF A BLOCK OF CODE NEXT:


   (define-syntax let-env/eff
     (syntax-rules ()
       [(_ ([env]) c)
        (λ (env) (c env))]))


   ;; Env Computation -> Computation
   ;; run c in a context where the environment is set to env
   (define (with-env/eff env c)
     (λ (env0) (c env)))

 
   ;; Value -> Computation
   ;; create a computation that yields n
   (define (return/eff v)
     (λ (env) v))

   ;; Computation -> Natural
   ;; run the given computation and produce its result
   (define (run/eff c env0)
     (c env0))

   ;; Compose two computations
   (define-syntax let/eff
     (syntax-rules ()
       [(_ ([x c1]) c2)
        (λ (env)
          (let ([x (c1 env)])
            (c2 env)))]))

   ;; Compose many computations
   (define-syntax let/eff*
     (syntax-rules ()
       [(_ () c) c]
       [(_ ([x c1] [x* c1*] ...) c2)
        (let/eff ([x c1])
                 (let/eff* ([x* c1*] ...) c2))]))

     ;; END OF CODE COMMENT

;; End of Effect Abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Value Value -> Computation
(define (add-value v1 v2)
  (return/eff (+ v1 v2)))

;; Value Value -> Computation
(define (sub-value v1 v2)
  (return/eff (- v1 v2)))


;; EXERCISE:  We can change this interpreter from a substitution-based one into
;; an environment-passing one *without touching interp-wae/eff*,
;; just interp-wae, interp-with, and interp-id (i.e. the toplevel driver, and
;; the only features that manipulate identifiers.

;; Comment out Effect Abstraction 1, Uncomment Effect Abstraction 2, and
;; modify interp-wae, interp-with, and interp-id to use environment-passing.


;; Symbol expr expr -> Computation
;; interpret the expression (with e1 e2)
(define (interp-with x e1 e2)
  #;(let/eff* ([v1 (interp-wae/eff e1)])
            (interp-wae/eff (subst e2 x (num v1))))
  (let/eff ([v1 (interp-wae/eff e1)])
           (let-env/eff ([env^])
                        (with-env/eff (extend-env env^ x v1)
                          (interp-wae/eff e2)))))
             
;; Symbol -> Computation
;; interpret the given identifier
(define (interp-id x)
  #;(error 'interp "Unbound identifier ~s." x)
  (let-env/eff ([env^])
               #;(λ (e) (lookup-env e x)) ; works
               ;; Ah shit supposed to use return/eff to get a comp from a val
               (return/eff (lookup-env env^ x))))


;; WAE -> Computation
;; consumes a WAE and computes the corresponding number
(define (interp-wae/eff wae)
  (type-case WAE wae
    [num (n) (return/eff n)]
    [add (l r) (let/eff* ([vl (interp-wae/eff l)]
                          [vr (interp-wae/eff r)])
                         (add-value vl vr))]
    [sub (l r) (let/eff* ([vl (interp-wae/eff l)]
                          [vr (interp-wae/eff r)])
                         (sub-value vl vr))]
    [with (x e1 e2)
          (interp-with x e1 e2)]
    [id (x)
        (interp-id x)]))


;; WAE -> Value
;; evaluate the given WAE expression
(define (interp-wae wae)
  #;(run/eff (interp-wae/eff wae))
  (run/eff (interp-wae/eff wae) empty-env))

(test (interp-wae AE1) 4)
(test (interp-wae AE2) 9)
(test (interp-wae AE3) 3)
(test (interp-wae WAE1) 4)
(test (interp-wae WAE2) 8)
(test (interp-wae WAE3) 20)
(test (interp-wae WAE4) 8)
(test (interp-wae WAE5) 10)

