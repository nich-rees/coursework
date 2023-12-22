#lang plai
(require "parsing.rkt")
(require "test-match.rkt")
(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates


;; Assignment 6: Snappy Languages on Crappy Runtimes

;; KRAken: a language with Konditionals, Recursion, and Arithmetic
;; A functional language with:
;; - numbers
;; - booleans
;; - conditional expressions
;; - first-class (recursive) functions


;; KRAken is a language much like TFA (assignment 4), but without those cursed
;; call-by-reference mutable variables.  As with most of our languages, KRAKen
;; has first-class procedures and proper tail calls.

;; We might like to integrate KRAken into a software system that is implemented
;; in a language like C, which supports neither first-class procedures nor
;; proper tail calls.  The first step in doing so is to transform its
;; implementation into a form that is suited for nearly direct (albeit tedious)
;; translation.  Your mission is to release the KRAken[*] in a form that is
;; suitable for transliteration into a language like C.
;; ([*] c.f. https://www.youtube.com/watch?v=Luuh1iwSddE )
;;

;; You are to transform the two functions interp/kra-env and apply/kra,
;; and then modify the interp/kra function to call its corresponding
;; interp/kra-env function with a KRAken program.

;; Specifically: progressively perform the following changes to the
;; three functions:
;; 1) Translate the two key functions (interp/kra-env and apply/kra) into
;;    continuation-passing style (CPS).
;;    give each of the three functions (i.e. including interp/kra)
;;    the suffix "/k" (save as a6-step1.rkt);
;; 2) Copy a6-step1.rkt to a6-step2.rkt.
;;    Defunctionalize the continuations of the two key functions from step1.
;;    give each of the three functions the suffix "/kd" (a6-step2.rkt);
;; 3) Copy a6-step2.rkt to a6-step3.rkt.
;;    Trampoline the two key functions from step2.
;;    give each of the three functions the suffix "/kdt" (a6-step3.rkt);
;; 4) Copy a6-step3.rkt to a6-step4.rkt.
;;    Defunctionalize the thunks of the two key functions from step3.
;;    give each function the suffix "/kdtd" (a6-step4.rkt);

;; *Do not* transform any of the other helper functions, including the
;; routines that implement environments.  Each of those performs a small bounded
;; amount of recursion, which is fine for translation to a language like C.


;; KID is Symbol
;; INVARIANT: a KID cannot be a KRA keyword
;; interp.  an identifier in the KRAken language
(define (kid? x)
  (let ([keywords
         '(+ nought? ifB andB notB with fun fixFun)])
    (and (symbol? x)
         (not
          (member x keywords)))))

(define KID0 'a)
(define KID1 'b)

;; No template: atomic data



(define-type KRA
  [num (n number?)]
  [add (lhs KRA?) (rhs KRA?)]
  [nought? (arg KRA?)]
  [bool (b boolean?)]
  [ifB (pred KRA?) (conseq KRA?) (altern KRA?)]
  [id (name kid?)]
  [fixFun (self kid?) (param kid?) (body KRA?)]
  [app (rator KRA?) (rand KRA?)])
;; interp. expressions in the KRAken language.
;; Its syntax is defined by the following BNF:
;; <KRA> ::=
;; (ARITHMETIC)
;;          <num>
;;        | {+ <KRA> <KRA>}
;;        | {nought? <KRA>}
;; (LOGIC)
;;        | <bool>
;;        | {ifB <KRA> <KRA> <KRA>}
;;        | {andB <KRA> <KRA>}
;;        | {notB <KRA> <KRA>}
;; (IDENTIFIERS)
;;        | <id>
;;        | {with {<id> <HG>} <HG>}
;; (FUNCTIONS)
;;        | {<KRA> <KRA>}
;;        | {fun {<id>} <KRA>}
;;        | {fixFun <id> {<id>} <KRA>}
;; {andB expr1 expr2} ≡ {ifB expr1 {ifB expr2 #t #f} #f}
;;   where x0 does not occur free in expr2
;; {notB expr} ≡ {ifB expr #f #t}
;; {fun {x} body} ≡ {fixFun f0 {x} body} where f0 does not occur free in body
;; {with {x named} body} ≡ {{fun {x} body} named}

;; Syntactic sugars
(define (andB expr1 expr2)
  (ifB expr1 (ifB expr2 (bool #t) (bool #f)) (bool #f)))
(define (notB expr) (ifB expr (bool #f) (bool #t)))
(define (fun x body) (fixFun (gensym) x body))
(define (with x named body) (app (fun x body) named))



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
(define (fn-for-kra f)
  (type-case KRA f
    [num (n) (... n)]
    [add (l r) (... (fn-for-kra l)
                    (fn-for-kra r))]
    [nought? (e) (... (fn-for-kra e))]
    [bool (b) (... b)]
    [ifB (p c a)
         (... (fn-for-kra p)
              (fn-for-kra c)
              (fn-for-kra a))]
    [id (x) (... x)]
    [fixFun (f x body) (... f x (fn-for-kra body))]
    [app (rator rand) (... (fn-for-kra rator)
                           (fn-for-kra rand))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Environments - custom tailored for KRAken
;;

(define-type Env
  [emptyE]
  [extend-env (env Env?) (x kid?) (v Value?)]
  [extend-env-fixFun  (env Env?)(f kid?) (x kid?) (body KRA?)])
;; interp.  bindings of identifiers to objects of type X,
;; including self-referential fixFun bindings

;; Env
;; empty environment value (to stick with our usual interface)
(define empty-env (emptyE))

#;
(define (fn-for-env env)
  (type-case Env env
    [emptyE () (...)]
    [extend-env (env x v)
                (... (fn-for-env env) x v)]
    [extend-env-fixFun (env f x body)
                       (... (fn-for-env env) f x body)]))



;;
;; Interpreter Values
;;



(define-type Value
  [numV (n number?)]
  [boolV (b boolean?)]
  [funV (x kid?) (body KRA?) (env Env?)])
;; interp.  Represents a potential value of interpretation

(define E0 empty-env)
(define E1 (extend-env E0 'a (numV 7)))
(define E2 (extend-env-fixFun E1 'f 'a (id 'a)))


(define RV1 (numV 7))
(define RV2 (funV 'x (id 'x) empty-env))
(define RV3 (funV 'x (id 'y) (extend-env empty-env 'y (numV 7))))
(define RV4 (boolV #f))
(define RV5 (boolV #t))

#;
(define (fn-for-value v)
  (type-case Value v
    [numV (n) (... n)]
    [boolV (b) (... b)]
    [funV (x body env) (... x
                            (fn-for-kra body)
                            env)]))



;; Env TID -> Value
;; produce the binding associated with the given symbol
;; Effect: Signal an error if the given symbol has no binding
(define (lookup-env env0 x0)
  (type-case Env env0
    [emptyE ()
            (error 'interp/kra "Unbound Identifier: ~a" x0)]
    [extend-env (env x v)
                (if (symbol=? x x0)
                    v
                    (lookup-env env x0))]
    [extend-env-fixFun (env f x body)
                       (if (symbol=? f x0)
                           (funV x body env0)
                           (lookup-env env x0))]))

(test/exn (lookup-env empty-env 'a) "Unbound")
(test (lookup-env (extend-env empty-env 'a (numV 7)) 'a) (numV 7))
(test/exn (lookup-env (extend-env empty-env 'a (numV 7)) 'b) "Unbound")
(test (lookup-env (extend-env (extend-env empty-env 'a (numV 7))
                              'a (numV 8)) 'a)
      (numV 8))
(test (lookup-env (extend-env (extend-env empty-env 'a (numV 7))
                              'b (numV 8)) 'a)
      (numV 7))
(test (lookup-env (extend-env (extend-env empty-env 'b (numV 8))
                              'a (numV 7)) 'a)
      (numV 7))

(test (lookup-env (extend-env
                   (extend-env-fixFun
                    (extend-env empty-env 'a (numV 3))
                    'f 'a (id 'a))
                   'a
                   (numV 9))
                  'f)
      (funV 'a
            (id 'a)
            (extend-env-fixFun
             (extend-env empty-env 'a (numV 3))
             'f 'a (id 'a))))

(test/exn (lookup-env (extend-env (extend-env empty-env 'b (numV 8))
                                  'a (numV 7)) 'c)
          "Unbound")


;;
;; Interpretation Functions
;;


;; Value -> Number
;; produce the number represented by the given value
;; Effect: signal an error if the value does not represent a number
(define (value->num v)
  (type-case Value v
    [numV (n) n]
    [else (error 'value->num "Bad number: ~a" v)]))

(test (value->num (numV 6)) 6)
(test (value->num (numV 9)) 9)
(test/exn (value->num (funV 'x (id 'x) empty-env)) "Bad number")



;; Value Value -> Value
;; produce the sum of two numbers
;; Effect: signal an error if either argument does not represent a number
(define (add/kra v1 v2)
  (let ([n1 (value->num v1)]
        [n2 (value->num v2)])
    (numV (+ n1 n2))))

(test (add/kra (numV 5) (numV 6)) (numV 11))
(test/exn (add/kra (numV 5) (funV 'x (id 'x) empty-env)) "Bad number")
(test/exn (add/kra (funV 'x (id 'x) empty-env) (numV 6)) "Bad number")
(test/exn (add/kra (funV 'x (id 'x) empty-env)
                   (funV 'x (id 'x) empty-env)) "Bad number")



;; Value -> Boolean
;; produce the boolean represented by the given value
;; Effect: signal an error if the value does not represent a boolean
(define (value->bool v)
  (type-case Value v
    [boolV (b) b]
    [else (error 'value->bool "Bad Boolean: ~a" v)]))

(test (value->bool (boolV #t)) #t)
(test (value->bool (boolV #f)) #f)
(test/exn (value->bool (funV 'x (id 'x) empty-env)) "Bad Boolean")



;; Value -> Value
;; produce true if v is a number and represents the number zero, else false
;; Effect: signals an error if the value does not represent a number
(define (nought?/kra v)
  (type-case Value v
    [numV (n) (boolV (zero? n))]
    [else (error 'nought? "Bad number: ~a" v)]))

(test (nought?/kra (numV 7)) (boolV #f))
(test (nought?/kra (numV 0)) (boolV #t))
(test/exn (nought?/kra (funV 'x (id 'x) empty-env)) "Bad number")



;; KID KID KRA Env -> Value
;; produce the result of (fixFun f x body) in environment env
;; Effect: signals an error in case of runtime error.
(define (fixFun/kra f x body env)
  (let ([env^ (extend-env-fixFun env f x body)])
    (lookup-env env^ f)))



;; KID Env -> Value
;; produce the value associated with variable x in environment env
(define (id/kra x env)
  (lookup-env env x))


(define-type trampoline
  [bounce [p procedure?]]
  [dismount [v (λ (x) #t)]])

;; (trampolineof X) is one of:
;; - (bounce ( -> (trampolineof X))
;; - (dismount X)
;; interp.  A 

(define (fn-for-trampoline t)
  (type-case trampoline t
    [bounce (p) (... (fn-for-trampoline (p)))]
    [dismount (v) (... v)]))


;; (trampolineof X) -> X
;; run the given trampoline to completion
(define (mount-trampoline t)
  (type-case trampoline t
    [bounce (p) (mount-trampoline (p))]
    [dismount (v) v]))


;; Kont is one of:
;; - `initial-k
;; - `(nought-k ,Kont)
;; - `(add1-k ,KRA ,Env ,Kont)
;; - `(add2-k ,Value ,Kont)
;; - `(if-k ,KRA ,KRA ,Env ,Kont)
;; - `(app1-k ,KRA ,Env ,Kont)
;; - `(app2-k ,Value ,Kont)
;; interp.  the rest of the computation

;; Kont Value -> (trampolineof Value)
;; apply k to v
(define (apply-k k0 v)
  (match k0
    [`initial-k (dismount v)]
    [`(nought-k ,k)
     (bounce (λ () (apply-k k (nought?/kra v))))]
    [`(add1-k ,r ,env ,k)
     (bounce
      (λ ()
        (interp/kra-env/kdt
         r env
         (add2-k v k))))]
    [`(add2-k ,vl ,k)
     (bounce (λ () (apply-k k (add/kra vl v))))]
    [`(if-k ,c ,a ,env ,k)
     (if (value->bool v)
         (bounce (λ () (interp/kra-env/kdt c env k)))
         (bounce (λ () (interp/kra-env/kdt a env k))))]
    [`(app1-k ,rand ,env ,k)
     (bounce
      (λ ()
        (interp/kra-env/kdt
         rand env
         (app2-k v k))))]
    [`(app2-k ,vrator ,k)
     (bounce (λ () (apply/kra/kdt vrator v k)))]))

(define initial-k
  `initial-k)

(define (nought-k k)
  `(nought-k ,k))

(define (add1-k r env k)
  `(add1-k ,r ,env ,k))

(define (add2-k vl k)
  `(add2-k ,vl ,k))

(define (if-k c a env k)
  `(if-k ,c ,a ,env ,k))

(define (app1-k rand env k)
  `(app1-k ,rand ,env ,k))

(define (app2-k vrator k)
  `(app2-k ,vrator ,k))

;; Accumulator: k is Kont
;; Invariant: the rest of the computation of kra0 after kra yields a value

;; Value Value Kont -> Value
;; produce the result of applying v1 to v2
;; Effect: signal an error if v1 does not represent a function
;; Effect: signal an error in case of subsequent runtime error
(define (apply/kra/kdt v1 v2 k)
  (type-case Value v1
    [funV (x body env)
          (bounce (λ () (interp/kra-env/kdt body (extend-env env x v2) k)))]
    [else (error 'apply/kra "Bad function: ~a" v1)]))


;; KRA Env Kont -> (trampolineof Value)
;; produce the result of interpreting kra in environment env
;; Effect: signal an error in case of runtime type mismatch

(define (interp/kra-env/kdt kra env k)
  (type-case KRA kra
    [num (n) (bounce (λ () (apply-k k (numV n))))]
    [add (l r)
         (bounce
          (λ ()
            (interp/kra-env/kdt
             l env
             (add1-k r env k))))]
    [nought? (e)
             (bounce
              (λ ()
                (interp/kra-env/kdt
                 e env
                 (nought-k k))))]
    [bool (b) (bounce (λ () (apply-k k (boolV b))))]
    [ifB (p c a)
         (bounce
          (λ ()
            (interp/kra-env/kdt
             p env
             (if-k c a env k))))]
    [id (x) (bounce (λ () (apply-k k (id/kra x env))))]
    [fixFun (f x body) (bounce (λ () (apply-k k (fixFun/kra f x body env))))]
    [app (rator rand)
         (bounce
          (λ ()
            (interp/kra-env/kdt
             rator env
             (app1-k rand env k))))]))


;; KRA -> Value
;; interpret the given KRA expression
;; EFFECTS: Signals an error in case of runtime type error.
(define (interp/kra/kdt kra0)
  (mount-trampoline
   (bounce
    (λ ()
      (interp/kra-env/kdt kra0 empty-env initial-k))))) ; stub for the examples


(test
 (interp/kra/kdt
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

;; KRAFS is one of:
;; - Number
;; - `{+ ,KRAFS ,KRAFS}
;; - `{nought? ,KRAFS}
;; - Boolean
;; - `{ifB ,KRAFS ,KRAFS ,KRAFS}
;; - `{andB ,KRAFS ,KRAFS}
;; - `{notB ,KRAFS}
;; -  KID
;; - `{with {,KID ,KRAFS} ,KRAFS}
;; - `{,KRAFS ,KRAFS}
;; - `{fun {,KID} ,KRAFS}
;; - `{fixFun ,KID {,KID} ,KRAFS}
;; - <any other s-expression>
;; interp.  any s-expression, focusing on those that represent
;;          KRAken expressions.


(define (fn-for-krafs krafs)
  (match krafs
    [`,n
     #:when (number? n)
     (... n)]
    [`{+ ,krafs1 ,krafs2}
     (... (fn-for-krafs krafs1)
          (fn-for-krafs krafs2))]
    [`{nought? ,krafs1}
     (... (fn-for-krafs krafs1))]
    [`,b
     #:when (boolean? b)
     (... b)]
    [`{ifB ,krafs1 ,krafs2 ,krafs3}
     (... (fn-for-krafs krafs1)
          (fn-for-krafs krafs2)
          (fn-for-krafs krafs3))]
    [`{andB ,krafs1 ,krafs2}
     (... (fn-for-krafs krafs1)
          (fn-for-krafs krafs2))]
    [`{notB ,krafs1}
     (... (fn-for-krafs krafs1))]
    [`,x
     #:when (kid? x)
     (... x)]
    [`{with {,x ,krafs1} ,krafs2}
     (... x
          (fn-for-krafs krafs1)
          (fn-for-krafs krafs2))]
    [`{,krafs1 ,krafs2}
     (... (fn-for-krafs krafs1)
          (fn-for-krafs krafs2))]
    [`{fun {,x} ,krafs1}
     (... x
          (fn-for-krafs krafs1))]
    [`{fixFun ,f {,x} ,krafs1}
     (... f
          x
          (fn-for-krafs krafs1))]
    [otherwise (...)]))


;; KRAFS -> KRA
;; parse the given s-expression into a KRA expression
;; Effect: signals an error on failure
(define (parse/kra krafs)
  (match krafs
    [`,n
     #:when (number? n)
     (num n)]
    [`{+ ,krafs1 ,krafs2}
     (add (parse/kra krafs1)
          (parse/kra krafs2))]
    [`{nought? ,krafs1}
     (nought? (parse/kra krafs1))]
    [`,b
     #:when (boolean? b)
     (bool b)]
    [`{ifB ,krafs1 ,krafs2 ,krafs3}
     (ifB (parse/kra krafs1)
          (parse/kra krafs2)
          (parse/kra krafs3))]
    [`{andB ,krafs1 ,krafs2}
     (andB (parse/kra krafs1)
           (parse/kra krafs2))]
    [`{notB ,krafs1}
     (notB (parse/kra krafs1))]
    [`,x
     #:when (kid? x)
     (id x)]
    [`{with {,x ,krafs1} ,krafs2}
     (with x
           (parse/kra krafs1)
           (parse/kra krafs2))]
    [`{,krafs1 ,krafs2}
     (app (parse/kra krafs1)
          (parse/kra krafs2))]
    [`{fun {,x} ,krafs1}
     (fun x
          (parse/kra krafs1))]
    [`{fixFun ,f {,x} ,krafs1}
     (fixFun f
             x
             (parse/kra krafs1))]
    [otherwise (error 'parse/kra "Bad KRA expression: ~a" krafs)]))


(test/match
 (parse/kra WAES4)
 (app (fixFun g1 'x (app (fixFun g2 'y (add (id 'y) (id 'y)))
                         (add (id 'x) (num -3))))
      (add (num 5) (num 5))))

(test/match
 (parse/kra WAES5)
 (app (fixFun g1 'x (add (id 'x)
                         (app (fixFun g2 'x (num 10)) (num 3))))
      (num 5)))

(test/match
 (parse/kra WAES6)
 (app (fixFun g1 'x (add (id 'x) (app (fixFun g2 'x (id 'x))
                                      (num 3))))
      (num 5)))

(test/match
 (parse/kra WAES7)
 (app (fixFun g1 'x (add (id 'x) (app (fixFun g2 'y (id 'x))
                                      (num 3))))
      (num 5)))


;;
;; PUTTING IT ALL TOGETHER  - an interpreter of files on disk
;;

;; S-expression -> Value
;; produce the result of interpreting the KRAken program in input
;; EFFECT: signals an error if interpretation signals a runtime error.
(define (interp-sexp pgm)
  (interp/kra/kdt
   (parse/kra
    pgm)))

(test (interp-sexp '{with {f {fixFun x {y}
                                     {ifB {nought? y}
                                          0
                                          {+ y {x {+ y -1}}}}}}
                          {f 5}})
      (numV 15))


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




;; String -> Value
;; produce the result of interpreting the KRAken program in input
;; Effect: signals an error if interpretation signals a runtime error.
(define (interp-string pgm)
  (interp/kra/kdt
   (parse/kra
    (read-from-string pgm))))


;; String -> Value
;; produce the result of interpreting the KRAken program from the file fname
;; Effect: signals an error if no file fname contains a KRAken representation
;; Effect: signals an error if interpretation signals a runtime error.
(define (interp-file fname)
  (interp/kra/kdt
   (parse/kra
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
