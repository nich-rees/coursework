#lang plai
(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

(require "test-match.rkt")
;; CPSC311 2023 Winter Term 1
;; Assignment 3:  Just a Little Lambda On Top Please

;; Released: Monday, October 9, 2023
;; Due: Monday, October 16, 2023 11:59pm

;; Student ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Name: Nicholas Rees
;; Student Number: 11848363
;; CWL: nrees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: This assignment has 3 PROBLEMS.  PROBLEM 3 will be the majority of the
;; work in this assignment.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Helpers


;; (listof Symbol) -> Boolean
;; produce true if each symbol in lox appears only once
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



;;
;; HoistFWAE (HFWAE) - a language of "hoisted" functions
;;

;; FWAE allows us to insert anonymous first-class functions *anywhere*
;; in a program, and thereby enables the creation of functions that
;; close over their surrounding environment, like the following FWAE
;; expression:
#;
'{with {f {with {a 5}
                {with {b 7}
                      {fun {x} {- {+ a x} b}}}}}
       {+ {f 6} {f 7}}}

;; the identifiers a and b are not bound in the scope of the 2 calls to f,
;; but thanks to lexical scoping, the fun expression closes over its
;; surrounding environment, capturing their values.  The ability to
;; define a function in arbitrary expression positions can be quite
;; convenient.  But it's not necessary.

;; This homework explores a slightly quirky language called HoistFWAE,
;; that is similar to F1WAE in that it supports second-class/first-order
;; functions defined at the top-level, and the language has separate namespaces
;; for second-class functions versus local identifiers.  But the language is 
;; similar to FWAE in that second-class functions immediately produce anonymous
;; first-class functions as values.  These first-class values can be bound to
;; regular identifiers.

;; To demonstrate, the FWAE program above translates to the following
;; HoistFWAE program:
#;#;
'{define-fn {F a b} {fun {x} {- {+ a x} b}}}
'{with {f {with {a 5}
                {with {b 7}
                      {F a b}}}}
       {+ {f 6} {f 7}}}

;; The result of the multi-argument second-class function call {F a b} is a
;; first-class function that is bound to f and later applied twice, each time
;; to a single argument.

;; HoistFWAE is designed specifically so that every FWAE program can be
;; translated to an equivalent HoistFWAE program. You can think of HoistFWAE as
;; the first step in translating a language with nested anonymous functions to a
;; language like C that only has global function pointers and data structures
;; (the next step is to make the lambdas go away entirely)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HFID is Symbol
;; interp. identifier in HFWAE language
;; INVARIANT: may not be any HFWAE keyword
(define (hfid? x)
  (and (symbol? x)
       (not (member x '(+ - with define-fn fun if0)))))

(define HFID0 'a)
(define HFID1 'b)

;; No template: atomic data



(define-type HFWAE    
  [num/hfwae (n number?)] 
  [add/hfwae (lhs HFWAE?) (rhs HFWAE?)]
  [sub/hfwae (lhs HFWAE?) (rhs HFWAE?)]
  [id/hfwae (name hfid?)]
  [with/hfwae (x hfid?) (named HFWAE?) (body HFWAE?)]
  [app2/hfwae (fun-name hfid?) (args (listof HFWAE?))]
  [app1/hfwae (rator HFWAE?) (rand HFWAE?)]
  [if0/hfwae (predicate HFWAE?) (consequent HFWAE?) (alternative HFWAE?)])
;; interp. expressions in a language that supports first-order (second-class)
;; functions that produce higher-order (first-class) functions as values.
;; Its syntax is defined by the following Extended BNF (EBNF):
;; <HFWAE> ::= <num>
;;           | {+ <HFWAE> <HFWAE>}
;;           | {- <HFWAE> <HFWAE>}
;;           | <id>
;;           | {<id> <HFWAE>*}   ;; second-class function call
;;           | {<HFWAE> <HFWAE>} ;; first-class function call
;;           | {if0 <HFWAE> <HFWAE> <HFWAE>}
;; A function call is second-class only if the identifier in operator position
;; occurs free in the program.  
(define HF0 (num/hfwae 0))
(define HF1 (num/hfwae 7))
(define HF2 (add/hfwae (num/hfwae 9)
                       (num/hfwae 10)))

(define HF3 (app2/hfwae 'f (list HF0 HF1 HF2)))

#;
(define (fn-for-hfwae hfwae)
  (type-case HFWAE hfwae
    [num/hfwae (n) (... n)]
    [add/hfwae (l r) (... (fn-for-hfwae l)
                          (fn-for-hfwae r))]
    [sub/hfwae (l r) (... (fn-for-hfwae l)
                          (fn-for-hfwae r))]
    [id/hfwae (x) (... x)]
    [with/hfwae (x named body) (... x
                                    (fn-for-hfwae named)
                                    (fn-for-hfwae body))]
    [app2/hfwae (fun-name rand*) (... fun-name
                                      (fn-for-lohfwae rand*))]
    [app1/hfwae (rator rand) (... (fn-for-hfwae rator)
                                  (fn-for-hfwae rand))]
    [if0/hfwae (p c a)
               (... (fn-for-hfwae p)
                    (fn-for-hfwae c)
                    (fn-for-hfwae a))]))

#;
(define (fn-for-lohfwae lohfwae)
  (cond
    [(empty? lohfwae) (...)]
    [else ;; (cons? lohfwae)
     (... (fn-for-hfwae (first lohfwae))
          (fn-for-lohfwae (rest lohfwae)))]))



(define-type FunDef
  [fundef/hfwae (name hfid?)
                (params2 (and/c (listof hfid?) unique?))
                (param1 hfid?)
                (body HFWAE?)])
;; interp. a second-class function definition with a name, formal parameters,
;;         and body that returns a first-class function of one parameter.
;; Its syntax is defined by following BNF:
;; <FunDef> ::= {define-fn {<id>*} {fun {<id>} <hfwae>}
(define FD*1 (fundef/hfwae 'f '(x y) 'z
                           (sub/hfwae (add/hfwae (id/hfwae 'x) (id/hfwae 'y))
                                      (id/hfwae 'z))))

#;
(define (fn-for-fundef fd)
  (type-case FunDef fd
    [fundef/hfwae (f x* x hfwae)
                  (... f
                       (fn-for-lox x*)
                       x
                       (fn-for-hfwae hfwae))]))
#;
(define (fn-for-lox lox)
  (cond
    [(empty? lox) (...)]
    [else ;; (cons? lox)
     (... (first lox)
          (fn-for-lox (rest lox)))]))


;; NOTE: For simplicity, we impose hfid? on WAE identifiers, rather than
;; waeid? (so as to save you some grief).
(define-type FWAE    
  [num/fwae (n number?)] 
  [add/fwae (lhs FWAE?) (rhs FWAE?)]
  [sub/fwae (lhs FWAE?) (rhs FWAE?)]
  [with/fwae (name hfid?) (named-expr FWAE?) (body FWAE?)]
  [id/fwae (name hfid?)]
  [fun/fwae (param hfid?) (body FWAE?)]
  [app/fwae (rator FWAE?) (arg FWAE?)]
  [if0/fwae (predicate FWAE?) (consequent FWAE?) (alternative FWAE?)])
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


(define AE1 (num/fwae 4))
(define AE2 (add/fwae AE1 (num/fwae 5)))
(define AE3 (sub/fwae (num/fwae 6) (num/fwae 3)))

;; Every WAE program is an FWAE program
(define WAE4 (with/fwae 'x (add/fwae (num/fwae 5) (num/fwae 5))
                        (with/fwae 'y (sub/fwae (id/fwae 'x) (num/fwae 3))
                                   (add/fwae (id/fwae 'y) (id/fwae 'y)))))

(define WAE5 (with/fwae 'x (num/fwae 5)
                        (add/fwae (id/fwae 'x)
                                  (with/fwae 'x (num/fwae 3) (num/fwae 10)))))

(define WAE6 (with/fwae 'x (num/fwae 5)
                        (add/fwae (id/fwae 'x)
                                  (with/fwae 'x (num/fwae 3) (id/fwae 'x)))))

(define WAE7 (with/fwae 'x (num/fwae 5)
                        (add/fwae (id/fwae 'x)
                                  (with/fwae 'y (num/fwae 3) (id/fwae 'x)))))

(define FWAE1 (fun/fwae 'x (add/fwae (id/fwae 'x) (id/fwae 'x))))

(define FWAE2 (fun/fwae 'x (add/fwae (id/fwae 'x) (num/fwae 1))))

(define FWAE3 (fun/fwae 'x (add/fwae (id/fwae 'x) (id/fwae 'y))))


#;
(define (fn-for-fwae f)
  (type-case FWAE f
    [num/fwae (n) (... n)]
    [add/fwae (l r) (... (fn-for-fwae l)
                         (fn-for-fwae r))]
    [sub/fwae (l r) (... (fn-for-fwae l)
                         (fn-for-fwae r))]
    [with/fwae (x named body)
               (... x
                    (fn-for-fwae named)
                    (fn-for-fwae body))]
    [id/fwae (x) (... x)]
    [fun/fwae (x body) (... x
                            (fn-for-fwae body))]
    [app/fwae (rator rand) (... (fn-for-fwae rator)
                                (fn-for-fwae rand))]
    [if0/fwae (p c a)
              (... (fn-for-fwae p)
                   (fn-for-fwae c)
                   (fn-for-fwae a))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Identifier (listof FunDef) -> FunDef
;; produce the named FunDef from the given list
;; Effect: signal an error on failure.
(define (lookup-fundef f fundefs)
  (let ([result (findf (λ (fd) (symbol=? f (fundef/hfwae-name fd))) fundefs)])
    (if result
        result
        (error 'lookup-fundef "No such function: ~a" f))))

(test/exn (lookup-fundef 'double (list)) "")
(test/exn (lookup-fundef 'double (list FD*1)) "")
(test (lookup-fundef 'f (list FD*1)) FD*1)



;;
;; Environments
;;

;; (envof X) is Symbol -> X
;; interp. bindings of symbols to X objects

;; Any -> Boolean
;; produce true if the given argument is an environment
(define (env? x)
  (procedure? x))

;; (envof X)
;; Effect: signals an error if variable is not there
(define empty-env
  (λ (x) (error 'lookup-env "Undefined identifier: ~a" x)))

(test/exn (empty-env 'x) "Undefined")



;; PROBLEM 1:  Complete the design of extend-env

;; (envof X) (listof Symbol) (listof X) -> (envof X)
;; produce an environment that binds distinct symbols in x* to objects in v*
;; ASSUME: (= (length x*) (length v*))
;; ASSUME: (unique? x*)
; (define (extend-env* env x* v*) empty-env) ; stub
(define (extend-env* env x* v*)
  (λ (x)
    (if (empty? (filter (λ (a) (symbol=? x a)) x*))
        (env x)
        (second (first (filter (λ (a) (symbol=? x (first a)))
                               (map (λ (x0 v0) (list x0 v0)) x* v*)))))))


(test ((extend-env* empty-env '(x y z) '(5 6 7)) 'y) 6)
(test ((extend-env*
        (extend-env* empty-env '(x y z) '(5 6 7))
        '(a x c) '(5 6 7)) 'x) 6)
(test ((extend-env*
        (extend-env* empty-env '(x y z) '(5 6 7))
        '(a x c) '(5 6 7)) 'z) 7)



;; (envof X) Symbol -> X
;; produce the binding for symbol
;; Effect: signals an error if no binding is found
(define (lookup-env env x) (env x))

(test (lookup-env (extend-env*
                   (extend-env* empty-env '(x y z) '(5 6 7))
                   '(a x c) '(5 6 7))
                  'z)
      7)

;; (envof X) Symbol X -> (envof X)
;; produce an environment that extends env to bind x to v
(define (extend-env env x v)
  (extend-env* env (list x) (list v)))

(test (lookup-env (extend-env empty-env 'x 5) 'x) 5)


;; Env is (envof Value)
;; interp. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-type Value
  [numV (n number?)]
  [funV (x hfid?) (body HFWAE?) (env env?)])

;; interp. an HFWAE value 
(define V1 (numV 7))
(define V2 (funV 'x (id/hfwae 'x) empty-env))

#;
(define (fn-for-value v)
  (type-case Value v
    [numV (n) (... n)]
    [funV (x body env) (... x
                            (fn-for-hfwae body)
                            env)]))
          

;; Value -> Number
;; produce the number represented by the given value
;; Effect: signal an error if the value does not represent a number
(define (value->num v)
  (type-case Value v
    [numV (n) n]
    [funV (x body env) (error 'evalue->num "Bad number: ~a" v)]))

(test (value->num (numV 6)) 6)
(test (value->num (numV 9)) 9)
(test/exn (value->num (funV 'x (id/hfwae 'x) empty-env)) "Bad number")


;; Value (Symbol FWAE Env -> X) -> X
;; extract the fields of ev as a funV and apply them to fn
;; Effect: signal an error if ev does not represent a funv
(define (with-value-as-funv ev fn)
  (type-case Value ev
    [funV (x body env) (fn x body env)]
    [numV (n) (error 'evalue->num "Bad function: ~a" ev)]))

(test (with-value-as-funv (funV 'x (id/hfwae 'y) empty-env)
        (λ (x body env) (funV x body env)))
      (funV 'x (id/hfwae 'y) empty-env)) 
(test/exn (with-value-as-funv (numV 9)
            (λ (x body env) (funV x body env)))
          "Bad")

;;
;; Helpers
;;
  
;; (Number Number -> Number) Value Value  -> Value
;; apply num-op to the numbers represented by v1 and v2
;; Effect: signal an error if either argument does not represent a number
(define (op/hf num-op v1 v2)
  (let ([n1 (value->num v1)]
        [n2 (value->num v2)])
    (numV (num-op n1 n2))))

(test (op/hf * (numV 5) (numV 6)) (numV 30))
(test/exn (op/hf * (numV 5) (funV 'x (id/hfwae 'x) empty-env)) "Bad number")
(test/exn (op/hf * (funV 'x (id/hfwae 'x) empty-env) (numV 6)) "Bad number")
(test/exn (op/hf * (funV 'x (id/hfwae 'x) empty-env)
                 (funV 'x (id/hfwae 'x) empty-env)) "Bad number")

  
;; Value Value -> Value
;; produce the sum of two numbers
;; Effect: signal an error if either argument does not represent a number
(define (add/hf v1 v2)
  (op/hf + v1 v2))

(test (add/hf (numV 5) (numV 6)) (numV 11))
(test/exn (add/hf (numV 5) (funV 'x (id/hfwae 'x) empty-env)) "Bad number")
(test/exn (add/hf (funV 'x (id/hfwae 'x) empty-env) (numV 6)) "Bad number")
(test/exn (add/hf (funV 'x (id/hfwae 'x) empty-env)
                  (funV 'x (id/hfwae 'x) empty-env)) "Bad number")


;; Value Value -> Value
;; produce the difference of two numbers
;; Effect: signal an error if either argument does not represent a number
(define (sub/hf v1 v2)
  (op/hf - v1 v2))

(test (sub/hf (numV 5) (numV 6)) (numV -1))
(test/exn (sub/hf (numV 5) (funV 'x (id/hfwae 'x) empty-env)) "Bad number")
(test/exn (sub/hf (funV 'x (id/hfwae 'x) empty-env) (numV 6)) "Bad number")
(test/exn (sub/hf (funV 'x (id/hfwae 'x) empty-env)
                  (funV 'x (id/hfwae 'x) empty-env)) "Bad number")


;; Value -> Boolean
;; produce true if ev1 represents the number zero, else false
(define (zero?/hf v)
  (type-case Value v
    [numV (n) (zero? n)]
    [funV (x body env) #f]))

(test (zero?/hf (numV 7)) #f)
(test (zero?/hf (numV 0)) #t)
(test (zero?/hf (funV 'x (id/hfwae 'x) empty-env)) #f)


;; Problem 2: Complete the definition of interp/hfwae.  Expect to add helper
;; functions to the local expression below.

;; HFWAE (listof FunDef) -> Value
;; interpret the given expression in the context of the given toplevel functions
;; Effect: Signals an error in case of unbound identifiers or mismatched values
(define (interp/hfwae hfwae0 fundefs)
  ;; Accumulator: env is Env
  ;; Invariant: bindings induced by pending substitutions into hfwae
  (local [;; HFWAE Env -> Value
          (define (interp/hfwae--env hfwae env)
            (type-case HFWAE hfwae
              [num/hfwae (n) (numV n)]
              [add/hfwae (l r) (add/hf (interp/hfwae--env l env)
                                       (interp/hfwae--env r env))]
              [sub/hfwae (l r) (sub/hf (interp/hfwae--env l env)
                                       (interp/hfwae--env r env))]
              [id/hfwae (x)
                        (with-handlers
                            ([exn:fail?
                              (λ (_)
                                (error 'interp/hfwae
                                       "Unbound identifier: ~a"
                                       x))])
                          (lookup-env env x))]
              [with/hfwae (x named body)
                          (let ([v (interp/hfwae--env named env)])
                            (interp/hfwae--env body
                                               (extend-env env x v)))]
              [app2/hfwae (fun-name rand*)
                          (local [(define the-fun-def
                                    (lookup-fundef fun-name fundefs))]
                            (interp/hfwae--env
                             (fundef/hfwae-body the-fun-def)
                             (extend-env env
                                         (fundef/hfwae-params2 the-fun-def)
                                         (interp/lohfwae--env rand*))))]
              [app1/hfwae (rator rand)
                          (interp/hfwae--env
                           rator
                           (extend-env env
                                       'x ;; This is here because idk what to do
                                       ;; I want to find the free ids in rator,
                                       ;; and replace this with (interp rand)
                                       ;; but I need to find them
                                       (interp/hfwae--env rand env)))]
              [if0/hfwae (p c a)
                         (if (zero?/hf (interp/hfwae--env p env))
                             (interp/hfwae--env c env)
                             (interp/hfwae--env a env))]))

          ;; (listof HFWAE) Env -> (listof Value)
          (define (interp/lohfwae--env lohfwae env)
            ;; for/list is analogous to map: its template would be similar
            (for/list ([hfwae lohfwae])
              (interp/hfwae--env hfwae env)))

          ;; ADD HELPER FUNCTIONS HERE

          ]   
    (interp/hfwae--env hfwae0 empty-env)))



(test (interp/hfwae (num/hfwae 7) empty) (numV 7))
(test (interp/hfwae (add/hfwae (num/hfwae 7)
                               (num/hfwae 7))
                    empty)
      (numV 14))

(test (interp/hfwae (sub/hfwae (num/hfwae 7)
                               (num/hfwae 7))
                    empty)
      (numV 0))

(test/exn (interp/hfwae (id/hfwae 'x) empty) "Unbound")


(test (interp/hfwae
       (app1/hfwae (app2/hfwae 'one empty) (num/hfwae 0))
       (list (fundef/hfwae 'one empty 'x (num/hfwae 7))))
      (numV 7))

(test (interp/hfwae
       (app1/hfwae (app2/hfwae 'f (list 
                                   (num/hfwae 3)
                                   (num/hfwae 7)))
                   (num/hfwae 2))
       (list FD*1))
      (numV 8))

(test/exn (interp/hfwae
           (app2/hfwae 'f (list 
                           (num/hfwae 3)
                           (num/hfwae 7)))
           empty)
          "No such function")

(test/exn (interp/hfwae
           (app1/hfwae (num/hfwae 7)
                       (num/hfwae 2))
           empty)
          "Bad function")

(test/exn (interp/hfwae
           (app2/hfwae 'f (list 
                           (num/hfwae 3))) (list FD*1))
          "Wrong number of arguments")


(define FD*2
  (fundef/hfwae
   'recur '(x) 'y
   (if0/hfwae (id/hfwae 'x)
              (id/hfwae 'y)
              (add/hfwae (id/hfwae 'x)
                         (app1/hfwae
                          (app2/hfwae 'recur
                                      (list (sub/hfwae (id/hfwae 'x)
                                                       (num/hfwae 1))))
                          (id/hfwae 'y))))))

(test (interp/hfwae
       (app1/hfwae (app2/hfwae 'recur (list
                                       (num/hfwae 3)))
                   (num/hfwae 7))
       (list FD*1 FD*2))
      (numV 13))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Raise the Roof^H^H^H^HLambdas!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This exercise is intended to exercise your understanding of lexical
;; scope and program-manipulating function design.

;; As alluded to at the beginning of this assignment, the first-class functions
;; that HoistFWAE bakes into its second-class functions suffice to express all
;; FWAE programs.  Your job is to demonstrate this by implementing an
;; interpreter for FWAE that works by translating FWAE programs into
;; HoistFWAE programs and then running them on the HoistFWAE interpreter.

;; The idea is this: each FWAE fun expression is translated into its own
;; HoistFWAE second-class *named* function, given a globally unique name
;; (using gensym).  The second-class function is parameterized on the
;; (zero or more) arguments each of the identifiers that both:
;; 1) appears free in the entire fun expression (*not* just its body);
;; 2) is bound in the context surrounding the expression.

;; The fun expression itself is replaced with a call to its corresponding
;; second-class function, applying it to the identifiers that are identified
;; above.

;; For example, the FWAE program:
#;
'{with {a 7}
       {fun {b} {+ a c}}}

;; translates into an HoistFWAE program (which may differ in function name):
#;#;
'{define-fn {g77 a} {fun {b} {+ a c}}}
'{with {a 7}
       {g77 a}}

;; Notice that the fun expression {fun {b} {+ a c}} has *two* free identifiers,
;; a and c, but the function g77 is only applied to a.  This ensures that the
;; HoistFWAE program executes successfully, just like the FWAE one.  The
;; function only produces an unbound identifier error if it is called.


;; There's some subtlety to this translation process:  A fun expression may
;; itself contain other fun expressions, and those too must be hoisted.  So
;; for example:

#;'{with {a 7}
         {with {d 4}
               {with {f {fun {b} {+ a {+ c {{fun {e} a} d}}}}}
                     {f 12}}}}

;; translates to
#;#;#;
'{define-fn {g77 a d} {fun {b} {+ a c {{g76 a} d}}}}
'{define-fn {g76 a} {fun {e} a}}
'{with {a 7}
       {with {d 4}
             {with {f {g77 a d}}
                   {f 12}}}}
      
;; So the body of the hoisted g77 function contains a call to the hoisted g76
;; function (to represent the formerly nested FWAE fun expression),
;; and the body of the main HoistFWAE expression replaces the outermost
;; fun expression with a call to g77.  Notice that g77 only takes one copy
;; of the a parameter, even though there are two free references to a in
;; its body.  Your translation should behave as such.

;; Your mission is to realize this translation in code.


;; PROBLEM 3: Complete the design of a function that translates a FWAE program
;; into a HoistFWAE expression and accompanying FunDefs, one for each fun
;; expression.

;; NOTES:
;; 1) Along the way you should expect to design some helper functions.
;;    Keep each function manageable in size and roughly performing "one task".
;;    In particular, expect to design a function specifically for handling
;;    fun expressions.
;; 2) Expect to make use of accumulators.  Also, based on the signature
;;    given below, you can expect to make use of compound return values.
;;    You do not need threaded accumulators!
;; 3) You may want to use PLAI set operations like set-add, set-member?, 
;;    set-union, and set-intersect which can treat lists as sets (lists of
;;    elements that appear at most once).

;; FWAE -> (list HFWAE (listof FunDef))
;; translate the given fwae expression to a corresponding HFWAE program
;(define (fwae->hfwae fwae) (list (num/hfwae 0) empty)) ; stub

(define (fwae->hfwae fwae)
  (type-case FWAE fwae
    [num/fwae (n) (list (num/hfwae n) empty)]
    [add/fwae (l r) (let ([left (fwae->hfwae l)]
                          [right (fwae->hfwae r)])
                      (list (add/hfwae (first left) (first right))
                            (append (second right) (second left))))]
    [sub/fwae (l r) (let ([left (fwae->hfwae l)]
                          [right (fwae->hfwae r)])
                      (list (sub/hfwae (first left) (first right))
                            (append (second right) (second left))))]
    [with/fwae (x named body)
               (let ([nam (fwae->hfwae named)]
                     [bod (fwae->hfwae body)])
               (list (with/hfwae x
                                 (first nam)
                                 (first bod))
                     (append (second bod) (second nam))))]
    [id/fwae (x) (list (id/hfwae x) empty)]
    [fun/fwae (x body)
              (let ([f (gensym)]
                    [bod (fwae->hfwae body)])
                (list
                 (app2/hfwae f (list (id/hfwae x)))
                 (cons
                  (fundef/hfwae f
                               empty ; This is not right... I think this is
                               ; supposed to be the ids in body
                               ; that are not x
                               x
                               (first bod))
                  (second bod))))]
    [app/fwae (rator rand)
              (let ([rat (fwae->hfwae rator)]
                    [ran (fwae->hfwae rand)])
              (list (app1/hfwae (first rat) (first rand))
                    (append (second rat) (second rand))))]
    [if0/fwae (p c a)
              (let ([pr (fwae->hfwae p)]
                    [co (fwae->hfwae c)]
                    [al (fwae->hfwae a)])
                (list (if0/hfwae (first p) (first c) (first a))
                      (append (second p) (append (second c) (second a)))))]))


;; These examples are *not* sufficient.  You should add more of your own
;; BEFORE moving on to implementing (and more as needed while implementing)
(test (fwae->hfwae (num/fwae 5)) (list (num/hfwae 5) empty))
(test (fwae->hfwae (id/fwae 'x)) (list (id/hfwae 'x) empty))
(test (fwae->hfwae (add/fwae (num/fwae 5)
                             (num/fwae 6)))
      (list (add/hfwae (num/hfwae 5)
                       (num/hfwae 6))
            empty))


(test/match
 (fwae->hfwae
  (with/fwae 'a (num/fwae 7)
             (fun/fwae 'b
                       (add/fwae (id/fwae 'a) (id/fwae 'c)))))
 (list (with/hfwae 'a (num/hfwae 7)
                   (app2/hfwae g77 (list (id/hfwae 'a))))
       (list
        (fundef/hfwae g77 (list 'a)
                      'b (add/hfwae (id/hfwae 'a) (id/hfwae 'c))))))

(test/match
 (fwae->hfwae
  (with/fwae 'f
             (with/fwae
              'a (num/fwae 5)
              (with/fwae 'b (num/fwae 7)
                         (fun/fwae 'x
                                   (sub/fwae (add/fwae (id/fwae 'a)
                                                       (id/fwae 'x))
                                             (id/fwae 'b)))))
             (add/fwae (app/fwae (id/fwae 'f) (num/fwae 6))
                       (app/fwae (id/fwae 'f) (num/fwae 6)))))
 (list
  (with/hfwae 'f
              (with/hfwae
               'a (num/hfwae 5)
               (with/hfwae 'b (num/hfwae 7)
                           (app2/hfwae g77 (list (id/hfwae 'a)
                                                 (id/hfwae 'b)))))
              (add/hfwae (app1/hfwae (id/hfwae 'f) (num/hfwae 6))
                         (app1/hfwae (id/hfwae 'f) (num/hfwae 6))))
  (list
   (fundef/hfwae g77 (list 'a 'b)
                 'x (sub/hfwae (add/hfwae (id/hfwae 'a)
                                          (id/hfwae 'x))
                               (id/hfwae 'b))))))
  

(test/match
 (fwae->hfwae
  (with/fwae
   'a (num/fwae 7)
   (with/fwae
    'd (num/fwae 4)
    (with/fwae
     'f (fun/fwae 'b
                  (add/fwae (id/fwae 'a)
                            (add/fwae (id/fwae 'c)
                                      (app/fwae (fun/fwae 'e (id/fwae 'a))
                                                (id/fwae 'd)))))
     (app/fwae (id/fwae 'f) (num/fwae 12))))))
 (list
  (with/hfwae
   'a (num/hfwae 7)
   (with/hfwae
    'd (num/hfwae 4)
    (with/hfwae
     'f (app2/hfwae g77 (list (id/hfwae 'a) (id/hfwae 'd)))
     (app1/hfwae (id/hfwae 'f) (num/hfwae 12)))))
  (list
   (fundef/hfwae g77 (list 'a 'd)
                 'b (add/hfwae (id/hfwae 'a)
                               (add/hfwae (id/hfwae 'c)
                                          (app1/hfwae
                                           (app2/hfwae g76 (list (id/hfwae 'a)))
                                           (id/hfwae 'd)))))
   (fundef/hfwae g76 (list 'a)
                 'e (id/hfwae 'a)))))



;;
;; OBSERVATION: Technically HoistFWAE doesn't even need with expressions!
;; We can first translate all {with {x e1} e2} expressions to {{fn {x} e2} e1},
;; and only then translate to HoistFWAE, thereby hoisting all THESE new funs.
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Putting it all together
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FWAE -> Value
;; evaluate the given FWAE expression
(define (interp/fwae fwae)
  (match-let ([`(,hfwae ,fundefs) (fwae->hfwae fwae)])
    (interp/hfwae hfwae fundefs)))

(test (interp/fwae AE1) (numV 4))
(test (interp/fwae AE2) (numV 9))
(test (interp/fwae AE3) (numV 3))
(test (interp/fwae WAE4) (numV 14))
(test (interp/fwae WAE5) (numV 15))
(test (interp/fwae WAE6) (numV 8))

(test/match (interp/fwae FWAE1)
            (funV 'x
                  (add/hfwae (id/hfwae 'x) (id/hfwae 'x))
                  env))
(test/match (interp/fwae FWAE2)
            (funV 'x
                  (add/hfwae (id/hfwae 'x) (num/hfwae 1))
                  env))
(test/match (interp/fwae FWAE3)
            (funV 'x
                  (add/hfwae (id/hfwae 'x) (id/hfwae 'y))
                  env))

(test
 (interp/fwae
  (with/fwae 'f
             (with/fwae
              'a (num/fwae 5)
              (with/fwae 'b (num/fwae 7)
                         (fun/fwae 'x
                                   (sub/fwae (add/fwae (id/fwae 'a)
                                                       (id/fwae 'x))
                                             (id/fwae 'b)))))
             (add/fwae (app/fwae (id/fwae 'f) (num/fwae 6))
                       (app/fwae (id/fwae 'f) (num/fwae 6)))))
 (interp/hfwae
  (with/hfwae 'f
              (with/hfwae
               'a (num/hfwae 5)
               (with/hfwae 'b (num/hfwae 7)
                           (app2/hfwae 'g77 (list (id/hfwae 'a)
                                                  (id/hfwae 'b)))))
              (add/hfwae (app1/hfwae (id/hfwae 'f) (num/hfwae 6))
                         (app1/hfwae (id/hfwae 'f) (num/hfwae 6))))
  (list
   (fundef/hfwae 'g77 (list 'a 'b)
                 'x (sub/hfwae (add/hfwae (id/hfwae 'a)
                                          (id/hfwae 'x))
                               (id/hfwae 'b))))))