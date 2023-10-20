#lang plai
(define (... . args) (cons '... args))
(print-only-errors)

(require "p1-appendix.rkt") ; commented-out code is defined in this file


;; Problem 1: Fruits from the Sugar Pair Tree

;; TL;DR:
;; Problem 1a (line XXX): design a helper to interpret match/pair expressions
;; Problem 1b (line XXX): add support for with as sugar to the SPL parser.


;; SPL is the Sugared Pair Language. It supports numbers and pairs, but instead
;; of a first and rest function, it uses a simple pattern-match notation to
;; access both elements of a pair.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

;; gensym?: S-expression -> Boolean
;; produce true if the given value is a gensym, otherwise false.

;; test/match A testing helper for using pattern-matching PLAI tests
;; WARNING: failures yield messy messages!

;; USAGE:
;; (test/match expr pat)
;; or
;; (test/match expr pat #:when bool-expr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Identifier is Symbol
;; interp. an SPL identifier
;; INVARIANT: id must not be an SPL keyword

;; identifier? : Any -> Boolean
;; produce true if the given object is an SPL identifier, otherwise false


;(define-type SPL    
;  [num (n number?)] 
;  [add  (lhs SPL?) (rhs SPL?)]
;  [pair (fst SPL?) (snd SPL?)]
;  [id (name identifier?)]
;  [match/pair (arg SPL?) (id1 identifier?) (id2 identifier?) (body SPL?)])
;; interp. expressions in a language that supports pairs and matching
;; functions. Its syntax is defined by the following BNF:
;; <SPL> ::= <num>
;;           | {+ <SPL> <SPL>}
;;           | <id>
;;           | {pair <SPL> <SPL>}
;;           | {match/pair <SPL> {{pair <id> <id>} <SPL>}}}
;;           | {with {<id> <SPL>} <SPL>}
;; INVARIANT: <id> must not be an SPL keyword
;; INVARIANT: the two identifiers bound in match/pair must be different
;;             (e.g. {match/pair {pair 1 2} {{pair y y} 7}} is illegal) 

(define SPL1 (num 0))
(define SPL2 (add (num 7) (num 5)))
(define SPL3 (match/pair (pair (num 7) (num 5))
                         'x 'y (add (id 'x) (id 'y))))
(define SPL4 (match/pair (pair (num 7) (num 5))
                         'x 'y (pair (id 'y) (id 'x))))
(define SPL5 (match/pair (pair (num 7) (pair (num 5) (num 3)))
                         'x 'a (match/pair (id 'a)
                                           'y 'z (pair (add (id 'x) (id 'z))
                                                       (add (id 'y) (id 'x))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments:

;; Env is Symbol -> Value
;; interp.  bindings of identifiers to values

;; empty-env : Env
;; Effect: signals an error if symbol is not there

;; extend-env : Env Symbol Value -> Env
;; extend the given environment with a new binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(define-type Value
;  [numV (n number?)]
;  [pairV (fst Value?) (snd Value?)])
;; interp.  an SPL runtime Value
#;
(define (fn-for-value v)
  (type-case Value v
    [numV (n) (... n)]
    [pairV (fst snd) (... (fn-for-value fst)
                          (fn-for-value snd))]))


;; Problem 1a:
;; Complete the design of an environment-passing interpreter for SPL by
;; implementing the interp-match/pair function.

;; NOTE: Examples are not required, but if you write some,
;; you must put them after line XXX or PLAI will likely give errors.

;; Value Identifier Identifier SPL Env -> Value
;; produce the result of interpreting the corresponding match/pair expression
;; Effect: signals an exception in case of runtime error 
#;
(define (interp-match/pair v x1 x2 body env)
  (... v x1 x2 body))

;; Solution
(define (interp-match/pair v x1 x2 body env)
  (type-case Value v
    [numV (n) (error 'interp-match/pair "Bad pair: ~a" v)]
    [pairV (v1 v2)
           (interp/spl--env body (extend-env (extend-env env x1 v1) x2 v2))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Problem 1b:
;; Since the SPL datatype does not have a "with" construct, we must desugar it
;; into the other language features.  Complete the design of the with
;; desugaring function below to complete the implementation of SPL.

;; Examples are not required, but are helpful. If you choose to write some,
;; consider using gensym? and test/match (see line 20)

;; Identifier SPL SPL -> SPL
;; produce an SPL datatype expression equivalent to a with expression
#;
(define (with x named body)
  (... x named body))

;; Solution
(define (with x named body)
  (match/pair (pair named (num 0)) x (gensym) body))

;; Two potential examples
#;
(test/match (with 'x (num 7) (id 'x)) ... ... ...)

;; SOLUTION (optional)
(test/match (with 'x (num 7) (id 'x))
            (match/pair (pair (num 7) (num 0)) 'x y (id 'x))
            #:when (gensym? y))

#;
(test/match (with 'x (add (num 2) (num 2))
                  (with 'y (id 'x) (pair (id 'y) (id 'y))))
            ... ... ...)

;; SOLUTION (optional
(test/match (with 'x (add (num 2) (num 2))
                  (with 'y (id 'x) (pair (id 'y) (id 'y))))
            (match/pair (pair (add (num 2) (num 2)) (num 0))
                        'x a (match/pair (pair (id 'x) (num 0))
                                         'y b (pair (id 'y) (id 'y))))
            #:when (andmap gensym? (list a b)))

;; END OF PROBLEM 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following is scaffolding to hook interp-match/pair into the interpreter
;; implementation located in p1-appendix.rkt
;;


;; SPL Env -> Value
;; produce the value of interpreting the SPL expression in the given environment
;; Effect: signals an exception in case of runtime error 
(define (interp/spl--env spl env)
  ((make-interp/spl--env interp-match/pair) spl env))

;; SPL -> Value
;; produce the value of interpreting the given SPL expression
;; Effect: signals an exception in case of runtime error 
(define (interp/spl spl0)
  (interp/spl--env spl0 empty-env))

(test (interp/spl SPL1) (numV 0))
(test (interp/spl SPL2) (numV 12))
(test (interp/spl SPL3) (numV 12))
(test (interp/spl SPL4) (pairV (numV 5) (numV 7)))
(test (interp/spl SPL5) (pairV (numV 10) (numV 12)))


;; IF YOU WANT EXAMPLES FOR interp-match/pair, PUT THEM HERE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; The following is scaffolding to hook with into the parser
;; implementation located in p1-appendix.rkt
;;

;; S-expression -> SPL
;; parse the given s-expression into an SPL expression
;; EFFECT: signals an error if the given s-expression does not represent an SPL
(define (parse/spl sexp)
  ((make-parse/spl with) sexp))

(define WAES4 '{with {x {+ 5 5}}
                     {with {y {+ x 3}}
                           {+ y y}}})
(test (interp/spl (parse/spl WAES4)) (numV 26))

;; INTERP EXAMPLES FOR WITH EXPRESSIONS

(define WAES5 '{with {x 5}
                     {+ x {with {x 3}
                                10}}})
(test (interp/spl (parse/spl WAES5)) (numV 15))

(define WAES6 '{with {x 5}
                     {+ x {with {x 3}
                                x}}})
(test (interp/spl (parse/spl WAES6)) (numV 8))

(define WAES7 '{with {x 5}
                     {+ x
                        {with {y 3}
                              x}}})
(test (interp/spl (parse/spl WAES7)) (numV 10))
