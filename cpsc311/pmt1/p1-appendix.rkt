#lang plai
(define (... . args) (cons '... args))
(print-only-errors)

;; p1-appendix.rkt - Helpers for Problem 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(define-syntax test/match
  (syntax-rules ()
    [(_ expr pat whens ...)
     (test (match expr
             [pat whens ... #t]
             [else #f #;(error 'test/match
                               "failed:\nexpected pattern: ~a\ngot ~a"
                               'pat expr)])
           #t)]))

;; S-expression -> Boolean
;; produce true if the given value is a gensym, otherwise false.
(define (gensym? s)
  (and (symbol? s)
       (not (symbol-interned? s))
       (not (symbol-unreadable? s))))

(test (gensym? 's) #f)
(test (gensym? (gensym)) #t)
(test (gensym? (string->unreadable-symbol "hello")) #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Identifier is Symbol
;; interp. an SPL identifier
;; INVARIANT: id must not be an SPL keyword


;; Any -> Boolean
;; produce true if the given object is an SPL identifier, otherwise false
(define (identifier? x)
  (and (symbol? x)
       (not (member x '(add sub match/val with)))))

(define-type SPL    
  [num (n number?)] 
  [add  (lhs SPL?) (rhs SPL?)]
  [pair (fst SPL?) (snd SPL?)]
  [id (name identifier?)]
  [match/pair (arg SPL?) (id1 identifier?) (id2 identifier?) (body SPL?)])
;; interp. expressions in a language that supports pairs and matching
;; functions. Its syntax is defined by the following BNF:
;; <SPL> ::= <num>
;;           | {+ <SPL> <SPL>}
;;           | <id>
;;           | {pair <SPL> <SPL>}
;;           | {match/pair <SPL> {{pair <id> <id>} <SPL>}}}
;;           | {with {<id> <SPL>} <SPL>}
;; INVARIANT: <id> must not be an SPL keyword
;; INVARIANT:  the two identifiers bound in match/pair must be different
;;             (e.g. {match/pair {pair 1 2} {{pair y y} 7}} is illegal) 
(define SPL1 (num 0))
(define SPL2 (add (num 7) (num 5)))
(define SPL3 (match/pair (pair (num 7) (num 5))
                         'x 'y (add (id 'x) (id 'y))))
(define SPL4 (match/pair (pair (num 7) (num 5))
                         'x 'y (pair (id 'y) (id 'x))))
(define SPL5 (match/pair (pair (num 7) (pair (num 5) (num 3)))
                         'x 'y (match/pair (id 'y)
                                           'y 'z (pair (add (id 'x) (id 'z))
                                                       (add (id 'y) (id 'x))))))

#;
(define (fn-for-spl spl)
  (type-case SPL spl 
    [num (n) (... n)]
    [add (lhs rhs) (... (fn-for-spl lhs) (fn-for-spl rhs))]
    [pair (lhs rhs) (... (fn-for-spl lhs) (fn-for-spl rhs))]
    [id (name) (... name)]
    [match/pair (arg id1 id2 body)  (... (fn-for-spl arg)
                                         id1 id2 (fn-for-spl body))]))

;;
;; Environments
;;

;; Env is Symbol -> Value
;; interp.  bindings of identifiers to objects of type X

;; empty-env : Env
;; Effect: signals an error if symbol is not there
(define empty-env
  (λ (x) (error 'lookup-env "Unbound symbol: ~a" x)))

;; extend-env : Env Symbol Value -> Env
(define (extend-env env x0 v)
  (λ (x)
    (if (symbol=? x x0)
        v
        (env x))))

(define (lookup-env env x) (env x))


(define-type Value
  [numV (n number?)]
  [pairV (fst Value?) (snd Value?)])
;; interp.  an SPL runtime Value
#;
(define (fn-for-value v)
  (type-case Value v
    [numV (n) (... n)]
    [pairV (fst snd) (... (fn-for-value fst)
                          (fn-for-value snd))]))

;; Value Value -> Value
(define (add-value v1 v2)
  (match (list v1 v2)
    [(list (numV n1) (numV n2)) (numV (+ n1 n2))]
    [else (error 'interp/spl "Bad addition arguments: ~a, ~a." v1 v2)]))

;; Symbol (envof EnvBinding) -> EnvValue
;; produce the value corresponding to x in env
;; Effect: signal an error if x is unbound in env
(define (interp-id x env)
  (with-handlers ([exn:fail?
                   (λ (e)
                     (error 'interp/ffwae-env "Unbound identifier: ~a" x))])
    (lookup-env env x)))



;; SPL -> Value
;; produce the result of interpreting SPL0
;; Effect: signals an exception in case of runtime error
(define (make-interp/spl--env interp-match/pair)
  ;; Accumulator: env is Env
  ;; Invariant: env represents the bindings (in inside-out order)
  ;;            of identifiers to values due to deferred substitutions
  (local [(define (interp/spl--env spl env)
            (type-case SPL spl 
              [num (n) (numV n)]
              [add (lhs rhs) (add-value (interp/spl--env lhs env)
                                        (interp/spl--env rhs env))]
              [pair (lhs rhs) (pairV (interp/spl--env lhs env)
                                     (interp/spl--env rhs env))]
              [id (name) (interp-id name env)]
              [match/pair (arg id1 id2 body)
                          (interp-match/pair (interp/spl--env arg env)
                                             id1 id2 body env)]))]
    interp/spl--env))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ffwaefs is one of:
;; - number
;; - `{+ ,ffwaefs ,ffwaefs}
;; - `{pair ,splfs ,splfs}
;; - `{match/pair ,splfs {{pair ,id ,id} ,splfs}}}
;; - `{with {,id ,splfs} ,splfs}
;; -  identifier
;; - <any other s-expression>
;; where id is any symbol except '+, '-, 'with, 'if0, 'fun,
;; 'fixFun, or 'rec
;; interp.  any s-expression, but with a focus on those that represent
;; FFWAE expressions.

#;
(define (fn-for-splfs splfs)
  (match splfs
    [`,n #:when (number? n) (... n)]
    [`{+ ,lhs ,rhs} (... (fn-for-splfs lhs) (fn-for-splfs rhs))]
    [`,x #:when (identifier? x) (... x)]
    [`{pair ,lhs ,rhs} (... (fn-for-splfs lhs) (fn-for-splfs rhs))]
    [`{match/pair ,spl {{pair ,x1 ,x2} ,body}}
     #:when (and (andmap identifier? (list x1 x2))
                 (not (symbol=? x1 x2)))
     (... spl x1 x2 (fn-for-splfs body))]
    [`{with {,id ,named-exp} ,body}
     #:when (identifier? id)
     (... id (fn-for-splfs named-exp) (fn-for-splfs body))]
    [_ (... splfs)]))



;; (Identifier SPL SPL -> SPL) -> S-expression -> FFWAE
;; parse the given s-expression into a FFWAE expression
;; EFFECT: signals an error on failure
(define (make-parse/spl with)
  (local [(define (parse/spl spl)
            (match spl
              [`,n #:when (number? n) (num n)]
              [`{+ ,lhs ,rhs} (add (parse/spl lhs) (parse/spl rhs))]
              [`,x #:when (identifier? x) (id x)]
              [`{pair ,lhs ,rhs} (pair (parse/spl lhs) (parse/spl rhs))]
              [`{match/pair ,spl
                            {{pair ,x1 ,x2} ,body}}
               #:when (andmap identifier? (list x1 x2))
               (match/pair spl x1 x2 (parse/spl body))]
              [`{with {,id ,named-exp} ,body}
               #:when (identifier? id)
               (with id (parse/spl named-exp)
                     (parse/spl body))]
              [_ (error 'parse/spl "Bad expression ~a" spl)]))]
    parse/spl))
