#lang plai

(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates


;; Lil Dan -
;; A functional language with:
;; - numbers
;; - conditional expressions
;; - first-class continuations

;; Named in honor of Daniel P. Friedman
;; (https://en.wikipedia.org/wiki/Daniel_P._Friedman)

(define-type LD
  [num (n number?)]
  [if0 (predicate LD?) (consequent LD?) (alternative LD?)]
  [id (name symbol?)]
  [letcc (name symbol?) (body LD?)]
  [throwcc (cont LD?) (arg LD?)])
  
;; interp. expressions in a language that supports using first-class
;; continuations. Its syntax is defined by the following BNF:
;; <LD> ::=
;; (ARITHMETIC)
;;          <num>
;; (IDENTIFIERS)
;;        | <id>
;; (CONDITIONALS)
;;        | {if0 <LD> <LD> <LD>}
;; (CONTINUATIONS)
;;        | {throwcc <LD> <LD>}
;;        | {letcc {<id>} <LD>}

(define LD1 (num 1))
(define LD2 (if0 LD1 (num 2) (num 3)))
(define LD3 (letcc 'k (num 5)))
(define LD4 (if0 (letcc 'k (if0 (throwcc (id 'k) (num 0))
                                (throwcc (id 'k) (num 3))
                                (throwcc (id 'k) (num 5))))
                 (num 7)
                 (num 11)))

(define LD5 (if0 (if0 (letcc 'k
                        (if0 (throwcc (id 'k) (num 0))
                             (id 'unbound)
                             (id 'wow-also-unbound)))
                      (num 10)
                      (num 11))
                 (num 17)
                 (num 42)))

(define LD1e (id 'z))
(define LD2e (letcc 'k (throwcc (num 3) (num 5))))


#;(define (fn-for-ld e)
    (type-case LD e
      [num (n) (... n)]
      [if0 (predicate consequent alternative)
           (... (fn-for-ld predicate)
                (fn-for-ld consequent)
                (fn-for-ld alternative))]
      [id (name) (... name)]
      [letcc (k body) (... k (fn-for-ld body))]
      [throwcc (cont arg) (... (fn-for-ld cont)
                               (fn-for-ld arg))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Procedural Environments
;;

;; (envof Value) is symbol -> Value
;; interp. environment representation for lexically scoped identifiers
;; Effect: signals an error if given symbol has no binding

;; empty-env : (envof Value)
;; an empty environment for lexical scoping 
;; Effect: signals an error if variable is not there
(define empty-env
  (λ (x) (error 'lookup-env "Unbound identifier: ~a" x)))

;; (envof Value) symbol X -> (envof Value)
(define (extend-env env x0 v)
  (λ (x)
    (if (symbol=? x x0)
        v
        (env x))))

;; (envof Value) symbol -> Value
;; produce the binding associated with the given symbol
;; Effect: Signal an error if the given symbol has no binding
(define (lookup-env env x) (env x))

;;
;; Interpreter Values
;;

(define-type Value
  [numV (n number?)])

#;
(define (fn-for-value v)
  (type-case Value v
    [numV (n) (... n)]))

;; Value LD LD Env -> Value
;; interprets an if0 expression, given the predicate and both branches
(define (interp-if0 predv conseq alt env)
  (type-case Value predv
    [numV (n) (if (zero? n)
                  (interp/dan-acc conseq env)
                  (interp/dan-acc alt env))]))
  

;; LD Env -> Value
;; interprets the given LD expression, under the given environment
(define (interp/dan-acc e env)
        (type-case LD e
          [num (n) (numV n)]
          [if0 (predicate consequent alternative)
               (interp-if0 (interp/dan-acc predicate env)
                          consequent
                          alternative
                          env)]
          [id (name) (lookup-env env name)]
          [letcc (kid body)
            ;; somehow need access to surrounding context
            ;; and bind it to k in the environment
            (... kid (interp/dan-acc body env))]
          [throwcc (cont arg)
                   ;; somehow have to remove the current
                   ;; surrounding context, and substitute
                   ;; arg for the hole in cont
                   (... (interp/dan-acc cont env)
                        (interp/dan-acc arg env))]))
;; LD -> Value
;; interprets the given LD 
(define (interp/dan e)
  (interp/dan-acc e empty-env))

(test (interp/dan LD1) (numV 1))
(test (interp/dan LD2) (numV 3))
#;(test (interp/dan LD3) (numV 5))
#;(test (interp/dan LD4) (numV 7))
#;(test (interp/dan LD5) (numV 42))
(test/exn (interp/dan LD1e) "Unbound")
#;(test/exn (interp/dan LD2e) "Bad continuation")