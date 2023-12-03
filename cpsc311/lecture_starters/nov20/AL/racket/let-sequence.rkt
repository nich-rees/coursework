#lang racket

;;
;; let-sequence.rkt - AL interpreter with explicitly sequenced eval-expressions.
;;    A first step toward continuation-passing style.
;;

(provide (all-defined-out))

(require plai/datatype)
(require (for-syntax "datatypes.rkt"))
(require "datatypes.rkt")


;; expr env -> value
(define (eval-expression ex env)
  (type-case expr ex
    [number-expr (n) (number-value n)]
    [boolean-expr (b) (boolean-value b)]
    [var-expr (x) (lookup-env env x)]
    [if-expr (p c a)
             (let* ([pv (eval-expression p env)])                    
               (if (not-false? pv)
                   (eval-expression c env)
                   (eval-expression a env)))]
    [mult-expr (e1 e2)
               (let* ([v1 (eval-expression e1 env)]
                      [v2 (eval-expression e2 env)])
                 (number-value
                  (* (to-number v1)
                     (to-number v2))))]
    [sub1-expr (e)
               (let ([v (eval-expression e env)])
                 (number-value (sub1 (to-number v))))]
    [zero-expr (e)
               (let ([v (eval-expression e env)])
                 (zero-value? v))]
    [letcc-expr (x e)
                (call/cc
                 (λ (k)
                   (eval-expression e (extend-env x (cont-value k) env))))]
    [throw-expr (e1 e2)
                (let* ([v1 (eval-expression e1 env)]
                       [v2 (eval-expression e2 env)])
                  ((to-cont v1) v2))]
    [lambda-expr (x e)
                 (procedure-value x e env)]
    [box-expr (e)
              (let ([v (eval-expression e env)])
                (box-value (box v)))]
    [unbox-expr (e)
                (let ([v (eval-expression e env)])
                  (unbox (to-box v)))]
    [set-box-expr (e1 e2)
                  (let* ([v1 (eval-expression e1 env)]
                         [v2 (eval-expression e2 env)])
                    (let ([old (unbox (to-box v1))])
                      (begin
                        (set-box! (to-box v1) v2)
                        old)))]
    [begin-expr (e1 e2)
                (let ([v1 (eval-expression e1 env)])
                  (eval-expression e2 env))]
    [app-expr (e1 e2)
              (let ([v1 (eval-expression e1 env)])
                (let-values ([(x e penv) (to-procedure v1)])
                  (let ([v2 (eval-expression e2 env)])
                    (eval-expression e (extend-env x v2 penv)))))]))

;; parse and evaluate a program
(define (ee e)
  (eval-expression (parse-expr e) empty-env))

