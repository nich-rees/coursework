#lang racket

;;
;; cps.rkt - AL interpreter in continuation-passing style.
;;

(provide (all-defined-out))

(require plai/datatype)
(require (for-syntax "datatypes.rkt"))
(require "datatypes.rkt")


;; expr env (value -> value) -> value
(define (eval-expression ex env k)
  (type-case expr ex
    [number-expr (n) (k (number-value n))]
    [boolean-expr (b) (k (boolean-value b))]
    [var-expr (x) (k (lookup-env env x))]
    [if-expr (p c a)
             (eval-expression
              p env
              (λ (pv)
                (if (not-false? pv)
                    (eval-expression c env k)
                    (eval-expression a env k))))]
    [mult-expr (e1 e2)
               (eval-expression
                e1 env
                (λ (v1)
                  (eval-expression
                   e2 env
                   (λ (v2)
                     (k (number-value
                         (* (to-number v1)
                            (to-number v2))))))))]
    [sub1-expr (e)
               (eval-expression
                e env
                (λ (v)
                  (k (number-value (sub1 (to-number v))))))]
    [zero-expr (e)
               (eval-expression
                e env
                (λ (v)
                  (k (zero-value? v))))]
    [letcc-expr (x e)
                (eval-expression e (extend-env x (cont-value k) env) k)]
    [throw-expr (e1 e2)
                (eval-expression
                 e1 env
                 (λ (v1)
                   (eval-expression
                    e2 env
                    (λ (v2)
                      ((to-cont v1) v2)))))]
    [lambda-expr (x e)
                 (k (procedure-value x e env))]
    [box-expr (e)
              (eval-expression
               e env
               (λ (v)
                 (k (box-value (box v)))))]
    [unbox-expr (e)
                (eval-expression
                 e env
                 (λ (v)
                   (k (unbox (to-box v)))))]
    [set-box-expr (e1 e2)
                  (eval-expression
                   e1 env
                   (λ (v1)
                     (eval-expression
                      e2 env
                      (λ (v2)
                        (let ([old (unbox (to-box v1))])
                          (begin
                            (set-box! (to-box v1) v2)
                            (k old)))))))]
    [begin-expr (e1 e2)
                (eval-expression
                 e1 env
                 (λ (v1) 
                   (eval-expression e2 env k)))]
    [app-expr (e1 e2)
              (eval-expression
               e1 env
               (λ (v1)
                 (let-values ([(x e penv) (to-procedure v1)])
                   (eval-expression
                    e2 env
                    (λ (v2)
                      (eval-expression e (extend-env x v2 penv) k))))))]))

;; parse and evaluate a program
(define (ee e)
  (eval-expression (parse-expr e) empty-env identity))

