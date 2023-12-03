#lang racket


;;
;; direct.rkt - a direct-style interpreter for the CPSC411 Applicative Language
;;   (AL)
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
             (let ([pv (eval-expression p env)])
               (if (not-false? pv)
                   (eval-expression c env)
                   (eval-expression a env)))]
    [mult-expr (e1 e2)
               (number-value
                (* (to-number (eval-expression e1 env))
                   (to-number (eval-expression e2 env))))]
    [sub1-expr (e)
               (number-value (sub1 (to-number (eval-expression e env))))]
    [zero-expr (e)
               (zero-value? (eval-expression e env))]
    [letcc-expr (x e)
                (call/cc
                 (λ (k)
                   (eval-expression e (extend-env x (cont-value k) env))))]
    [throw-expr (e1 e2)
                ((to-cont (eval-expression e1 env))
                 (eval-expression e2 env))]
    [lambda-expr (x e)
                 (procedure-value x e env)]
    [box-expr (e)
              (box-value (box (eval-expression e env)))]
    [unbox-expr (e)
                (unbox (to-box (eval-expression e env)))]
    [set-box-expr (e1 e2)
                  (let* ([v1 (eval-expression e1 env)]
                         [old (unbox (to-box v1))])
                    (begin
                      (set-box! (to-box v1)
                                (eval-expression e2 env))
                      old))]
    [begin-expr (e1 e2)
                (let ([v1 (eval-expression e1 env)])
                  (eval-expression e2 env))]
    [app-expr (e1 e2)
              (let-values ([(x e penv) (to-procedure (eval-expression e1 env))])
                (let ([v (eval-expression e2 env)])
                  (eval-expression e (extend-env x v penv))))]))

;; parse and evaluate a program
(define (ee e)
  (eval-expression (parse-expr e) empty-env))


