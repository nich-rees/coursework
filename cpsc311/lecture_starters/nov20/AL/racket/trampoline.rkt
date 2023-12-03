#lang racket

;;
;; trampoline.rkt - interpreter in trampoline style:
;;   restricted use of Racket call stack.
;;

(provide (all-defined-out))

(require plai/datatype)
(require (for-syntax "datatypes.rkt"))
(require "datatypes.rkt")


(define-type trampoline
  [bounce [p procedure?]]
  [dismount [v value?]])

;; NOT USED HERE: provided for comparison to Java
(define (done? t)
  (type-case trampoline t
    [bounce (p) #f]
    [dismount (v) #t]))


;; expr env (value -> value) -> value
(define (eval-expression ex env k)
  (type-case expr ex
    [number-expr (n)
                 (bounce
                  (λ ()
                    (k (number-value n))))]
    [boolean-expr (b)
                  (bounce
                   (λ ()
                     (k (boolean-value b))))]
    [var-expr (x) (bounce (λ () (k (lookup-env env x))))]
    [if-expr (p c a)
             (bounce
              (λ () 
                (eval-expression
                 p env
                 (λ (pv)
                   (if (not-false? pv)
                       (eval-expression c env k)
                       (eval-expression a env k))))))]
    [mult-expr (e1 e2)
               (bounce
                (λ () 
                  (eval-expression
                   e1 env
                   (λ (v1)
                     (eval-expression
                      e2 env
                      (λ (v2)
                        (k (number-value
                            (* (to-number v1)
                               (to-number v2))))))))))]
    [sub1-expr (e)
               (bounce
                (λ () 
                  (eval-expression
                   e env
                   (λ (v)
                     (k (number-value (sub1 (to-number v))))))))]
    [zero-expr (e)
               (bounce
                (λ () 
                  (eval-expression
                   e env
                   (λ (v)
                     (k (zero-value? v))))))]
    [letcc-expr (x e)
                (bounce 
                 (λ () 
                   (eval-expression e (extend-env x (cont-value k) env) k)))]
    [throw-expr (e1 e2)
                (bounce
                 (λ () 
                   (eval-expression
                    e1 env
                    (λ (v1)
                      (eval-expression
                       e2 env
                       (λ (v2)
                         ((to-cont v1) v2)))))))]
    [lambda-expr (x e)
                 (bounce
                  (λ () 
                    (k (procedure-value x e env))))]
    [box-expr (e)
              (bounce
               (λ () 
                 (eval-expression
                  e env
                  (λ (v)
                    (k (box-value (box v)))))))]
    [unbox-expr (e)
                (bounce
                 (λ () 
                   (eval-expression
                    e env
                    (λ (v)
                      (k (unbox (to-box v)))))))]
    [set-box-expr (e1 e2)
                  (bounce
                   (λ () 
                     (eval-expression
                      e1 env
                      (λ (v1)
                        (eval-expression
                         e2 env
                         (λ (v2)
                           (let ([old (unbox (to-box v1))])
                             (begin
                               (set-box! (to-box v1) v2)
                               (k old)))))))))]
    [begin-expr (e1 e2)
                (bounce
                 (λ () 
                   (eval-expression
                    e1 env
                    (λ (v1) 
                      (eval-expression e2 env k)))))]
    [app-expr
     (e1 e2)
     (bounce
      (λ () 
        (eval-expression
         e1 env
         (λ (v1)
           (let-values ([(x e penv) (to-procedure v1)])
             (eval-expression
              e2 env
              (λ (v2)
                (eval-expression e (extend-env x v2 penv) k))))))))]))


(define (loop t)
  (type-case trampoline t
    [bounce (p) (loop (p))]
    [dismount (v) v]))

(define (initial-k v)
  (dismount v))

;; parse and evaluate a program
(define (ee e)
  (loop
   (eval-expression (parse-expr e) empty-env initial-k)))

