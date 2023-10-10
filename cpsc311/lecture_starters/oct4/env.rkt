#lang plai


;; Env is Symbol -> Number
;; interp a table for looking up identifier bindings

;; Env
(define empty-env
  (λ (x) (error 'lookup-env "Undefined identifier: ~a" x)))

;; Env Symbol Number -> Env
;; extend the given environment to bind x0 to v0
(define (extend-env env x0 v0)
  (λ (x)
    (if (symbol=? x x0)
        v0
        (env x))))

;; Env Symbol -> Number
;; look up x in env
(define (lookup-env env x)
  (env x))
