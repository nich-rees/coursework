#lang racket

;;
;; datatypes.rkt - common data structures for the applicative language AL
;;

(require plai/datatype)
(provide (all-defined-out))


;;
;; Expressions
;;

;; Abstract Syntax

(define-type expr
  [number-expr [n number?]]
  [boolean-expr [b boolean?]]
  [var-expr [x symbol?]]
  [if-expr [p expr?] [c expr?] [a expr?]]
  [mult-expr [e1 expr?] [e2 expr?]]
  [sub1-expr [e expr?]]
  [zero-expr [e expr?]]
  [letcc-expr [x symbol?] [e expr?]]
  [throw-expr [e1 expr?] [e2 expr?]]
  [lambda-expr [x symbol?] [e expr?]]
  [box-expr [e expr?]]
  [unbox-expr [e expr?]]
  [set-box-expr [e1 expr?] [e2 expr?]]
  [begin-expr [e1 expr?] [e2 expr?]]
  [app-expr [e1 expr?] [e2 expr?]])


;; Template
#;
(define (fn-for-expr ex)
  (type-case expr ex
    [number-expr (n) (number-value n)]
    [boolean-expr (b) #f]
    [var-expr (x) #f]
    [if-expr (p c a) #f]
    [mult-expr (e1 e2) #f]
    [sub1-expr (e) #f]
    [zero-expr (e) #f]
    [letcc-expr (x e) #f]
    [throw-expr (e1 e2) #f]
    [lambda-expr (x e) #f]
    [box-expr (e) #f]
    [unbox-expr (e) #f]
    [set-box-expr (e1 e2) #f]
    [begin-expr (e1 e2) #f]
    [app-expr (e1 e2) #f]))




;; Concrete Syntax
;; e ::= n 
;;     | b
;;     | x
;;     | (if e e e)
;;     | (* e e)
;;     | (sub1 e)
;;     | (zero? e)
;;     | (letcc x e)
;;     | (call/cc e) == (letcc x (e (lambda (v) (throw x v)))
;;     | (throw e e)
;;     | (lambda (x) e)
;;     | (box e)
;;     | (set-box! e e)
;;     | (unbox e)
;;     | (e e)


;; SExpr -> Expr
;; parse the given S-expression into a corresponding expr
(define (parse-expr e)
  (match e
    [`,n #:when (number? n) (number-expr n)]
    [`,b #:when (boolean? b) (boolean-expr b)]
    [`,x #:when (symbol? x) (var-expr x)]
    [`(if ,e1 ,e2 ,e3)
     (if-expr (parse-expr e1) (parse-expr e2) (parse-expr e3))]
    [`(* ,e1 ,e2) (mult-expr (parse-expr e1) (parse-expr e2))]
    [`(sub1 ,e) (sub1-expr (parse-expr e))]
    [`(zero? ,e) (zero-expr (parse-expr e))]
    [`(letcc ,x ,e) #:when (symbol? x) (letcc-expr x (parse-expr e))]
    [`(call/cc ,e)
     (let ([x (gensym)]
           [v (gensym)])
       (parse-expr
        `(letcc ,x (,e (lambda (,v) (throw ,x ,v))))))]
    [`(throw ,e1 ,e2) (throw-expr (parse-expr e1) (parse-expr e2))]
    [`(lambda (,x) ,e) #:when (symbol? x) (lambda-expr x (parse-expr e))]
    [`(box ,e) (box-expr (parse-expr e))]
    [`(unbox ,e) (unbox-expr (parse-expr e))]
    [`(set-box! ,e1 ,e2) (set-box-expr (parse-expr e1) (parse-expr e2))]
    [`(begin ,e1 ,e2) (begin-expr (parse-expr e1) (parse-expr e2))]
    [`(begin ,e1 . ,e*) (begin-expr (parse-expr e1)
                                   (parse-expr `(begin . ,e*)))]
    [`(let ([,x ,e1]) ,e2) (parse-expr `((lambda (,x) ,e2) ,e1))]
    [`(rec ,f (,x) ,e)
     (parse-expr
      `(let ([Y (lambda (f)
                  (let ([w (lambda (x) (f (lambda (a) ((x x) a))))])
                    (w w)))])
         (Y (lambda (,f) (lambda (,x) ,e)))))]
    [`(,e1 ,e2) (app-expr (parse-expr e1) (parse-expr e2))]
    [`,otherwise (error 'parse-expr "Bad Expression: ~a" otherwise)]))

;;
;; Values
;;

(define-type value
  [number-value [n number?]]
  [boolean-value [b boolean?]]
  [cont-value [k procedure?]]
  [procedure-value [x symbol?] [e expr?] [env list?]]
  [box-value [b box?]])

(define (to-number v)
  (type-case value v
    [number-value (n) n]
    [else (error 'to-number "Bad number: ~a" v)]))

(define (to-boolean v)
  (type-case value v
    [boolean-value (b) b]
    [else (error 'to-boolean "Bad boolean: ~a" v)]))

(define (to-cont v)
  (type-case value v
    [cont-value (k) k]
    [else (error 'to-cont "Bad cont: ~a" v)]))

(define (to-procedure v)
  (type-case value v
    [procedure-value (x e env) (values x e env)]
    [else (error 'to-procedure "Bad procedure: ~a" v)]))

(define (to-box v)
  (type-case value v
    [box-value (n) n]
    [else (error 'to-box "Bad box: ~a" v)]))

;;
;; Helpers
;;

;; value -> Boolean
;; is the given value a false boolean value?
(define (not-false? v)
  (type-case value v
    [boolean-value (b) b]
    [else #t]))


;; value -> boolean-value
;; is the given value a zero number value?  Produce an AL boolean answer
(define (zero-value? v)
  (type-case value v
    [number-value (n) (boolean-value (= n 0))]
    [else (boolean-value #f)]))


;;
;; Environments
;;
(define empty-env '())
(define (extend-env x v env) (cons (cons x v) env))
(define (lookup-env env x)
  (cond
    [(not (symbol? x)) (error 'lookup-env "Bad variable ~a" x)]
    [(assoc x env) => cdr]
    [else (error 'lookup-env "Unbound variable: ~a" x)]))


