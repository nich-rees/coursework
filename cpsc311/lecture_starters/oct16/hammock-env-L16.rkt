#lang plai
(require "../util/parsing.rkt")
(require "../util/env.rkt")  ;; For environments

;; hammock-subst-L19-factored - Substitution-based Interpreter

(require "hammock-common.rkt") ;; Parser and language data types

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Environment-Passing Interpreter
(define environment? procedure?)

;;
;; Interpretation Functions
;;

;; In call-by-name, identifiers denote **expressions** not just values...

(define-type Denotable
  [thunkD (delayed HK?) (env procedure?)])
;; interp. an object that can be denoted by an identifier, so can
;;         appear in an environment:
;;  - thunkD represents an expression pending evalution, along with the
;;    environment in which it is to be evaluated

(define (fn-for-denotable d)
  (type-case Denotable d
    [thunkD (e env) (... (fn-for-hk e) env)]))

;;
;; Interpreter Values - now a refinement of HK!
;;

(define-type Value
  [numV (n number?)]
  [funV (param hkid?) (body HK?) (env environment?)]
  [pairV (e1 HK?) (e2 HK?) (env environment?)]
  [mtV])
;; interp.  Those HK values that count as runtime values

(define (fn-for-value v)
  (type-case Value v
    [numV  (n) (... n)]
    [funV (x e env) (... x (fn-for-hk e) env)]
    [pairV (e1 e2 env) (... (fn-for-hk e1)
                            (fn-for-hk e2)
                            env)]
    [mtV () (...)]))


;; Value -> Number
;; produce the number represented by the given value
;; Effect: signal an error if the value does not represent a number
(define (value->num v)
  (type-case Value v
    [numV (n) n]
    [else (error 'value->num "Bad number: ~a" v)]))

(test (value->num (numV 6)) 6)
(test (value->num (numV 9)) 9)
(test/exn (value->num (funV 'x (id 'x) empty-env)) "Bad number")


;; Value -> pairV
;; produce the same v if it is a pair
;; Effect: signal an error if the value does not represent a pair
(define (value->pairV v)
  (type-case Value v
    [pairV (l r env) v]
    [else (error 'value->num "Bad pair: ~a" v)]))

(test (value->pairV (pairV (num 0) (num 1) empty-env))
      (pairV (num 0) (num 1) empty-env))
(test/exn (value->pairV (numV 6)) "Bad pair")


;; (Number Number -> Number) Value Value  -> Value
;; apply num-op to the numbers represented by v1 and v2
;; Effect: signal an error if either argument does not represent a number
(define (num-binop-value num-op v1 v2)
  (let ([n1 (value->num v1)]
        [n2 (value->num v2)])
    (numV (num-op n1 n2))))

(test (num-binop-value * (numV 5) (numV 6)) (numV 30))
(test/exn (num-binop-value * (numV 5) (funV 'x (id 'x) empty-env))
          "Bad number")
(test/exn (num-binop-value * (funV 'x (id 'x) empty-env) (numV 6))
          "Bad number")
(test/exn (num-binop-value * (funV 'x (id 'x) empty-env)
                           (funV 'x (id 'x) empty-env)) "Bad number")


;; Value Value -> Value
;; produce the sum of two numbers
;; Effect: signal an error if either argument does not represent a number
(define (add-value v1 v2)
  (num-binop-value + v1 v2))

(test (add-value (numV 5) (numV 6)) (numV 11))
(test/exn (add-value (numV 5) (funV 'x (id 'x) empty-env)) "Bad number")
(test/exn (add-value (funV 'x (id 'x) empty-env) (numV 6)) "Bad number")
(test/exn (add-value (funV 'x (id 'x) empty-env)
                     (funV 'x (id 'x) empty-env)) "Bad number")


;; Value Value -> Value
;; produce the difference of two numbers
;; Effect: signal an error if either argument does not represent a number
(define (sub-value v1 v2)
  (num-binop-value - v1 v2))

(test (sub-value (numV 5) (numV 6)) (numV -1))
(test/exn (sub-value (numV 5) (funV 'x (id 'x) empty-env)) "Bad number")
(test/exn (sub-value (funV 'x (id 'x) empty-env) (num 6)) "Bad number")
(test/exn (sub-value (funV 'x (id 'x) empty-env)
                     (funV 'x (id 'x) empty-env)) "Bad number")


;; Value -> Boolean
;; produce true if v1 represents the number zero, else false
(define (zero-value? v)
  (type-case Value v
    [numV (n) (zero? n)]
    [else #f]))

(test (zero-value? (numV 7)) #f)
(test (zero-value? (numV 0)) #t)
(test (zero-value? (funV 'x (id 'x) empty-env)) #f)



;; Value HK Env-> Value
;; produce the result of applying v1 to e2 (where env2 goes with e2
;; Effect: signal an error if v1 does not represent a function
(define (apply-value v1 e2 env2)
  (type-case Value v1
    [funV (x body env)  ;; *** THE INTERESTING CASE! ***
          (interp/hk--env body (extend-env env x (thunkD e2 env2)))]
    [else (error 'apply-value "Bad function: ~a" v1)]))

;; apply-value EXAMPLES below interp/hk because of mutual reference

;; Value -> Value
;; produce the value of the left member of a pair
;; Effect: Signal an error if the given value is not a pair
;; Effect: Signal an error if evaluating the left member does so (***)
(define (left-value v)
  (match-let ([(pairV e1 e2 env) (value->pairV v)])
    (interp/hk--env e1 env)))

;; ***
;; left-value EXAMPLES below interp/hk because of mutual reference

;; Value -> Value
;; produce the value of the right member of a pair
;; Effect: Signal an error if the given value is not a pair
;; Effect: Signal an error if evaluating the right member does so (***)
(define (right-value v)
  (match-let ([(pairV e1 e2 env) (value->pairV v)])
    (interp/hk--env e2 env)))

;; ***
;; right-value EXAMPLES below interp/hk because of mutual reference


;; Value -> Value
;; produce a 0 if the value is a number, otherwise 1
(define (isnumz-value v)
  (if (numV? v) (numV 0) (numV 1)))

(test (isnumz-value (numV 0)) (numV 0))
(test (isnumz-value (funV 'x (id 'x) empty-env)) (numV 1))
(test (isnumz-value (pairV (num 0) (num 0) empty-env)) (numV 1))
(test (isnumz-value (mtV)) (numV 1))


;; Value -> Value
;; produce a 0 if the value is a function, otherwise 1
(define (isfunz-value v)
  (if (funV? v) (numV 0) (numV 1)))

(test (isfunz-value (numV 0)) (numV 1))
(test (isfunz-value (funV 'x (id 'x) empty-env)) (numV 0))
(test (isfunz-value (pairV (num 0) (num 0) empty-env)) (numV 1))
(test (isfunz-value (mtV)) (numV 1))


;; Value -> Value
;; produce a 0 if the value is a pair, otherwise 1
(define (ispairz-value v)
  (if (pairV? v) (numV 0) (numV 1)))

(test (ispairz-value (numV 0)) (numV 1))
(test (ispairz-value (funV 'x (id 'x) empty-env)) (numV 1))
(test (ispairz-value (pairV (num 0) (num 0) empty-env)) (numV 0))
(test (ispairz-value (mtV)) (numV 1))

;; Value -> Value
;; produce a 0 if the value is mt, otherwise 1
(define (ismtz-value v)
  (if (mtV? v) (numV 0) (numV 1)))

(test (ismtz-value (numV 0)) (numV 1))
(test (ismtz-value (funV 'x (id 'x) empty-env)) (numV 1))
(test (ismtz-value (pairV (num 0) (num 0) empty-env)) (numV 1))
(test (ismtz-value (mtV)) (numV 0))


;; Symbol HK Env -> Value
;; produce the result of interpreting the given fix expression.
;; Effect: Signal an exception in case of runtime error
(define (interp-fix f body env)
  ;; Since Hammock is nonstrict, fix is similar to FFWAE's fixFun!
  ;; So we exploit procedural environments...
  (letrec ([env^ (λ (x0)
                   (if (symbol=? x0 f)
                       (thunkD body env^)
                       (env x0)))])
    (interp/hk--env body env^)))

;; Symbol Env -> Value
;; produce the result of the expression bound to the given identifier.
;; Effect: Signal an exception in case of runtime error
(define (interp-id x env)
  (match-let ([(thunkD e env^)
               (with-handlers
                   ([exn:fail?
                     (λ (_)
                       (error 'interp/hk "Unbound identifier: ~a" x))])
                 (lookup-env env x))])
    (interp/hk--env e env^)))

;; HK Env -> Value
;; EFFECT: Signal an exception in case of runtime error
(define (interp/hk--env hk0 env)
  (local [(define (recur hk)
            (type-case HK hk
              [num (n) (numV n)]
              [isnumz (e) (isnumz-value (recur e))]
              [add (l r) (add-value (recur l) (recur r))]
              [sub (l r) (sub-value (recur l) (recur r))]
              [id (x) (interp-id x env)]
              [fun (x body) (funV x body env)]
              [isfunz (e) (isfunz-value (recur e))]
              [app (rator rand) (apply-value (recur rator) rand env)] ;; ***
              [if0 (p c a)
                   (if (zero-value? (recur p))
                       (recur c)
                       (recur a))]
              [fix (f body) (interp-fix f body env)]
              [pair (l r) (pairV l r env)] ;; ***
              [ispairz (e) (ispairz-value (recur e))]
              [left (e) (left-value (recur e))]
              [right (e) (right-value (recur e))]
              [mt () (mtV)]
              [ismtz (e) (ismtz-value (recur e))]))]
    (recur hk0)))


;; HK -> Value
;; produce the result of interpreting the given expression
;; EFFECT: Signal an exception in case of runtime error
(define (interp/hk hk)
  (interp/hk--env hk empty-env))

;; apply-value examples

(test (apply-value (funV 'x (add (id 'x) (num 7)) empty-env)
                   (num 12)
                   empty-env)
      (numV 19))


(test/exn (apply-value (numV 12)
                       (fun 'x (add (id 'x) (num 7)))
                       empty-env)
          "Bad function")

;; left-value examples
(test (left-value (pairV (add (num 5) (num -5)) (num 1) empty-env)) (numV 0))
(test (left-value (pairV (id 'x) (num 1)
                         (extend-env empty-env 'x (thunkD (num 2) empty-env))))
      (numV 2))
(test/exn (left-value (numV 6)) "Bad pair")

;; right-value examples
(test (right-value (pairV (num 0) (num 1) empty-env)) (numV 1))
(test (right-value (pairV (num 1) (id 'x)
                          (extend-env empty-env 'x (thunkD (num 2) empty-env))))
      (numV 2))
(test/exn (right-value (numV 6)) "Bad pair")


;; interp/hk examples

(test (interp/hk (ismtz (mt))) (numV 0))
(test (interp/hk (ismtz (num 0))) (numV 1))
(test (interp/hk (isnumz (num 0))) (numV 0))
(test (interp/hk (isnumz (mt))) (numV 1))
(test (interp/hk (ispairz (pair (mt) (mt)))) (numV 0))
(test (interp/hk (ispairz (num 0))) (numV 1))

(test (interp/hk (left (pair (mt) (num 7)))) (mtV))
(test (interp/hk (right (pair (mt) (num 7)))) (numV 7))


;;;;;;;;
;; Observing/printing final Hammock values 
(define-type DeepPair
  [pairDV (v1 DeepValue?) (v2 DeepValue?)])
;; DeepValue is one of:
;; - (numV number)
;; - (funV symbol HK Env)
;; - (pairDV DeepValue DeepValue) ;; ** force the non-strict pairs for viewing
;; - (mtV)
;; interp.  Hammock values that contain no pending computations
(define (DeepValue? x)
  (ormap (λ (p?) (p? x)) (list numV? funV? pairDV? mtV?)))


;; Value -> DeepValue
;; force all pending computations in the given value
(define (force-value v)
  (match v
    [(numV n) (numV n)]
    [(funV x e env) (funV x e env)]
    [(pairV e1 e2 env) (pairDV (force-value (interp/hk--env e1 env))
                               (force-value (interp/hk--env e2 env)))]
    [(mtV) (mtV)]))

(test (force-value (numV 7)) (numV 7))
(test (force-value (pairV (add (num 6) (num 6))
                          (if0 (num 0)
                               (mt)
                               (num 9))
                          (extend-env empty-env 'x (thunkD (num 2) empty-env))))
      (pairDV (numV 12) (mtV)))



;;
;; PUTTING IT ALL TOGETHER  - an interpreter of files on disk
;;

;; S-expression -> Value
;; produce the result of interpreting the HK program represented as an sexp.
;; EFFECT: signals an error if interpretation signals a runtime error.
(define (interp-sexp sexp)
  (interp/hk
   (parse-expr sexp)))

(test/exn (interp-sexp '{with {y {- 10 x}}
                              {with {x 7}
                                    y}})
          "Unbound")


;; String -> Value
;; produce the result of interpreting the HK program in input
;; EFFECT: signals an error if interpretation signals a runtime error.
(define (interp-string pgm)
  (interp/hk
   (parse-expr
    (read-from-string pgm))))


;; String -> Value
;; produce the result of interpreting the HK stored in the file fname
;; EFFECT: signals an error if no file fname contains an AE representation
;;         or if interpretation signals a runtime error.
(define (interp-file fname)
  (interp/hk
   (parse-expr
    (read-from-file fname))))

(test (with-temporary-data-file "{+ 3 7}\n"
        (λ (fname) (interp-file fname)))
      (numV 10))

(test (with-temporary-data-file "{+ {- 3 4} 7}\n"
        (λ (fname) (interp-file fname)))
      (numV 6))

(test (with-temporary-data-file "{with {x 5} {+ x {with {y 3} x}}}"
        (λ (fname) (interp-file fname)))
      (numV 10))


(test (with-temporary-data-file
          "{with {double {fun {x} {+ x x}}} {with {x 5} {double x}}}"
        (λ (fname) (interp-file fname)))
      (numV 10))


(test (with-temporary-data-file
          "{with {* {fixFun mult {lhs} {fun {rhs}
                                {if0 rhs
                                     0
                                     {+ lhs {{mult lhs} {- rhs 1}}}}}}}
             {{* 20} 3}}"
        (λ (fname) (interp-file fname)))
      (numV 60))

(test (with-temporary-data-file
          "{with {* {fix mult {fun {lhs} {fun {rhs}
                                {if0 rhs
                                     0
                                     {+ lhs {{mult lhs} {- rhs 1}}}}}}}}
             {{* 20} 3}}"
        (λ (fname) (interp-file fname)))
      (numV 60))


(define DOUBLE-LIST
  "{with {dbl
          {fixFun recur {l} {if0 {ismtz l}
                               {mt}
                               {pair {+ {left l} {left l}}
                                     {recur {right l}}}}}}
    {dbl {pair 5 {pair 4 {pair 3 {pair 2 {pair 1 {mt}}}}}}}}
")

(test (with-temporary-data-file
          DOUBLE-LIST
        (λ (fname)
          (force-value ;; ***
           (interp-file fname))))
      (pairDV (numV 10)
              (pairDV (numV 8)
                      (pairDV (numV 6)
                              (pairDV (numV 4)
                                      (pairDV (numV 2)
                                              (mtV)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some example code, mostly based on Chapter 7 of the textbook
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; take
(define TAKE
  '{fixFun take {n}
           {fun {ls}
                {if0 n
                     {mt}
                     {if0 {ismtz ls}
                          {mt}
                          {pair {left ls}
                                {{take {- n 1}} {right ls}}}}}}})

(test
 (force-value
  (interp-sexp
   `{with {take ,TAKE}
          {{take 3} {pair 4 {pair 5 {pair 6 {pair 7 {mt}}}}}}}))
 (pairDV (numV 4) (pairDV (numV 5) (pairDV (numV 6) (mtV)))))

;; ones
(define ONES
  '{fix f {pair 1 f}})

;; map
(define MAP
  '{fixFun map {fn}
           {fun {ls}
                {if0 {ismtz ls}
                     {mt}
                     {pair {fn {left ls}}
                           {{map fn} {right ls}}}}}})

;; add1
(define ADD1
  '{fun {n} {+ n 1}})

;; nats
(define NATS
  `{with {map ,MAP}
         {with {add1 ,ADD1}
               {fix f {pair 1 {{map add1} f}}}}})

(define FIRST-FIVE
  `{with {take ,TAKE}
         {{take 5} ,NATS}})

(test (force-value (interp-sexp `{left {right ,NATS}}))
      (numV 2))

(test (force-value (interp-sexp FIRST-FIVE))
      (pairDV (numV 1)
              (pairDV (numV 2)
                      (pairDV (numV 3)
                              (pairDV (numV 4)
                                      (pairDV (numV 5)
                                              (mtV)))))))

;; "cyclic" list generator 
(define CYCLE
  `{fun {ls0}
        {with {cycle
               {fixFun cycle {ls}
                       {if0 {ismtz ls}
                            {cycle ls0}
                            {pair {left ls}
                                  {cycle {right ls}}}}}}
              {cycle ls0}}})

(test (force-value (interp-sexp `{{,TAKE 5}
                                  {,CYCLE {pair 3 {pair 2 {pair 1 {mt}}}}}}))
      (pairDV (numV 3)
              (pairDV (numV 2)
                      (pairDV (numV 1)
                              (pairDV (numV 3)
                                      (pairDV (numV 2)
                                              (mtV)))))))

;; zipOp
(define ZIPOP
  '{fun {f}
        {fix recur
             {fun {ls1}
                  {fun {ls2}
                       {if0 {ismtz ls1}
                            {mt}
                            {if0 {ismtz ls2}
                                 {mt}
                                 {pair {{f {left ls1}} {left ls2}}
                                       {{recur {right ls1}} {right ls2}}}}}}}}})

(test (force-value (interp-sexp `{{{,ZIPOP {fun {a} {fun {b} {pair a b}}}}
                                   {pair 1 {pair 2 {pair 3 {mt}}}}}
                                  {pair 3 {pair 2 {pair 1 {mt}}}}}))
      (pairDV (pairDV (numV 1) (numV 3))
              (pairDV (pairDV (numV 2) (numV 2))
                      (pairDV (pairDV (numV 3) (numV 1)) (mtV)))))

;; fibs
(define FIBS
  `{fix recur
        {pair 1
              {pair 1
                    {{{,ZIPOP {fun {a} {fun {b} {+ a b}}}}
                      recur}
                     {right recur}}}}})

(test (force-value (interp-sexp `{{,TAKE 12} ,FIBS}))
      (local [(define (for-cons n rlon) (pairDV (numV n) rlon))
              (define for-empty (mtV))]
        (foldr for-cons for-empty (list 1 1 2 3 5 8 13 21 34 55 89 144))))
