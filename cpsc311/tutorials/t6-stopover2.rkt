#lang plai

(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tutorial:  BackStop, Won't Stop

;; BackStop is a Variant of FWAE with a slight difference:
;;   sometimes free identifiers don't cause a runtime error.

;; Normally, a free identifier causes an error

#;
'x

;; evaluates to Error: Unbound identifier: 'x

;; But we can solve that problem by adding a back-stop:

#;
'{backstop 7
           'x}
;; evaluates to 7

;; A back-stop expression adds just a little bit of dynamic scoping to the
;; language so that a value can be (dynamically) bound to any free identifiers.

;; It's evaluation rule is:
;; 1) evaluate the first argument
;; 2) evaluate the second argument
;; 3) produce the value of the second argument (assuming no errors occurred)

;; During the evaluation of the second argument, if ever a free identifier is
;; encountered, then produce the value of the inner-most back-stop value.
;; If there is no back-stop, then signal an Unbound identifer error.

;; Here are a few examples:
#;
'{backstop 7
           {+ x y}}

;; evaluates to 14, because both x and y are free.  However,
#;
'{backstop 7
           {+ x {backstop 8 y}}}
;; evaluates to 15 because x is backstopped by 7, and y is backstopped by 8.
;; Both
#;
'{with {x 8}
       {backstop 7
                 {+ x y}}}
;; and
#;
'{backstop 7
           {with {x 8}
                 {+ x y}}}
          
;; evaluate to 15 because x is bound and y is free, so only y gets backstopped.

;; Finally
#;
'{backstop {fun {g} 7}
           {with {x 8}
                 {with {f {backstop 9
                                    {fun {y} {z y}}}}
                       {f 10}}}}

;; evaluates to 7.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; TUTORIAL:
;; Your mission is to change the present back-stop expression, which adds
;; a little bit of **dynamic scoping**, into one that adds just a little bit of
;; **state**, so that now value can be (dynamically) **assigned** to any free
;; identifiers.
;;



;;
;; DataTypes
;;

;; BSID is Symbol
;; INVARIANT: a BSID cannot be equal to any Backstop keyword
(define (bsid? x)
  (local [(define KEYWORDS
            '(+ - with fun backstop))]
    (and (symbol? x)
         (not (member x KEYWORDS)))))

(define BSID0 'a)
(define BSID1 'b)


(define-type BS    
  [num (n number?)] 
  [add (lhs BS?) (rhs BS?)]
  [sub (lhs BS?) (rhs BS?)]
  [id (name symbol?)]
  [fun (param symbol?) (body BS?)]
  [app (rator BS?) (arg BS?)]
  [backstop (bs BS?) (body BS?)])
;; interp. expressions in a language that supports applying first-class
;; functions. Its syntax is defined by the following BNF:
;; <BS> ::= <num>
;;           | {+ <BS> <BS>}
;;           | {- <BS> <BS>}
;;           | {with {<id> <BS>} <BS>}
;;           | <id>
;;           | {fun {<id>} <BS>}
;;           | {<BS> <BS>}
;;           | {backstop <BS> <BS>}
;; where
;; {with {x e1} e2} ≡ {{fun {x} e2} e1}

;; BSID BS BS -> BS
;; desugar a with expression into an equivalent BS expression 
(define (with name named-expr body)
  (app (fun name body) named-expr))

;; Examples stolen from above.  Add more as needed.
(define BS1 (id 'x))

(define BS2 (backstop (num 7)
                      (id 'x)))

(define BS3
  (backstop (num 7)
            (add (id 'x) (id 'y))))

(define BS4
  (backstop (num 7)
            (add (id 'x) (backstop (num 8) (id 'y)))))

(define BS5
  (with 'x (num 8)
        (backstop (num 7)
                  (add (id 'x) (id 'y)))))


(define BS6
  (backstop (num 7)
            (with 'x (num 8)
                  (add (id 'x) (id 'y)))))
          
(define BS7
  (backstop (fun 'g (num 7))
            (with 'x (num 8)
                  (with 'f (backstop (num 9)
                                     (fun 'y (app (id 'z) (id 'y))))
                        (app (id 'f) (num 10))))))


;; Template intentionally elided

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Procedural Environments
;;


;; (envof X) is Symbol -> X
;; interp.  bindings of identifiers to objects of type X
;; Effect: signals an error if variable is not there


;; Any -> Boolean
;; produce true if the given object *might* be an environment, else false
(define (env? x)
  (procedure? x))


;; (envof X)
;; the empty environment
(define empty-env
  (λ (x) (error 'lookup-env "Undefined identifier: ~a" x)))


;; (envof X) (listof Symbol) (listof X) -> (envof X)
;; produce an environment that binds distinct symbols in x* to objects in v*
;; ASSUME: (= (length x*) (length v*))
;; ASSUME: (unique? x*)
(define (extend-env* env x* v*)
  (let ([alist (map cons x* v*)]) ;; make an association list
    (λ (x)
      (cond
        [(assoc x alist) => cdr] ;; => is a clever cond feature
        [else (env x)]))))


;; (envof X) Symbol X -> (envof X)
;; produce an environment that extends env to bind x to v
(define (extend-env env x v)
  (extend-env* env (list x) (list v)))


;; (envof X) Symbol -> X
;; produce the binding for symbol x from env
;; Effect: signals an error if no binding is found
(define (lookup-env env x)
  (env x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-type Value
  [numV (n number?)]
  [funV (x bsid?) (body BS?) (env env?)])
;; interp.  value resulting from evaluating a Backstop expression.

;; Value -> Number
;; produce the number represented by the given value
;; Effect: signal an error if the value does not represent a number
(define (value->num v)
  (type-case Value v
    [numV (n) n]
    [funV (x body env) (error 'evalue->num "Bad number: ~a" v)]))

(test (value->num (numV 6)) 6)
(test (value->num (numV 9)) 9)
(test/exn (value->num (funV 'x (id 'x) empty-env)) "Bad number")


;; Value (Symbol BS Env -> X) -> X
;; extract the fields of ev as a funV and apply them to fn
;; Effect: signal an error if ev does not represent a funv
(define (with-value-as-funv ev fn)
  (type-case Value ev
    [funV (x body env) (fn x body env)]
    [numV (n) (error 'evalue->num "Bad function: ~a" ev)]))

(test (with-value-as-funv (funV 'x (id 'y) empty-env)
        (λ (x body env) (funV x body env)))
      (funV 'x (id 'y) empty-env)) 
(test/exn (with-value-as-funv (numV 9)
            (λ (x body env) (funV x body env)))
          "Bad")


;; MaybeValue is one of
;; - Value
;; - false
;; interp.  Possible presence of a value
(define MV1 #f)
(define MV2 (numV 7))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helpers
;;
  
;; (Number Number -> Number) Value Value  -> Value
;; apply num-op to the numbers represented by v1 and v2
;; Effect: signal an error if either argument does not represent a number
(define (op/bs num-op v1 v2)
  (let ([n1 (value->num v1)]
        [n2 (value->num v2)])
    (numV (num-op n1 n2))))

(test (op/bs * (numV 5) (numV 6)) (numV 30))
(test/exn (op/bs * (numV 5) (funV 'x (id 'x) empty-env)) "Bad number")
(test/exn (op/bs * (funV 'x (id 'x) empty-env) (numV 6)) "Bad number")
(test/exn (op/bs * (funV 'x (id 'x) empty-env)
                 (funV 'x (id 'x) empty-env)) "Bad number")

  
;; Value Value -> Value
;; produce the sum of two numbers
;; Effect: signal an error if either argument does not represent a number
(define (add/bs v1 v2)
  (op/bs + v1 v2))

(test (add/bs (numV 5) (numV 6)) (numV 11))
(test/exn (add/bs (numV 5) (funV 'x (id 'x) empty-env)) "Bad number")
(test/exn (add/bs (funV 'x (id 'x) empty-env) (numV 6)) "Bad number")
(test/exn (add/bs (funV 'x (id 'x) empty-env)
                  (funV 'x (id 'x) empty-env)) "Bad number")


;; Value Value -> Value
;; produce the difference of two numbers
;; Effect: signal an error if either argument does not represent a number
(define (sub/bs v1 v2)
  (op/bs - v1 v2))

(test (sub/bs (numV 5) (numV 6)) (numV -1))
(test/exn (sub/bs (numV 5) (funV 'x (id 'x) empty-env)) "Bad number")
(test/exn (sub/bs (funV 'x (id 'x) empty-env) (numV 6)) "Bad number")
(test/exn (sub/bs (funV 'x (id 'x) empty-env)
                  (funV 'x (id 'x) empty-env)) "Bad number")


;; Symbol Env -> Value or false
;; look up the value of x in env; produce false if x is not present
(define (maybe-lookup-env env x)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (lookup-env env x)))

(test (maybe-lookup-env empty-env 'x) #f)
(test (maybe-lookup-env (extend-env empty-env 'x (numV 5)) 'x) (numV 5))


;; Symbol Env MaybeValue -> Value
;; produce the result of the expression bound to the given identifier.
;; Effect: Signal an exception in case of unbound unbackstopped id
(define (id/bs x env bstopV)
  (let ([maybe-value (maybe-lookup-env env x)])
    (cond [maybe-value maybe-value]
          [bstopV bstopV]
          [else (error 'interp/bs "Unbound identifier: ~a" x)])))


(test/exn (id/bs 'x empty-env #f) "Unbound")
(test (id/bs 'x empty-env (numV 7)) (numV 7))
(test (id/bs 'x (extend-env empty-env 'x (numV 7)) #f) (numV 7))


;; BS -> Value
;; produce the result of evaluating bs0
;; Effect: signal an error on type mismatch or unbound identifier
(define (interp/bs bs0)
  ;; Accumulator: env is (envof Value)
  ;; Invariant: env represents the bindings (in inside-out order)
  ;;            of identifiers to values due to deferred substitutions
  ;; Accumulator: bstopV is MaybeValue
  ;; Invariant: innermost backstop in bs0 around bs, or #f if none
  (local [;; Value Value MaybeValue -> (list Value MaybeValue)
          (define (apply/bs vrator vrand bstopV)
            (with-value-as-funv vrator
              (λ (x body env) ;; vrator = (funV x ev-body env)
                (fn-for-bs body (extend-env env x vrand) bstopV))))

          ;; BS Env MaybeValue -> (list Value MaybeValue)
          (define (fn-for-bs f env bstopV)
            (type-case BS f
              [num (n) (list (numV n) bstopV)]
              [add (l r)
                   (match-let* ([(list lv bstopV^) (fn-for-bs l env bstopV)]
                                [(list rv bstopV^^) (fn-for-bs r env bstopV^)])
                     (list (add/bs lv rv) bstopV^^))]
              [sub (l r)
                   (match-let* ([(list lv bstopV^) (fn-for-bs l env bstopV)]
                                [(list rv bstopV^^) (fn-for-bs r env bstopV^)])
                     (list (sub/bs lv rv) bstopV^^))]
              [id (x) (list (id/bs x env bstopV) bstopV)]
              [fun (x body) (list (funV x body env) bstopV)]
              [app (rator rand)
                   (match-let*
                       ([(list ratorv bstopV^) (fn-for-bs rator env bstopV)]
                        [(list randv bstopV^^) (fn-for-bs rand env bstopV^)])
                     (apply/bs ratorv randv bstopV^^))]
              [backstop (bs body)
                        (match-let ([(list new-bstopV bstopV^)
                                     (fn-for-bs bs env bstopV)])
                          (fn-for-bs body env new-bstopV))]))]
    (match-let ([(list bsV bstopV) (fn-for-bs bs0 empty-env #f)])
      bsV)))

(test/exn (interp/bs BS1) "Unbound")
(test (interp/bs BS2) (numV 7))
(test (interp/bs BS3) (numV 14))
(test (interp/bs BS4) (numV 15))
(test (interp/bs BS5) (numV 15))
(test (interp/bs BS6) (numV 15))
#;(test (interp/bs BS7) (numV 7))
(test/exn (interp/bs BS7) "Bad function")
(test (interp/bs (add (backstop (num 7) (num 9)) (id 'x))) (numV 16))
(test/exn (interp/bs (add (id 'x) (backstop (num 7) (num 9)))) "Unbound")