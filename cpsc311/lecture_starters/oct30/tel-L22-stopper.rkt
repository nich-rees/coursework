#lang plai

;(require "parsing.rkt")

(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

;;
;; TEL: a Tiny Exceptional Language
;;

;; TID is Symbol
;; INVARIANT: a TID cannot be equal to any TEL keyword
;; interp. a TEL identifier
(define (tid? x)
  (and (symbol? x)
       (not (member x '(+ fun fix fixFun with rec raze match/handle)))))

(define TID0 'a)
(define TID1 'b)

;; No template: atomic data



;; Tag is Symbol
;; interp. an exception tag
(define (tag? x)
  (symbol? x))

(define Tag0 'a)
(define Tag1 'b)
(define Tag2 '+)

;; No template: atomic data


(define-type TEL    
  [num (n number?)]
  [add (lhs TEL?) (rhs TEL?)]
  [id (name tid?)] 
  [fun (param tid?) (body TEL?)]
  [app (rator TEL?) (arg TEL?)]
  [if0 (predicate TEL?) (consequent TEL?) (alternative TEL?)]
  [fix (name tid?) (body TEL?)]
  [raze (tag tag?) (expr TEL?)]
  [match/handle (expr TEL?)
                (value-id tid?) (value-body TEL?)
                (exn-tag tag?) (exn-id tid?) (exn-body TEL?)])
;; interp. expressions in an eager language that supports
;; arithmetic, functions, conditionals, and exceptions.
;; Its syntax is defined by the following BNF:
;; <TEL> ::=
;; (ARITHMETIC)
;;          <num>
;;        | {+ <TEL> <TEL>}
;; (IDENTIFIERS)
;;        | {with {<id> <TEL>} <TEL>}
;;        | <id>
;; (CONDITIONALS)
;;        | {if0 <TEL> <TEL> <TEL>}
;; (FUNCTIONS)
;;        | {<TEL> <TEL>}
;;        | {fun {<id>} <TEL>}
;; (RECURSION)
;;        | {fix <id> <TEL>}
;;        | {fixFun <id> <id> <HK>}
;;        | {rec {<id> <HK>} <HK>}
;; (EXCEPTIONS) *NEW*
;;        | {match/handle <TEL>
;;           [<id> <TEL>]
;;           [{raze <tag> <id>} <TEL>]}
;;        | {raze <tag> <TEL>}
;; where <tag> is any symbol

;; Syntactic Sugar
(define (with name named-expr body)
  (app (fun name body) named-expr))

(define (fixFun f x body) (fix f (fun x body)))

(define (rec name named-expr body)
  (with name (fix name named-expr) body))

#;
(define (fn-for-tel f)
  (type-case TEL f
    [num (n) (... n)]
    [add (l r) (... (fn-for-tel l)
                    (fn-for-tel r))]
    [id (x) (... x)]
    [fun (x body) (... x
                       (fn-for-tel body))]
    [app (rator rand) (... (fn-for-tel rator)
                           (fn-for-tel rand))]
    [if0 (p c a)
         (... (fn-for-tel p)
              (fn-for-tel c)
              (fn-for-tel a))]
    [fix (x body) (... x
                       (fn-for-tel body))]
    [match/handle (expr vid vbody etag eid ebody)
                  (... (fn-for-tel expr)
                       vid (fn-for-tel vbody)
                       etag eid (fn-for-tel ebody))]
    [raze (tag expr) (... tag
                          (fn-for-tel expr))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TEL TID TEL -> TEL
;; substitute sub-expr for x0 in expr1
(define (subst expr1 x0 sub-expr)
  (local [;; TEL TID -> TEL
          ;; substitute sub-expr for x0 in e if binder x is not x0
          (define (maybe-subst-in e x)
            (if (symbol=? x x0)
                e
                (recur e)))

          ;; TEL -> TEL
          (define (recur expr)
            (type-case TEL expr
              [num (n) (num n)]
              [add (l r) (add (recur l)
                              (recur r))]
              [id (x) (if (symbol=? x x0)
                          sub-expr
                          (id x))]
              [fun (x body) (fun x (maybe-subst-in body x))]
              [app (rator rand) (app (recur rator)
                                     (recur rand))]
              [if0 (p c a)
                   (if0 (recur p)
                        (recur c)
                        (recur a))]
              [fix (x body) (fix x (maybe-subst-in body x))]
              [match/handle
               (expr vid vbody etag eid ebody)
               (match/handle (recur expr)
                             vid (maybe-subst-in vbody vid)
                             etag eid (maybe-subst-in ebody eid))]
              [raze (tag expr) (raze tag
                                     (recur expr))]))]
    (recur expr1)))


(test (subst (num 5) 'x (num 7)) (num 5))
(test (subst (id 'x) 'x (num 7)) (num 7))
(test (subst (id 'y) 'x (num 7)) (id 'y))
(test (subst (fun 'y (id 'x)) 'x (num 7)) (fun 'y (num 7)))
(test (subst (fun 'x (id 'x)) 'x (num 7)) (fun 'x (id 'x)))
(test (subst (raze 'doh (id 'x)) 'x (num 7)) (raze 'doh (num 7)))
(test (subst (match/handle (num 7) 'x (id 'x) 'x 'y (id 'x)) 'x (num 7))
      (match/handle (num 7) 'x (id 'x) 'x 'y (num 7)))


;; Value is one of:
;; - (num Number)
;; - (fun TID TEL)
#;
(define (fn-for-value v)
  (match v
    [(num n) (... n)]
    [(fun id body) (... id (fn-for-tel body))]))


;; TEL -> Boolean
;; produce true if tel is a value, otherwise false
(define (Value? tel)
  (match tel
    [(num n) #t]
    [(fun id body) #t]
    [else #f]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effect Abstraction (Exceptions)

;; (raise/eff t v) - raise an exception with tag t and payload v

;; (match-exn/eff e1
;;  [val (x) e2]
;;  [handle tag (x) e3])
;; -- dispatch on whether e1 produced a value or an exn


;; Generic Interface (expresses dependencies between computations)
;; (return/eff e) - returns the value of e
;; (run/eff e) - run an effectful computation, and turn exceptions into
;;               PLAI errors

;; (let/eff ([x e1]) e2) - bind x to the value of e1 in e2 or
;;                         propagate an exception from e1


;;
;; Implementation
;;
;;
;; Canonical Forms (Values and Exceptions) *****
;;
(define-type Computation
  [value (v Value?)]
  [razed (tag tag?) (payload Value?)])
;; interp. a canonical form produced by interpretation
;;  * value denotes a normal value produced during execution
;;  * razed indicates that an exception was thrown
#;
(define (fn-for-computation c)
  (type-case Computation c
    [value (v) (... (fn-for-value v))]
    [razed (tag payload) (... tag
                              (fn-for-value payload))]))

;; Tag Value -> Computation
(define (raise/eff t v)
  (razed t v))

;; Value -> Computation
(define (return/eff e)
  (value e))

;; Computation -> Value
;; Effect: thows an exception if the given computation represents an error
(define (run/eff c)
  (type-case Computation c
    [value (v) v]
    [razed (tag payload) (error 'interp/tel "Uncaught Exception: ~a"
                                (razed tag payload))]))

(define-syntax let/eff
  (syntax-rules ()
    [(let/eff ([x e1]) e2)
     (type-case Computation e1
       [value (v) (let ([x v]) e2)]
       [razed (tag payload) (razed tag payload)])]))


;; Compose many computations
(define-syntax let/eff*
  (syntax-rules ()
    [(_ () e) e]
    [(_ ([x e1] [x* e1*] ...) e2)
     (let/eff ([x e1])
              (let/eff* ([x* e1*] ...) e2))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers

;; Value -> Number
;; produce the number corresponding to the given Value
;; Effect: signal an error if the value does not represent a number
(define (value->num v)
  (match v
    [(num n) n]
    [else (error 'interp "bad number: ~a" v)]))


;; Value Value -> Computation
;; produce the result of adding v1 to v2
;; Effect: signal an error if the values do not represent numbers
(define (add-value v1 v2)
  (let ([n1 (value->num v1)]
        [n2 (value->num v2)])
    (return/eff (num (+ n1 n2)))))


;; Value -> Boolean
;; produce true if the v is zero, otherwise false
(define (zero?-value v)
  (match v
    [(num n) (= n 0)]
    [else #f]))


;; Value Value -> Computation
;; Effect: signal an error if v1 is not a function or any other runtime errors.
(define (apply-value v1 v2)
  (match v1
    [(fun id body) (interp/tel-cf (subst body id v2))]
    [else (error 'interp "bad function: ~a" v1)]))


;; TID TEL -> Computation
;; interpret the self-referential expression (fix x tel)
;; Effect: signal an error in case of runtime error
(define (interp-fix x tel)
  (interp/tel-cf (subst tel x (fix x tel))))


;; Computation TID TEL TAG TID TEL -> Computation
;; interpret the given match/handle operation
;; Effect: signal an error in case of runtime error
(define-syntax interp-match/handle
  (syntax-rules ()
    [(_ cexpr val-id val-body etag eid ebody)
     (type-case Computation cexpr
       [value (v) (interp/tel-cf (subst val-body val-id v))]
       [razed (tag payload) (if (symbol=? tag etag)
                                (interp/tel-cf (subst ebody eid payload))
                                (razed tag payload))])]))

#;
(type-case Computation c
  [value (v) (... (fn-for-value v))]
  [razed (tag payload) (... tag
                            (fn-for-value payload))])


;; Tag Computation  -> Computation
;; interpret the given raze operation
(define-syntax interp-raze
  (syntax-rules ()
    [(_ tag cexpr)
     (let/eff ([v cexpr])
              (raise/eff tag v))])
  #;
  (type-case Computation cexpr
    [value (v) (razed tag v)]
    [razed (tag payload) (razed tag payload)]))

;; Tel -> Computation
;; produce the canonical form result of interpreting the given TEL
(define (interp/tel-cf tel)
  (type-case TEL tel
    [num (n) (return/eff (num n))]
    [add (l r)
         (let/eff ([vl (interp/tel-cf l)])
                  (let/eff ([vr (interp/tel-cf r)])
                           (add-value vl vr))
                  #;
                  (type-case Computation (interp/tel-cf r)
                    [value (vr) (add-value vl vr)]
                    [razed (tag payload) (razed tag payload)]))
         #;
         (type-case Computation (interp/tel-cf l)
           [value (vl)
                  (type-case Computation (interp/tel-cf r)
                    [value (vr) (add-value vl vr)]
                    [razed (tag payload) (razed tag payload)])]
           [razed (tag payload) (razed tag payload)])]
    [id (x) (error 'interp "Unbound identifier: ~a" x)]
    [fun (x body) (return/eff (fun x body))]
    [app (rator rand)
         (let/eff* ([vrator (interp/tel-cf rator)]
                    [vrand  (interp/tel-cf rand)])
                   (apply-value vrator vrand))
         #;
         (type-case Computation (interp/tel-cf rator)
           [value (vrator)
                  (type-case Computation (interp/tel-cf rand)
                    [value (vrand) (apply-value vrator vrand)]
                    [razed (tag payload) (razed tag payload)])]
           [razed (tag payload) (razed tag payload)])]
    [if0 (p c a)
         (type-case Computation (interp/tel-cf p)
           [value (vp)
                  (if (zero?-value vp)
                      (interp/tel-cf c)
                      (interp/tel-cf a))]
           [razed (tag payload) (razed tag payload)])]
    [fix (x body) (interp-fix x body)]
    [match/handle (expr vid vbody etag eid ebody)                  
                  (interp-match/handle (interp/tel-cf expr)
                                       vid vbody
                                       etag eid ebody)]
    [raze (tag expr)
          (interp-raze tag (interp/tel-cf expr))]))


;; BAD!  Examples expose the implementation details of the effect abstraction
;; EXERCISE: Rewrite in terms of the effect abstraction operations
(test (interp/tel-cf (num 5)) (value (num 5)))
(test (interp/tel-cf (raze 'oops (num 5))) (razed 'oops (num 5)))
(test (interp/tel-cf
       (match/handle
        (add (raze 'oops (num 7)) (num 3))
        'x (add (id 'x) (num 9))
        'oops 'x (add (id 'x) (num 12))))
      (value (num 19)))

(test (interp/tel-cf
       (match/handle 
        (match/handle
         (add (raze 'oops (num 7)) (num 3))
         'x (add (id 'x) (num 9))
         'oops 'x (add (id 'x) (num 12)))
        'y (add (num -5) (id 'y))
        'doh 'y (id 'y))) 
      (value (num 14)))

(test (interp/tel-cf
       (match/handle 
        (match/handle
         (add (raze 'doh (num 7)) (num 3))
         'x (add (id 'x) (num 9))
         'oops 'x (add (id 'x) (num 12)))
        'y (add (num -5) (id 'y))
        'doh 'y (id 'y)))       
      (value (num 7)))


;; Tel -> Value
;; interpret the given expression
;; Effect: Signal an exception for runtime errors *or uncaught user exceptions*
(define (interp/tel tel)
  (run/eff (interp/tel-cf tel)))



(test (interp/tel (num 5)) (num 5))
(test/exn (interp/tel (raze 'oops (num 5))) "Uncaught")
(test (interp/tel
       (match/handle
        (add (raze 'oops (num 7)) (num 3))
        'x (add (id 'x) (num 9))
        'oops 'x (add (id 'x) (num 12))))
      (num 19))

(test (interp/tel
       (match/handle 
        (match/handle
         (add (raze 'oops (num 7)) (num 3))
         'x (add (id 'x) (num 9))
         'oops 'x (add (id 'x) (num 12)))
        'y (add (num -5) (id 'y))
        'doh 'y (id 'y))) 
      (num 14))

(test (interp/tel
       (match/handle 
        (match/handle
         (add (raze 'doh (num 7)) (num 3))
         'x (add (id 'x) (num 9))
         'oops 'x (add (id 'x) (num 12)))
        'y (add (num -5) (id 'y))
        'doh 'y (id 'y)))       
      (num 7))
