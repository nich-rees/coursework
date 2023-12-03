#lang plai

;(require "parsing.rkt")
(define ... list) ;; enables us to use ... in templates

(print-only-errors #t)

;;
;; boxtel.rkt - tiny exceptional language with mutable boxes!
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

;; Location is symbol
;; interp. an address in a store.
(define location? symbol?)



(define-type TEL    
  [num (n number?)]
  [add (lhs TEL?) (rhs TEL?)]
  [id (name symbol?)] 
  [fun (param symbol?) (body TEL?)]
  [app (rator TEL?) (arg TEL?)]
  [if0 (predicate TEL?) (consequent TEL?) (alternative TEL?)]
  [fix (name tid?) (body TEL?)]
  [raze (tag tag?) (expr TEL?)]
  [match/handle (expr TEL?)
                (value-id tid?) (value-body TEL?)
                (exn-tag tag?) (exn-id tid?) (exn-body TEL?)]
  [newbox (e TEL?)]
  [boxV (l location?)] ;; part of datatype, but not surface syntax!
  ;;;;;;;;;;;;;;;;;;;;; only created by interp
  [setbox (e1 TEL?) (e2 TEL?)]
  [openbox (e TEL?)])
 
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
;; (EXCEPTIONS) *NEW*
;;        | {match/handle <TEL>
;;           [<id> <TEL>]
;;           [<tag> <id> <TEL>]}
;;        | {raze <tag> <TEL>}
;; (BOXES)
;;        | {newbox <HG>}
;;        | {setbox <HG> <HG>}
;;        | {openbox <HG>}
;; (SEQUENCING)
;;        | {seqn <HG> <HG>}

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
    [raze (tag expr) (... tag
                          (fn-for-tel expr))]
    [match/handle (expr val-id val-body etag eid ebody)
                  (... (fn-for-tel expr)
                       val-id (fn-for-tel val-body)
                       etag eid (fn-for-tel ebody))]
    [newbox (e) (... (fn-for-tel e))]
    [boxV (l) (... l)]
    [setbox (e1 e2) (... (fn-for-tel e1)
                         (fn-for-tel e2))]
    [openbox (e) (... (fn-for-tel e))]))


;; Syntactic Sugar
(define (with x named body) (app (fun x body) named))
(define (seqn e1 e2) (with (gensym) e1 e2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TEL TID TEL -> TEL
;; substitute tel0 for x0 in tel
;(define (subst tel x0 tel0) (num 7)) ; stub

(define (subst tel x0 tel0)
  (local [;; TEL -> TEL
          (define (recur expr1)
            (local [;; TEL TID -> TEL
                    ;; substitute tel0 for x0 in tel if binder x is not x0
                    (define (maybe-subst-in e x)
                      (if (symbol=? x x0)
                          e
                          (recur e)))]
              (type-case TEL expr1
                [num (n) (num n)]
                [add (l r) (add (recur l)
                                (recur r))]
                [id (x) (if (symbol=? x x0)
                            tel0
                            (id x))]
                [fun (x body) (fun x (maybe-subst-in body x))]
                [app (rator rand) (app (recur rator)
                                       (recur rand))]
                [if0 (p c a)
                     (if0 (recur p)
                          (recur c)
                          (recur a))]
                [fix (x body) (fix x (maybe-subst-in body x))]
                [raze (tag expr) (raze tag
                                       (recur expr))]
                [match/handle
                 (expr vid vbody etag eid ebody)
                 (match/handle (recur expr)
                               vid (maybe-subst-in vbody vid)
                               etag eid (maybe-subst-in ebody eid))]
                [newbox (e) (newbox (recur e))]
                [boxV (l) (boxV l)]
                [setbox (e1 e2) (setbox (recur e1)
                                        (recur e2))]
                [openbox (e) (openbox (recur e))])))]
    (recur tel)))


(test (subst (num 5) 'x (num 7)) (num 5))
(test (subst (id 'x) 'x (num 7)) (num 7))
(test (subst (id 'y) 'x (num 7)) (id 'y))
(test (subst (fun 'y (id 'x)) 'x (num 7)) (fun 'y (num 7)))
(test (subst (fun 'x (id 'x)) 'x (num 7)) (fun 'x (id 'x)))
(test (subst (raze 'doh (id 'x)) 'x (num 7)) (raze 'doh (num 7)))
(test (subst (match/handle (num 7) 'x (id 'x) 'x 'y (id 'x)) 'x (num 7))
      (match/handle (num 7) 'x (id 'x) 'x 'y (num 7)))


;; Value is one of:
;; - (num number)
;; - (fun TID TEL)
;; - (boxV Location)
;; interp. a runtime value
#;
(define (fn-for-value v)
  (match v
    [(num n) (... n)]
    [(fun id body) (... id (fn-for-tel body))]
    ;; NEW!!!
    [(boxV l) (... l)]))


;; TEL -> boolean
;; produce true if tel is a value, otherwise false
(define (Value? tel)
  (match tel
    [(num n) #t]
    [(fun id body) #t]
    [(boxV l) #t]
    [else #f]))



;;
;; Stores
;;

;; Store is (listof (list Location Value))
;;

;; Store
;; an empty store (initial store)
(define empty-store empty)

;; Store 
(define (lookup-store store loc)
  (cond
    [(assoc loc store) => cadr] ;; consult cond docs if this looks crazy :)
    [else (error 'interp "Unbound Store Location: ~a" loc)]))


;; Store -> (list Location Store)
;; produce a new location in the given store
;; Effect: performs a gensym
(define (new-location store) (list (gensym) store))


;; Store Location Value -> Store
;; produce a new store that is updated with the new binding
(define (update-store store loc value)
  (cond
    [(empty? store) (list (list loc value))]
    [else ;; cond
     (if (symbol=? (first (first store)) loc)
         (cons (list loc value) (rest store))
         (cons (first store)
               (update-store (rest store) loc value)))]))


;; Any -> Boolean
;; produce true if the given value is a store, otherwise false
(define (Store? s)
  (and (list s)
       (for/and ([b s])
         (match s
           [(list loc value) (and (location? loc) (Value? value))]
           [else #f]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effect Abstraction (Exceptions + State)

;;
;; Effect Interface
;;

;; (raise/eff t v) - raise an exception with tag t and payload v
;; (match-exn/eff e1
;;  [val (x) e2]
;;  [handle tag (x) e3]) - dispatch on whether e1 produced a value or an exn

;; (let-state/eff ([s]) c) - bind s to the current state value and evaluate c
;; (update-state/eff s) - make s the new state, and return a garbage value

;; The Generic Interface Components:
;; (return/eff e) - return the value of e
;; (run/eff c) - run an effectful computation, and turn exceptions into errors
;; (let/eff ([x c1] c2)) - bind x to the value of c1 in c2 or propagate c1 exns
;; (let/eff* ([x1 c1*] [x2 c2] ...) c) - sequentialize instances of let/eff

;;
;; Effect Implementation
;;


;; Computation is ... (see below)


;; Exception-based Version
;(define-type Computation
;  [value (v Value?)]
;  [razed (tag Tag?) (payload Value?)])
;;; interp. a canonical form produced by interpretation
;;;  * value denotes a normal value produced during execution
;;;  * razed indicates that an exception was thrown
;#;
;(define (fn-for-computation c)
;  (type-case Computation c
;    [value (v) (... (fn-for-value v))]
;    [razed (tag payload) (... tag
;                              (fn-for-value payload))]))


;; State-based Version
;; Computation is Natural -> (list Natural Natural)
;; interp.  a function that awaits a threaded accumulator value and produces a
;; number and an updated accumulated value.


;; COMBINED VERSION: State layered atop Exceptions.  A computation may use state
;; (the threaded accumulator) to determine whether the computation produces a
;; value or an exception.


(define-type Object
  [value (v Value?)]
  [razed (tag tag?) (payload Value?)])
;; interp. a "Java 4" style (i.e., before Java Generics)) universal data object.
;;  * value denotes a normal value produced during execution
;;  * razed indicates that an exception was thrown
;; Formerly called Canonical:  the name change is in preparation for future
;; extensions that are not related to "canonical forms".

#;
(define (fn-for-object o)
  (type-case Object o
    [value (v) (... (fn-for-value v))]
    [razed (tag payload) (... tag
                              (fn-for-value payload))]))

;; Computation is Store -> (list Object Store)
;; interp.  a computation with threaded store accumulator
;; (and exceptions, encoded inside Object)




;; Tag Value -> Computation
;; raise an exception with the given tag and payload
#;
(define (raise/eff tag v)
  (... tag v))

(define (raise/eff tag v)
  (λ (state) (list (razed tag v) state)))


; The old exception-only implementation:
;(define (raise/eff tag v)
;  (razed tag v))


;; distinguish values from exceptions
#;
(define-syntax match-exn/eff
  (syntax-rules (val handle)
    [(_ c1 
        [val (x2) c2]
        [handle tag (x3) c3])
     (... ...)]))

(define-syntax match-exn/eff
  (syntax-rules (val handle)
    [(_ c1 
        [val (x2) c2]
        [handle tag (x3) c3])
     (λ (state)
       (match-let ([`(,o ,state^) (c1 state)])
         (type-case Object o
           [value (x2) (c2 state^)]
           [razed (tag x3) (c3 state^)])))]))


;; Exception-only Version
;(define-syntax match-exn/eff
;  (syntax-rules (val handle)
;    [(_ e1 
;        [val (x2) e2]
;        [handle tag (x3) e3])
;     (type-case Computation e1
;       [value (x2) e2]
;       [razed (tag x3) e3])]))


;; reify the current state in the given computation
#;
(define-syntax let-state/eff
  (syntax-rules ()
    [(_ ([s]) c)
     (... ...)]))

(define-syntax let-state/eff
  (syntax-rules ()
    [(_ ([state]) c)
     (λ (state) (c state))]))

;; State-only Version
;(define-syntax let-state/eff
;  (syntax-rules ()
;    [(_ ([s]) c)
;     (λ (state)
;       (let ([s state])
;         (c state)))]))


;; Store -> Computation
;; set the current state
#;
(define (update-state/eff new-state)
  (... new-state)) 

(define (update-state/eff new-state)
  (λ (old-state)
    (list (value (num -99)) new-state))) ; uninformative return value


;; The old state-only version
;(define (update-state/eff new-state)
;  (λ (state) (list -99 new-state))) ;; uninformative return value


;; Natural -> Computation
;; create a computation that yields v
#;
(define (return/eff v)
  (... v))

(define (return/eff v)
  (λ (state) (list (value v) state)))

;; State-based Version
;;(define (return/state n)
;;  (λ (state) (list n state)))

;; Exception-based Version
;;(define (return/eff v)
;;  (value v))


;; Computation -> Value
;; produce a value corresponding to the given Computation
;; Effect: Signal a runtime error if it is an exception
#;
(define (run/eff c state0)
  (... c state0))

(define (run/eff c state0)
  (match-let ([`(,o ,state) (c state0)])
    (type-case Object o
      [value (v) v]
      [razed (tag payload) (error 'interp "Uncaught Exception: ~a"
                                  (razed tag payload))])))

;; State-based Version
;(define (run/eff c state0)
;  (match-let ([`(,result ,state) (c state0)])
;    result))


;; Exception-based Version
;(define (run/eff c)
;  (type-case Computation c
;    [value (v) v]
;    [razed (tag payload) (error 'interp "Uncaught Exception: ~a"
;                                (razed tag payload))]))


#;
(define-syntax let/eff
  (syntax-rules ()
    [(_ ([x e1]) e2)
     (... ...)]))

(define-syntax let/eff
  (syntax-rules ()
    [(_ ([x c1]) c2)
     (λ (state)
       (match-let ([`(,o ,state^) (c1 state)])
         (type-case Object o
           [value (v) (let ([x v]) (c2 state^))]
           [razed (tag payload) (list (razed tag payload) state^)])))]))


;; State-based version
;(define-syntax let/eff
;  (syntax-rules ()
;    [(_ ([x c1]) c2)
;     (λ (state)
;       (match-let ([`(,x ,state^) (c1 state)])
;         (c2 state^)))]))


;; Exception-based version
;(define-syntax let/eff
;  (syntax-rules ()
;    [(_ ([x e1]) e2)
;     (let ([o e1])
;       (type-case Object o
;         [value (v) (let ([x v]) e2)]
;         [razed (tag payload) (razed tag payload)]))]))


;; Compose many computations in sequence
(define-syntax let/eff*
  (syntax-rules ()
    [(_ () e) e]
    [(_ ([x e1] [x* e1*] ...) e2)
     (let/eff ([x e1])
              (let/eff* ([x* e1*] ...) e2))]))

;; End of Effect Abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
    [(fun id body) (interp-tel/eff (subst body id v2))]
    [else (error 'interp "bad function: ~a" v1)]))


;; TID TEL -> Computation
;; interpret the self-referential expression (fix x tel)
;; Effect: signal an error in case of runtime error
(define (interp-fix x tel)
  (interp-tel/eff (subst tel x (fix x tel))))


;; TEL TID TEL Tag TID TEL -> Computation
;; interpret the given match/handle operation
;; Effect: signal an error in case of runtime error
(define (interp-match/handle expr val-id val-body etag eid ebody)
  (match-exn/eff (interp-tel/eff expr)
                 [val (v) (interp-tel/eff (subst val-body val-id v))]
                 [handle tag (payload)
                         (if (symbol=? tag etag)
                             (interp-tel/eff (subst ebody eid payload))
                             (raise/eff tag payload))]))

;; Examples follow interp-tel/eff


;; Value -> Computation
;; Allocate a new mutable box containing v
#;
(define (newbox-value v)
  (return/eff (num 0))) ; stub

(define (newbox-value v)
  (let ([loc (gensym)])
    (let-state/eff ([store])
                   (let/eff ([_ (update-state/eff (update-store store loc v))])
                            (return/eff (boxV loc))))))

(test (match ((newbox-value (num 9)) empty-store)
        [(list (value (boxV l)) `((,l ,(num 9)))) #t]
        [else #f])
      #t)


;; Value -> boxV
;; coerce the given value to a box location
;; Effect: signal an error if v does not denote a box location
(define (value->loc v)
  (match v
    [(boxV l) l]
    [else (error 'interp "bad box location: ~a" v)]))


;; Value Value -> Computation
;; assign v2 to the location denoted by v1
;; Effect: Signal an error if v1 does not denote a location
#;
(define (setbox-value v1 v2)
  (return/eff (num 0))) ; stub

(define (setbox-value v1 v2)
  (let ([loc (value->loc v1)])
    (let-state/eff ([store])
                   (let/eff ([_ (update-state/eff (update-store store loc v2))])
                            (return/eff (boxV loc))))))

(test (match ((setbox-value (boxV 'l) (num 22)) `((l ,(num 9))))
        [(list (value garbage) store) (equal? (lookup-store store 'l) (num 22))]
        [else #f])
      #t)


;; Value -> Computation
;; produce the contents of the location denoted by v
;; Effect: Signal an error if v does not denote a location
#;
(define (openbox-value v)
  (return/eff (num 0))) ; stub

(define (openbox-value v)
  (let ([loc (value->loc v)])
    (let-state/eff ([store])
                   (return/eff (lookup-store store loc)))))


(test (match ((openbox-value (boxV 'l)) `((l ,(num 9))))
        [(list (value (num 9)) `((l ,(num 9)))) #t]
        [else #f])
      #t)


;; Tel -> Computation
;; produce the result of interpreting the given TEL
;; Effect: signal an error in case of runtime error
(define (interp-tel/eff f)
  (type-case TEL f
    [num (n) (return/eff (num n))]   
    [add (l r)
         (let/eff* ([vl (interp-tel/eff l)]
                    [vr (interp-tel/eff r)])
                   (add-value vl vr))]
    [id (x) (error 'interp "Unbound identifier: ~a" x)]
    [fun (x body) (return/eff (fun x body))]
    [app (rator rand) (let/eff* ([vrator (interp-tel/eff rator)]
                                 [vrand (interp-tel/eff rand)])
                                (apply-value vrator vrand))]
    [if0 (p c a)
         (let/eff ([vp (interp-tel/eff p)])
                  (if (zero?-value vp)
                      (interp-tel/eff c)
                      (interp-tel/eff a)))]
    [fix (x body) (interp-fix x body)]
    [raze (tag expr)
          (let/eff* ([vexpr (interp-tel/eff expr)])
                    (raise/eff tag vexpr))]
    [match/handle (expr val-id val-body etag eid ebody)
                  (interp-match/handle expr
                                       val-id val-body
                                       etag eid ebody)]
    [newbox (e)
            (let/eff ([v (interp-tel/eff e)])
                     (newbox-value v))]
    [boxV (l)
          (return/eff (boxV l))]
    [setbox (e1 e2)
            (let/eff* ([v1 (interp-tel/eff e1)]
                       [v2 (interp-tel/eff e2)])
                      (setbox-value v1 v2))]
    [openbox (e)
             (let/eff ([v (interp-tel/eff e)])
                      (openbox-value v))]))

;; Examples for interp-tel/eff
(test (run/eff (interp-tel/eff (num 5)) empty-store)
      (num 5))

(test/exn (run/eff (interp-tel/eff (raze 'oops (num 5))) empty-store)
          "Uncaught")

(test (run/eff (interp-tel/eff
                (match/handle
                 (add (raze 'oops (num 7)) (num 3))
                 'x (add (id 'x) (num 9))
                 'oops 'x (add (id 'x) (num 12))))
               empty-store)
      (num 19))

(test (run/eff (interp-tel/eff
                (match/handle 
                 (match/handle
                  (add (raze 'oops (num 7)) (num 3))
                  'x (add (id 'x) (num 9))
                  'oops 'x (add (id 'x) (num 12)))
                 'y (add (num -5) (id 'y))
                 'doh 'y (id 'y)))
               empty-store)
      (num 14))

(test (run/eff (interp-tel/eff
                (match/handle 
                 (match/handle
                  (add (raze 'doh (num 7)) (num 3))
                  'x (add (id 'x) (num 9))
                  'oops 'x (add (id 'x) (num 12)))
                 'y (add (num -5) (id 'y))
                 'doh 'y (id 'y)))
               empty-store)
      (num 7))


;; Tel -> Value
;; interpret the given expression
;; Effect: Signal an exception for runtime errors *or uncaught user exceptions*
(define (interp-tel tel)
  (run/eff (interp-tel/eff tel) empty-store))


(test (interp-tel (num 5)) (num 5))
(test/exn (interp-tel (raze 'oops (num 5))) "Uncaught")

(test
 (match (interp-tel (newbox (num 7)))
   [(boxV l) #t]
   [else #f])
 #t)
 
(test (interp-tel (with 'x (newbox (num 7))
                        (seqn (setbox (id 'x)
                                      (add (openbox (id 'x)) (num 2)))
                              (openbox (id 'x)))))
      (num 9))
