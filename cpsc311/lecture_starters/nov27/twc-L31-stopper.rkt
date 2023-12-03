#lang plai
(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

(require "../util/parsing.rkt")
(require "../util/test-match.rkt") ;; for test/match
(require "../nov24/datatype-311.rkt") ; define-type with side-conditions

;;
;; This WAE Closed (TWC): a language of only *closed* WAE expressions
;;


;;
;; WAE - a language of Arithmetic Expressions with "with"
;;


;; Any -> Boolean
;; produce true if the given value is a gensym, otherwise false.
(define (gensym? x)
  (and (symbol? x)
       (not (symbol-interned? x))
       (not (symbol-unreadable? x))))

(test (gensym? 's) #f)
(test (gensym? (gensym)) #t)
(test (gensym? (string->unreadable-symbol "hello")) #f)



;; WaeID is Symbol
;; INVARIANT: a WaeID cannot be equal to '+, '-, or 'with
(define (wid? x)
  (and (symbol? x)
       (not (member x '(+ - with)))))

(define WID0 'a)
(define WID1 'b)

;; No template: atomic data



(define-type WAE     
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (id wid?) (named WAE?) (body WAE?)]
  [id (name wid?)])
;; interp.  program in the WAE language, corresponding to the following
;; Backus-Naur Form (BNF) specification 
;;   <WAE> ::= <num>
;;          | { + <WAE> <WAE> }
;;          | { - <WAE> <WAE> }
;;          | { with {<id> <WAE>} <WAE>}
;;          | <id>

;; Every AE program is a WAE program
(define AE1 (num 4))
(define AE2 (add AE1 (num 5)))
(define AE3 (sub (num 6) (num 3)))

;; NOTE: WAES* are corresponding S-expression representations of WAE*

;; WAE can associate identifiers with expressions
(define WAES1 '{with {a 4} a})
(define WAE1 (with 'a (num 4) (id 'a)))
(define WAES2 '{with {a 4} {+ a a}})
(define WAE2 (with 'a (num 4) (add (id 'a) (id 'a))))


(define WAES3 '{with {a {+ 5 5}}
                     {+ a a}})
;; equivalent to the PLAI expression:
#;(let ([a (+ 5 5)])
    (+ a a))
(define WAE3 (with 'a (add (num 5) (num 5))
                   (add (id 'a) (id 'a))))

(define WAES4 '{with {a {+ 5 5}}
                     {with {b {- a 3}}
                           {+ b b}}})
(define WAE4 (with 'a (add (num 5) (num 5))
                   (with 'b (sub (id 'a) (num 3))
                         (add (id 'b) (id 'b)))))

(define WAES5 '{with {a 5} {+ a {with {a 3} 10}}})
(define WAE5 (with 'a (num 5) (add (id 'a) (with 'a (num 3) (num 10)))))

(define WAES6 '{with {a 5}
                     {+ a {with {a 3} a}}})
#;(let ([a 5])
    (+ a (let ([a 3]) a)))
(define WAE6 (with 'a (num 5)
                   (add (id 'a) (with 'a (num 3)
                                      (id 'a)))))

(define WAES7 '{with {a 5}
                     {+ a
                        {with {b 3}
                              a}}})
#;(let ([a 5])
    (+ a (let ([b 3]) a)))
(define WAE7 (with 'a (num 5)
                   (add (id 'a)
                        (with 'b (num 3)
                              (id 'a)))))

;; a is not bound in the first reference 
;(let ([a a]) a)

#;
(define (fn-for-wae wae)
  (type-case WAE wae
    [num (n) (... n)]
    [add (l r) (... (fn-for-wae l)
                    (fn-for-wae r))]
    [sub (l r) (... (fn-for-wae l)
                    (fn-for-wae r))]
    [with (id named body) (... id
                               (fn-for-wae named)
                               (fn-for-wae body))]
    [id (x) (... x)]))



;;
;; Type Refinement
;;

;; NumWAE is (num Number)
;; interp. a numeric WAE expression
(define NW1 (num 0))
(define NW2 (num 1))

(define (fn-for-nw nw)
  (... (num-n nw)))

;; NOTE: every NumWAE is also a WAE., (i.e. NumWAE <: WAE)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments

;; Env is (listof (list Symbol Value))
;;

;; Env
;; an empty env (initial environment)
(define empty-env empty)

;; Store 
(define (lookup-env env x)
  (cadr (assoc x env)))


;; Env Symbol Value -> Env
;; produce a new environment that is updated with the new binding
(define (update-env env x value)
  (cond
    [(empty? env) (list (list x value))]
    [else ;; cond
     (if (symbol=? (first (first env)) x)
         (cons (list x value) (rest env))
         (cons (first env)
               (update-env (rest env) x value)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; WAE Env -> Value
;; consumes a wae and environment and computes the corresponding number
(define (interp/wae--env wae env)
  (type-case WAE wae
    [num (n) (num n)]
    [add (l r)
         (num (+ (num-n (interp/wae--env l env))
                 (num-n (interp/wae--env r env))))]
    [sub (l r)
         (num (- (num-n (interp/wae--env l env))
                 (num-n (interp/wae--env r env))))]
    [with (id named-e body)
          (let ([val (interp/wae--env named-e env)])
            (interp/wae--env body (update-env env id val)))]
    [id (name)
        (lookup-env env name)]))

;; WAE -> Value
;; produce the result of the given wae
(define (interp/wae wae)
  (interp/wae--env wae empty-env))


(test (interp/wae WAE1) (num 4))
(test (interp/wae WAE2) (num 8))
(test (interp/wae WAE3) (num 20))
(test (interp/wae WAE4) (num 14))
(test (interp/wae WAE5) (num 15))
(test (interp/wae WAE6) (num 8))
(test (interp/wae WAE7) (num 10))

#;(test/exn (interp/wae (id 'a)) "Unbound")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TWC - Closed WAE Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WAE -> Boolean
;; produces true if every identifier reference in wae is bound, otherwise false
;(define (closed-wae? wae) #f)
(define (closed-wae? wae)
  ;; Accumulator: bounds is (listof Symbol)
  ;; Invariant: list of identifiers bound in the surrounding context
  (local [;; WAE (listof Symbol) -> boolean          
          (define (closed-wae?--bounds wae bounds)
            (type-case WAE wae
              [num (n) #t]
              [add (l r) (and (closed-wae?--bounds l bounds)
                              (closed-wae?--bounds r bounds))]
              [sub (l r) (and (closed-wae?--bounds l bounds)
                              (closed-wae?--bounds r bounds))]
              [with (id named body)
                    (and (closed-wae?--bounds named bounds)
                         (closed-wae?--bounds body(cons id bounds)))]
              [id (x) (if (member x bounds) #t #f)]))]
    (closed-wae?--bounds wae empty)))

(test (closed-wae? (num 7)) #t)
(test (closed-wae? (id 'x)) #f)
(test (closed-wae? (with 'x (id 'x)
                         (id 'x)))
      #f)
(test (closed-wae? (with 'x (num 0)
                         (with 'x (id 'x)
                               (id 'x))))
      #t)
(test (closed-wae? (with 'x (num 0)
                         (with 'x (id 'x)
                               (id 'y))))
      #f)
(test (closed-wae? (with 'x (num 0)
                         (with 'x (id 'y)
                               (id 'x))))
      #f)


;; TWC is WAE
;; INVARIANT: (closed-wae? twc) must be true:

;; TWC -> Value
;; produce the result of evaluating the given TWC expression
(define (interp/twc twc)
  (interp/wae twc))

;; SExpression -> Value
;; produce the value of the given twc program
;; Effect: signal an error if sexp is not a TWC
(define (interp/twc-sexp sexp)
  (let ([wae (parse sexp)])
    (begin
      (unless (closed-wae? wae)
        (error 'interp/twc-sexp "Wae is not a TWC: ~a" wae))
      (interp/twc wae))))

;; parse : SExpression -> WAE
;; GAP!!! --> plugged by closed-wae? plus error-signaling 
;; interp/twc : TWC -> Value
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inductive Definitions

;; Inductive definitions are essentially "BNFs on steroids".  They are strictly
;; more powerful.  For reference, here is the BNF for WAE:

;; Backus-Naur Form (BNF) specification 
;;   <WAE> ::= <num>
;;          | { + <WAE> <WAE> }
;;          | { - <WAE> <WAE> }
;;          | { with {<id> <WAE>} <WAE>}
;;          | <id>

;; <WAE> refines S-expressions down to those that have the above shape.
;; During parsing, we use waefs (a re-phrasing of S-expressions)
;; to identify <WAE>s.

;; LOID is (listof WID)
;; interp. a list of identifiers in the TWC language
(define loid? (listof symbol?))

;; Judgment:
;; LOID ⊢ WAE wf 
;; [ NOTE: the name of the Judgment is "• ⊢ • wf", where the loid and wae
;;   are parameters to it.  This kind of notation is called "mixed fix"
;;   (or "mixfix") notation, since it is not "prefix notation",
;;   "infix notation", or "postfix notation", but rather a mix of them ]
;;
;; interp. wae is a well-formed TWC *expression* with respect to
;;         identifiers in loid. It is defined by the following
;;         *Inductive Rules*:
;;         loid ⊢ wae wf holds whenever all the free identifiers in
;;           wae appear in loid

;; Note:  • ⊢ • wf refines  "pairs of LOID and WAE objects"

;; loid ⊢ wae wf
(define-type WAE/wf
  ;; [num/wf]
  ;; ---------------------  
  ;;    loid  ⊢ n wf
  [num/wf (loid loid?) (n number?)]

  ;;[id/wf]
  ;; --------------------- 
  ;;    loid   ⊢ x wf
  ;; CHECK:  (x ∈ loid)
  [id/wf (loid loid?) (x wid?)
         #:when (member x loid)]
  
  ;; [add/wf] loid1  ⊢ wae1 wf   loid2  ⊢ wae2 wf
  ;; -------------------------------------------- 
  ;;      loid1  ⊢ {+ wae1 wae2} wf
  ;; CHECK: loid1 = loid2
  [add/wf (wae/wf1 WAE/wf?) (wae/wf2 WAE/wf?)
          #:when (equal? (wae/wf-loid wae/wf1) (wae/wf-loid wae/wf2))]
  
  ;; [sub/wf] loid1  ⊢ wae1 wf   loid2  ⊢ wae2 wf
  ;; -------------------------------------------- 
  ;;      loid1  ⊢ {- wae1 wae2} wf
  ;; CHECK: loid1 = loid2
  [sub/wf (wae/wf1 WAE/wf?) (wae/wf2 WAE/wf?)
          #:when (equal? (wae/wf-loid wae/wf1) (wae/wf-loid wae/wf1))]
  
  ;; [with/wf] loid1  ⊢ wae1 wf  loid2  ⊢ wae2 wf
  ;; ---------------------------------------------------
  ;;    loid1  ⊢ {with {x wae1} wae2} wf                         
  ;; CHECK:  loid2 = `(x . ,loid1)
  [with/wf (x wid?)
           (wae/wf1 WAE/wf?) (wae/wf2 WAE/wf?)
           #:when (equal? (wae/wf-loid wae/wf2) `(,x . ,(wae/wf-loid wae/wf1)))]
  ) ; define-type


;; Selector functions: (doesn't pair an loid with a form, pushes induction
;; -------------------
;; Each tree *represents* an loid+wae pair, but to refer to these components
;; we need special functions to produce them

;; WAE/wf -> LOID
;; produce the loid associated with a wf derivation
(define (wae/wf-loid wae/wf)
  (type-case WAE/wf wae/wf
    [num/wf (loid n) loid]
    [id/wf (loid x) loid]
    [add/wf (wae/wf1 wae/wf2)
            ;; shouldn't matter which branch we produce, they should be equal
            (wae/wf-loid wae/wf1)]
    [sub/wf (wae/wf1 wae/wf2)
            ;; shouldn't matter which branch we produce, they should be equal
            (wae/wf-loid wae/wf2)]
    [with/wf (x wae/wf1 wae/wf2)
             ;; this one matters b/c wae/wf2 has one extra identifier
             (wae/wf-loid wae/wf1)]))

(test (wae/wf-loid (num/wf '(x y a) 5))
      '(x y a))

(test (wae/wf-loid (with/wf 'q (num/wf '(x y a) 7) (id/wf '(q x y a) 'q)))
      '(x y a))


;; WAE/wf -> WAE
;; produce the wae associated with a wf derivation
(define (wae/wf-wae wae/wf)
  (type-case WAE/wf wae/wf
    [num/wf (loid n) (num n)]
    [id/wf (loid x) (id x)]
    [add/wf (wae/wf1 wae/wf2) (add (wae/wf-wae wae/wf1)
                                   (wae/wf-wae wae/wf2))]
    [sub/wf (wae/wf1 wae/wf2) (sub (wae/wf-wae wae/wf1)
                                   (wae/wf-wae wae/wf2))]
    [with/wf (x wae/wf1 wae/wf2) (with x
                                       (wae/wf-wae wae/wf1)
                                       (wae/wf-wae wae/wf2))]))

(test (wae/wf-wae (num/wf '(x y a) 5))
      (num 5))

(test (wae/wf-wae (with/wf 'q (num/wf '(x y a) 7) (id/wf '(q x y a) 'q)))
      (with 'q (num 7) (id 'q)))



;; Exercise: write the certificate for '{} ⊢ {+ 5 7} wf
(define Wwf0 (add/wf (num/wf '() 5) (num/wf '() 7)))

;; '{} ⊢ {+ x 7}
#;(define Wwf1 (add/wf (id/wf '() 'x) (num/wf '() 7)))
;; Gives error because need variables defined on left to use on right

;; '(x) ⊢ {+ x 7}
#;(define Wwf1 (add/wf (id/wf '(x) 'x) (num/wf '() 7)))
;; still a precondition violation: because we said loid on left = loid on right
;; for add, even though the 7 exp does not have any identifiers
;; '(x) ⊢ {+ x 7}
(define Wwf1 (add/wf (id/wf '(x) 'x) (num/wf '(x) 7)))
;; Can now query in interactions:
;; > (wae/wf-wae Wwf1)
;; (add (id 'x) (num 7))

(define Wae2 '{with {x 9} {+ x 7}})

#;(define Wwf2 (... Wwf1))
(define Wwf2 (with/wf 'x (num/wf '() 9) Wwf1))
;; > Wwf2
;; (with/wf 'x (num/wf '() 9) (add/wf (id/wf '(x) 'x) (num/wf '(x) 7)))
;; > (wae/wf-wae Wwf2)
;; (with 'x (num 9) (add (id 'x) (num 7)))

;; FF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; wae-focused-sexp (waefs) is one of:
;; - number
;; - `(+ ,waefs ,waefs)
;; - `(- ,waefs ,waefs)
;; - `(with ,identifier ,waefs ,waefs
;; -  identifier
;; - "any other s-expression"
;; where identifier is any symbol except +, -, or with
;; interp.  any s-expression, but with a focus on those that represent
;; WAE expressions.

(define (identifier? x)
  (and (symbol? x)
       (not (member x '(+ - with)))))
#;
(define (fn-for-wae-focused-sexp sexp)
  (match sexp
    [`,n
     #:when (number? n)
     (... n)]
    [`(+ ,sexp1 ,sexp2)
     (... (fn-for-wae-focused-sexp sexp1)
          (fn-for-wae-focused-sexp sexp2))]
    [`(- ,sexp1 ,sexp2)
     (... (fn-for-wae-focused-sexp sexp1)
          (fn-for-wae-focused-sexp sexp2))]
    [`(with (,x ,sexp1) ,sexp2)
     #:when (identifier? x)
     (... x
          (fn-for-wae-focused-sexp sexp1)
          (fn-for-wae-focused-sexp sexp2))]
    [`,x
     #:when (identifier? x)
     (... x)]
    [else (... sexp)] ))


;; wae-focused-sexp -> WAE
;; produce a WAE value corresponding to the given WAE s-expression 
;; Effect: signals an error if the given s-expression does not represent a wae

(define (parse sexp)
  (match sexp
    [`,n
     #:when (number? n)
     (num n)]
    [`(+ ,sexp1 ,sexp2)
     (add (parse sexp1)
          (parse sexp2))]
    [`(- ,sexp1 ,sexp2)
     (sub (parse sexp1)
          (parse sexp2))]
    [`(with (,x ,sexp1) ,sexp2)
     #:when (identifier? x)
     (with x
           (parse sexp1)
           (parse sexp2))]
    [`,x
     #:when (identifier? x)
     (id x)]
    [else (error 'parse "bad WAE: ~a" sexp)]))

(test (parse '5) (num 5))
(test (parse 5) (num 5))
(test/exn (parse '+) "bad WAE")
(test (parse '(+ 5 (- 3 2))) (add (num 5)
                                  (sub (num 3) (num 2))))
(test/exn (parse '(+ 5 (- 3 #f))) "bad WAE")


(test (parse '{with {x {+ 5 5}} {+ x x}})
      (with 'x (add (num 5) (num 5))
            (add (id 'x) (id 'x))))

(test (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})
      (with 'x (add (num 5) (num 5))
            (with 'y (sub (id 'x) (num 3))
                  (add (id 'y) (id 'y)))))

(test (parse '{with {x 5} {+ x {with {x 3} 10}}})
      (with 'x (num 5)
            (add (id 'x)
                 (with 'x (num 3)
                       (num 10)))))

(test (parse '{with {x 5} {+ x {with {x 3} x}}})
      (with 'x (num 5)
            (add (id 'x)
                 (with 'x (num 3)
                       (id 'x)))))

(test (parse '{with {x 5} {+ x {with {y 3} x}}})
      (with 'x (num 5)
            (add (id 'x)
                 (with 'y (num 3)
                       (id 'x)))))


;;
;; PUTTING IT ALL TOGETHER  - an interpreter of files on disk
;;

;; String -> Number
;; produce the result of interpreting the AE stored in the file fname
;; EFFECT: signals an error if no file fname contains an AE representation
(define (interp-file fname)
  (interp/wae
   (parse
    (read-from-file fname))))

(test (with-temporary-data-file "{+ 3 7}\n"
        (λ (fname) (interp-file fname)))
      (num 10))

(test (with-temporary-data-file "{+ {- 3 4} 7}\n"
        (λ (fname) (interp-file fname)))
      (num 6))

(test (with-temporary-data-file "{with {x 5} {+ x {with {y 3} x}}}"
        (λ (fname) (interp-file fname)))
      (num 10))

