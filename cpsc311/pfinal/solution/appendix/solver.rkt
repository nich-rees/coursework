#lang plai
(define ... (λ args (cons '... args)))
(print-only-errors #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Expressions

;; Variable is symbol
;; interp.  a placeholder for a type expression
(define (variable? x)
  (symbol? x))

(define-type TypeExpr
  [emptyType]     ;; NO OPERATIONS, NO VALUES!
  [numType]
  [funType (arg TypeExpr?) (result TypeExpr?)]
  [tupleType (elems (listof TypeExpr?))]
  [unifVar (id variable?)])
;; interp. an expression of a type that has 0 or more as yet unknown components,
;;         denoted by variables X.  Corresponds to the following BNF:
;; <TypeExpr> ::= empty | number | <Type> -> <Type> | X

#;
(define (fn-for-type-expr typeX)
  (type-case TypeExpr typeX
    [emptyType () (...)]
    [numType () (...)]
    [funType (dom rng) (... (fn-for-type-expr dom)
                            (fn-for-type-expr rng))]
    [tupleType (typeXs) (... (fn-for-listof-type-expr typeXs))]
    [unifVar (X) (... X)]))

#;
(define (fn-for-listof-type-expr typeXs)
  (... typeXs))

;; Type is one of
;; - (emptyType)
;; - (numType)
;; - (funType Type Type)
;; - (tupleType (listof Type))
;; interp. a TypeExpr containing no metavariables

#;
(define (fn-for-type type)
  (match type
    [(emptyType) (...)]
    [(numType) (...)]
    [(funType dom rng) (... (fn-for-type dom)
                            (fn-for-type rng))]
    [(tupleType typeXs) (... (fn-for-listof-type typeXs))]))


;; TypeExpr -> (listof Variable)
;; produce the (free) variables that appear in the given type expression
(define (free-vars typeX)
  (type-case TypeExpr typeX
    [emptyType () '()]
    [numType () '()]
    [funType (t1 t2) (append (free-vars t1) (free-vars t2))]
    [tupleType (typeXs) (apply append (map free-vars typeXs))]
    [unifVar (X) (list X)])) 

;; any -> Boolean
;; produce true if x is a type, otherwise false
(define (Type? x)
  (and (TypeExpr? x)
       (equal? (free-vars x) '())))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Substitutions

;; (substof X) is Variable -> X
;; interp. a mapping from type variables to objects of type X


;; (substof X)
;; a substitution that maps all variables to their corresponding type expression
(define (empty-subst X) (unifVar X))


;; Variable TypeExpr -> TypeExprSub
;; produce a subst that replaces X with typeX, otherwise acts like empty-subst
(define (make-subst X typeX)
  (λ (Y)
    (if (equal? X Y)
        typeX
        (empty-subst Y))))


;; Given a type substitution, we can "apply" it to a type expression,
;; i.e. replace each variable with whatever the substitution maps it to

;; (substof TypeExpr) TypeExpr -> TypeExpr
;; replace all type variables in typeX with their mapping in subst
(define (apply-subst subst typeX)
  (type-case TypeExpr typeX
    [emptyType () (emptyType)]
    [numType () (numType)]
    [funType (dom range)
             (funType (apply-subst subst dom)
                      (apply-subst subst range))]
    [tupleType (typeXs)
               (tupleType (for/list ([typeX typeXs])
                            (apply-subst subst typeX)))]
    [unifVar (X) (subst X)]))


;; (substof TypeExpr) (substof TypeExpr) -> (substof TypeExpr)
;; compose two substitutions by applying them sequentially
(define (compose-subst subst2 subst1)
  (λ (Y)
    (apply-subst subst2 (subst1 Y))))


;; Finally, we can turn a (substof TypeExpr) into a (substof Type)
;; by applying the substitution and then replacing any variables with emptyType

;; (substof TypeExpr) -> (substof Type)
;; adapt the given subst to produce (emptyType) in place of any variables
(define (finalize-subst subst)
  (compose-subst (λ (Y) (emptyType)) subst))

;; EXAMPLES


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equations

(define-type Equation
  [assert-equal (lhs TypeExpr?) (rhs TypeExpr?)])
;; interp.  An equation over type expressions.

(define (apply-subst-eqn subst eqn)
  (type-case Equation eqn
    [assert-equal (lhs rhs) (assert-equal (apply-subst subst lhs)
                                          (apply-subst subst rhs))]))

;; EXAMPLES HERE

;; SystemOfEqns is (listof Equation)
;; interp. a system of equations over types.

;; (substof TypeExpr) SystemOfEqns -> SystemOfEqns
;; apply subst to all the expressions in  soeqns
(define (apply-subst-soeqns subst soeqns)
  (for/list ([eqn soeqns]) (apply-subst-eqn subst eqn)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unification -- a procedure for solving systems of type equations

;; Given a system of equations soeqn, we we want to produce a (substof Type)
;; subst such that for every (assert-equal typeX1 typeX2) in soeqn:
;; (equal? (apply-subst subst typeX1)
;;         (apply-subst subst typeX2))


;; unify : SystemOfEqns -> (substof Type)
;; postcondition: for each (t1 t2) in the constraint set,
;; applying the result of unification to t1 and t2 should produce the same type.
(define (unify soeqns)
  (finalize-subst
   (unify--with-subst soeqns)))


;; Problem 1: Complete this function

;; unify--with-subst : ConstraintSet (substof TypeExpr) -> (substof TypeExpr)
;; Given a starting substitution, extend it to satisfy
;; all of the equations in the given constraint set

(define (unify--with-subst soeqns)
  (cond
    ;; *any substitution* solves an empty system of equations (think about it)
    ;; so use the empty one, because it makes the fewest commitments
    [(empty? soeqns) empty-subst]
    [else
     (let ([eqn (first soeqns)]
           [soeqns^ (rest soeqns)])
       (match eqn
         ;; Vacuous equation, discard
         [(assert-equal (unifVar X) (unifVar X))
          (unify--with-subst soeqns^)]          
         ;; Variable on the left: map it to the type expression on the right
         ;; Question: when can this go wrong?
         [(assert-equal (unifVar X) typeX2)
          (begin
            (when (member X (free-vars typeX2)) ;; occurs check
            (error 'unify "Cannot solve ~a = ~a." (unifVar X) typeX2))
            (let ([subst1 (make-subst X typeX2)])
              (let ([subst2 (unify--with-subst
                             (apply-subst-soeqns subst1 soeqns^))])
                (compose-subst subst2 subst1))))]
         ;; Variable on the right: map it to the type expression on the left
         ;; (same idea as above)
         [(assert-equal typeX1 (unifVar X))
          (begin
            (when (member X (free-vars typeX1)) ;; occurs check
            (error 'unify "Cannot solve ~a = ~a." typeX1 (unifVar X)))
          (let ([subst1 (make-subst X typeX1)])
            (let ([subst2 (unify--with-subst
                           (apply-subst-soeqns subst1 soeqns^))])
              (compose-subst subst2 subst1))))]
         ;; Structural cases: if we have two types, they can only
         ;; be unified if they have the same constructors,
         ;; and we can unify their fields
         [(assert-equal (emptyType) (emptyType))
          (unify--with-subst soeqns^)]
         [(assert-equal (numType) (numType))
          (unify--with-subst soeqns^)]
         [(assert-equal (funType dom1 rng1) (funType dom2 rng2))
          (unify--with-subst (cons (assert-equal dom1 dom2)
                                   (cons (assert-equal rng1 rng2)
                                         soeqns^)))]
         [(assert-equal (tupleType typeXs1) (tupleType typeXs2))
          (unless (equal? (length typeXs1) (length typeXs2))
            (error 'unify "Cannot solve ~a = ~a."
                   (tupleType typeXs1) (tupleType typeXs2)))
          ;; CHECK EQUAL LENGTH
          (let ([eqns (for/list ([typeX1 typeXs1] [typeX2 typeXs2])
            (assert-equal typeX1 typeX2))])
            (unify--with-subst (append eqns soeqns^)))]
         ;; Everything else,
         ;; e.g. (numType) (emptyType)
         ;; (funType t1 t2) (numType)
         [(assert-equal typeX1 typeX2)
          (error 'unify "Cannot solve ~a = ~a." typeX1 typeX2)]))]))

;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;

;; Helper to unify two type expressions
(define (unify-pair typeX1 typeX2)
  (unify (list (assert-equal typeX1 typeX2))))

;; Test helper to unify two types,
;; print the solution, and check that the substitution
;; actually makes the two sides equal
(define-syntax-rule (test-unify t1 t2)
  (let* ([soln (unify-pair t1 t2)]
         [t1Sub (apply-subst soln t1)]
         [t2Sub (apply-subst soln t2)])
    (begin
      #;(for ([x (remove-duplicates (append (free-vars t1) (free-vars t2)))])
          (printf "~a := ~a\n" x (soln x)))
      (test t1Sub t2Sub))))

;; Some tests
(test/exn (unify-pair (numType) (emptyType)) "Cannot solve")
(test-unify (numType) (numType))
(test-unify  (emptyType) (emptyType))
(test-unify
 (funType (numType) (numType))
 (funType (numType) (numType)))

(test-unify (unifVar 'X) (unifVar 'X))
(test-unify (numType) (unifVar 'x))

(test-unify
 (funType (unifVar 'x) (numType))
 (funType (unifVar 'y) (numType)))

;; (x -> num) = (num -> x)
(test-unify
 (funType (unifVar 'x) (numType))
 (funType (numType) (unifVar 'x)))

(test-unify
 (tupleType (list (numType) (unifVar 'X) (emptyType)))
 (tupleType (list (unifVar 'Y) (emptyType) (unifVar 'Z))))

(test-unify
 (tupleType (list (numType) (unifVar 'X) (emptyType)))
 (tupleType (list (unifVar 'Y) (emptyType) (unifVar 'X))))

(test/exn
 (unify-pair
 (tupleType (list (numType) (unifVar 'X) (emptyType)))
 (tupleType (list (unifVar 'X) (emptyType) (unifVar 'Z))))
 "Cannot solve")
(test/exn
 (unify-pair
 (tupleType (list (numType) (unifVar 'X) (emptyType)))
 (tupleType (list (unifVar 'X) (unifVar 'Z))))
 "Cannot solve")

(test/exn (unify-pair (unifVar 'X) (funType (numType) (unifVar 'X)))
          "Cannot solve")
(test/exn (unify-pair (funType (numType) (unifVar 'X)) (unifVar 'X))
          "Cannot solve")
