#lang plai
(require "../util/parsing.rkt")

(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

;;
;; WAE - Arithmetic Expressions with identifiers
;;

;; S-expression -> Boolean
;; produce true if the given value is a gensym, otherwise false.
(define (gensym? s)
  (and (symbol? s)
       (not (symbol-interned? s))
       (not (symbol-unreadable? s))))

(test (gensym? 's) #f)
(test (gensym? (gensym)) #t)
(test (gensym? (string->unreadable-symbol "hello")) #f)

(define-type WAE     
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (id symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)])
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

;; WAE can associate identifiers with expressions
(define WAES1 '{with {x 4} x})
(define WAE1 (with 'x (num 4) (id 'x)))
(define WAES2 '{with {x 4} {+ x x}})
(define WAE2 (with 'x (num 4) (add (id 'x) (id 'x))))


(define WAES3 '{with {x {+ 5 5}} {+ x x}})
;; equivalent to
#;(let ([x (+ 5 5)])
    (+ x x))
(define WAE3 (with 'x (add (num 5) (num 5))
                   (add (id 'x) (id 'x))))

(define WAES4 '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})
(define WAE4 ...)

(define WAES5 '{with {x 5} {+ x {with {x 3} 10}}})
(define WAE5 ...)

(define WAES6 '{with {x 5} {+ x {with {x 3} x}}})
#;(let ([x 5])
    (+ x (let ([x 3]) x)))
(define WAE6 ...)

(define WAES7 '{with {x 5} {+ x {with {y 3} x}}})
#;(let ([x 5])
    (+ x (let ([y 3]) x)))
(define WAE7 ...)

;; x is not bound in the first reference 
;(let ([x x]) x)

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

;; NOTE: every NumWAE is also a WAE.


;;
;; Free Identifiers
;;

;; Exercise:

;; WAE -> (listof symbol)
;; produce a list of the free identifier instances in the given expressions
;; (NOTE: since we want "instances" they need not be unique!)

; (define (free-instance-ids initial-wae) empty) ; stub

;; Template with accumulator blended in:
#;
(define (free-instance-ids wae0)
  ;; Accumulator: binding-instaces is ...
  ;; Invariant: 
  (local [(define (fn-for-wae wae binding-instances)
            (type-case WAE wae
              [num (n) (... n binding-instances)]
              [add (l r) (... binding-instances
                              (fn-for-wae l (... binding-instances))
                              (fn-for-wae r (... binding-instances)))]
              [sub (l r) (... binding-instances
                              (fn-for-wae l (... binding-instances))
                              (fn-for-wae r (... binding-instances)))]
              [with (id named body)
                    (... binding-instances
                         id
                         (fn-for-wae named (... binding-instances))
                         (fn-for-wae body (... binding-instances)))]
              [id (x) (... binding-instances x)]))]
    (fn-for-wae wae0 ...)))

(define (free-instance-ids wae0)
  ;; Accumulator: binding-instaces is (listof Symbol)
  ;; Invariant: all identifiers in wae0 with binding instances around wae
  (local [(define (fn-for-wae wae binding-instances)
            (type-case WAE wae
              [num (n) empty]
              [add (l r) (append (fn-for-wae l binding-instances)
                                 (fn-for-wae r binding-instances))]
              [sub (l r) (append (fn-for-wae l binding-instances)
                                 (fn-for-wae r binding-instances))]
              [with (id named body)
                    (append (fn-for-wae named binding-instances)
                            (fn-for-wae body (cons id binding-instances)))]
              [id (x) (if (not (member x binding-instances))
                          (list x)
                          empty)]))]
    (fn-for-wae wae0 empty)))

(test (free-instance-ids (id 'x)) (list 'x))
(test (free-instance-ids (with 'x (num 6) (id 'x))) (list))
(test (free-instance-ids (with 'x (num 6) (id 'y))) (list 'y))
(test (free-instance-ids (with 'x (id 'x) (id 'x))) (list 'x))
(test (free-instance-ids (with 'x (id 'x) (id 'y))) (list 'x 'y))
(test (free-instance-ids (with 'y (num 7)
                               (with 'x (id 'x) (id 'y)))) (list 'x))
(test (free-instance-ids (with 'x (num 7)
                               (with 'y (id 'x)
                                     (id 'y)))) (list))

(test (free-instance-ids (with 'x (num 7)
                               (with 'y  (id 'x)
                                     (add (id 'y) (id 'y))))) (list))

(test (free-instance-ids (with 'x (num 7)
                               (with 'y  (id 'x)
                                     (sub (id 'y) (id 'y))))) (list))
;;
;; Bound Identifiers
;;
;; WAE -> (listof symbol)
;; produce a list of the binder instances of ids.  Need not be unique!
(define (binding-instance-ids wae) empty)

(test (binding-instance-ids (id 'x)) (list))
(test (binding-instance-ids (with 'x (num 6) (id 'x))) (list 'x))
(test (binding-instance-ids (with 'x (num 6) (id 'y))) (list 'x))
(test (binding-instance-ids (with 'x (id 'x) (id 'x))) (list 'x))
(test (binding-instance-ids (with 'x (id 'x) (id 'y))) (list 'x))
(test (binding-instance-ids (with 'y (num 7)
                                  (with 'x (id 'x) (id 'y)))) (list 'y 'x))
(test (binding-instance-ids (with 'x (num 7)
                                  (with 'y (id 'x) (id 'y)))) (list 'x 'y))




;; Exercise:

;; WAE -> (listof symbol)
;; produce a list of the bound identifier references in the given expressions
;; (NOTE: since we want "instances" they need not be unique!)
(define (bound-instance-ids wae) empty)


;;
;; Substitution
;;


;; WAE Symbol NumWAE -> WAE
;; naïvely substitute nw for free instances of x in wae
;; INVARIANT: expr must be closed (equal? (free-instance-ids 
(define (subst wae sub-id nw) (num 0)) ; stub


(test (subst (id 'x) 'x (num 1)) (num 1))
(test (subst (id 'x) 'x (num 2)) (num 2))

(test (subst (id 'y) 'x (num 1)) (id 'y))
(test (subst (num 10) 'x (num 1)) (num 10))
(test (subst (add (id 'x) (num 10)) 'x (num 2)) (add (num 2) (num 10)))


(test (subst (with 'x (num 2) (num 3)) 'x (num 1))
      (with 'x (num 2) (num 3)))
(test (subst (with 'y (num 2) (num 3)) 'x (num 1))
      (with 'y (num 2) (num 3)))

(test (subst (with 'x (num 2) (id 'x)) 'x (num 1))
      (with 'x (num 2) (id 'x)))

(test (subst (with 'y (num 2) (id 'x)) 'x (num 1))
      (with 'y (num 2) (num 1)))


(test (subst (with 'y (id 'x) (id 'x)) 'x (num 1))
      (with 'y (num 1) (num 1)))

(test (subst (with 'x (id 'x) (id 'x)) 'x (num 1))
      (with 'x (num 1) (id 'x)))


;; interp : WAE -> Number
;; consumes a WAE and computes the corresponding number
(define (interp-wae wae) 0) ; stub
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WaeID is Symbol
;; INVARIANT: a WaeID cannot be equal to '+, '-, or 'with
(define (identifier? x)
  (and (symbol? x)
       (not (member x '(+ - with)))))

(define WID0 'x)
(define WID1 'y)


;; WAEFS is one of:
;; - Number
;; - `(+ ,WAEFS ,WAEFS)
;; - `(- ,WAEFS ,WAEFS)
;; - `(with ,WaeID ,WAEFS ,WAEFS)
;; -  WAEID
;; - <any other s-expression>
;; interp. any s-expression, focusing on those that represent WAE expressions.
(define WAEFS1 '{with {x 4} x})
(define WAEFS2 '{with {x 4} {+ x x}})
(define WAEFS3 '{with {x {+ 5 5}} {+ x x}})
(define WAEFS4 (list 'sleep 'is 'precious))

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


;; WAEFS -> WAE
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

(test (parse 5) (num 5))
(test/exn (parse '+) "bad WAE")
(test (parse '(+ 5 (- 3 2))) (add (num 5)
                                  (sub (num 3) (num 2))))
(test/exn (parse '(+ 5 (- 3 #f))) "bad WAE")


(test (parse WAEFS1) WAE1)
(test (parse WAEFS2) WAE2)
(test (parse WAEFS3) WAE3)
(test/exn (parse WAEFS4) "bad WAE")


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
  (interp-wae
   (parse
    (read-from-file fname))))

(test (with-temporary-data-file "{+ 3 7}\n"
        (λ (fname) (interp-file fname)))
      10)

(test (with-temporary-data-file "{+ {- 3 4} 7}\n"
        (λ (fname) (interp-file fname)))
      6)

(test (with-temporary-data-file "{with {x 5} {+ x {with {y 3} x}}}"
        (λ (fname) (interp-file fname)))
      10)

