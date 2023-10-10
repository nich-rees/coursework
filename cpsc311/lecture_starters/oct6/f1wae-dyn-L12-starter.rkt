#lang plai
(require "../util/parsing.rkt")
(require "../util/env.rkt")

(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

;; F1WAE - Expressions for a language with first-order (second-class) functions
;; Augmented with "if zero" operator
;; DYNAMICALLY SCOPED EDITION

(define-type F1WAE    
  [num (n number?)] 
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [sub (lhs F1WAE?) (rhs F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?) (arg F1WAE?)]
  [if0 (predicate F1WAE?) (consequent F1WAE?) (alternative F1WAE?)])
;; interp. expressions in a language that supports applying first-order
;; functions. Its syntax is defined by the following BNF:
;; <F1WAE> ::= <num>
;;           | {+ <F1WAE> <F1WAE>}
;;           | {- <F1WAE> <F1WAE>}
;;           | {with {<id> <F1WAE>} <F1WAE>}
;;           | <id>
;;           | {<id> <F1WAE>}
;;           | {if0 <F1WAE> <F1WAE> <F1WAE>}
;; Every AE program is an F1WAE program
;; Every AE program is a WAE program
(define AE1 (num 4))
(define AE2 (add AE1 (num 5)))
(define AE3 (sub (num 6) (num 3)))

;; Every WAE program is an F1WAE program
(define WAES4 '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})
(define WAE4 (with 'x (add (num 5) (num 5))
                   (with 'y (sub (id 'x) (num 3))
                         (add (id 'y) (id 'y)))))

(define WAES5 '{with {x 5} {+ x {with {x 3} 10}}})
(define WAE5 (with 'x (num 5) (add (id 'x) (with 'x (num 3) (num 10)))))

(define WAES6 '{with {x 5} {+ x {with {x 3} x}}})
#;(let ([x 5])
    (+ x (let ([x 3]) x)))
(define WAE6 (with 'x (num 5) (add (id 'x) (with 'x (num 3) (id 'x)))))

(define WAES7 '{with {x 5} {+ x {with {y 3} x}}})
#;(let ([x 5])
    (+ x (let ([y 3]) x)))
(define WAE7 (with 'x (num 5) (add (id 'x) (with 'y (num 3) (id 'x)))))

;; F1WAE expressions waiting for function definitions
(define F1WAES1 '{with {x 5} {double x}})
(define F1WAE1 (with 'x (num 5) (app 'double (id 'x))))
(define F1WAES2 '{with {x 5} {one-plus x}})
(define F1WAE2 (with 'x (num 5) (app 'one-plus (id 'x))))

;; Conditional expressions using if0
(define F1WAES3 '{if0 0 5 6})
(define F1WAE3 (if0 (num 0) (num 5) (num 6)))

(define F1WAES4 '{if0 1 5 6})
(define F1WAE5 (if0 (num 1) (num 5) (num 6)))

#;
(define (fn-for-f1wae f)
  (type-case F1WAE f
    [num (n) (... n)]
    [add (l r) (... (fn-for-f1wae l)
                    (fn-for-f1wae r))]
    [sub (l r) (... (fn-for-f1wae l)
                    (fn-for-f1wae r))]
    [with (id named body)
          (... id
               (fn-for-f1wae named)
               (fn-for-f1wae body))]
    [id (x) (... x)]
    [app (fun-name arg) (... fun-name (fn-for-f1wae arg))]
    [if0 (p c a)
         (... (fn-for-f1wae p)
              (fn-for-f1wae c)
              (fn-for-f1wae a))]))


(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])
;; interp. a first-order function definition with a name, formal parameter,
;;         and body.  Its syntax is defined by following BNF:
;; <FunDef> ::= {define-fn {<id> <id>} <F1WAE> }

(define FD1-sexpr '{define-fn {double x} {+ x x}})
(define FD1 (fundef 'double 'x (add (id 'x) (id 'x))))
(define FD2-sexpr '{define-fn {one-plus x} {+ x 1}})
(define FD2 (fundef 'one-plus 'x (add (id 'x) (num 1))))
(define FD3-sexpr '{define-fn {freevar x} {+ x y}})
(define FD3 ...)

(define EVEN-sexp '{define-fn {even x} {if0 x 0 {odd x}}})
(define EVEN (fundef 'even 'x (if0 (id 'x) (num 0) (app 'odd (id 'x)))))
(define ODD-sexp '{define-fn {odd x} {if0 x 1 {even x}}})
(define ODD (fundef 'odd 'x (if0 (id 'x) (num 1) (app 'even (id 'x)))))


;; Prog is (cons (listof FunDef) F1WAE)
;; interp.  A full program in the F1WAE language.  Its syntax is defined by
;;          the following BNF:
;; <Prog> ::= <F1WAE>
;;          | <FunDef> <Prog>
;; NOTE: The concrete (on-disk) syntax for <Prog> is NOT wrapped in
;; outermost brackets, so does not read as a single s-expression,
;; but rather a sequence of them.  Our internal syntax
;; representation will still be a list of s-expressions.
(define IS-SEVEN-EVEN? (cons (list EVEN ODD) (app 'even (num 7))))
(define IS-SEVEN-ODD? (cons (list EVEN ODD) (app 'even (num 7))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; F1WAE identifier F1WAE -> F1WAE
;; Substitute f1wae2 for free instances of 'x in f1wae1
(define (subst-f1wae  f1wae1 x f1wae2)
  (type-case F1WAE f1wae1
    [num (n) (num n)]
    [add (l r) (add (subst-f1wae l x f1wae2)
                    (subst-f1wae r x f1wae2))]
    [sub (l r) (sub (subst-f1wae l x f1wae2)
                    (subst-f1wae r x f1wae2))]
    [with (x0 named body)
          (let ([subst-named (subst-f1wae named x f1wae2)])
            (if (symbol=? x0 x)
                (with x0 subst-named body)
                (with x0 subst-named
                      (subst-f1wae body x f1wae2))))]
    [id (x0) (if (symbol=? x0 x)
                 f1wae2
                 (id x0))]
    [app (fun-name arg) (app fun-name (subst-f1wae arg x f1wae2))]
    [if0 (p c a)
         (if0 (subst-f1wae p x f1wae2)
              (subst-f1wae c x f1wae2)
              (subst-f1wae a x f1wae2))]))

(test (subst-f1wae (id 'x) 'x (num 1)) (num 1))
(test (subst-f1wae (id 'x) 'x (num 2)) (num 2))

(test (subst-f1wae (id 'y) 'x (num 1)) (id 'y))
(test (subst-f1wae (num 10) 'x (num 1)) (num 10))
(test (subst-f1wae (add (id 'x) (num 10)) 'x (num 2)) (add (num 2) (num 10)))


(test (subst-f1wae (with 'x (num 2) (num 3)) 'x (num 1))
      (with 'x (num 2) (num 3)))
(test (subst-f1wae (with 'y (num 2) (num 3)) 'x (num 1))
      (with 'y (num 2) (num 3)))

(test (subst-f1wae (with 'x (num 2) (id 'x)) 'x (num 1))
      (with 'x (num 2) (id 'x)))

(test (subst-f1wae (with 'y (num 2) (id 'x)) 'x (num 1))
      (with 'y (num 2) (num 1)))


(test (subst-f1wae (with 'y (id 'x) (id 'x)) 'x (num 1))
      (with 'y (num 1) (num 1)))

(test (subst-f1wae (with 'x (id 'x) (id 'x)) 'x (num 1))
      (with 'x (num 1) (id 'x)))


;; identifier (listof FunDef) -> Fundef
;; produce the named FunDef from the given list
;; Effect: signal an error on failure.
(define (lookup-fundef f fundefs)
  (let ([result (findf (λ (fd) (symbol=? f (fundef-fun-name fd))) fundefs)])
    (if result
        result
        (error "No such function: ~a" f))))

(test/exn (lookup-fundef 'double (list)) "")
(test (lookup-fundef 'double (list FD1 FD2)) FD1)
(test (lookup-fundef 'double (list FD2 FD1)) FD1)
  
;; F1WAE (listof FunDef) -> Number
;; interpret the expression f1wae in the context of the fundefs
(define (interp-f1wae f1wae0 fundefs)
  ;; Accumulator bindings is Env
  ;; Invariant: bindings (in inside-out order) of identifiers to values
  ;;            *in the surrounding context*
  ;;            (c.f. lexical scope: "*due to pending substitutions*")
  (local [(define (interp-f1wae f1wae bindings)
            (type-case F1WAE f1wae
              [num (n) n]
              [add (l r) (+ (interp-f1wae l bindings)
                            (interp-f1wae r bindings))]
              [sub (l r) (- (interp-f1wae l bindings)
                            (interp-f1wae r bindings))]
              [with (x named body)
                    (let ([val (interp-f1wae named bindings)])
                      #;(let ([subst-body (subst-f1wae body x (num val))])
                          (interp-f1wae subst-body))
                      (interp-f1wae body (extend-env bindings x val)))]
              [id (x) (with-handlers ([exn:fail?
                                       (λ (_)
                                         (error "Unbound identifier ~a" x))])
                        (lookup-env bindings x))]
              [app (fun-name arg)
                   (let ([fundef (lookup-fundef fun-name fundefs)])
                     (let ([val (interp-f1wae arg bindings)])
                       #;(let ([subst-body (subst-f1wae (fundef-body fundef)
                                                        (fundef-arg-name fundef)
                                                        (num val))])
                           (interp-f1wae subst-body))
                       (interp-f1wae
                        (fundef-body fundef)
                        (extend-env bindings ; DYNAMIC SCOPING (c.f. empty-env)
                                    (fundef-arg-name fundef) val))))]
              [if0 (p c a)
                   (if (zero? (interp-f1wae p bindings))
                       (interp-f1wae c bindings)
                       (interp-f1wae a bindings))]))]
    (interp-f1wae f1wae0 empty-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; F1WAEFS is one of:
;; - number
;; - `(+ ,F1WAEFS ,F1WAEFS)
;; - `(- ,F1WAEFS ,F1WAEFS)
;; - `(with ,identifier ,F1WAEFS ,F1WAEFS)
;; -  identifier
;; - `(,identifier ,F1WAEFS)
;; - `(if0 ,F1WAEFS ,F1WAEFS ,F1WAEFS)
;; - <any other s-expression>
;; where identifier is any symbol except +, -, with, if0, or define-fn
;; interp. any s-expression, focusing on those that represent F1WAE expressions.

(define (identifier? x)
  (and (symbol? x)
       (not (member x '(+ - with if0 define-fn)))))

#;
(define (fn-for-f1waefs sexp)
  (match sexp
    [`,n
     #:when (number? n)
     (... n)]
    [`(+ ,sexp1 ,sexp2)
     (... (fn-for-f1waefs sexp1)
          (fn-for-f1waefs sexp2))]
    [`(- ,sexp1 ,sexp2)
     (... (fn-for-f1waefs sexp1)
          (fn-for-f1waefs sexp2))]
    [`(with (,x ,sexp1) ,sexp2)
     #:when (identifier? x)
     (... x
          (fn-for-f1waefs sexp1)
          (fn-for-f1waefs sexp2))]
    [`,x
     #:when (identifier? x)
     (... x)]
    [`(,f ,sexp1)
     #:when (identifier? f)
     (... f (fn-for-f1waefs sexp1))]
    [`(if0 ,sexp1 ,sexp2 ,sexp3)
     (... (fn-for-f1waefs sexp1)
          (fn-for-f1waefs sexp2)
          (fn-for-f1waefs sexp3))]
    [else (... sexp)] ))


;; FunDefFS is one of:
;; - `{define-fn {,identifier ,identifier} ,F1WAEFS}
;; - <any other s-expression>
;; where identifier is any symbol except +, -, with, if0, or define-fn
;; interp. any s-expression, focusing on those that represent F1WAE expressions.
(define FDFS1 '{define-fn {foo x} y})
(define FDFS2 '{define-fn {bar z} {+ z 1}})
(define FDFS3 #\g)

#;
(define (fn-for-fundeffs sexp)
  (match sexp
    [`{define-fn {,fun-name ,arg-name} ,body}
     #:when (and (identifier? fun-name)
                 (identifier? arg-name))
     (... fun-name arg-name (fn-for-f1waefs body))]
    [_ (error 'parse-fundef "bad function definition: ~a" sexp)]))


;; parse-expr : s-expression -> F1WAE
;; parse the given s-expression into a F1WAE expression
;; EFFECT: signals an error on failure
(define (parse-expr sexp)
  (match sexp
    [`,n #:when (number? n) (num n)]
    ; we're accepting ANY symbol as an ID reference, unlike our usual
    ; it's worth thinking what impact that will have on the language!
    [`{+ ,lhs ,rhs} (add (parse-expr lhs) (parse-expr rhs))]
    [`{- ,lhs ,rhs} (sub (parse-expr lhs) (parse-expr rhs))]
    [`{with {,id ,named-exp} ,body}
     #:when (symbol? id)
     (with id (parse-expr named-exp)
           (parse-expr body))]
    [`,x #:when (identifier? x) (id x)]
    [`{,fun-name ,arg-exp} #:when (symbol? fun-name)
                           (app fun-name (parse-expr arg-exp))]
    [`{if0 ,pred ,conseq ,altern}
     (if0 (parse-expr pred) (parse-expr conseq) (parse-expr altern))]
    [_ (error 'parse "bad expression ~a" sexp)]))


;; parse-fundef: s-expression -> FunDef
;; parse the given s-expression into an internal function representation
;; EFFECTS: signals an error on failure
(define (parse-fundef sexp)
  (match sexp
    [`{define-fn {,fun-name ,arg-name} ,body}
     #:when (and (identifier? fun-name)
                 (identifier? arg-name))
     (fundef fun-name arg-name (parse-expr body))]
    [_ (error 'parse-fundef "bad function definition: ~a" sexp)]))


(test (parse-fundef '{define-fn {foo x} y}) (fundef 'foo 'x (id 'y)))
(test (parse-fundef '{define-fn {bar z} {+ z 1}})
      (fundef 'bar 'z (add (id 'z) (num 1))))
(test/exn (parse-fundef '{broken {x y} 1}) "bad function") 


;; <FProg> ::= <F1WAE>
;;           | <FunDef> <FProg>

;; FProg is one of:
;; - (cons F1WAE empty)
;; - (cons FunDef FProg)

#;
(define (fn-for-fprog fp)
  (cond [(empty? (rest fp)) (... (fn-for-f1wae (first fp)))]
        [else (... (fn-for-fundef (first fp))
                   (fn-for-fprog (rest fp)))]))


;; Fprog -> (cons F1WAE (listof FunDef))
;; parse the F1WAE Program into an expression and list of functions
;; Effect: signals an erro upon bad parse
(define (parse-fprog initial-fp)
  (local
    ;; Accumulator fundefs is (listof FunDef)
    ;; Invariant: contains the FunDef representions of surrounding function defs
    [(define (parse-fprog--acc fp fundefs)
       (cond [(empty? (rest fp)) (cons (parse-expr (first fp))
                                       fundefs)]
             [else (parse-fprog--acc (rest fp)
                                     (cons (parse-fundef (first fp))
                                           fundefs))]))]
    (parse-fprog--acc initial-fp empty)))


(test (parse-fprog (list '{define-fn {double x} {+ x x}}
                         '{with {x 5} {double x}}))
      (cons F1WAE1 (list FD1)))


;;
;; PUTTING IT ALL TOGETHER  - an interpreter of files on disk
;;

;; string -> number
;; produce the result of interpreting the AE stored in the file fname
;; EFFECT: signals an error if no file fname contains an AE representation
(define (interp-file fname)
  (match-let ([`(,f1wae . ,fundefs) (parse-fprog (read-from-file* fname))])
    (interp-f1wae f1wae fundefs)))

(test (with-temporary-data-file "{+ 3 7}\n"
        (λ (fname) (interp-file fname)))
      10)

(test (with-temporary-data-file "{+ {- 3 4} 7}\n"
        (λ (fname) (interp-file fname)))
      6)

(test (with-temporary-data-file "{with {x 5} {+ x {with {y 3} x}}}"
        (λ (fname) (interp-file fname)))
      10)

(test (with-temporary-data-file
          "{define-fn {double x} {+ x x}}\n{with {x 5} {double x}}"
        (λ (fname) (interp-file fname)))
      10)

(test (local [(define pgm
                "{define-fn {even x} {if0 x 1 {odd {- x 1}}}}
                 {define-fn {odd x} {if0 x 0 {even {- x 1}}}}
                 {even 9}")]
        (with-temporary-data-file pgm
          (λ (fname) (interp-file fname))))
      0)

(test (local [(define pgm
                "{define-fn {f x} y}
                     {with {y 7}
                           {f 6}}")]
        (with-temporary-data-file pgm
          (λ (fname) (interp-file fname))))
      7)

