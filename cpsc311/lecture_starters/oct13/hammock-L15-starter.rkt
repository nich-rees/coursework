#lang plai

(require "../util/parsing.rkt")
(require "../util/test-diverge.rkt")
(define (... . args) (cons '... args)) ;; enables us to use ... in templates
(print-only-errors)

(define DIVERGE-DELAY 5)



;; Hammock - a "lazy" language (really it's *non-strict*)
;; A functional language with:
;; - arithmetic
;; - conditional expressions
;; - pairs/lists
;; - first-class functions


;; Named in memory of Summer  :: sniff! ::

(define-type HG    
  [num (n number?)]
  [isnumz (e HG?)]
  [add (lhs HG?) (rhs HG?)]
  [sub (lhs HG?) (rhs HG?)]
  [id (name symbol?)]
  [fun (param symbol?) (body HG?)]
  [isfunz (e HG?)]
  [app (rator HG?) (arg HG?)]
  [if0 (predicate HG?) (consequent HG?) (alternative HG?)]
  [fix (name symbol?) (body HG?)]
  [pair (left HG?) (right HG?)]
  [ispairz (e HG?)]
  [left (e HG?)]
  [right (e HG?)]
  [mt]
  [ismtz (e HG?)])


;; interp. expressions in a language that supports applying first-class
;; functions. Its syntax is defined by the following BNF:
;; (HG stands for Hammocks Good!)
;; <HG> ::=
;; (ARITHMETIC)
;;          <num>
;;        | {isnumz <HG>} *
;;        | {+ <HG> <HG>}
;;        | {- <HG> <HG>}
;; (IDENTIFIERS)
;;        | {with {<id> <HG>} <HG>}
;;        | <id>
;;        | {set <id> HG}     (NEW VERSION 2.0 FEATURE!!!)
;; (CONDITIONALS)
;;        | {if0 <HG> <HG> <HG>}
;; (FUNCTIONS)
;;        | {<HG> <HG>}
;;        | {fun {<id>} <HG>}
;;        | {isfunz <HG>}
;; (RECURSION)
;;        | {fix <id> <HG>}
;;        | {fixFun <id> <id> <HG>} (equiv. to {<fix> <id> {<fun> {<id>} <HG>}})
;; (PAIRS/LISTS)
;;        | {pair <HG> <HG>}
;;        | {ispairz <HG>}
;;        | {left <HG>}
;;        | {right <HG>}
;;        | {mt}
;;        | {ismtz}

;; Every AE program is a HG program
(define FOUR (num 4))
(define FOURPLUS5 (add FOUR (num 5)))
(define SIXMINUSTHREE (sub (num 6) (num 3)))

;; with and fixFun are syntactic sugar
(define (with x named body) (app (fun x body) named))
(define (fixFun f x body) (fix f (fun x body)))

;; Every WAE program is a HG program
(define WAE-FOUR (with 'x (num 4) (id 'x)))
(define WAE-EIGHT (with 'x (num 4) (add (id 'x) (id 'x))))

(define WAE1-sexp'{with {x {+ 5 5}} {+ x x}})
(define WAE1 (with 'x (add (num 5) (num 5))
                   (add (id 'x) (id 'x))))

(define WAE2-sexp '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})
(define WAE2 (with 'x (add (num 5) (num 5))
                   (with 'y (sub (id 'x) (num 3))
                         (add (id 'y) (id 'y)))))

(define WAE3-sexp '{with {x 5} {+ x {with {x 3} 10}}})
(define WAE3 (with 'x (num 5)
                   (add (id 'x)
                        (with 'x (num 3)
                              (num 10)))))

(define WAE4-sexp '{with {x 5} {+ x {with {x 3} x}}})
(define WAE4 (with 'x (num 5)
                   (add (id 'x)
                        (with 'x (num 3)
                              (id 'x)))))

(define WAE5-sexp '{with {x 5} {+ x {with {y 3} x}}})
(define WAE5 (with 'x (num 5)
                   (add (id 'x)
                        (with 'y (num 3)
                              (id 'x)))))

;; F1WAE *expressions* are also HG expressions
;; (but represented using a different data type!)
(define F1WAE1-sexp '{with {x 5} {double x}})
;;(define F1WAE1 (with 'x (num 5) (app 'double (id 'x))))
(define F1WAE1 (with 'x (num 5) (app (id 'double) (id 'x))))

(define F1WAE2-sexp '{with {x 5} {one-plus x}})
;;(define F1WAE2 (with 'x (num 5) (app 'one-plus (id 'x))))
(define F1WAE2 (with 'x (num 5) (app (id 'one-plus) (id 'x))))

;; Thanks to fix and fun we can encode simple recursive functions
(define HG4-sexp '{fix f {fun {x} {if0 x 9 {f {- x 1}}}}})
(define HG4 (fix 'f (fun 'x (if0 (id 'x)
                                 (num 9)
                                 (app (id 'f) (sub (id 'x) (num 1)))))))

(define HG4b-sexp '{fixFun f {x} {if0 x 9 {f {- x 1}}}})
(define HG4b (fixFun 'f 'x (if0 (id 'x)
                                (num 9)
                                (app (id 'f) (sub (id 'x) (num 1))))))


(define HG5-sexp `{,HG4-sexp 1})
(define HG5 (app HG4 (num 1)))

(define HG5b-sexp `{,HG4b-sexp 1})

(define HG5b (app HG4b (num 1)))

#;
(define (fn-for-hg hg)
  (type-case HG hg
    [num (n) (... n)]
    [isnumz (e) (... (fn-for-hg e))]
    [add (l r) (... (fn-for-hg l)
                    (fn-for-hg r))]
    [sub (l r) (... (fn-for-hg l)
                    (fn-for-hg r))]
    [id (x) (... x)]
    [fun (x body) (... x
                       (fn-for-hg body))]
    [isfunz (e) (... (fn-for-hg e))]
    [app (rator rand) (... (fn-for-hg rator)
                           (fn-for-hg rand))]
    [if0 (p c a)
         (... (fn-for-hg p)
              (fn-for-hg c)
              (fn-for-hg a))]
    [fix (x body) (... x
                       (fn-for-hg body))]
    [pair (l r) (... (fn-for-hg l)
                     (fn-for-hg r))]   
    [ispairz (e) (... (fn-for-hg e))]
    [left (e) (... (fn-for-hg e))]
    [right (e) (... (fn-for-hg e))]
    [mt () (...)]
    [ismtz (e) (... (fn-for-hg e))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HG symbol HG -> HG
;; produce the result of substituting sub-expr for x in expr1
(define (subst expr1 x0 sub-expr)
  (local [(define (recur expr1)
            (type-case HG expr1
              [num (n) (num n)]
              [isnumz (e) (isnumz (recur e))]
              [add (l r) (add (recur l)
                              (recur r))]
              [sub (l r) (sub (recur l)
                              (recur r))]
              [id (x) (if (symbol=? x x0)
                          sub-expr
                          (id x))] ;; INTERESTING
              [fun (x body) (if (symbol=? x x0)
                                (fun x body)
                                (fun x (recur body)))] ;; INTERESTING
              [isfunz (e) (isfunz (recur e))]
              [app (rator rand) (app (recur rator)
                                     (recur rand))] ;; NOT INTERESTING
              [if0 (p c a)
                   (if0 (recur p)
                        (recur c)
                        (recur a))]
              [fix (x body) (if (symbol=? x x0)
                                (fix x body)
                                (fix x (recur body)))] ;; INTERESTING
              [pair (l r) (pair (recur l) (recur r))]   
              [ispairz (e) (ispairz (recur e))]
              [left (e) (left (recur e))]
              [right (e) (right (recur e))]
              [mt () (mt)]
              [ismtz (e) (ismtz (recur e))]))]
    (recur expr1)))


;;
;; Interpreter Values - now a refinement of HG!
;;

;; Value is one of:
;; - (num number)
;; - (fun symbol HG)
;; - (pair HG HG) ;; Notice that pair contains HG's not Values! Similar to fun!
;; - (mt)
;; interp.  Those HG values that count as runtime values
#;
(define (fn-for-value v)
  (match v
    [(num n) (... n)]
    [(fun x e) (... x (fn-for-hg e))]
    [(pair e1 e2) (... (fn-for-hg e1)
                       (fn-for-hg e2))]
    [(mt) (...)]))
      

;; Value -> number
;; produce the number represented by the given value
;; Effect: signal an error if the value does not represent a number
(define (value->num v)
  (match v
    [(num n) n]
    [else (error 'value->num "Bad number: ~a" v)]))

(test (value->num (num 6)) 6)
(test (value->num (num 9)) 9)
(test/exn (value->num (fun 'x (id 'x))) "Bad number")


;; Value -> pair
;; produce the same v if it is a pair
;; Effect: signal an error if the value does not represent a pair
(define (value->pair v)
  (match v
    [(pair l r) v]
    [else (error 'value->pair "Bad pair: ~a" v)]))

(test (value->pair (pair (num 0) (num 1))) (pair (num 0) (num 1)))
(test/exn (value->pair (num 6)) "Bad pair")      


;; (number number -> number) Value Value  -> Value
;; apply num-op to the numbers represented by v1 and v2
;; Effect: signal an error if either argument does not represent a number
(define (num-binop-value num-op v1 v2)
  (let ([n1 (value->num v1)]
        [n2 (value->num v2)])
    (num (num-op n1 n2))))

(test (num-binop-value * (num 5) (num 6)) (num 30))
(test/exn (num-binop-value * (num 5) (fun 'x (id 'x)))
          "Bad number")
(test/exn (num-binop-value * (fun 'x (id 'x)) (num 6))
          "Bad number")
(test/exn (num-binop-value * (fun 'x (id 'x))
                           (fun 'x (id 'x))) "Bad number")

  
;; Value Value -> Value
;; produce the sum of two numbers
;; Effect: signal an error if either argument does not represent a number
(define (add-value v1 v2)
  (num-binop-value + v1 v2))

(test (add-value (num 5) (num 6)) (num 11))
(test/exn (add-value (num 5) (fun 'x (id 'x))) "Bad number")
(test/exn (add-value (fun 'x (id 'x)) (num 6)) "Bad number")
(test/exn (add-value (fun 'x (id 'x))
                     (fun 'x (id 'x))) "Bad number")


;; Value Value -> Value
;; produce the difference of two numbers
;; Effect: signal an error if either argument does not represent a number
(define (sub-value v1 v2)
  (num-binop-value - v1 v2))

(test (sub-value (num 5) (num 6)) (num -1))
(test/exn (sub-value (num 5) (fun 'x (id 'x))) "Bad number")
(test/exn (sub-value (fun 'x (id 'x)) (num 6)) "Bad number")
(test/exn (sub-value (fun 'x (id 'x))
                     (fun 'x (id 'x))) "Bad number")


;; Value -> boolean
;; produce true if v1 represents the number zero, else false
(define (zero-value? v)
  (match v
    [(num n) (zero? n)]    
    [else #f]))

(test (zero-value? (num 7)) #f)
(test (zero-value? (num 0)) #t)
(test (zero-value? (fun 'x (id 'x))) #f)



;; Value Hg -> Value
;; produce the result of applying v1 to e2
;; Effect: signal an error if v1 does not represent a function
(define (apply-value v1 e2)
  (match v1
    [(fun x body)
     (interp/hg (subst body x e2))]
    [else (error 'apply-value "Bad function: ~a" v1)]))

;; apply-value EXAMPLES below interp-hg-acc because of mutual reference

;; symbol HG -> Value
(define (interp-fix f body)
  (interp/hg (subst body f (fix f body))))

;; interp-fix Examples below interp-hg-acc because of mutual reference

;; symbol -> HG
;; produce the value bound to a given identifier, unboxing bindings as needed
;; Effect: signal an error in case of a black hole
(define (interp-id x)
  (error 'interp-hg "Unbound identifier: ~a" x))

(test/exn (interp-id 'x) "Unbound")



;; Value -> Value
;; produce the left member of a pair
;; Effect: Signal an error if the given value is not a pair
(define (left-value v)
  (match-let ([(pair e1 e2) (value->pair v)])
    (interp/hg e1)))

;; left-value examples after interp-hg due to mutual reference


;; Value -> Value
;; produce the right member of a pair
;; Effect: Signal an error if the given value is not a pair
(define (right-value v)
  (match-let ([(pair e1 e2) (value->pair v)])
    (interp/hg e2)))

;; right-value examples after interp-hg due to mutual reference



;; Value -> Value
;; produce a 0 if the value is a number, otherwise 1
(define (isnumz-value v)
  (if (num? v) (num 0) (num 1)))

(test (isnumz-value (num 0)) (num 0))
(test (isnumz-value (fun 'x (id 'x))) (num 1))
(test (isnumz-value (pair (num 0) (num 0))) (num 1))
(test (isnumz-value (mt)) (num 1))


;; Value -> Value
;; produce a 0 if the value is a function, otherwise 1
(define (isfunz-value v)
  (if (fun? v) (num 0) (num 1)))

(test (isfunz-value (num 0)) (num 1))
(test (isfunz-value (fun 'x (id 'x))) (num 0))
(test (isfunz-value (pair (num 0) (num 0))) (num 1))
(test (isfunz-value (mt)) (num 1))


;; Value -> Value
;; produce a 0 if the value is a pair, otherwise 1
(define (ispairz-value v)
  (if (pair? v) (num 0) (num 1)))

(test (ispairz-value (num 0)) (num 1))
(test (ispairz-value (fun 'x (id 'x))) (num 1))
(test (ispairz-value (pair (num 0) (num 0))) (num 0))
(test (ispairz-value (mt)) (num 1))

;; Value -> Value
;; produce a 0 if the value is mt, otherwise 1
(define (ismtz-value v)
  (if (mt? v) (num 0) (num 1)))

(test (ismtz-value (num 0)) (num 1))
(test (ismtz-value (fun 'x (id 'x))) (num 1))
(test (ismtz-value (pair (num 0) (num 0))) (num 1))
(test (ismtz-value (mt)) (num 0))


;; interp-hg : HG -> Value
;; produce the result of interpreting the given expression
;; EFFECTS: Signals an error in case of runtime type error.
(define (interp/hg hg)
  (type-case HG hg
    [num (n) (num n)]
    [isnumz (e) (isnumz-value (interp/hg e))]
    [add (l r) (add-value (interp/hg l)
                          (interp/hg r))]
    [sub (l r) (sub-value (interp/hg l)
                          (interp/hg r))]
    [id (x) (interp-id x)]
    [fun (x body) (fun x body)]
    [isfunz (e) (isfunz-value (interp/hg e))]
    [app (rator rand) (apply-value (interp/hg rator)
                                   rand)] ;; !!! Don't evaluate operand!
    [if0 (p c a)
         (if (zero-value? (interp/hg p))
             (interp/hg c)
             (interp/hg a))]
    [fix (x body) (interp-fix x body)]
    [pair (l r) (pair l r)]   ;; !! (pair is already a value)
    [ispairz (e) (ispairz-value (interp/hg e))]
    [left (e) (left-value (interp/hg e))]
    [right (e) (right-value (interp/hg e))]
    [mt () (mt)]
    [ismtz (e) (ismtz-value (interp/hg e))]))



;; apply-value examples
(test (apply-value (fun 'x (add (id 'x) (num 7)))
                   (num 12))
      (num 19))


(test/exn (apply-value (num 12)
                       (fun 'x (add (id 'x) (num 7))))
          "Bad function")

;; interp-fix examples
(test/diverge (interp-fix 'f (id 'f)) DIVERGE-DELAY)
(test/diverge (interp-fix 'f (add (num 7) (id 'f))) DIVERGE-DELAY)

(test (interp-fix 'f (fun 'x (id 'f)))
      (fun 'x (fix 'f (fun 'x (id 'f)))))


;; left-value examples
(test (left-value (pair (num 0) (num 1))) (num 0))
(test (left-value (pair (add (num 5) (num 7)) (num 1))) (num 12))
(test/exn (left-value (num 6)) "Bad pair")      


;; right-value examples
(test (right-value (pair (num 0) (num 1))) (num 1))
(test (right-value (pair (num 1) (add (num 5) (num 7)))) (num 12))
(test/exn (right-value (num 6)) "Bad pair")      


;; interp-hg examples
(test (interp/hg (ismtz (mt))) (num 0))
(test (interp/hg (ismtz (num 0))) (num 1))
(test (interp/hg (isnumz (num 0))) (num 0))
(test (interp/hg (isnumz (mt))) (num 1))
(test (interp/hg (ispairz (pair (mt) (mt)))) (num 0))
(test (interp/hg (ispairz (num 0))) (num 1))

(test (interp/hg (left (pair (mt) (num 7)))) (mt))
(test (interp/hg (right (pair (mt) (num 7)))) (num 7))


;; Using the built-in recursion
(test
 (interp/hg
  (with '* (fixFun 'mult 'lhs
                   (fun 'rhs
                        (if0 (id 'rhs)
                             (num 0)
                             (add (id 'lhs) (app (app (id 'mult)
                                                      (id 'lhs))
                                                 (sub (id 'rhs)
                                                      (num 1)))))))
        (app (app (id '*)
                  (num 20))
             (num 3))))
 (num 60))


;; DeepValue is one of:
;; - (num number)
;; - (fun symbol HG)
;; - (pair DeepValue DeepValue) ;; compare to Value!
;; - (mt)
;; interp.  Those HG values that contain no pending computations

;; Value -> DeepValue
;; force all pending computations in the given value
;(define (force v) (num 7)) ; stub
(define (force-hg v)
  (match v
    [(num n) (num n)]
    [(fun x e) (fun x e)]
    [(pair e1 e2) (pair (force-hg (interp/hg e1))
                        (force-hg (interp/hg e2)))]
    [(mt) (mt)]))



(test (force-hg (num 7)) (num 7))
(test (force-hg (pair (add (num 6) (num 6))
                      (if0 (num 0)
                           (mt)
                           (num 9))))
      (pair (num 12) (mt)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hg-focused-sexp (hgfs) is one of:
;; - number
;; - `{isnumz ,hgfs}
;; - `{+ ,hgfs ,hgfs}
;; - `{- ,hgfs ,hgfs}
;; - `{with ,identifier ,hgfs ,hgfs
;; -  identifier
;; - `{set-var ,identifier ,hgfs} ;; !!! NEW THING!!!
;; - `{fun ,identifier hgfs}
;; - `{isfunz ,hgfs}
;; - `{,hgfs ,hgfs}
;; - `{fix ,identifier ,hgfs}
;; - `{fixFun ,identifier {,identifier} ,hgfs}
;; - `{if0 ,hgfs ,hgfs ,hgfs}
;; - `{pair ,hgfs ,hgfs}
;; - `{ispairz ,hgfs}
;; - `{left ,hgfs}
;; - `{right ,hgfs}
;; - `{mt}
;; - `{ismtz ,hgfs}
;; - `{newbox ,hgfs}
;; - `{isboxz ,hgfs}
;; - `{setbox ,hgfs ,hgfs}
;; - `{openbox ,hgfs}
;; - "any other s-expression"
;; where identifier is any symbol except +, -, with, if0, or fun
;; interp.  any s-expression, but with a focus on those that represent
;; HG expressions.


(define (identifier? x)
  (and (symbol? x)
       (not (member x '(+ - with if0 fun fix fixFun)))))

#;
(define (fn-for-hg-focused-sexp sexp)
  (match sexp
    [`,n
     #:when (number? n)
     (... n)]
    [`{isnumz ,sexp1} (... (fn-for-hg-focused-sexp sexp1))]
    [`{+ ,sexp1 ,sexp2}
     (... (fn-for-hg-focused-sexp sexp1)
          (fn-for-hg-focused-sexp sexp2))]
    [`{- ,sexp1 ,sexp2}
     (... (fn-for-hg-focused-sexp sexp1)
          (fn-for-hg-focused-sexp sexp2))]
    [`{with {,x ,sexp1} ,sexp2}
     #:when (identifier? x)
     (... x
          (fn-for-hg-focused-sexp sexp1)
          (fn-for-hg-focused-sexp sexp2))]
    [`,x
     #:when (identifier? x)
     (... x)]
    [`{setvar ,x ,sexp1} (... x (fn-for-hg-focused-sexp sexp1))]
    [`{if0 ,sexp1 ,sexp2 ,sexp3}
     (... (fn-for-hg-focused-sexp sexp1)
          (fn-for-hg-focused-sexp sexp2)
          (fn-for-hg-focused-sexp sexp3))]
    [`{fun {,x} ,sexp1}
     #:when (identifier? x)
     (... x
          (fn-for-hg-focused-sexp sexp1))]
    [`{isfunz ,sexp1} (... (fn-for-hg-focused-sexp sexp1))]
    [`{fix ,f ,sexp1}
     #:when (identifier? f)
     (... f
          (fn-for-hg-focused-sexp sexp1))]
    [`{fixFun ,f {,x} ,sexp1}
     #:when (and (identifier? f) (identifier? x))
     (... f
          x
          (fn-for-hg-focused-sexp sexp1))]
    [`{pair ,sexp1 ,sexp2}
     (... (fn-for-hg-focused-sexp sexp1)
          (fn-for-hg-focused-sexp sexp2))]
    [`{ispairz ,sexp1} (... (fn-for-hg-focused-sexp sexp1))]
    [`{left ,sexp1} (... (fn-for-hg-focused-sexp sexp1))]
    [`{right ,sexp1} (... (fn-for-hg-focused-sexp sexp1))]
    [`{mt} (...)]
    [`{ismtz ,sexp1} (... (fn-for-hg-focused-sexp sexp1))]
    [`{newbox ,sexp1} (... (fn-for-hg-focused-sexp sexp1))]
    [`{isboxz ,sexp1} (... (fn-for-hg-focused-sexp sexp1))]
    [`{setbox ,sexp1 ,sexp2}
     (... (fn-for-hg-focused-sexp sexp1)
          (fn-for-hg-focused-sexp sexp2))]
    [`{openbox ,sexp1} (... (fn-for-hg-focused-sexp sexp1))]
    ;; Notice that application is now the last focused case...
    [`{,sexp1 ,sexp2}
     (... (fn-for-hg-focused-sexp sexp1)
          (fn-for-hg-focused-sexp sexp1))]    
    [else (... sexp)] ))



;; parse-expr : s-expression -> HG
;; parse the given s-expression into a HG expression
;; EFFECT: signals an error on failure
(define (parse-expr sexp)
  (match sexp
    [`,n #:when (number? n) (num n)]
    [`{isnumz ,sexp1} (isnumz (parse-expr sexp1))]
    [`{+ ,lhs ,rhs} (add (parse-expr lhs) (parse-expr rhs))]
    [`{- ,lhs ,rhs} (sub (parse-expr lhs) (parse-expr rhs))]
    [`{with {,id ,named-exp} ,body}
     #:when (identifier? id)
     ;; desugar with into anonymous function application
     (with id (parse-expr named-exp) (parse-expr body))]
    [`,x #:when (identifier? x) (id x)]
    [`{if0 ,pred ,conseq ,altern}
     (if0 (parse-expr pred) (parse-expr conseq) (parse-expr altern))]
    ;; Notice that application is now last...
    [`{fun {,x} ,sexp1}
     #:when (identifier? x)
     (fun x (parse-expr sexp1))]
    [`{isfunz ,sexp1} (isfunz (parse-expr sexp1))]    
    [`{fix ,f ,sexp1}
     #:when (identifier? f)
     (fix f (parse-expr sexp1))]
    [`{fixFun ,f {,x} ,sexp1}
     #:when (and (identifier? f) (identifier? x))
     ;; desugar to fix and fun
     (fixFun f x (parse-expr sexp1))]
    [`{pair ,sexp1 ,sexp2}
     (pair (parse-expr sexp1)
           (parse-expr sexp2))]
    [`{ispairz ,sexp1} (ispairz (parse-expr sexp1))]
    [`{left ,sexp1} (left (parse-expr sexp1))]
    [`{right ,sexp1} (right (parse-expr sexp1))]
    [`{mt} (mt)]
    [`{ismtz ,sexp1} (ismtz (parse-expr sexp1))]
    [`{,rator-exp ,arg-exp} 
     (app (parse-expr rator-exp) (parse-expr arg-exp))]
    [_ (error 'parse-expr "bad expression ~a" sexp)]))


(test (parse-expr WAE1-sexp) WAE1)
(test (parse-expr WAE2-sexp) WAE2)
(test (parse-expr WAE3-sexp) WAE3)
(test (parse-expr WAE4-sexp) WAE4)
(test (parse-expr WAE5-sexp) WAE5)

(test (parse-expr F1WAE1-sexp) F1WAE1)
(test (parse-expr F1WAE2-sexp) F1WAE2)


(test (parse-expr HG4-sexp) HG4)
(test (parse-expr HG4b-sexp) HG4b)
(test (parse-expr HG5-sexp) HG5)
(test (parse-expr HG5b-sexp) HG5b)

;;
;; PUTTING IT ALL TOGETHER  - an interpreter of files on disk
;;

;; s-expression -> number
;; produce the result of interpreting the HG program represented as an sexp.
;; EFFECT: signals an error if interpretation signals a runtime error.
(define (interp-sexp sexp)
  (interp/hg
   (parse-expr sexp)))


;; string -> number
;; produce the result of interpreting the HG program in input
;; EFFECT: signals an error if interpretation signals a runtime error.
(define (interp-string pgm)
  (interp/hg
   (parse-expr
    (read-from-string pgm))))


;; string -> number
;; produce the result of interpreting the HG stored in the file fname
;; EFFECT: signals an error if no file fname contains an AE representation
;;         or if interpretation signals a runtime error.
(define (interp-file fname)
  (interp/hg
   (parse-expr
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


(test (with-temporary-data-file
          "{with {double {fun {x} {+ x x}}} {with {x 5} {double x}}}"
        (λ (fname) (interp-file fname)))
      (num 10))


(test (with-temporary-data-file
          "{with {* {fixFun mult {lhs} {fun {rhs}
                                {if0 rhs
                                     0
                                     {+ lhs {{mult lhs} {- rhs 1}}}}}}}
             {{* 20} 3}}"
        (λ (fname) (interp-file fname)))
      (num 60))

(test (with-temporary-data-file
          "{with {* {fix mult {fun {lhs} {fun {rhs}
                                {if0 rhs
                                     0
                                     {+ lhs {{mult lhs} {- rhs 1}}}}}}}}
             {{* 20} 3}}"
        (λ (fname) (interp-file fname)))
      (num 60))


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
        (λ (fname) (force-hg (interp-file fname))))
      (pair (num 10)
            (pair (num 8)
                  (pair (num 6)
                        (pair (num 4)
                              (pair (num 2)
                                    (mt)))))))

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
 (force-hg
  (interp-sexp
   `{with {take ,TAKE}
          {{take 3} {pair 4 {pair 5 {pair 6 {pair 7 {mt}}}}}}}))
 (pair (num 4) (pair (num 5) (pair (num 6) (mt)))))

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

(test (force-hg (interp-sexp `{left {right ,NATS}}))
      (num 2))

(test (force-hg (interp-sexp FIRST-FIVE))
      (pair (num 1)
            (pair (num 2)
                  (pair (num 3)
                        (pair (num 4)
                              (pair (num 5)
                                    (mt)))))))

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

(test (force-hg (interp-sexp `{{,TAKE 5}
                               {,CYCLE {pair 3 {pair 2 {pair 1 {mt}}}}}}))
      (pair (num 3)
            (pair (num 2)
                  (pair (num 1)
                        (pair (num 3)
                              (pair (num 2)
                                    (mt)))))))

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

(test (force-hg (interp-sexp `{{{,ZIPOP {fun {a} {fun {b} {pair a b}}}}
                                {pair 1 {pair 2 {pair 3 {mt}}}}}
                               {pair 3 {pair 2 {pair 1 {mt}}}}}))
      (pair (pair (num 1) (num 3))
            (pair (pair (num 2) (num 2))
                  (pair (pair (num 3) (num 1)) (mt)))))

;; fibs
(define FIBS
  `{fix recur
        {pair 1
              {pair 1
                    {{{,ZIPOP {fun {a} {fun {b} {+ a b}}}}
                      recur}
                     {right recur}}}}})

(test
 (force-hg (interp-sexp `{{,TAKE 12} ,FIBS}))
 (pair (num 1)
       (pair (num 1)
             (pair (num 2)
                   (pair (num 3)
                         (pair (num 5)
                               (pair (num 8)
                                     (pair (num 13)
                                           (pair (num 21)
                                                 (pair (num 34)
                                                       (pair (num 55)
                                                             (pair
                                                              (num 89)
                                                              (pair
                                                               (num 144)
                                                               (mt))))))))))))))