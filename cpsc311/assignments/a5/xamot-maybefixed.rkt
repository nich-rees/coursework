#lang plai
(print-only-errors #t)
(require racket/stream)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

(require "env-data.rkt")
(require "test-diverge.rkt")


;;
;; Part 1: Xamot: a language for structured description of,
;; and manipulation of, character strings.
;;

;; Xamot is a language for assembling strings.
;; As a language for printing strings, it is relatively simple,
;; much like the WAE language from early in the course.
;; Xamot programs produce strings as values, but may also
;; signal an error in case of an unbound identifier.

;; - a character evaluates to a single-character string
;; - a string evaluates to itself
;; - {and a b} concatenates the results of a and b
;; - {nil} yields the empty string, so
;;   {and a {nil}} and {and {nil} a} produce the same value as a.

;; Like WAE, Xamot has identifier bindings, but with some enhancements.
;; Just like in WAE, one can write {with {x xt1} xt2}, such that xt2 can
;; refer to x.  For example:
#;
'{with {a "yo"}
       {and a a}}

;; evaluates to "yoyo"

;; However, Xamot also supports *array bindings* in whose body a particular
;; element of an array may be referenced.  For example:
#;
'{with {array a "way" "free"}
       {and {aref a 1} {aref a 0}}}

;; evaluates to "freeway"

;; Since arrays are not values in Xomat, it is an error to refer to an array
;; using a standard identifier reference.  Similarly, it is an error to refer
;; to a standard reference using aref, or to refer to an out-of-bound index
;; for an array.

;; Xamot is a call-by-name language.  At first, this may seem to make little
;; difference for such a simple language, but this semantics allows us to
;; quite flexibly manipulate string outputs.


;; STRING MANIPULATION: OVERRIDES

;; The Xamot interpreter takes not only a Xamot program, but also a list of
;; *overrides*: a list of instructions for when to replace a particular
;; identifier reference at runtime with an arbitrary Xamot expression.
;; This facility allows us to quite flexibly modify programs.

;; For example, given the program above:
#;
'{with {array a "way" "free"}
       {and {aref a 1} {aref a 0}}}

;; we can supplement the program with the override
#;
'{
  { {path {aref a 1}} "high" }
  }

;; and then it will print "highway"
;; the override is made up of two parts:
;; 1) a path:
;; 2) a replacement:


;; To describe a path, it helps to see think about how an identifier
;; *reference* can backward chain to the expression that defines it,
;; which may involve other identifier references.  
;; Consider the program:
#;
'{with {a "hi"}
       {with {b a}
             {with {c a}
                   {and b c}}}}
;; which typically prints "hihi"

;; Instead of thinking about stepping, think about working your way *up*
;; from the reference to b. the reference to b is defined by a. So in this
;; situation, we use the path {path a b} to describe the reference to a that
;; arises because of the reference to b.  On the other hand, {path a c}
;; describes a reference to a that is ultimately due to a reference to c.
;; We can use these paths to differentiate the two expressions that ultimately
;; depend on a's binding, and manipulate the output of this program accordingly.

;; If we supplement the program with the override:
#;
'{
  { {path a b} "mo"}
  { {path a c} "jo"}
  }
;; then the program yields "mojo:.  However, we may wish that *any* reference to
;; a be replaced with a common value, regardless of which other references may
;; be in the dynamic reference path. To achieve this, we can end a path with a
;; wildcard, namely a *:
#;
'{
  { {path a *} "mu"}
  }

;; when supplied alongside our program, the result is "mumu" since this pattern
;; matches both dynamic contexts.

;; overrides can inject arbitrary expressions, and those expressions can even
;; refer to identifiers that are bound at the site of the reference. For
;; instance, we can supplement the earlier program:
#;
'{with {array a "way" "free"}
       {and {aref a 1} {aref a 0}}}

;; with the override
#;
'{
  { {path {aref a 1}} {aref a 0}}
  { {path {aref a 0}} {aref a 1}}
  }
;; to yield "wayfree".  This may appear as though it would lead to an infinite
;; regress, but it does not, because overriden identifier references are
;; considered to be part of the dynamic reference path.  So for example,
;; augmenting this override with:
#;
'{
  { {path {aref a 1}} {aref a 0}}
  { {path {aref a 0}} {aref a 1}}
  { {path {aref a 1} {aref a 0}} " to go!"}
  }

;; yields "way to go!"  This is because the last path refers to what happens
;; when the second argument to and, {aref a 0} gets overridden with {aref a 1}
;; and then the replacement {aref a 1} is evaluated, matching the final
;; override's path.

;; Thanks to wildcards, it *is* possible to diverge: the simplest example is the
;;program
#;
'doh
;; with the override
#;
'{
  { {path doh *} doh}
  }
;; which keeps overriding doh with doh until the current dynamic context
;; uses up too much memory and the interpreter crashes.


;; This language design approach, where new code can be weaved into a
;; preëxisting program, is a simple example of Aspect-Oriented Programming,
;; a language design technique pioneered by world-renowned PL researcher and
;; UBC meme sensation Gregor Kiczales.


;; PROBLEM 1: Redesign the Xamot Interpreter so that it uses an effect
;; abstraction to manage the environment, the current path, and the override
;; table.


;; XTID is Symbol
;; INVARIANT: an xtid may not be any Xamot symbol
;; interp. identifier from the Xamot language

;; Any -> Boolean
;; produce true if x is a valid Xamot identifier, otherwise false
(define (xtid? x)
  (and (symbol? x)
       (not (member x '(and nil with array aref)))))



(define-type Xamot
  [char/xt (c char?)]
  [and/xt (lhs Xamot?) (rhs Xamot?)]
  [nil/xt]
  [with/xt (x xtid?) (xt xtbinding?) (body Xamot?)]
  [id/xt (x xtid?)]
  [aref/xt (lst xtid?) (n number?)])
;; interp. internal representation of a Xamot program
;; <Binding> ::= {<xtid> <Xamot>}
;;             | {array <xtid> <Xamot>*}
;;
;; <Xamot> ::= | <char>
;;             | <string>
;;             | {and <Xamot> <Xamot>}
;;             | {nil}
;;             | {with <Binding> <Xamot>}
;;             | <xtid>
;;             | {aref <xtid> <num>}
;; where a <string> is a conjunction of <char>s

;; String -> TOMAX
;; produce a Tomax expression equivalent to the given string
(define (str/xt str)
  (foldr (λ (c rs) (and/xt (char/xt c) rs)) (nil/xt) (string->list str)))


(define XT1 (char/xt #\q))
(define XT2 (char/xt #\r))
(define XT3 (and/xt XT1 XT2))
(define XT4 (and/xt XT3 (nil/xt)))
(define XT5 (and/xt (nil/xt) XT4))
  

(define (fn-for-xt xt)
  (type-case Xamot xt
    [char/xt (c) (... c)]
    [and/xt (lhs rhs) (... (fn-for-xt lhs)
                           (fn-for-xt rhs))]
    [nil/xt () (...)]
    [with/xt (x xtb body)
             (... x
                  (fn-for-xtb xtb)
                  (fn-for-xt body))]
    [id/xt (x) (... x)]
    [aref/xt (x n) (... x n)]))

;; XTBinding is one of
;; - Xamot
;; - (listof Xamot)
;; interp. binding to which a Xamot expression is bound,
;;    either a single Xamot expression (for elements)
;;    or a list of Xamot expressions (for "arrays")

;; Any -> Boolean
;; produce true if x is an XTBinding, otherwise false
(define (xtbinding? x)
  (or (Xamot? x)
      ((listof Xamot?) x)))

(define XTB1 empty)
(define XTB2 (nil/xt))
(define XTB3 (list (char/xt #\a) (char/xt #\x) (char/xt #\e)))

(define (fn-for-xtb xtb)
  (cond [(Xamot? xtb) (... (fn-for-xt xtb))]
        [else ; (cons? xtb)
         (... (#;fn-for-loxt xtb))]))


;; XTValue is String 
;; interp. result of interpreting a Xamot expression

(define-type XTDenotable
  [thunkxtb/xt (xtb xtbinding?) (env env?)])
;; interp. binding to a Xamot identifier.  May either be
;; 1) a Xamot expression, which is denoted by an identifier reference
;; 2) an array of Xamot expressions, denoted by an aref expression

(define (fn-for-for-xtd xtd)
  (match-let ([(thunkxtb/xt xtb env) xtd])
    (... (fn-for-xtb xtb)
         (#;fn-for-env env))))

;; Env is (envof XTDenotable)
(define (env? x) (envof XTDenotable?))
;; interp. a map from Xamot identifiers to their denotations


;; Ref is one of:
;; - (id/xt XTID)
;; - (aref/xt XTID Number)
;; interp. a reference to a Xamot expression

;; Any -> Boolean
;; produce true if the given object is a Ref, otherwise false
(define (ref? x)
  (match x
    [(id/xt x) #t]
    [(aref/xt x n) #t]
    [else #f]))

(define R1 (id/xt 'x))
(define R2 (aref/xt 'x 4))

(define (fn-for-ref ref)
  (match ref
    [(id/xt x) (... x)]
    [(aref/xt x n) (... x n)]))


;; XTBinding XTID -> Xamot
;; coerce the given binding to a Xamot expression
(define (binding-id xtb x)
  (cond [(Xamot? xtb) xtb]
        [else ; (cons? xtb)
         (error 'interp "Bad id binding for ~a: ~a" x xtb)]))


;; XTBinding XTID Number -> Xamot

(define (binding-aref xtb x n)
  (cond [(Xamot? xtb)
         (error 'interp "Bad array binding for {aref ~a ~a}: ~a" x n xtb)]
        [else ; (cons? xtb)
         (begin
           (unless (< n (length xtb))
             (error 'interp "Index ~a out of range for binding: ~a" n xtb))
           (list-ref xtb n))]))

;; Path is (listof Ref)

;; PathPattern is one of:
;; - (cons '* empty)
;; - (cons Ref empty)
;; - (cons Ref PathPattern)
;; interp. a "path" describes the dynamic context leading up to a Xamot
;; identifier or array reference, as a list of identifier/array references,
;; most-recent first, leading to the current reference under consideration.
;; The '* pattern matches any remaining path.
;; 
(define P0 (list R1))
(define P1 (list R1 R2))
(define PP0 (list R1 '*))

(define (fn-for-pp pp)
  (match pp
    [`(*) (...)]
    [`(,r) (... (fn-for-ref r))]
    [`(,r . ,pp^) (... (fn-for-ref r)
                       (fn-for-pp pp^))]))


;; OverrideList is (listof (list PathPattern Xamot))
;; interp.  Syntactic overrides for dynamic Xamot identifier references
(define OV0 (list))
(define OV1 (list
             (list P1 (str/xt "eat!"))
             (list (list '*) (char/xt #\q))))



;; PathPattern Path -> Boolean
;; produce true if pp matches p, otherwise false
(define (match-path? pp p)
  (cond [(equal? (first pp) '*) #t] ;; * matches anything
        [(empty? p) #f] ;; path patterns are never empty
        [(equal? (first pp) (first p))
         (if (empty? (rest pp))
             (empty? (rest p))
             (match-path? (rest pp) (rest p)))]
        [else #f]))

(test (match-path? (list '*) empty) #t)
(test (match-path? (list (id/xt 'a)) empty) #f)
(test (match-path? (list (id/xt 'a))
                   (list (id/xt 'a)))
      #t)
(test (match-path? (list (id/xt 'a) '*)
                   (list (id/xt 'a)))
      #t)
(test (match-path? (list (id/xt 'a) (id/xt 'b))
                   (list (id/xt 'a))) #f)
(test (match-path? (list (id/xt 'a))
                   (list (id/xt 'a) (id/xt 'b))) #f)
(test (match-path? (list (id/xt 'a) '*)
                   (list (id/xt 'a) (id/xt 'b))) #t)
(test (match-path? (list (id/xt 'a) (id/xt 'b))
                   (list (id/xt 'a) (id/xt 'b))) #t)
(test (match-path? (list (id/xt 'a) (id/xt 'b))
                   (list (id/xt 'a) (id/xt 'c))) #f)



;; OverrideList Path -> Xamot or false
;; produce the Xamot expression that overrides path, or false if none
(define (lookup-ovr ovr path)
  (cond [(assf (λ (k) (match-path? k path)) ovr)
         => (λ (asc) (cadr asc))]
        [else #f]))

(test (lookup-ovr (list) (list (id/xt 'x))) #f)
(test (lookup-ovr (list (list (list (id/xt 'x)) (char/xt #\g)))
                  (list (id/xt 'x)))
      (char/xt #\g))
(test (lookup-ovr (list (list (list (aref/xt 'x 2)) (char/xt #\g)))
                  (list (aref/xt 'x 2)))
      (char/xt #\g))

(test (lookup-ovr (list (list (list (aref/xt 'x 3)) (char/xt #\g)))
                  (list (aref/xt 'x 2)))
      #f)

(test (lookup-ovr (list (list (list (aref/xt 'x 3)) (char/xt #\g))
                        (list (list '*) (char/xt #\h)))
                  (list (aref/xt 'x 2)))
      (char/xt #\h))



;; Env is (envof XTDenotable)
;; interp. mapping from identifiers to Xamot thunks



;; XTID Xamot Xamot Env Path OverrideList -> XTValue
(define (interp-with x named body env path ovr)
  (let ([xtd (thunkxtb/xt named env)])
    (interp/env body (extend-env env x xtd) path ovr)))



;; XTID Env Path OverrideList (XTBinding -> Xamot) -> XTValue
;; interpret a reference, either as an override, if present, or relevant binding
(define (interp-ref x env path ovr resolve-xtb)
  (cond [(lookup-ovr ovr path)
         =>
         (λ (xt)
           (interp/env xt env path ovr))]
        [else
         (match-let ([(thunkxtb/xt xtb env^)
                      (with-handlers
                          ([exn:fail?
                            (λ (_) (error 'interp "Unbound identifier: ~a" x))])
                        (lookup-env env x))])
           (interp/env (resolve-xtb xtb) env^ path ovr))]))



;; XTID Env Path OverrideList -> XTValue
;; interpret an identifier ref: its override, if present, otherwise its binding
(define (interp-id/xt x env path ovr)
  (let ([path^ (cons (id/xt x) path)])
    (interp-ref x env path^ ovr (λ (xtb) (binding-id xtb x)))))



;; XTID Natural Env Path OverrideList -> XTValue
;; interpret an array ref: its override, if present, otherwise its binding
(define (interp-aref/xt x n env path ovr)
  (let ([path^ (cons (aref/xt x n) path)])
    (interp-ref x env path^ ovr (λ (xtb) (binding-aref xtb x n)))))



;; Xamot Env Path OverrideList -> XTValue
(define (interp/env xt env path ovr)
  (type-case Xamot xt
    [char/xt (c) (string c)]
    [and/xt (lhs rhs)
            (string-append (interp/env lhs env path ovr)
                           (interp/env rhs env path ovr))]
    [nil/xt () ""]
    [with/xt (x named body)
             (interp-with x named body env path ovr)]
    [id/xt (x) (interp-id/xt x env path ovr)]
    [aref/xt (x n) (interp-aref/xt x n env path ovr)]))



;; Xamot OverrideList -> XTValue
;; produce a string corresponding to xt0, accounting for overrides
(define (interp/xt xt0 ovr)
  (interp/env xt0 empty-env (list) ovr))

(test (interp/xt (nil/xt) (list)) "")
(test (interp/xt XT1 (list)) "q")
(test (interp/xt XT2 (list)) "r")
(test (interp/xt XT3 (list)) "qr")
(test (interp/xt XT4 (list)) "qr")
(test (interp/xt XT5 (list)) "qr")

(test (interp/xt (with/xt 'a (char/xt #\q)
                          (id/xt 'a))
                 (list))
      "q")

(test/exn (interp/xt (with/xt 'a (char/xt #\q)
                              (id/xt 'b))
                     (list))
          "Unbound")

(test (interp/xt (with/xt 'a (list (str/xt "meep!")
                                   (str/xt "moop!"))
                          (aref/xt 'a 0))
                 (list))
      "meep!")

(test (interp/xt (with/xt 'a (list (str/xt "meep!")
                                   (str/xt "moop!"))
                          (aref/xt 'a 1))
                 (list))
      "moop!")


;; ref/binding mismatches

(test/exn (interp/xt (with/xt 'a (list (str/xt "meep!")
                                       (str/xt "moop!"))
                              (aref/xt 'a 2))
                     (list))
          "Index")


(test/exn (interp/xt (with/xt 'a (list (str/xt "meep!")
                                       (str/xt "moop!"))
                              (id/xt 'a))
                     (list))
          "Bad id binding")



(test/exn (interp/xt (with/xt 'x (char/xt #\q)
                              (aref/xt 'x 0))
                     (list))
          "Bad array binding")

;; Overrides

(test (interp/xt (with/xt 'x (char/xt #\q)
                          (id/xt 'x))
                 (list (list (list '*) (char/xt #\n))))
      "n")

(test (interp/xt (with/xt 'x (char/xt #\q)
                          (id/xt 'x))
                 (list (list (list (id/xt 'x)) (char/xt #\n))))
      "n")

(test (interp/xt (with/xt 'x (char/xt #\q)
                          (id/xt 'x))
                 (list (list (list (id/xt 'y)) (char/xt #\n))))
      "q")


(test (interp/xt (with/xt 'x (char/xt #\q)
                          (id/xt 'x))
                 (list (list (list (aref/xt 'y 7)) (char/xt #\n))))
      "q")

(test (interp/xt (with/xt 'a (list (str/xt "meep!")
                                   (str/xt "moop!"))
                          (aref/xt 'a 1))
                 (list (list (list '*) (char/xt #\n))))
      "n")

(test (interp/xt (with/xt 'a (list (str/xt "meep!")
                                   (str/xt "moop!"))
                          (aref/xt 'a 1))
                 (list (list (list (aref/xt 'a 1)) (char/xt #\n))))
      "n")

;; pay close attention to dynamic context here
(test (interp/xt (with/xt 'a (list (str/xt "meep!")
                                   (str/xt "moop!"))
                          (aref/xt 'a 1))
                 (list (list (list (aref/xt 'a 1))
                             (and/xt (aref/xt 'a 1) (aref/xt 'a 1)))))
      "moop!moop!")

;; this one diverges: pay attention to override pattern
#;
(test/diverge
 (interp/xt (with1/xt 'a (list (str/xt "meep!")
                               (str/xt "moop!"))
                      (aref/xt 'a 1))
            (list (list (list (aref/xt 'a 1) '*)
                        (and/xt (aref/xt 'a 1) (aref/xt 'a 1)))))
 5)

(test (interp/xt (with/xt 'x
                          (with/xt 'y (char/xt #\q)
                                   (and/xt (id/xt 'y) (id/xt 'y)))
                          (id/xt 'x))
                 (list (list (list (id/xt 'y) (id/xt 'x)) (char/xt #\n))))
      "nn")

(test (interp/xt (with/xt 'x
                          (with/xt 'y (char/xt #\q)
                                   (and/xt (id/xt 'y) (id/xt 'y)))
                          (id/xt 'x))
                 (list (list (list (id/xt 'y)) (char/xt #\n))))
      "qq")

(test (interp/xt (with/xt 'x
                          (with/xt 'y (char/xt #\q)
                                   (and/xt (id/xt 'y) (id/xt 'y)))
                          (id/xt 'x))
                 (list (list (list (id/xt 'y) '*) (char/xt #\n))))
      "nn")

(test (interp/xt (with/xt 'x
                          (with/xt 'y (char/xt #\q)
                                   (and/xt (id/xt 'y) (id/xt 'y)))
                          (id/xt 'x))
                 (list (list (list (id/xt 'x)) (char/xt #\n))))
      "n")

;; Examples from the description

(test (interp/xt
       (with/xt 'a (list (str/xt "way") (str/xt "free"))
                (and/xt (aref/xt 'a 1) (aref/xt 'a 0)))
       
       (list (list (list (aref/xt 'a 1)) (str/xt "high"))))
      "highway")


(test (interp/xt
       (with/xt 'a (str/xt "hi")
                (with/xt 'b (id/xt 'a)
                         (with/xt 'c (id/xt 'a)
                                  (and/xt (id/xt 'b) (id/xt 'c)))))
       (list))
      "hihi")

(test (interp/xt
       (with/xt 'a (str/xt "hi")
                (with/xt 'b (id/xt 'a)
                         (with/xt 'c (id/xt 'a)
                                  (and/xt (id/xt 'b) (id/xt 'c)))))
       (list
        (list (list (id/xt 'a) (id/xt 'b)) (str/xt "mo"))
        (list (list (id/xt 'a) (id/xt 'c)) (str/xt "jo"))
        ))
      "mojo")

(test (interp/xt
       (with/xt 'a (str/xt "hi")
                (with/xt 'b (id/xt 'a)
                         (with/xt 'c (id/xt 'a)
                                  (and/xt (id/xt 'b) (id/xt 'c)))))
       (list
        (list (list (id/xt 'a) '*) (str/xt "mu"))
        ))
      "mumu")

(test (interp/xt
       (with/xt 'a (list (str/xt "way") (str/xt "free"))
                (and/xt (aref/xt 'a 1) (aref/xt 'a 0)))
       (list
        (list (list (aref/xt 'a 1)) (aref/xt 'a 0))
        (list (list (aref/xt 'a 0)) (aref/xt 'a 1))))
      "wayfree")

(test (interp/xt
       (with/xt 'a (list (str/xt "way") (str/xt "free"))
                (and/xt (aref/xt 'a 1) (aref/xt 'a 0)))
       (list
        (list (list (aref/xt 'a 1)) (aref/xt 'a 0))
        (list (list (aref/xt 'a 0)) (aref/xt 'a 1))
        (list (list (aref/xt 'a 1) (aref/xt 'a 0)) (str/xt " to go!"))))
      "way to go!")

#;
(test/diverge (interp/xt (id/xt 'doh)
                         (list
                          (list (list (id/xt 'doh) '*) (id/xt 'doh))))
              5)


;; Lexical scoping example from Anon. Poet @353_f1
(test (interp/xt (with/xt 'b (char/xt #\b)
                          (with/xt 'a (id/xt 'b)
                                   (with/xt 'b (char/xt #\X) (id/xt 'a))))
                 (list))
      "b")
