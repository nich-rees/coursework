#lang plai
(print-only-errors #t)
(require racket/stream)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

(require "env-data.rkt")
(require "xamot-solution.rkt")

;; (define "parsing.rkt")


;;
;; Part 2: Tomax: a language for matching string patterns.
;;

;; Tomax is a language for recognizing strings according to string patterns,
;; and expressing the relationship(s) between the pattern and the string.

;; The Tomax interpreter accepts a Tomax program and a string as input,
;; and produces a list of *Xamot programs* as output.  Each Xamot program
;; produces the original string when run, but they may differ with respect
;; to how names introduced in the Tomax program correspond to bound
;; identifiers in a Xamot program.  This also allows us to use Xamot's
;; override facility to perform quite flexible string replacements,
;; considering how small and simplified the languages are.


;; The Language:

;; "Some people, when confronted with a problem, think 'I know, I'll use regular
;; expressions.' Now they have two problems."
;; -- Jamie Zawinski

;; A Tomax program is somewhat similar to a Xamot program, and has many
;; similarly-named components.  However those components in Tomax describe
;; attempts to *match* a given string, rather than *generate* one.  This
;; twin correspondence is no coincidence!

;; - a character represents an attempt to match a single-character of input;
;; - a string represents an attempt to match the prefix of the input;
;; - {and a b} tries to match a against some input prefix , and then b;
;; - {nil} always matches (i.e. it matches the empty string prefix), so
;;   {and a {nil}} and {and {nil} a} both match if a matches.

;; Tomax also has some choice operations:
;; - {or a b} first tries to match a against the input prefix: if that fails,
;;   it tries to match b
;; - {never} always fails, so {or a {never}} and {or {never} a} both match
;;   if a does
;; - {maybe a} tries to match the input prefix, or successfully matches nothing


;; Tomax also provides two mechanisms for naming its subexpressions.  These
;; names are reflected in the output Xomat programs and facilitate replacement
;; via Xomat overrides:

;; - {named {x a} b} matches like (and a b) but associates the name x with a
;; - {named-many {x a} b} matches zero or more instances of a, then b. It is
;;   similar to the Kleene star from regular expressions.

;; PROBLEM 2:  Complete the implementation of the Tomax Effect Abstraction
;;  (search for the TODOs)

;; TXID is Symbol
;; INVARIANT: a txid may not be any Tomax symbol
;; INVARIANT: a txid may not end in "*"
;; interp. identifier from the Tomax language

;; Any -> Boolean
;; produce true if x is a valid Tomax identifier, otherwise false
(define (txid? x)
  (and (symbol? x)
       (not (member x '(char str and or emp never any maybe many)))
       (not (char=? #\* (last (string->list (symbol->string x)))))))

(define-type Tomax
  [char/tx (c char?)]
  [any/tx]
  [and/tx (lhs Tomax?) (rhs Tomax?)]
  [nil/tx]
  [or/tx (lhs Tomax?) (rhs Tomax?)]
  [never/tx]
  [named-many/tx (x txid?) (tx Tomax?) (body Tomax?)]
  [named/tx (x txid?) (named Tomax?) (body Tomax?)])

;; interp. internal language for Tomax, whose BNF is:
;; <Tomax> ::=
;;           | <char>
;;           | <string>
;;           | {any} any character 
;;           | {and <Tomax> <Tomax>}
;;           | {nil} matches empty string, always succeeds --> string listofchar
;;           | {or <Tomax> <Tomax>}
;;           | {never} --> needs no print because never matches.
;;           | {maybe <Tomax>} ;; argument or empty string
;;           | {named {<TXID> <Tomax>} <Tomax>}
;;           | {named-many {<TXID> <Tomax>} <Tomax>} ;;"named" Kleene star
;; where a <string> is a conjunction of <char>s
;; {maybe tx} ≡ {or tx nil}

;; String -> Tomax
;; produce a Tomax expression equivalent to the given string
(define (str/tx str)
  (foldr (λ (c rtx) (and/tx (char/tx c) rtx)) (nil/tx) (string->list str)))

;; Tomax -> Tomax
;; produce a Tomax expression equivalent to optional expression
(define (maybe/tx tx) (or tx (nil/tx)))


(define TX1 (char/tx #\a))
(define TX2 (and/tx (char/tx #\a) (nil/tx)))
(define TX3 (named-many/tx 'a (char/tx #\a) (nil/tx)))

#;
(define (fn-for-tx tx)
  (type-case Tomax tx
    [char/tx (c) (... c)]
    [any/tx () (...)]
    [and/tx (lhs rhs)
            (... (fn-for-tx lhs)
                 (fn-for-tx rhs))]
    [nil/tx () (...)]
    [or/tx (lhs rhs)
           (... (fn-for-tx lhs)
                (fn-for-tx rhs))]
    [never/tx () (...)]
    [named/tx (x named body)
              (... x
                   (fn-for-tx named)
                   (fn-for-tx body))]
    [named-many/tx (x named body)
                   (... x
                        (fn-for-tx named)
                        (fn-for-tx body))]))

;; TXValue is Xamot
;; interp.  A Tomax language value is a Xamot program. (paired with...?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effect Interface
;;

;; (amb/eff c1 c2) - produce the result of c1 unless it (or its continuation)
;;                   fails, in which case produce the result of c2
;; (fail/eff) - fail the current computation
;; (pop/eff c) - if c is #f or matches the first input, pop it, otherwise fail
;; (empty/eff v) If the input is empty, produce v, otherwise fail


;; The Generic Interface Components:
;; (return/eff e) - return the value of e
;; (run/eff c n) - run an effectful computation, producing a list of <=n results
;; (let/eff ([x c1] c2)) - bind x to the next successful value of c1 in c2
;; (let/eff* ([x* c1*] ...) c2) - sequentialize instances of let/eff

;;
;; Effect Implementation
;;

;; "A Parser of Things
;;  is a function from Strings
;;  to Lists
;;  of Pairs
;;  of Things and Strings!"
;;  -- Fritz Ruehr


;; Input is (listof Character)
;; Computation is  Input -> (streamof (list TXValue Input))


;; Computation Computation -> Computation
;; merge the two given streams
(define-syntax amb/eff
  (syntax-rules ()
    [(_ c1 c2)
     (λ (input)
       (stream-append (c1 input) (c2 input)))]))

#;(define-syntax amb/eff
    (syntax-rules ()
      [(_ c1 c2) (lazy-append-fn c1 (delay c2))]))

;; -> Computation
(define (fail/eff)
  (λ (input) empty-stream))


;; pop/eff : (Char or false) (Char -> TXValue) -> Computation
;; if mc is #f or matches the nonempty first input, pop into p, otherwise fail
;; EXERCISE: implement pop effect abstraction
(define (pop/eff mc p)
  (λ (input)
    (cond [(empty? input) empty-stream]
          [(or (false? mc) (char=? mc (first input)))
           (stream (list (p (first input)) (rest input)))]  
          [else empty-stream]))) 


;; empty/eff : TXValue -> Computation
;; if the input is empty, produce the given value, otherwise fail
;; EXERCISE: implement empty effect abstraction
(define (empty/eff v)
  (λ (input)
    (if (empty? input)
        (stream (list v input))
        empty-stream)))


;; TXValue -> Computation
;; "return" a successful value (by embedding it in a Computation)
(define (return/eff v)
  (λ (input) (stream (list v input))))


;; Computation Input (Natural or false) -> (listof Value)
;; produce at most n-initial (or all if false) values of a given Computation
;; Effect: Signal a runtime error if it is an exception
(define (run/eff c0 input0 n0)
  (local [(define (maybe-sub1 n)
            (if (false? n) #f (sub1 n)))
          (define (done? n)
            (if (false? n) #f (zero? n)))
          (define c (let/eff ([x c0]) (empty/eff x)))]
    (let loop ([results (c input0)] [n n0])
      (if (done? n)
          empty
          (cond
            [(stream-empty? results) empty]
            [else ; stream-cons
             (match-let ([(list v input^) (stream-first results)])
               (cons v (loop (stream-rest results) (maybe-sub1 n))))])))))


;; (streamof (streamof X)) -> (streamof X)
;; produce a stream that concatenates each element of a stream of streams
(define (stream-flatten cc)
  (cond
    [(stream-empty? cc) empty-stream]
    [else ;; stream-cons
     (stream-append (stream-first cc)
                    (stream-flatten (stream-rest cc)))]))

;; Compose two computations
(define-syntax let/eff
  (syntax-rules ()
    [(_ ([x c1]) c2)
     (λ (input)
       (stream-flatten
        (stream-map
         (match-lambda [(list x input^) (c2 input^)])
         (c1 input))))]))


;; Compose many computations
(define-syntax let/eff*
  (syntax-rules ()
    [(_ () e) e]
    [(_ ([x e1] [x* e1*] ...) e2)
     (let/eff ([x e1])
              (let/eff* ([x* e1*] ...) e2))]))

;; End of Effect Abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Interpreter

;; TXID Tomax Tomax -> Computation
;; produce a computation that interpretes a named-many expression
;; Effect: signals an error incase of runtime interpretation error
(define (interp-named-many x named body)
  (let loop ([rxt* empty] [rref* empty] [n 0])
    (amb/eff
     (let/eff ([bxt (interp/tx-eff body)])
              (let ([xt* (reverse rxt*)]
                    [x* (string->symbol (string-append (symbol->string x) "*"))]
                    [many (foldr and/xt (nil/xt) (reverse rref*))])
                (return/eff (with/xt x xt*
                                      (with/xt x* many
                                                (and/xt (id/xt x*) bxt))))))
     (let/eff* ([l (interp/tx-eff named)])
               (loop (cons l rxt*) (cons (aref/xt x n) rref*) (add1 n))))))

;; Tomax -> Computation
;; produce a computation that interprets tx
;; Effect: signals an error incase of runtime interpretation error
(define (interp/tx-eff tx)
  (type-case Tomax tx
    [char/tx (c) (pop/eff c (λ (c) (char/xt c)))]
    [any/tx () (pop/eff #f (λ (c) (char/xt c)))]
    [and/tx (lhs rhs)
            (let/eff* ([lv (interp/tx-eff lhs)]
                       [rv (interp/tx-eff rhs)])
                      (return/eff (and/xt lv rv)))]
    [nil/tx () (return/eff (nil/xt))]
    [or/tx (lhs rhs)
           (amb/eff (interp/tx-eff lhs) (interp/tx-eff rhs))]
    [never/tx () (fail/eff)]
    [named/tx (x named body)
              (let/eff* ([n (interp/tx-eff named)]
                         [b (interp/tx-eff body)])
                        (return/eff
                         (with/xt x n
                                   (and/xt (id/xt x) b))))]
    [named-many/tx (x named body)
                   (interp-named-many x named body)]))


;; Tomax String -> (listof TXValue)
;; and
;; Tomax String (Natural or false) -> (listof TXValue)
;; produce a list of matches of str against tx
;; Effect: signals an error incase of runtime interpretation error
(define (interp/tx tx str [mn #f])
  (run/eff (interp/tx-eff tx) (string->list str) mn))

(test (interp/tx (nil/tx) "") (list (nil/xt)))
(test (interp/tx (nil/tx) "x") (list))

(test (interp/tx (char/tx #\c) "c")
      (list (char/xt #\c)))

(test (interp/tx (any/tx) "c")
      (list (char/xt #\c)))

(test (interp/tx (str/tx "wassup?") "wassup?")
      (list (str/xt "wassup?")))

(test (interp/tx (or/tx
                  (and/tx (str/tx "was") (str/tx "sup?"))
                  (str/tx "wassup?")) "wassup?")
      (list (and/xt (str/xt "was") (str/xt "sup?"))
            (str/xt "wassup?")))

(test (interp/tx (and/tx
                  (or/tx
                   (str/tx "super")
                   (str/tx "supe"))
                  (str/tx "rman"))
                  "superman")
      (list (and/xt (str/xt "supe") (str/xt "rman"))))

(test (interp/tx (named/tx 'a (str/tx "was") (str/tx "sup?")) "wassup?")
      (list (with/xt 'a (str/xt "was") (and/xt (id/xt 'a) (str/xt "sup?")))))

(test (interp/tx (named-many/tx 'a (any/tx) (nil/tx)) "c")
      (list (with/xt 'a (list (char/xt #\c))
                      (with/xt 'a* (and/xt (aref/xt 'a 0) (nil/xt))
                                (and/xt (id/xt 'a*) (nil/xt))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Putting it all together:

;; String Tomax OverrideList -> (listof String)
;; produce a list of manipulations of str determined by tx and ovr
;; Effect: signal an error in case of runtime interpretation fault
(define (match&replace str tx ovr)
  (map (λ (xt) (interp/xt xt ovr))
   (interp/tx tx str)))

(test (match&replace "hi" (str/tx "hi") (list))
      (list "hi"))

(test (match&replace "hi" (str/tx "bye") (list))
      (list))

(test (match&replace "hihihi" (named-many/tx 'a (str/tx "hi") (nil/tx)) (list))
      (list "hihihi"))

(test
 (match&replace "hihihi" (named-many/tx 'a (str/tx "hi") (nil/tx))
                (list
                 (list (list (aref/xt 'a 1) (id/xt 'a*))  (str/xt "bye"))))
      (list "hibyehi"))

(test
 (match&replace "hihihi" (named-many/tx 'a (str/tx "hi") (nil/tx))
                (list
                 (list (list (id/xt 'a*) '*)  (str/xt "bye"))))
      (list "bye"))

(test
 (match&replace "hihihi" (named-many/tx 'a
                                        (or/tx
                                         (named/tx 'h (str/tx "h") (nil/tx))
                                         (str/tx "i")) (nil/tx))
                (list
                 (list (list (id/xt 'h) '*)  (str/xt "H"))))
      (list "HiHiHi"))

(test
 (match&replace "hhhhh"
                (and/tx (named-many/tx 'a (str/tx "h") (nil/tx))
                        (named-many/tx 'a (str/tx "h") (nil/tx)))
                (list))
      (list "hhhhh"
            "hhhhh"
            "hhhhh"
            "hhhhh"
            "hhhhh"
            "hhhhh"))

(test
 (match&replace "hhhhh"
                (and/tx (named-many/tx 'a (str/tx "h") (nil/tx))
                        (named-many/tx 'b (str/tx "h") (nil/tx)))
                (list
                 (list (list (aref/xt 'b 0) '*) (str/xt "b"))))
      (list "bhhhh"
            "hbhhh"
            "hhbhh"
            "hhhbh"
            "hhhhb"
            "hhhhh"))
