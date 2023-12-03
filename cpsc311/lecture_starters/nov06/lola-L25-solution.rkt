#lang plai

(require racket/stream)
;(require "parsing.rkt")
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

(print-only-errors #t)

;;
;; lola.rkt - a language with native support for generalized search
;;  Named in honour of Run Lola Run (https://www.imdb.com/title/tt0130827/)
;;  


;; LID is Symbol
;; INVARIANT: a LID cannot be equal to any LOLA keyword
;; interp. a TEL identifier
(define (lid? x)
  (local [(define keyword
            '(+ - with fun if0 fix pair left right mt choose fail require0))]
  (and (symbol? x)
       (not (member x keyword)))))

(define LID0 'a)
(define LID1 'b)

;; No template: atomic data




(define-type LOLA    
  [num (n number?)]
  [add (lhs LOLA?) (rhs LOLA?)]
  [sub (lhs LOLA?) (rhs LOLA?)]
  [id (name lid?)] 
  [fun (param lid?) (body LOLA?)]
  [app (rator LOLA?) (arg LOLA?)]
  [if0 (predicate LOLA?) (consequent LOLA?) (alternative LOLA?)]
  [fix (name lid?) (body LOLA?)]
  [pair (left LOLA?) (right LOLA?)]
  [left (e LOLA?)]
  [right (e LOLA?)]
  [mt]
  [choose (lns LOLA?) (rhs LOLA?)] ;; ***
  [fail]) ;; ***
 
;; interp. expressions in an eager language that supports
;; arithmetic, functions, conditionals, and exceptions.
;; Its syntax is defined by the following BNF:
;; <LOLA> ::=
;; (ARITHMETIC)
;;          <num>
;;        | {+ <LOLA> <LOLA>}
;;        | {- <LOLA> <LOLA>}
;; (IDENTIFIERS)
;;        | {with {<id> <LOLA>} <LOLA>}
;;        | <id>
;; (CONDITIONALS)
;;        | {if0 <LOLA> <LOLA> <LOLA>}
;; (FUNCTIONS)
;;        | {<LOLA> <LOLA>}
;;        | {fun {<id>} <LOLA>}
;; (RECURSION)
;;        | {fix <id> <LOLA>}
;; (PAIRS/LISTS)
;;        | {pair <LOLA> <LOLA>}
;;        | {left <LOLA>}
;;        | {right <LOLA>}
;;        | {mt}
;; (SEARCH) *NEW*
;;        | {choose <LOLA> <LOLA>}
;;        | {fail}
;;        | {require0 <LOLA>}
;; (SEQUENCING)
;;        | {seqn <LOLA> <LOLA>}
;; where
;; {with {x named} body} ≡ {{fun {x} body} named}
;; {seqn expr1 expr2} ≡ {with {x0 expr1} expr2}
;;                where x0 does not occur free in expr2
;; {require0 expr} ≡ {if0 expr 0 {fail}}}

;; Syntactic Sugar
(define (with x named body) (app (fun x body) named))
(define (require0 expr) (if0 expr (num 0) (fail)))
(define (seqn e1 e2) (with (gensym) e1 e2))


(define L0 (choose (num 1) (num 0)))
(define L1 (if0 (choose (num 1) (num 0))
                (num 22)
                (num 33)))

(define L2 (if0 (add (choose (num 1) (num -3))
                     (choose (num 3) (num -1)))
                (num 22)
                (num 33)))

(define L3 (with 'x (choose (num 5) (num 7))
                 (seqn (require0 (sub (id 'x) (num 7)))
                       (add (id 'x) (num 3)))))


#;
(define (fn-for-lola f)
  (type-case LOLA f
    [num (n) (... n)]
    [add (l r) (... (fn-for-lola l)
                    (fn-for-lola r))]
    [sub (l r) (... (fn-for-lola l)
                    (fn-for-lola r))]
    [id (x) (... x)]
    [fun (x body) (... x
                       (fn-for-lola body))]
    [app (rator rand) (... (fn-for-lola rator)
                           (fn-for-lola rand))]
    [if0 (p c a)
         (... (fn-for-lola p)
              (fn-for-lola c)
              (fn-for-lola a))]
    [fix (x body) (... x
                       (fn-for-lola body))]
    [pair (l r) (... (fn-for-lola l)
                     (fn-for-lola r))]   
    [left (e) (... (fn-for-lola e))]
    [right (e) (... (fn-for-lola e))]
    [mt () (...)]
    [choose (l r) (... (fn-for-lola l)
                       (fn-for-lola r))]
    [fail () (...)]))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; LOLA LID LOLA -> LOLA
;; substitute lola0 for x0 in lola
;(define (subst lola x0 lola0) (num 7)) ; stub

(define (subst lola x0 lola0)
  (local [;; LOLA -> LOLA
          (define (recur expr1)
            (local [;; LOLA LID -> LOLA
                    ;; substitute lola0 for x0 in lola if binder x is not x0
                    (define (maybe-subst-in e x)
                      (if (symbol=? x x0)
                          e
                          (recur e)))]
              (type-case LOLA expr1
                [num (n) (num n)]
                [add (l r) (add (recur l)
                                (recur r))]
                [sub (l r) (sub (recur l)
                                (recur r))]
                [id (x) (if (symbol=? x x0)
                            lola0
                            (id x))]
                [fun (x body) (fun x (maybe-subst-in body x))]
                [app (rator rand) (app (recur rator)
                                       (recur rand))]
                [if0 (p c a)
                     (if0 (recur p)
                          (recur c)
                          (recur a))]
                [fix (x body) (fix x (maybe-subst-in body x))]
                [pair (l r) (pair (recur l)
                                  (recur r))]   
                [left (e) (left (recur e))]
                [right (e) (right (recur e))]
                [mt () (mt)]
                [choose (l r) (choose (recur l)
                                      (recur r))]
                [fail () (fail)])))]
    (recur lola)))


(test (subst (num 5) 'x (num 7)) (num 5))
(test (subst (id 'x) 'x (num 7)) (num 7))
(test (subst (id 'y) 'x (num 7)) (id 'y))
(test (subst (fun 'y (id 'x)) 'x (num 7)) (fun 'y (num 7)))
(test (subst (fun 'x (id 'x)) 'x (num 7)) (fun 'x (id 'x)))


;; Value is one of:
;; - (num number)
;; - (fun LID LOLA)
;; - (pair Value Value)
;; - (mt)
;; interp. a runtime value
#;
(define (fn-for-value v)
  (match v
    [(num n) (... n)]
    [(fun id body) (... id (fn-for-lola body))]
    [(pair v1 v2) (... (fn-for-value v1)
                       (fn-for-value v2))]
    [(mt) (...)]))


;; LOLA -> Boolean
;; produce true if lola is a value, otherwise false
(define (Value? lola)
  (match lola
    [(num n) #t]
    [(fun id body) #t]
    [(pair e1 e2) (and (Value? e1)
                       (Value? e2))]
    [(mt) #t]
    [else #f]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effect Abstraction

;;
;; Effect Interface
;;

;; (amb/eff c1 c2) - produce the result of c1 unless it (or its continuation)
;;                   fails, in which case produce the result of c2
;; (fail/eff) - fail the current computation

;; The Generic Interface Components:
;; (return/eff e) - return the value of e
;; (run/eff c n) - run an effectful computation, producing a list of <=n results
;; (let/eff ([x c1] c2)) - bind x to the next successful value of c1 in c2
;; (let/eff* ([x* c1*] ...) c2) - sequentialize instances of let/eff

;;
;; Effect Implementation
;;

;;
;; Computation is (streamof Value)
;; interp. a lazy stream of computations to be attempted in order.
#;
(define (fn-for-computation c)
  (cond
    [(empty-stream? c) (...)]
    [else (... (stream-first c)
               (fn-for-computation (stream-rest c)))]))

;; Computation (promiseof Computation) -> Computation
(define (lazy-append-fn c dc)
  (cond
    [(stream-empty? c) (force dc)]
    [else (stream-cons (stream-first c)
                       (lazy-append-fn (stream-rest c) dc))]))

;; Computation Computation -> Computation
;; merge the two given streams
(define-syntax amb/eff
  (syntax-rules ()
    [(_ c1 c2) (lazy-append-fn c1 (delay c2))]))

(define (fail/eff) empty-stream)


;; Value -> Computation
;; "return" a successful value (by embedding it in a Computation)
(define (return/eff v)
  (stream v))


;; Computation (Natural or false) -> (listof Value)
;; produce at most n-initial (or all if false) values of a given Computation
;; Effect: Signal a runtime error if it is an exception
(define (run/eff c-initial n-initial)
  (if n-initial
      (let loop ([c c-initial] [n n-initial])
        (if (zero? n)
            empty
            (cond
              [(stream-empty? c) empty]
              [else ; stream-cons
               (cons (stream-first c)
                     (loop (stream-rest c) (sub1 n)))])))
      (stream->list c-initial)))


;; (streamof (streamof Value)) -> (streamof Value)
;; produce a stream that concatenates each element of a stream of streams
(define (stream-flatten cc)
  (cond
    [(stream-empty? cc) empty-stream]
    [else ;; stream-cons
     (lazy-append-fn (stream-first cc)
                     (delay (stream-flatten (stream-rest cc))))]))

;; Compose two computations
(define-syntax let/eff
  (syntax-rules ()
    [(_ ([x c1]) c2)
     (stream-flatten (stream-map (λ (x) c2) c1))]))


;; Compose many computations
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


;; Value Value -> Value
;; produce the result of adding v1 to v2
;; Effect: signal an error if the values do not represent numbers
(define (add-value v1 v2)
  (let ([n1 (value->num v1)]
        [n2 (value->num v2)])
    (num (+ n1 n2))))


;; Value Value -> Value
;; produce the result of subtracting v2 from v1
;; Effect: signal an error if the values do not represent numbers
(define (sub-value v1 v2)
  (let ([n1 (value->num v1)]
        [n2 (value->num v2)])
    (num (- n1 n2))))


;; Value -> Boolean
;; produce true if the v is zero, otherwise false
(define (zero?-value v)
  (match v
    [(num n) (= n 0)]
    [else #f]))


;; Value Value -> Computation
;; produce a computation that applies v1 to v2
;; Effect: signal an error if v1 is not a function or any other runtime errors.
(define (apply-value v1 v2)
  (match v1
    [(fun id body) (interp-lola/eff (subst body id v2))]
    [else (error 'interp "bad function: ~a" v1)]))

;; Value -> Value
;; produce the left value of a pair
;; Effect: signal an error if the given value is not a pair
(define (left-value v)
  (match v
    [(pair v1 v2) v1]
    [else (error 'interp "Bad pair: ~a" v)]))

;; Value -> Value
;; produce the right value of a pair
;; Effect: signal an error if the given value is not a pair
(define (right-value v)
  (match v
    [(pair v1 v2) v2]
    [else (error 'interp "Bad pair: ~a" v)]))

;; LID LOLA -> Computation
;; interpret the self-referential expression (fix x lola)
;; Effect: signal an error in case of runtime error
(define (interp-fix x lola)
  (interp-lola/eff (subst lola x (fix x lola))))

;; Examples follow interp-lola/eff

;; LOLA LOLA -> Computation
;; interpret the choice expression (choose lola1 lola2)
(define (interp-choose lola1 lola2)
  (amb/eff (interp-lola/eff lola1)
           (interp-lola/eff lola2)))

;; -> Computation
;; interpret the failure expression (fail)
(define (interp-fail)
  (fail/eff))

  
;; LOLA -> Computation
;; produce the result of interpreting the given LOLA
;; Effect: signal an error in case of runtime error
(define (interp-lola/eff f)
  (type-case LOLA f
    [num (n) (return/eff (num n))]
    [add (l r)
         (let/eff* ([vl (interp-lola/eff l)]
                    [vr (interp-lola/eff r)])
                   (return/eff (add-value vl vr)))]
    [sub (l r)
         (let/eff* ([vl (interp-lola/eff l)]
                    [vr (interp-lola/eff r)])
                   (return/eff (sub-value vl vr)))]
    [id (x) (error 'interp "Unbound identifier: ~a" x)]
    [fun (x body) (return/eff (fun x body))]
    [app (rator rand)
         (let/eff* ([vrator (interp-lola/eff rator)]
                    [vrand (interp-lola/eff rand)])
                   (apply-value vrator vrand))]
    [if0 (p c a)
         (let/eff ([vp (interp-lola/eff p)])
                  (if (zero?-value vp)
                      (interp-lola/eff c)
                      (interp-lola/eff a)))]
    [fix (x body) (interp-fix x body)]
    [pair (l r)
          (let/eff* ([vl (interp-lola/eff l)]
                     [vr (interp-lola/eff r)])
                    (return/eff (pair vl vr)))]   
    [left (e)
          (let/eff ([v (interp-lola/eff e)])
                       (return/eff (left-value v)))]
    [right (e)
           (let/eff ([v (interp-lola/eff e)])
                       (return/eff (right-value v)))]
    [mt () (return/eff (mt))]
    [choose (l r) (interp-choose l r)]
    [fail () (interp-fail)]))



;; Examples for interp-lola/eff
(test (stream->list (interp-lola/eff (num 5))) (list (num 5)))

;; LOLA (Number or false) -> (listof Value)
;; interpret the given expression, producing n results (or all if n=false)
;; Effect: signal an exception for runtime errors *or uncaught user exceptions*
(define (interp-lola lola n)
  (run/eff (interp-lola/eff lola) n))

(define (interp-lola* lola)
  (interp-lola lola #f))



(test (interp-lola (num 5) 7) (list (num 5)))

(test (interp-lola* (choose (num 5) (num 7))) (list (num 5) (num 7)))

(test (interp-lola* (add (choose (num 5) (num 7))
                         (choose (num 7) (num 8))))
      (list (num 12) (num 13) (num 14) (num 15)))

(test (interp-lola* (seqn
                     (fail)
                     (choose (num 5) (num 7)))) empty)

(test (interp-lola* (add (choose (num 5) (num 7))
                         (choose (fail) (num 8))))
      (list  (num 13)  (num 15)))

(test (interp-lola* (with 'x (choose (num 5) (choose (num 6) (num 7)))
                          (add (id 'x) (num 12))))
      (list (num 17) (num 18) (num 19)))

;;
;; Fun examples with numbers
;;

;; THIS EXAMPLE JUSTIFIES THE TYPE OF lazy-append-fn's SECOND ARGUMENT
(define ONES (fix 'recur (choose (num 1) (id 'recur))))

(define NATS-FROM (fix 'recur
                       (fun 'n
                            (choose (id 'n)
                                    (app (id 'recur) (add (id 'n) (num 1)))))))

(define NATS (app NATS-FROM (num 0)))

(define NATS-BETWEEN
  (fun 'm0
       (fun 'n
            (app (fix 'recur
                      (fun 'm
                           (if0 (sub (id 'n) (id 'm))
                                (fail)
                                (choose (id 'm)
                                        (app (id 'recur)
                                             (add (id 'm) (num 1)))))))
                 (id 'm0)))))


(define FIVE-TO-TWELVE (app (app NATS-BETWEEN (num 5)) (num 12)))

(define ONE-TO-SEVEN (app (app NATS-BETWEEN (num 1)) (num 7)))

(define SEVEN-SUM (with 'a ONE-TO-SEVEN
                        (with 'b ONE-TO-SEVEN
                              (seqn
                               (require0 (sub (add (id 'a) (id 'b)) (num 7)))
                               (pair (id 'a) (id 'b))))))


;; LOLA solves the CLOWN LAB from CPSC110

;; produce 0 if a <= b, where a and b are positive or zero
(define LEQ0
  (fix 'leq
       (fun 'a
            (fun 'b
                 (if0 (id 'a)
                      (num 0)
                      (if0 (id 'b)
                           (num 1)
                           (app (app (id 'leq) (sub (id 'a) (num 1)))
                                (sub (id 'b) (num 1)))))))))

(define (<=/lola lhs rhs)
  (app (app LEQ0 lhs) rhs))

;; a sequence of requirements
(define (seq* . lor)
  (foldr (λ (r rlor) (seqn r rlor)) (num 99) lor))

(define (with* . args)
  (match args
    [`(,body) body]
    [`(,x ,named . ,rest)
     (with x named (apply with* rest))]))

(test (with* 'x (num 5)
             'y (num 6)
             (add (id 'y) (id 'x)))
      (with 'x (num 5)
            (with 'y (num 6)
                  (add (id 'y) (id 'x)))))

(define (lizt . args)
  (foldr (λ (c rloc) (pair c rloc))
                      (mt)
                      args))

;; Marny, Nifty, Oscar, Porky, Rascal, Sunny, Vinny
(define CLOWNS (list 'M 'N 'O 'P 'R 'S 'V))

(define CLOWN-PUZZLE
  (with* 'M (choose (num 0) (num 1))
         'N (choose (num 0) (num 1))
         'O (choose (num 0) (num 1))
         'P (choose (num 0) (num 1))
         'R (choose (num 0) (num 1))
         'S (choose (num 0) (num 1))
         'V (choose (num 0) (num 1))
         (seqn (seq*
                ;; at least 4 clowns in the car
                (require0 (<=/lola (num 4) (foldr (λ (c rloc) (add (id c) rloc))
                                                  (num 0)
                                                  CLOWNS)))
                ;; exactly 2 of Rascal, Sunny, and Vinny are selected.
                (require0 (sub (add (id 'R) (add (id 'S) (id 'V))) (num 2)))
                ;; either Nifty is selected, Oscar is selected, or both
                (require0 (<=/lola (num 1) (add (id 'N) (id 'O))))
                ;; Oscar cannot be selected unless Rascal is selected.
                (require0 (<=/lola (id 'O) (id 'R)))
                ;; Porky cannot be selected unless Sunny is selected.
                (require0 (<=/lola (id 'P) (id 'S)))
                ;; If Marny is selected, then Nifty cannot be selected.
                (require0 (<=/lola (id 'M) (sub (num 1) (id 'N)))))
               ;; print the clown statuses (1 means in the car, 0 means not)
               (lizt (id 'M) (id 'N) (id 'O) (id 'P) (id 'R) (id 'S) (id 'V)))))
