#lang plai

;; NOTE:  YOU SHOULD NOT NEED TO READ THIS FILE
;;        IT IS HERE TO SUPPORT THE OTHERS

;;
;; appendix.rkt - support file for MT2
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments

;; (envof X) is (listof (cons Symbol X))
;; interp.  A collection of deferred substitutions

;; (envof X)
;; the empty environment
(define empty-env '())

;; (envof X) Symbol X -> (envof X)
;; extend env with a binding from x0 to v
(define (extend-env old-env x value)
  (cons (cons x value) old-env))

;; (envof X) Symbol -> X
;; look up the identifier x in the environment env
;; Effect: Signals an error if identifier is not present
(define (lookup-env env x)
  (match env
    ['() (error 'lookup-env "Unbound identifier: ~a" x)]
    [`( ( ,y . ,value) . ,rest ) #:when (equal? x y) value]
    [else (lookup-env (rest env) x)]))

;; Env is (envof Boolean)
;; interp.  environment for Prop and SATelLite

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effect Abstraction

;;
;; Effect Interface
;;

;; Computation Computation -> Computation
;; (amb/eff c1 c2) - produce the result of c1 unless it (or its continuation)
;;                   fails, in which case produce the result of c2

;;   -> Computation
;; (fail/eff) - fail the current computation

;; The Generic Effect Abstraction API: 
;; Boolean -> Computation
;; (return/eff e) - produce a computation that returns the value of e

;; Computation (Natural or false) -> (listof Boolean)
;; (run/eff c n) - run an effectful computation, producing a list of at most n
;;                  results, or as many as possible if n is #f

;; x : Boolean, c1 : Computation, c2 : Computation (may reference x),
;; produces a Computation
;; (let/eff ([x c1] c2)) - bind x to the next successful value of c1 in c2

;; (let/eff* ([x1 c1] [x2 c2] ...) cn) - sequentialize instances of let/eff
;;  equivalent to:
;; (let/eff ([x1 c1]) (let/eff ([x2 c2]) ... cn))

;;
;; Effect Implementation
;;

;;
;; Computation is (streamof Boolean)
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


;; Boolean -> Computation
;; "return" a successful value (by embedding it in a Computation)
(define (return/eff v)
  (stream v))


;; Computation (Natural or false) -> (listof Boolean)
;; produce a value corresponding to the given Computation
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


;; (streamof (streamof Boolean)) -> (streamof Boolean)
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
