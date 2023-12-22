#lang plai
(print-only-errors #t)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

;; Child: mom, where do delay, force, and streams come from?

;; mom:
;;  https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; promises: Racket already has them natively, but you could implement them
;; yourself, or port them (or at least their behaviour)
;; to your language of choice
;;

;; Infinite computation, useful for testing
(define (DIVERGE) (DIVERGE))


(define-type Promise
  [promise (thunk procedure?)])


;; ( -> X) -> (promiseof X)
;; construct a promise that produces the *first* returned value (Scheme style)
(define (construct-promise thunk)
  (let ([result-ready? #f]
        [result (void)])
    (promise
     (λ ()
       (if result-ready?
           result
           (let ([x (thunk)])
             ;; A promise may include a self-reference, and even force itself.
             ;; Forcing such a promise may cause the promise to be forced a
             ;; second time before the value of the first force has been
             ;; computed.  The following code handles that case.
             (if result-ready?
                 result
                 (begin (set! result-ready? #t)
                        (set! result x)
                        result))))))))


;; construct a promise that dies if it is reentered while running (PLAI style)
#;
(define construct-promise
  (λ (thunk)
    (let ([promise-running? #f]
          [result-ready? #f]
          [result (void)])
      (promise
       (λ ()
         (cond [result-ready? result]
               [promise-running? (error 'delay "reentrant promise")]
               [else
                (begin
                  (set! promise-running? #t)
                  (set! result (thunk))
                  (set! promise-running? #f)
                  (set! result-ready? #t)
                  result)]))))))

;; (promiseof X) -> X
(define (force p)
  (type-case Promise p
    [promise (thunk) (thunk)]))

;; (promiseof X) is (construct-promise ( -> X))


(define-syntax delay
  (syntax-rules ()
    [(_ expression)
     (construct-promise (λ () expression))]))

(test (Promise? (delay (DIVERGE))) #t)
(test (let ([p (delay (+ 9 7))])
        (force p))
      16)

;; Example of how a promise is only computed once.  Observe the timing
(require htdp/draw) ; for sleep-for-awhile

(define (force-example)
  (local [(define p (delay (begin (sleep-for-a-while 5) "hello")))]
    (time (force p))
    (time (force p))))


;; You should not use delay with set!, but nonetheless the extra machinery
;; in construct-promise guarantees that the first value ever produced by forcing
;; a promise (even if the promise forces *itself*) will from then on be the
;; guaranteed result of forcing it in the future.  To understand the following
;; example, compare it to the "thunking" variant that follows it.
(test (local [(define x #f)
              (define pth
                (delay
                  (if x
                      x
                      (begin (set! x #t)
                             (not (force pth))))))]
        (force pth))
      #t)

;; like the last one, but replace delay with (λ () ...)
;; and (force ...) with (...)
(test (local [(define x #f)
              (define pth
                (λ ()
                  (if x
                      x
                      (begin (set! x #t)
                             (not (pth))))))]
        (pth))
      #f)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Streams
;;
(define-type Stream
  [sempty]
  [scons (head Promise?)
         (tail Promise?)])

;; (streamof X)
(define stream-empty (sempty))


;; any -> Boolean
(define empty-stream? sempty?)

(define-syntax stream-cons
  (syntax-rules ()
    [(_ hd tl) (scons (delay hd) (delay tl))]))

;; (streamof X) is one of:
;; - stream-empty
;; - (stream-cons X (streamof X))
;; interp.  a stream (lazy potentially infinite list) of values

;; (streamof X) -> X
(define (stream-first s)
  (type-case Stream s
    [sempty () (error 'stream-first "empty stream")]
    [scons (hd tl) (force hd)]))

;; (streamof X) -> (streamof X)
;; produce the rest of the given stream
;; Effect: signal an error if the stream is empty
(define (stream-rest s)
  (type-case Stream s
    [sempty () (error 'stream-rest "empty stream")]
    [scons (hd tl) (force tl)]))

;; Natural (streamof X) -> (listof X)
;; produce a list of at most the first n elements of s
(define (stream-take n s)
  (cond [(zero? n) empty]
        [(empty-stream? s) empty]
        [else ;;(stream-cons hd tl)
         (cons (stream-first s) (stream-take (sub1 n) (stream-rest s)))]))


(test (Stream? (stream-cons (DIVERGE) (DIVERGE))) #t)

(test (let ([s (stream-cons (+ 7 7) (DIVERGE))])
        (stream-first s))
      14)

(test (let ([s (stream-cons (DIVERGE) stream-empty)])
        (empty-stream? (stream-rest s)))
      #t)


(define ones-stream (stream-cons 1 ones-stream))

(test (stream-take 6 ones-stream) (list 1 1 1 1 1 1))


;; EXERCISE: define a self-referential stream of alternating 1's and 2's
(define alt-stream (stream-cons 1 (stream-cons 2 alt-stream)))

(test (stream-take 6 alt-stream) (list 1 2 1 2 1 2))


;; EXERCISE: define a function that takes two streams and a function and
;;   produces a stream that is:
;;   1) as long as the shorter of the two streams
;;   2) contains the result of applying the function to corresponding elements
;;      of the two streams
;; (You need not provide a cross-product of types table, but are welcome to)

;; (X Y -> Z) (streamof X) (streamof Y) -> (streamof Z)
;; produce a stream that uses f to combine s1 and s2
(define (stream-map2 f s1 s2)
  (cond [(empty-stream? s1) sempty]
        [(empty-stream? s2) sempty]
        [else (stream-cons (f (stream-first s1) (stream-first s2))
                           (stream-map2 f (stream-rest s1) (stream-rest s2)))]))
                              
(test (stream-take 6 (stream-map2 + ones-stream ones-stream))
      (list 2 2 2 2 2 2))
        
;; EXERCISE: define a stream of natural numbers
(define nats (stream-cons 0 (stream-map2 + nats ones-stream)))

(test (stream-take 6 nats)
      (list 0 1 2 3 4 5))


;; EXERCISE: define a stream of fibonnacci numbers
(define fibs (stream-cons 1 (stream-cons 1 (stream-map2 + fibs
                                                        (stream-rest fibs)))))


(test (stream-take 6 fibs)
      (list 1 1 2 3 5 8))




