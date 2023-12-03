#lang plai
(define ... list)

(print-only-errors #t)
;;
;; store-a-lot - the pointless solitary card game this pandemic needs.
;;   NOW WITH AN ABSTRACTED THREADED ACCUMULATOR



;; Game 2: Split Score-A-Lot
;; Like Simple Score-A-Lot, but rather than just drawing a card from one deck,
;; keep splitting the deck until you have one card and draw *that*.  Then
;; get back to splitting.  Count scores as above.

(define-type _Cut [cut (first split?) (second split?)]) 
;; any -> Boolean
;; produce true if the given object is a split, else false
(define (split? x)
  (cond [(empty? x) #t]
        [(number? x) (<= 2 x 10)]
        [(cut? x) (and (split? (cut-first x))
                       (split? (cut-second x)))]
        [else #f]))

;; Split is one of:
;; - empty
;; - Natural[2,10]
;; - (cut Split Split)
;; interp.  Models a deck split oh so many ways.
;;          empty represents an empty deck
;;          a number represents a single card
;;          (cut f s) represents a deck split into two decks

(define S1 (cut
            (cut
             (cut (cut 2 2) (cut 2 2))
             (cut (cut 3 3) 3))
            (cut
             (cut (cut 3 (cut 4 4))
                  (cut (cut (cut 4 4) (cut 5 5)) (cut 5 5)))
             (cut 6 (cut (cut 7 8) (cut 9 10))))))
(define S2 '())
(define S3 (cut
            (cut (cut (cut 5 (cut 5 5))
                      (cut (cut 5 2) 2))
                 (cut (cut 2 2)
                      (cut 3 (cut 3 3))))
            (cut (cut (cut (cut 3 4) 4)
                      (cut 4 4))
                 (cut 6 (cut 7 (cut 8 (cut 9 10)))))))

#;
(define (fn-for-split s)
  (cond [(empty? s) (...)]
        [(number? s) (... s)]
        [else ; (cut? s)
         (... (fn-for-split (cut-first s))
              (fn-for-split (cut-second s)))]))


;; Threaded-accumulator edition FOR SHOW

;; Split -> Natural
;; Calculate the score induced by this split
(define (split-score split0)
  ;; Accumulator: factor is Natural
  ;; Invariant: factor is (expt 2 N) where N is the number of 5's in
  ;;            split0 before split, following in-order traversal.
  (local [;; Split Natural -> (list Natural Natural)
          ;; Calculate the score induced by s, and an updated factor
          (define (split-score--acc s factor)
            (cond [(empty? s) (list 0 factor)]
                  [(number? s) (list (* s factor)
                                     (if (= s 5)
                                         (* factor 2)
                                         factor))]
                  [else ; (cut? s)
                   (match-let* ([`(,sum1 ,factor^)
                                 (split-score--acc (cut-first s) factor)]
                                [`(,sum2 ,factor^^)
                                 (split-score--acc (cut-second s) factor^)])
                     (list (+ sum1 sum2) factor^^))]))]
    (match-let ([`(,sum ,factor) (split-score--acc split0 1)])
      sum)))


(test (split-score S1) 751)
(test (split-score S2) 0)
(test (split-score S3) 1291)


;; NOW WITH AN *ABSTRACTED* THREADED ACCUMULATOR (OUR EXERCISE FOR TODAY)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effect Abstraction

;;
;; Effect Interface
;;


;; ??? (effect specific operators)



;; The Generic Interface Components:
;; (return/eff e) - produce a computation that simply returns the value of e
;; (run/eff c) - run an effectful computation c, and turn exceptions into errors
;; (let/eff ([x c1] c2)) - bind x to the value of c1 in c2, threading state
;; (let/eff* ([x1 c1] [x1 c2] ...) c) - sequentialize instances of let/eff

;;
;; Effect Implementation
;;

;; Computation is Natural -> (list Natural Natural)
;; interp.  a threaded accumulator computation


;; ??? (effect-specific operators)

;; return/eff
(define-syntax return/eff
  (syntax-rules ()
    [(_ v) (λ (f) (list v f))]))

;; run/eff
(define-syntax run/eff
  (syntax-rules ()
    [(_ c vi)
     (match-let ([`(,vo ,thacc) (c vi)])
       vo)]))

;; let/eff
(define-syntax let/eff
  (syntax-rules ()
    [(_ ([x c1]) c2)
     (λ (f)
       (match-let ([`(,x ,f^)
                    (c1 f)])
         (c2 f^)))]))


;; Compose many computations
(define-syntax let/eff*
  (syntax-rules ()
    [(_ () c) c]
    [(_ ([x c1] [x* c1*] ...) c2)
     (let/eff ([x c1])
              (let/eff* ([x* c1*] ...) c2))]))


;; End of Effect Abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax use&update
  (syntax-rules (use update)
    [(_ acc
        [use e1]
        [update e2])     
     (λ (acc)
       (list e1 e2))]))

(define-syntax get-state
  (syntax-rules ()
    ((_) (λ (acc) (list acc acc)))))

(define-syntax update-state
  (syntax-rules ()
    [(_ v) (λ (acc0) (list acc0 v))]))

;; interp.  a function that awaits an accumulator value and produces a
;; number and an accumulated value.

(define (split-score2 split0)
  (local [;; Split -> Computation
          (define (split-score--acc s)
            (cond [(empty? s) (return/eff 0)]
                  [(number? s)
                   #;
                   (let ([value (* s factor)])
                     (begin
                       (when (= s 5)
                         (set! factor (* factor 2)))
                       value))
                   (let/eff* ([factor (get-state)]
                              [value (return/eff (* s factor))]
                              [_ (if (= s 5)
                                     (update-state (* factor 2))
                                     (return/eff -99))])
                             (return/eff value))]
[else ; (list? s)
 (let/eff ([sum1 (split-score--acc (cut-first s))])
          (let/eff ([sum2 (split-score--acc (cut-second s))])
                   (return/eff (+ sum1 sum2))))]))]
(run/eff (split-score--acc split0) 1)))


(test (split-score2 S1) 751)
(test (split-score2 S2) 0)
(test (split-score2 S3) 1291)




;; And now with a mutation-based accumulator

(define (split-score! split0)
  ;; Accumulator: factor is Natural
  ;; Invariant: factor is (expt 2 N) where N is the number of 5's between
  ;;            split0 and split, following in-order traversal.
  (local [(define factor (void))
          ;; Split -> Natural
          ;; Effect: mutates factor
          (define (split-score--acc s)
            (cond [(empty? s) 0]
                  [(number? s) (let ([value (* s factor)])
                                 (begin
                                   (when (= s 5)
                                     (set! factor (* factor 2)))
                                   value))]
                  [else ; (cut? s)
                   (let* ([sum1 (split-score--acc (cut-first s))]
                          [sum2 (split-score--acc (cut-second s))])
                     (+ sum1 sum2))]))]
    (begin (set! factor 1)
           (split-score--acc split0))))


(test (split-score! S1) 751)
(test (split-score! S2) 0)
(test (split-score! S3) 1291)

