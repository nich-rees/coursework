#lang plai
(define (... . args) (cons '... args))
(print-only-errors)

;;
;; score-a-lot - the pointless solitary card game this pandemic needs.
;;


;; Game 1: Simple Score-A-Lot
;; 1. Take all the numbered cards in your card deck (no aces, jokers, faces).
;;    Feel free to combine as many janky old decks as you want.
;; 2. Shuffle.
;; 3. Take the deck and start turning cards off the top.
;; 4. Start summing up your score.
;; 5. Whenever you draw a 5, double the worth of every subsequent card.
;; 6. Sum up your score and bask in your big score!


;; Run is (listof Natural[2,10])
;; interp.  a sequence of plays (numeric playing cards, to be scored)
(define R1 '(2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 7 8 9 10))
(define R2 '())
(define R3 '(5 5 5 5 2 2 2 2 3 3 3 3 4 4 4 4 6 7 8 9 10))

;; Template: list template 


;; Run -> Natural
;; Calculate the score induced by this run
(define (run-score run0)
  ;; Accumulator: factor is Natural
  ;; Invariant: factor is (expt 2 N) where N is the number of 5's in run0
  ;;            before run.
  (local [;; Run Natural -> Natural
          (define (run-score--acc run factor)
            (cond [(empty? run) 0]
                  [else (+ (* (first run) factor)
                           (run-score--acc (rest run)
                                           (if (= (first run) 5)
                                               (* factor 2)
                                               factor)))]))]
    ;; 1 = (expt 2 0)
    (run-score--acc run0 1)))


(test (run-score empty) 0)
(test (run-score '(1 2 3 4)) 10)
(test (run-score '(1 2 3 4 5)) 15)
(test (run-score '(5 1 2 3 4)) 25)
(test (run-score '(5 1 2 3 5 4)) 43)
(test (run-score R1) 751)
(test (run-score R3) 1291)


;; Problem 1: design of the same function, but using a MUTATION-based
;; accumulator (gasp!)

;; Run -> Natural
;; Calculate the score induced by this run
(define (run-score! run0)
  ;; Accumulator: factor is Natural
  ;; Invariant: factor is (expt 2 N) where N is the number of 5's in run0
  ;;            before run.
  (local [(define factor (void))
          ;; Run -> Natural
          ;; Effect: mutates factor
          (define (run-score--acc run)
            (cond [(empty? run) 0]
                  [else
                   (let ([value (* (first run) factor)])
                     (begin
                       (set! factor (if (= (first run) 5)
                                        (* factor 2)
                                        factor))
                       (+ value 
                          (run-score--acc (rest run)))))]))]
    ;; 1 = (expt 2 0)
    (begin (set! factor 1)
           (run-score--acc run0))))


(test (run-score! empty) 0)
(test (run-score! '(1 2 3 4)) 10)
(test (run-score! '(1 2 3 4 5)) 15)
(test (run-score! '(5 1 2 3 4)) 25)
(test (run-score! '(5 1 2 3 5 4)) 43)
(test (run-score R1) 751)
(test (run-score R3) 1291)





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


;; Problem 2: Complete the design of a scoring function for Split Score-a-lot

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


;; Problem 3: design of the same function, but using a MUTATION-based
;; accumulator

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

