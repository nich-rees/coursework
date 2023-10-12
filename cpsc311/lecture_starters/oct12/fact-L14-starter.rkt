#lang plai
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

;; Natural -> Natural
;; produce the factorial of x
(define (fact x)
  (if (zero? x)
      1
      (* x (fact (sub1 x)))))

(fact 5)


;; (Natural -> Natural) -> Natural -> Natural  (parentheses?)
;; something something kinda like fact
(define fact-wrap
  (λ (fact)
    (λ (x)
      (if (zero? x)
          1
          (* x (fact (sub1 x)))))))

(define F0 (fact-wrap identity))
(define F1 (fact-wrap F0))

(define F5 (foldr (λ (_ fn) (fact-wrap fn)) identity (range 5)))

(define factish (foldr (λ (_ fn) (fact-wrap fn)) fact (range 5)))
;; do the correct thing:
;; making layers of fact-wrap, and then after that, call the real factorial

;; fact can somehow replicate itself as much it needs to... like fix
;; ... but we can actually do this just with λ


;; FactMaker is FactMaker -> Natural -> Natural
;; interp. a factorial maker

;; something new: no constructer for FactMaker
;; Other true facts:
;; FactMaker is (FactMaker -> Natural -> Natural) -> Natural -> Natural
;; FactMaker is ((FactMaker -> Natural -> Natural) -> Natural -> Natural)
;;                                                 -> Natural -> Natural
;; Could say it is infinitely expansible

;; (...) -> Natural -> Natural
(define fact-maker
  (λ (fm)
    (λ (x)
      (if (zero? x)
          1
          (* x ((fm fm) (sub1 x)))))))

(define new-fact (fact-maker fact-maker))

(define bad-fact-maker
  (λ (fm)
    (let ([no (fm fm)])
      (λ (x)
        (if (zero? x)
            1
            (* x (no (sub1 x))))))))
;; Need to delay throwing our layers to do the computation
;; If we throw all the layers at once, will crash DrRacket
;; before we can compute anything


;; ((X -> X) -> (X -> X)) -> (X -> X)
(define Z
  (λ (f)
    (let ([w (λ (x) (f (λ (a) ((x x) a))))])
      (w w))))

;; (X -> X) -> X
(define Z^
  (λ (f)
    ;; A is A -> X
    ;; w : (A -> X) -> (A -> X)
    (let ([w (λ (x) (f (λ (a) (x x))))])
      (w w))))

;; (X -> X) -> X
(define Y
  (λ (f)
    ;; A is A -> X
    ;; w : (A -> X) -> (A -> X)
    (let ([w (λ (x) (f (x x)))])
      (w w))))
;; Doesn't work? Next time, will see some languages where
;; the Y combinator works