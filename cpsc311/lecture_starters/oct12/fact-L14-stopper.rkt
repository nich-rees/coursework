#lang plai
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

;; Natural -> Natural
;; produce the factorial of x
(define fact
  (λ (x)
    (if (zero? x)
        1
        (* x (fact (sub1 x))))))

(fact 5)

;; (Natural -> Natural) -> Natural -> Natural
;; something something kinda like fact
(define fact-wrap
  (λ (some-fun)
    (λ (x)
      (if (zero? x)
          1
          (* x (some-fun (sub1 x)))))))

(define F0 identity)
(define F1 (fact-wrap identity))
(define F2 (fact-wrap F1))

(define F5 (foldr (λ (_ fn) (fact-wrap fn)) identity (range 5)))

(define JamieFact (foldr (λ (_ fn) (fact-wrap fn)) (λ (x) 1) (range 5)))

(define factish (foldr (λ (_ fn) (fact-wrap fn)) fact (range 5)))

;; FactMaker is FactMaker -> (Natural -> Natural)
;; interp.  a factorial maker

;; Other true facts:
;; FactMaker is (FactMaker -> (Natural -> Natural)) -> (Natural -> Natural)
;; FactMaker is ((FactMaker -> (Natural -> Natural)) -> (Natural -> Natural)) ->
;;              Natural -> Natural

;; FactMaker -> Natural -> Natural 
(define fact-maker
  (λ (fm)
    (λ (x)
      (if (zero? x)
          1
          (* x ((fm fm) (sub1 x)))))))


(define new-fact (fact-maker fact-maker))

(define new-new-fact ((λ (fm)
                        (λ (x)
                          (if (zero? x)
                              1
                              (* x ((fm fm) (sub1 x))))))
                      (λ (fm)
                        (λ (x)
                          (if (zero? x)
                              1
                              (* x ((fm fm) (sub1 x))))))))

(define bad-fact-maker
  (λ (fm)
    (let ([no (fm fm)])
      (λ (x)
        (if (zero? x)
            1
            (* x (no (sub1 x))))))))


;; Z : ((X -> X) -> (X -> X)) -> (X -> X)
(define Z
  ;; f : (X -> X) -> (X -> X)
  (λ (f) 
    ;; A is A -> (X -> X)
    ;; w : A
    ;;     same as (A -> (X -> X)) -> (X -> X)
    ;;     same as A -> (X -> X)
    (let ([w
           ;; x : A -> (X -> X)
           ;;     same as (A -> (X -> X)) -> (X -> X) 
           (λ (x)
             ;; (λ (a) ((x x) a)) : X -> X
             ;; so (f (λ (a) ((x x) a)) : X -> X
             (f
              ;; a : X
              (λ (a)
                ;; so (x x) : X -> X
                ;; so ((x x) a) : X
                ((x x) a))))])
      ;; so (w w) : X -> X
      (w w))))


;; Y : (X -> X) -> X
(define Y
  ;; f : X -> X
  (λ (f) 
    ;; A is A -> X
    ;; w : (A -> X) -> X
    ;;     same as A -> X
    ;;     same as A
    (let ([w
            ;; x : A -> X 
            ;;     same as (A -> X) -> X
           (λ (x)
             ;; so (x x) : X
             ;; so (f (x x)) : X
             (f (x x)))])
      ;; so (w w) : X
      (w w))))


(define new^3-fact (Z fact-wrap))

(new^3-fact 5)