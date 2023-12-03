#lang plai

;; Number -> String
;; prints the intervals of the first n levels of the middle thirds Cantor set
#;(define (cantor n0)
    (cond [(not (and (number? n0) (not (negative? n0))))
           (error 'interp "Bad number: ~a" n0)]
          [else
           ;; Accumulators: n is Natural
           ;;               anum is Natural
           ;;               aden is Natural
           ;;               bnum is Natural
           ;;               bden is Natural
           ;; Invariants: the current level
           ;;             numerator of the left bound
           ;;             denominator of the left bound
           ;;             numerator of the right bound
           ;;             denominator of the right bound
           (local [;; Natural Natural Natural Natural -> String
                   (define (frac-interval ln ld rn rd)
                     (string-append "[" ans "/" ads "," bns "/" bds "]"))

                   ;; Natural Natural Natural Natural Natural -> String
                   (define (cantor/helper n anum aden bnum bden)
                     (let ([ns (number->string n)]
                           [ans (number->string anum)]
                           [ads (number->string aden)]
                           [bns (number->string bnum)]
                           [bds (number->string bden)])
                       (cond [(> n n0) ""]
                             [(zero? anum)
                              (string-append "C_{" ns "}: "
                                             (frac-interval ans ads bns bds)
                                             ", ")]
                             [(zero? anum)
                              (string-append "C_{" ns "}: "
                                             (frac-interval ans ads bns bds)", "
                                             (cantor/helper n ))]
                             [else])))]
             (cantor/helper 0))]))


;; Number -> String
;; returns the intervals of the nth level of the middle thirds
;; Cantor set, λ style!
(define (baby-cantor n0)
  ;; Accumulators: n is Natural
  ;;               anum is Natural
  ;;               aden is Natural
  ;;               bnum is Natural
  ;;               bden is Natural
  ;; Invariants: the current level
  ;;             numerator of the left bound
  ;;             denominator of the left bound
  ;;             numerator of the right bound
  ;;             denominator of the right bound
  (local [;; Natural Natural Natural Natural -> String
          (define (frac-interval ln ld rn rd)
            (string-append "[" ln "/" ld "," rn "/" rd "]"))

          ;; Natural Natural Natural Natural Natural -> String
          (define (cantor/helper n anum aden bnum bden)
            (cond [(= n n0)
                   (let ([ns (number->string n)]
                         [ans (number->string anum)]
                         [ads (number->string aden)]
                         [bns (number->string bnum)]
                         [bds (number->string bden)])
                     (string-append
                      (if (zero? anum)
                          (string-append "C_{" ns "}: ")
                          "")
                      (string-append (frac-interval ans ads bns bds) ", ")))]
                  [else
                   (let ([3an (* 3 anum)]
                         [3ad (* 3 aden)])
                     (string-append
                      (cantor/helper (add1 n) 3an 3ad (+ 1 3an) 3ad)
                      (cantor/helper (add1 n) (+ 2 3an) 3ad
                                     (* 3 bnum) 3ad)))]))]
    (cantor/helper 0 0 1 1 1)))

;; Natural -> Display
;; prints the first nth levels of the middle thirds Cantor set
(define (baby-cantor/display n0)
  ;; Accumulator: n is Natural
  ;; Invariant: current level
  (cond [(not (and (number? n0) (not (negative? n0))))
         (error 'baby-cantor "Bad number: ~a" n0)]
        [else
         (display (local [;; Natural -> String
                          (define (display/helper n)
                            (cond [(> n n0) ""]
                                  [else
                                   (string-append (baby-cantor n)
                                                  "\n"
                                                  (display/helper (add1 n)))]))]
                    (display/helper 0)))]))

(baby-cantor/display "-1")