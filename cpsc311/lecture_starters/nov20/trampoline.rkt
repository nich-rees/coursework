#lang racket
(require plai/datatype)
(provide (all-defined-out))
(define (... . args) (cons '... args))

;; Trampolining! - translate tail calls into a language that lacks them

(define-type trampoline
  [bounce [p procedure?]]
  [dismount [v (λ (x) #t)]])

;; (trampolineof X) is one of:
;; - (bounce ( -> (trampolineof X))
;; - (dismount X)
;; interp.  A 

(define (fn-for-trampoline t)
  (type-case trampoline t
    [bounce (p) (... (fn-for-trampoline (p)))]
    [dismount (v) (... v)]))

;; NOT USED HERE: provided for comparison to Java

;; (trampolineof X) -> Boolean
;; determine whether a trampoline exhibits a done computation
(define (done? t)
  (type-case trampoline t
    [bounce (p) #f]
    [dismount (v) #t]))


;; (trampolineof X) -> X
;; run the given trampoline to completion
(define (mount-trampoline t)
  (type-case trampoline t
    [bounce (p) (mount-trampoline (p))]
    [dismount (v) v]))

;;
;; Java-Style trampoline scheduler
;;

;; While loop implementation (using tails calls and macros of course!)
;; (-> Boolean) (-> Void) -> Void
(define (while-fn pred do)
  (when (pred)
    (begin (do)
           (while-fn pred do))))

(define-syntax while
  (syntax-rules ()
    [(while pred body)
     (while-fn (λ () pred) (λ () body))]))


;; (trampolineof X) -> X
;; run the given trampoline to completion
(define (loop-trampoline t0)
  ;; Accumulator: t is (trampolineof X)
  ;; Invariant: t represents pending computation (if any)
  (local [(define t (void))] ;; I always start mutable accumulators at (void)
    (begin
      (set! t t0)
      (while (bounce? t)
             (let ([c (bounce-p t)])
               (set! t (c))))
      ;; t is now a dismount
      (dismount-v t))))