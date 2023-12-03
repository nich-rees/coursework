#lang racket
(require "../../lecture_starters/nov20/trampoline.rkt")

(define (fib n)
  (cond
    [(zero? n) 1]
    [(= 1 n) 1]
    [else
     (+ (fib (- n 1)) (fib (- n 2)))]))

(define (fib2 n0)
  ;; accumulator blahbla bhlaghq
  (local [(define (fib n k)
            (cond
              [(zero? n) (bounce (λ () (k 1)))]
              [(= 1 n) (bounce (λ () (k 1)))]
              [else
               (bounce
                (λ ()
                  (fib (- n 1)
                       (λ (v1)
                         (bounce
                          (λ ()
                            (fib (- n 2)
                                 (λ (v2)
                                   (k (+ v1 v2))))))))))]))]
    (mount-trampoline (bounce (λ () (fib n0 (λ (n) (dismount n))))))))