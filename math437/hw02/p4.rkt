#lang plai

(require math/number-theory)

(define (print-vals n)
  (local [(define (ligma x fact)
            (cond [(eq? x n)
                   (list x
                      (expt 2
                            (foldl (λ (p e rsf)
                                     (* rsf
                                        (+ (max-dividing-power p fact)
                                           e 1)))
                                   1
                                   (prime-divisors x)
                                   (prime-exponents x))))]
                  [(cons
                    (list x
                       (expt 2
                             (foldl (λ (p e rsf)
                                      (* rsf
                                         (+ (max-dividing-power p fact)
                                            e 1)))
                                    1
                                    (prime-divisors x)
                                    (prime-exponents x))))
                   (ligma (+ x 1) (* fact x)))]))]
(ligma 2 1)))