#lang plai

;; test-diverge.rkt: Helpers for diverging programs


;; Resource-bounded tests
(require racket/sandbox)
;; test/timeout works like test, but fails if actual running
;; takes more than 5 seconds or more than 256mb of memory
(define-syntax test/timeout
  (syntax-rules ()
    [(test/timeout actual expected)
     (test (with-limits 5 256 actual) expected)]))

;; (test/diverge expr time) ensures expr runs for at least n seconds 
;; (because we can't guarantee it will run forever...
(define-syntax test/diverge
  (syntax-rules ()
    [(test/diverge expr n)
     (test/exn
      (with-handlers ([exn:fail:resource?
                       (λ (e) (error 'diverge "Probably diverges"))])
        (with-limits n #f expr)) "Probably diverges")]))
