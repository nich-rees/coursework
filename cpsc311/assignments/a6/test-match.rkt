#lang plai
(print-only-errors)

;;
;; test-match.rkt: A helper for using match with PLAI tests
;;


;; Utility Helpers

;; A testing helper: use match in PLAI tests
;; WARNING: Serious tomfoolery to get a decent failure message
;; WARNING: MOAR tomfoolery to get decent source line info in error messages
(define-syntax test/match
  (λ (x)
    (syntax-case x ()
      [(k expr pat)
       (datum->syntax #'k
                      (syntax->datum
                       #'(local [(define (|test/match success| e)
                                   (match e
                                     [pat #t]
                                     [else #f]))]
                           (test/pred expr |test/match success|)))
                      #'k)]
      [(k expr pat #:when when-expr)
       (datum->syntax #'k
                      (syntax->datum
                       #'(local [(define (|test/match success| e)
                                   (match e
                                     [pat #:when when-expr #t]
                                     [else #f]))]
                           (test/pred expr |test/match success|)))
                      #'k)])))


(test/match (let ([g 'g])
              `{with {,g 3} {+ ,g b}})
            `{with {,g 3} {+ ,g b}})

(test/match (let ([g (gensym)])
              `{with {,g 3} {+ ,g b}})
            `{with {,g 3} {+ ,g b}} #:when (not (symbol-interned? g)))

;; Following demonstrates a test failure, and menh error message.
#;
(test/match (let ([g 'g])
              `{with {,g 3} {+ ,g b}})
            `{with {,g 3} {+ ,g b}} #:when (not (symbol-interned? g)))