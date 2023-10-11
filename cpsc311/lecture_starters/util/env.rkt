#lang plai
(print-only-errors)
;;
;; Procedural Environments
;;


;; (envof X) is Symbol -> X
;; interp.  bindings of identifiers to objects of type X

;; (envof X)
(define empty-env
  (λ (x) (error 'lookup-env "Undefined identifier: ~a" x)))

;; (envof X) Symbol X -> (envof X)
;; extend the given environment to bind x0 to v0
(define (extend-env env x0 v0)
  (λ (x)
    (if (symbol=? x x0)
        v0
        (env x))))

;; (envof X) Symbol -> X
;; look up x in env
(define (lookup-env env x)
  (env x))

(test/exn (lookup-env empty-env 'a) "Undefined")
(test (lookup-env (extend-env empty-env 'a 7) 'a) 7)
(test/exn (lookup-env (extend-env empty-env 'a 7) 'b) "Undefined")
(test (lookup-env (extend-env (extend-env empty-env 'a 7)
                              'a 8) 'a)
      8)
(test (lookup-env (extend-env (extend-env empty-env 'a 7)
                              'b 8) 'a)
      7)
(test (lookup-env (extend-env (extend-env empty-env 'b 8)
                              'a 7) 'a)
      7)

(test/exn (lookup-env (extend-env (extend-env empty-env 'b 8)
                                  'a 7) 'c)
          "Undefined")
