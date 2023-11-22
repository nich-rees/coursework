#lang plai
(print-only-errors)

;;
;; env-data.rkt Association-List-Based Environments
;;


;; (envof X) is (listof (list Symbol X))
;; interp.  bindings of identifiers to objects of type X

;; (X -> Boolean) -> Y -> Boolean
;; produce a predicate that yields true if given an (envof P) otherwise false
(define (envof p?)
  (local [(define (pred? x)
            (match x
              ['() #t]
              [`((,k ,v) . ,x^)
               (and (symbol? k)
                    (p? v)
                    (pred? x^))]
              [else #f]))]
    pred?))

;; (envof X)
(define empty-env empty)

;; (envof X) (listof Symbol) (listof X) -> (envof X)
;; produce an environment that binds distinct symbols in x* to objects in v*
;; ASSUME: (= (length x*) (length v*))
;; ASSUME: (unique? x*)
(define (extend-env* env x* v*)
  (let ([alist (map list x* v*)]) ;; make an association list
    (append alist env)))


;; (envof X) Symbol X -> (envof X)
;; extend the given environment to bind x0 to v0
(define (extend-env env x v)
  (extend-env* env (list x) (list v)))


;; (envof X) Symbol -> X
;; look up x in env
;; Effect: signal an error if the identifier is not in the environment
(define (lookup-env env0 x0)
  (let loop ([env env0])
    (match env
      ['() (error 'lookup-env "Undefined identifier: ~a" x0)]
      [`((,x ,v) . ,env^)
       (if (symbol=? x x0)
           v
           (loop env^))])))

(test (lookup-env (extend-env* empty-env '(x y z) '(5 6 7)) 'y) 6)
(test (lookup-env (extend-env*
        (extend-env* empty-env '(x y z) '(5 6 7))
        '(a x c) '(5 6 7)) 'x) 6)
(test (lookup-env (extend-env*
        (extend-env* empty-env '(x y z) '(5 6 7))
        '(a x c) '(5 6 7)) 'z) 7)

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
