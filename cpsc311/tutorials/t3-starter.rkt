#lang plai
(define (... . args) (cons '... args))
(print-only-errors #t)

;; CPSC 311 Winter Term 1
;; Tutorial 3: The WAE to Obfuscate

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Utility Helpers

;; A testing helper: use match in PLAI tests
;; WARNING: failures yield messy messages!
(define-syntax test/match
  (syntax-rules ()
    [(_ expr pat whens ...)
     (test (match expr
             [pat whens ... #t]
             [else #f #;(error 'test/match
                               "failed:\nexpected pattern: ~a\ngot ~a"
                               'pat expr)])
           #t)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type WAE     
  [num/wae (n number?)]
  [add/wae (lhs WAE?) (rhs WAE?)]
  [sub/wae (lhs WAE?) (rhs WAE?)]
  [with/wae (id symbol?) (named-expr WAE?) (body WAE?)]
  [id/wae (name symbol?)])
;; interp.  program in the WAE language, corresponding to the following
;; Backus-Naur Form (BNF) specification 
;;   <WAE> ::= <num>
;;          | { + <WAE> <WAE> }
;;          | { - <WAE> <WAE> }
;;          | { with {<id> <WAE>} <WAE>}
;;          | <id>

(define WAE1 (with/wae 'x (add/wae (num/wae 5)
                                   (num/wae 5))
                       (add/wae (id/wae 'x)
                                (id/wae 'x))))
(define WAE2 (with/wae 'x (add/wae (num/wae 5)
                                   (num/wae 5))
                       (with/wae 'y (sub/wae (id/wae 'x)
                                             (num/wae 3))
                                 (add/wae (id/wae 'y)
                                          (id/wae 'y)))))
(define WAE3 (with/wae 'x (num/wae 5)
                       (add/wae (id/wae 'x)
                                (with/wae 'x (num/wae 3)
                                          (num/wae 10)))))
(define WAE4 (with/wae 'x (num/wae 5)
                       (add/wae (id/wae 'x)
                                (with/wae 'x (num/wae 3)
                                          (id/wae 'x)))))
(define WAE5 (with/wae 'x (num/wae 5)
                       (add/wae (id/wae 'x)
                                (with/wae 'y (num/wae 3)
                                          (id/wae 'x)))))
(define WAE6 (with/wae 'x (with/wae 'x (num/wae 5)
                                    (add/wae (id/wae 'x)
                                             (num/wae 2)))
                       (add/wae (id/wae 'x)
                                (with/wae 'y (num/wae 3)
                                          (id/wae 'x)))))

#;
(define (fn-for-wae wae)
  (type-case WAE wae
    [num/wae (n) (... n)]
    [add/wae (l r) (... (fn-for-wae l)
                        (fn-for-wae r))]
    [sub/wae (l r) (... (fn-for-wae l)
                        (fn-for-wae r))]
    [with/wae (id named body) (... id
                                   (fn-for-wae named)
                                   (fn-for-wae body))]
    [id/wae (x) (... x)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (listof Symbol) -> Boolean
;; Produce true if each symbol in x* appears only once.
(define (unique? lox)
  (if (check-duplicates lox) #f #t))

(test (unique? '()) #t)
(test (unique? '(a)) #t)
(test (unique? '(a b c d)) #t)
(test (unique? '(a b c b)) #f)
(test (unique? '(a a c d)) #f)

;; (listof s-expr) -> Boolean
;; Produce true if the input is a list of unique s-expressions, else false.
(define (unique-symbols? lox)
  (and (andmap symbol? lox)
       (unique? lox)))

(test (unique-symbols? '(x y z)) #t)
(test (unique-symbols? '(x y x)) #f)
(test (unique-symbols? '(x y 1)) #f)

;;
;; Identifier name mappings
;;

;; Mappings is (listof (cons Symbol Symbol))
;; interp. a table mapping from identifier names to identifier names

;; Mappings
;; An empty mappings table.
(define mt-mappings empty)

;; Symbol Symbol Mappings -> Mappings
;; Adds a mapping from the first identifier name to the second.
(define (extend-mappings x y mappings)
  (cons `(,x . ,y) mappings))

(test (extend-mappings 'a 'b empty) '((a . b)))
(test (extend-mappings 'a 'b '((c . d))) '((a . b) (c . d)))
(test (extend-mappings 'a 'b '((p . q) (a . e))) '((a . b) (p . q) (a . e)))

;; Symbol Mappings -> Symbol
;; Produces the first identifier name that the given name maps to if any,
;; otherwise itself.
(define (resolve-mapping x mappings)
  (cond [(assoc x mappings) => cdr]
        [else x]))

(test (resolve-mapping 'x empty) 'x)
(test (resolve-mapping 'x '((y . z) (a . b))) 'x)
(test (resolve-mapping 'x '((x . y))) 'y)
(test (resolve-mapping 'x '((a . b) (x . z))) 'z)
(test (resolve-mapping 'x '((x . a) (x . b))) 'a)

;;
;; EXERCISE: Obfuscate/Deobfuscate
;;

;; Complete the design of a function that obfuscates bound identifier names
;; in WAE using gensym. The function must also produce a deobfuscation table.
;; Note that free identifiers should not be obfuscated.

;; WAE -> (list WAE Mappings)
;; Obfuscates a WAE program's names and produces its deobfuscation table
;; (i.e. NEW to OLD names).
(define (obfuscate/wae wae)
  ;; Accumulator: mappings is Mappings
  ;; Invariant: Maps variables bound in context to their obfuscated name
  ;; (i.e. OLD to NEW names).
  (local [;; WAE Mappings -> (list WAE Mappings)
          (define (obfuscate/wae wae mappings) ;; TODO: complete this function
            (type-case WAE wae
              [num/wae (n) (list (num/wae n) mappings)]
              [add/wae (l r)
                       (let ([lres (obfuscate/wae l mappings)]
                             [rres (obfuscate/wae l mappings)])
                         (list (add/wae (car lres) (car rres))
                               (append (cadr lres) (cadr rres))))]
              [sub/wae (l r)
                       (let ([lres (obfuscate/wae l mappings)]
                             [rres (obfuscate/wae l mappings)])
                         (list (sub/wae (car lres) (car rres))
                               (append (cadr lres) (cadr rres))))]
              [with/wae (x named body)
                        (let* ([g (gensym)]
                               [namedres (obfuscate/wae named mappings)]
                               [mappings^ (cadr namedres)])
                          (if (empty?
                               (filter (λ (s) (symbol=? x (car s)))
                                       mappings^))
                              (let ([bodyres
                                     (obfuscate/wae
                                      body (extend-mappings x x mappings^))])
                                (list (with/wae x (car namedres) (car bodyres))
                                      (cdr bodyres)))
                              (let ([bodyres
                                     (obfuscate/wae
                                      body (extend-mappings g x mappings^))])
                                (list (with/wae g (car namedres) (car bodyres))
                                      (cdr bodyres)))))]
              [id/wae (x) (list (let ([lopox
                                       (filter (λ (s) (symbol=? (cdr s) x))
                                               mappings)])
                                  (if (empty? lopox)
                                      (id/wae x) ;; The interp handles the error
                                      (id/wae (caar lopox))))
                                mappings)]))]
    (obfuscate/wae wae mt-mappings)))

(test/match (obfuscate/wae WAE1)
            (list (with/wae x (add/wae (num/wae 5)
                                       (num/wae 5))
                            (add/wae (id/wae x)
                                     (id/wae x)))
                  (list (cons x 'x)))
            #:when (symbol? x))
(test/match (obfuscate/wae WAE2)
            (list (with/wae x (add/wae (num/wae 5)
                                       (num/wae 5))
                            (with/wae y (sub/wae (id/wae x)
                                                 (num/wae 3))
                                      (add/wae (id/wae y)
                                               (id/wae y))))
                  (list (cons x 'x)
                        (cons y 'y)))
            #:when (unique-symbols? (list x y)))
(test/match (obfuscate/wae WAE3)
            (list (with/wae x1 (num/wae 5)
                            (add/wae (id/wae x1)
                                     (with/wae x2 (num/wae 3)
                                               (num/wae 10))))
                  (list (cons x1 'x)
                        (cons x2 'x)))
            #:when (unique-symbols? (list x1 x2)))
(test/match (obfuscate/wae WAE4)
            (list (with/wae x1 (num/wae 5)
                            (add/wae (id/wae x)
                                     (with/wae x2 (num/wae 3)
                                               (id/wae x2))))
                  (list (cons x1 'x)
                        (cons x2 'x)))
            #:when (unique-symbols? (list x1 x2)))
(test/match (obfuscate/wae WAE5)
            (list (with/wae x (num/wae 5)
                            (add/wae (id/wae x)
                                     (with/wae y (num/wae 3)
                                               (id/wae x))))
                  (list (cons x 'x)
                        (cons y 'y)))
            #:when (unique-symbols? (list x y)))
(test/match (obfuscate/wae WAE6)
            (list (with/wae x1 (with/wae x2 (num/wae 5)
                                         (add/wae (id/wae x2)
                                                  (num/wae 2)))
                            (add/wae (id/wae x1)
                                     (with/wae y (num/wae 3)
                                               (id/wae x1))))
                  (list (cons x1 'x)
                        (cons x2 'x)
                        (cons y 'y)))
            #:when (unique-symbols? (list x1 x2 y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; List-Based Environments
;;

;; Env is (listof (cons symbol number))

;; empty-env : Env
(define empty-env '())

;; extend-env : Env (listof Symbol) (listof Number) -> Env
;; Produce an environment that binds distinct symbols in x* to values in v*.
;; ASSUME: (= (length x*) (length v*))
;; ASSUME: (unique? x*)

(define (extend-env env x* v*)
  (append (map cons x* v*) env))

(test (extend-env empty-env '(x y z) '(5 6 7)) '((x . 5)
                                                 (y . 6)
                                                 (z . 7)))
(test (extend-env
       (extend-env empty-env '(x y z) '(5 6 7))
       '(a x c) '(5 6 7))
      '((a . 5)
        (x . 6)
        (c . 7)
        (x . 5)
        (y . 6)
        (z . 7)))

;; lookup-env : Env Symbol -> Number
;; Produce the binding for the given symbol.
;; Effect: Signals an error if no binding is found.
(define (lookup-env env x)
  (cond [(empty? env) (error 'lookup-env "unbound identifier: ~a" x)]
        [else (if (symbol=? x (car (first env)))
                  (cdr (first env))
                  (lookup-env (rest env) x))]))

(test (lookup-env (extend-env
                   (extend-env empty-env '(x y z) '(5 6 7))
                   '(a x c) '(5 6 7))
                  'z)
      7)

;; interp/wae : WAE -> Number
;; Interpret the given expression.
;; Effect: Signals an error if an unbound identifier is present.

(define (interp/wae wae)
  (local [;; Accumulator: env is Env
          ;; Invariant: env represents bindings induced by pending
          ;; substitutions into wae.
          
          ;; WAE Env -> number
          (define (interp/wae--env wae env)
            (type-case WAE wae
              [num/wae (n) n]
              [add/wae (l r) (+ (interp/wae--env l env)
                                (interp/wae--env r env))]
              [sub/wae (l r) (- (interp/wae--env l env)
                                (interp/wae--env r env))]
              [with/wae (id named body)
                        (interp/wae--env
                         body
                         (extend-env env
                                     (list id)
                                     (list (interp/wae--env named env))))]
              [id/wae (x) (lookup-env env x)]))]
    (interp/wae--env wae empty-env)))

(test (interp/wae WAE1) 20)
(test (interp/wae WAE2) 14)
(test (interp/wae WAE3) 15)
(test (interp/wae WAE4) 8)
(test (interp/wae WAE5) 10)
(test (interp/wae WAE6) 14)
