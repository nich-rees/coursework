#lang plai
(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

(require "../sept18/parsing.rkt")
;;
;; AE - a language of Arithmetic Expressions
;;


;; The following data definition represents AE programs in an
;; easy-to-work-with form

(define-type AE     
  [num (n number?)] ; Why AE? not num?
  [add (l AE?) (r AE?)]
  [sub (l AE?) (r AE?)])
;; interp.  program in the AE language, corresponding to the following
;; Backus-Naur Form (BNF)-ish specification 
;;   <AE> ::= <num>
;;          | { + <AE> <AE> }
;;          | { - <AE> <AE> }

;; Exercise: Write some AE examples
(define AE1 (num 1))
(define AE2 (num -2))
(define AE3 (add (num 3) (num 2)))
(define AE4 (sub AE3 AE2))


;; Exercise:  Flesh out the template for AE programs
(define (fn-for-ae ae)
  (type-case AE ae
    [num (n) (... n)]
    [add (l r) (... (fn-for-ae l)
                    (fn-for-ae r))]
    [sub (l r) (... (fn-for-ae l)
                    (fn-for-ae r))]))


;; Value is Number
;; interp. The result of evaluating an AE expression
(define V0 9)
(define V1 99)

(define (fn-for-value v)
  (... v))



;; Exercise: Design an interpreter for AE programs

;; Helper Functions will go here



;; The interp function is the "glue" that binds your helpers (i.e. API) together

;; AE -> number
;; produces the number to which the given ae program evaluates

;(define (interp/ae ae) 0)
(define (interp/ae ae)
  (type-case AE ae
    [num (n) n]
    [add (l r) (+ (interp/ae l)
                    (interp/ae r))]
    [sub (l r) (- (interp/ae l)
                    (interp/ae r))]))

;(define AE1 (num 1))
(test (interp/ae AE1) 1)
;(define AE2 (num -2))
(test (interp/ae AE2) -2)
;(define AE3 (add (num 3) (num 2)))
(test (interp/ae AE3) 5)
;(define AE4 (sub AE3 AE2))
(test (interp/ae AE4) 7)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; AEFS (AE-focused s-expression) is one of:
;; - Number
;; - `{+ ,AEFS ,AEFS}
;; - `{- ,AEFS ,AEFS}
;; - <any other s-expression>
;; interp.  a symbolic expression, but with a focus on those that
;; represent AE expressions.

;; Exercise: Write some AEFS examples
;(define AE1 (num 1))
(define AEFS1 1)
;(define AE2 (num -2))
(define AEFS2 -2)
;(define AE3 (add (num 3) (num 2)))
(define AEFS3 '(+ 3 2))
;(define AE4 (sub AE3 AE2))
(define AEFS4 `(- ,AEFS3 ,AEFS2))

;; EXERCISE: Elaborate a template for AEFS
#;
(define (fn-for-aefs sexp)
  (cond [(number? sexp) (... sexp)]
        [(and (list? sexp)
               (= (length sexp) 3)
               (symbol=? (first sexp) '+))
         (... (fn-for-aefs (second sexp))
              (fn-for-aefs (third sexp)))]
        [(and (list? sexp)
               (= (length sexp) 3)
               (symbol=? (first sexp) '-))
         (... (fn-for-aefs (second sexp))
              (fn-for-aefs (third sexp)))]
        [else (... sexp)]))

;; Math-based template
#;
(define (fn-for-aefs sexp)
  (match sexp
    [`,n #:when (number? n) (... n)]
    [`(+ ,sexp1 ,sexp2) (... (fn-for-aefs sexp1)
                             (fn-for-aefs sexp2))]
    [`(- ,sexp1 ,sexp2) (... (fn-for-aefs sexp1)
                             (fn-for-aefs sexp2))]
    [else (... sexp)]))



;; EXERCISE: Design a parser for AE s-expressions (moar examples first!)

;; AEFS -> AE
;; produce an AE value corresponding to the given AE s-expression 
;; Effect: signals an error if the given s-expression does not represent an ae
; (define (parse sexp) (num 0))
#;
(define (parse sexp)
  (cond [(number? sexp) (num sexp)]
        [(and (list? sexp)
               (= (length sexp) 3)
               (symbol=? (first sexp) '+))
         (add (parse (second sexp))
              (parse (third sexp)))]
        [(and (list? sexp)
               (= (length sexp) 3)
               (symbol=? (first sexp) '-))
         (sub (parse (second sexp))
              (parse (third sexp)))]
        [else (error "bad AE:" sexp)]))

(define (parse sexp)
  (match sexp
    [`,n #:when (number? n) (num n)]
    [`(+ ,sexp1 ,sexp2) (add (parse sexp1)
                             (parse sexp2))]
    [`(- ,sexp1 ,sexp2) (sub (parse sexp1)
                             (parse sexp2))]
    [else (error "bad AE:" sexp)]))

(test (parse '5) (num 5))
(test (parse 5) (num 5))
(test/exn (parse '+) "bad AE")
(test (parse '(+ 5 (- 3 2))) (add (num 5)
                                  (sub (num 3) (num 2))))
(test/exn (parse '(+ 5 (- 3 #f))) "bad AE")

;; Using our data definition examples
(test (parse AEFS1) AE1)
(test (parse AEFS2) AE2)
(test (parse AEFS3) AE3)
(test (parse AEFS4) AE4)

;; EXERCISE: Provide some more examples, especially *bad* ones


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUTTING IT ALL TOGETHER  - an interpreter of files on disk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; string -> Number
;; produce the result of interpreting the AE stored in the file fname
;; EFFECT: signals an error if no file fname contains an AE representation
(define (interp-file fname)
  (interp/ae
   (parse
    (read-from-file fname))))

(test (with-temporary-data-file "{+ 3 7}\n"
        (λ (fname) (interp-file fname)))
      10)

(test (with-temporary-data-file "{+ {- 3 4} 7}\n"
        (λ (fname) (interp-file fname)))
      6)

