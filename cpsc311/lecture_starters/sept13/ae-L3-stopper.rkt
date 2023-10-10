#lang plai
(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

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

(define (interp-ae) 0)


