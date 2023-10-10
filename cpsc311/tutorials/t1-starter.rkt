#lang plai
(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

;; Tutorial 1: Grammars, Data Types, Parsing, and All That


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Background Material: AE

(define-type AE     
  [num (n number?)] ; Why AE? not num?
  [add (l AE?) (r AE?)]
  [sub (l AE?) (r AE?)])
;; interp.  program in the AE language, corresponding to the following
;; Backus-Naur Form (BNF) specification 
;;   <AE> ::= <num>
;;          | { + <AE> <AE> }
;;          | { - <AE> <AE> }

(define AE1 (num 4))
(define AE2 (add AE1 (num 5)))
(define AE3 (sub (num 6) (num 3)))
(define AE4 (add (sub (num 6) (num 3))
                 (sub (num 7) (add (num 3) (num 1)))))

#;
(define (fn-for-ae ae)
  (type-case AE ae
    [num (n) (... n)]
    [add (l r) (... (fn-for-ae l)
                    (fn-for-ae r))]
    [sub (l r) (... (fn-for-ae l)
                    (fn-for-ae r))]))


;; AEFS (AE-focused s-expression) is one of:
;; - Number
;; - `(+ ,AEFS ,AEFS)    ;;; (cons '+ (cons AEFS (cons AEFS empty)))
;; - `(- ,AEFS ,AEFS)
;; - <any other s-expression>
;; interp.  a symbolic expression, but with a focus on those that
;; represent AE expressions.
(define AEFS1 4)
(define AEFS2 `{+ ,AEFS1 5})
(define AEFS3 '{- 6 3})
(define AEFS4 `{+ {- 6 3}
                  {- 7 {+ 3 1}}})

#;
(define (fn-for-aefs sexp)
  (match sexp
    [`,n  #:when (number? sexp) (... sexp)]
    [`(+ ,l ,r)
     (... (fn-for-aefs l) (fn-for-aefs r))]
    [`(- ,l ,r)
     (... (fn-for-aefs l) (fn-for-aefs r))]
    [else (... sexp)]))

;; AEFS -> AE
;; produce an AE value corresponding to the given AE s-expression 
;; Effect: signals an error if the given s-expression does not represent an ae
(define (parse-ae sexp)
  (match sexp
    [s #:when (number? s) (num s)]
    [`(+ ,s1 ,s2) (add (parse-ae s1) (parse-ae s2))]
    [`(- ,s1 ,s2) (sub (parse-ae s1) (parse-ae s2))]
    [else (error 'parse "bad AE: ~a" sexp)]))

(test (parse-ae AEFS1) AE1)
(test (parse-ae AEFS2) AE2)
(test (parse-ae AEFS3) AE3)
(test (parse-ae AEFS4) AE4)
(test/exn (parse-ae '+) "bad AE")
(test/exn (parse-ae '(+ 5 (- 3 #f))) "bad AE")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START OF TUTORIAL

;; Extended BNF (EBNF) adds a few convenience operations on nonterminals, that
;; capture common BNF patterns.    


;; 1) Sequences:
;; Given a nonterminal production <X>, putting an asterisk AFTER the nonterminal
;; Like <X>* denotes a sequence of zero or more <X> elements in a row.  In
;; traditional grammars, this typically means juxtaposing characters
;; with some separator (spaces or commas for example) between them,
;; but for S-expressions, we can often use .'s (as in cons pairs) to
;; make things more precise.

;; For example, take our running example:
;; EBNF specification for non-empty lists of AEs
;;   <NELOAE> ::= { <AE> <AE>* }

;; We could rewrite this as a non-extended BNF as follows:
;; <NELOAE> ::= { <AE> . <SOAE> }
;; <SOAE> ::= ()
;;          | { <AE> . <SOAE> }

;; With this format, an s-expression like
;; {5 {+ 9 7} 12}
;; which is equivalent to
;; {5 . { {+ 9 7} .  { 12 . ()} } }
;; counts as a NELOAE.
;; In short, { <AE>* } is to EBNF a lot like what (listof AE) is to data
;; definitions, except that we often want to embed these sequences within
;; a longer list, as you'll see with this week's assignment.



;; 2) Non-empty Sequences
;; Since Non-empty sequences come up often enough, there is already an EBNF
;; notation for them:
;; <NELOAE> ::= { <AE>+ }

;; So + indicates "1 or more" and * indicates "0 or more"


;; 3) Optional (0 or 1)
;; Finally, the notation <X>? means either there is an X or not, for instance:
;; <L> ::= { <X> <Y>? <Z> }
;; can be written in BNF as:
;; <L> ::= {<X> <Z>}
;;       | { <X> <Y> <Z> }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXERCISES

;; Problem 1: Define-type practice
;; Create a define-type for NELOAE

(define-type NELOAE
  [ncons (head AE?) (tail (listof AE?))])
;; interp. non-empty list of AE Expressions
(define NE0 (ncons (num 2) empty))
(define NE1 (ncons (num 3)
                   (list (add (num 1) (num 2))
                         (sub (num 3) (num 4)))))
(define NE2 (ncons (add (num 1) (num 2))
                   (list (num 3)
                         (sub (num 4) (num 5)))))

;; (listof AE) template
#;
(define (fn-for-neloae neloae)
  (type-case NELOAE neloae
    [ncons (head tail) (... (fn-for-ae head)
                            (fn-for-loae tail))]))

;; Problem 2: Match practice
;; Use match to write a parser from s-expressions to NELOAEs

;; LOAEFS is one of:
;; - empty
;; - '(,AEFS . ,LOAEFS)
;; - <any other s-expression>
;; interp. NELOAE-focused s-expressions
;; LOAEFS template

;; Every NELOAEFS is an example of an LOAEFS

#;
(define (fn-for-loaefs sexp)
  (match sexp
    ['() (...)]
    ['(,hd . ,t1) ;; (cons hd t1)
     (... (fn-for-aefs hd)
          (fn-for-loaefs t1))]
    [else (... sexp)]))

;; NELOAEFS is one of:
;; - '(,AFES . ,LOAEFS)
;; - <any other s-expression>
;; interp. NELOAE-focused s-expressions
(define NELOAEFS1 '(7 . (9 . (12 . ()))))
(define NELOAEFS '(7 9 12)) ; same thing as NELOAEFS1
(define NELOAEFS3 7) ; any s-expression counts!

#;
(define (fn-for-neloaefs sexp)
  (match sexp
    ['(.hd . ,t1) ;; (cons hd t1)
     ;; alternatively: (... (fn-for-loaefs '(,hd . ,t1)))
     (... (fn-for-aefs hd)
          (fn-for-loaefs t1))]
    [else (error 'fn-for-neloaefs "msg: ~a" sexp)]))

;; LOAEFS ->
;; ff

;; Problem 3: Accumulator practice
;; Given an NELOAE, generate the AE that subtracts the sum of the tail AEs from
;; the head AE.  To compute the AE that computes the sum of the tail AEs,
;; design a helper function that uses an accumulator.
;; HINT: compute the sum in reverse.
