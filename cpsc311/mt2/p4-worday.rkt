#lang plai

(define (... . args) (cons '... args)) ;; enables us to use ... in templates
(print-only-errors #t)

;; Worday: Word Play Without The "PL"


;; Problem 4a (line 116): complete the design of the slctns function 
;; Problem 4b (line 140): complete the design of the perms function


;; Effect abstractions can be especially useful for designing programs that
;; "normal people"  (unlike us cool PL people) would not consider to count as
;; a programming language (though all data must be interpreted).

;; In this problem, you will exploit a backtracking effect abstraction to
;; implement some programs that are a PITA to implement correctly without it.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effect Abstraction

;;
;; Effect Interface
;;

;; (amb/eff c1 c2) - produce the result of c1 unless it (or its continuation)
;;                   fails, in which case produce the result of c2
;; (fail/eff) - fail the current computation

;; The Generic Interface Components:
;; (return/eff e) - return the value of e
;; (run/eff c n) - run an effectful computation, producing a list of <=n results
;; (let/eff ([x c1] c2)) - bind x to the next successful value of c1 in c2
;; (let/eff* ([x* c1*] ...) c2) - sequentialize instances of let/eff

;;
;; Effect Implementation
;;


;; (fkof X) is ( -> X)
;; interp. a failure continuation

;; (skof X) is (X (fkof X) -> X)


;;
;; (computationof X) is (skof X) (fkof X) -> X
;; interp. a two-continuation model of backtracking computations


;; (computationof X) (computationof X) -> (computationof X)
;; merge the two given streams
(define-syntax amb/eff
  (syntax-rules ()
    [(_ c1 c2)
     (λ (sk0 fk0)
       (local [(define sk sk0)
               (define (fk)
                 (c2 sk0 fk0))]
         (c1 sk fk)))]))

;; (computationof X)
;; return a failing (computationof X)
(define (fail/eff)
  (λ (sk fk) (fk)))


;; Value -> (computationof X)
;; "return" a successful value (by embedding it in a (computationof X))
(define (return/eff v)
  (λ (sk fk) (sk v fk)))


;; (computationof X) (Natural or false) -> (listof X)
;; produce at most n-initial (or all if false) values from c-initial
;; Effect: Signal a runtime error if it is an exception
(define (run/eff c-initial n-initial)
  (local [(define n n-initial)
          (define (sk v fk)
            (cond [(false? n) (cons v (fk))]
                  [(= 1 n) (cons v empty)]
                  [else (begin
                          (set! n (sub1 n))
                          (cons v (fk)))]))
          (define (fk) empty)]
    (if (and (number? n) (zero? n))
        empty
        (c-initial sk fk))))


;; Compose two computations
(define-syntax let/eff
  (syntax-rules ()
    [(_ ([x c1]) c2)
     (λ (sk0 fk0)
       (local [(define (sk x fk) (c2 sk0 fk))
               (define fk fk0)]
         (c1 sk fk)))]))      


;; Compose many computations
(define-syntax let/eff*
  (syntax-rules ()
    [(_ () e) e]
    [(_ ([x e1] [x* e1*] ...) e2)
     (let/eff ([x e1])
              (let/eff* ([x* e1*] ...) e2))]))

;; End of Effect Abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Problem 4a: Complete the design of the slctns function using an effect
;; abstraction to generate the variations.

;; The slctns function produces a list of all possible "selection strings" of
;; characters, in order, from a given string.  A selection keeps some characters
;; and omits others: indeed the name "slctns" is a selection of "selections".
;; As another example, the selections of the string "ape" are
;; 8 strings including "ae", "pe" and "" among them.  In particular,
;; slctns returns (exp 2 (length str)) selection strings for any string str.

;; SlctnComputation is (computationof String)
;; interp. a selection from some input

;; String -> (listof String)
;; produce a list of all selection strings from str
(define (slctns str)
  (local [;; (listof Character) -> SlctnComputation
          (define (slctn/eff chars) ; stub
            (cond [(empty? chars) (return/eff "")]
                  [else (let/eff ([result (slctn/eff (rest chars))])
                                 (amb/eff (return/eff
                                           (string-append (string (first chars))
                                                          result))
                                          (return/eff
                                           (string-append "" result))))]))]
    (run/eff (slctn/eff (string->list str)) #f)))
  
(test (slctns "") (list ""))
(test (slctns "ae") (list "ae" "e" "a" ""))


;; Problem 4b: Complete the design of the perms function
;; NOTE!!!! ONLY WORK ON THIS PROBLEM ONCE YOU ARE SATISFIED WITH YOUR
;; WORK ON THE REMAINDER OF THE EXAM.  OUR MARKING SCHEME IS BASED AROUND
;; THAT EXPECTATION


;; The perms function produces a list of all permutations of the given
;; string.  A permutation is a reordering of the given characters.
;; For example, permutations of "ape" are 6 strings including "ape",
;; "pea", and "eap". Note that permutations are of characters, so if you have
;; any repeated characters, then some of your permutations will appear more
;; than once.  In particular In particular, perms returns (fact (length str))
;; strings for any string str.
;; Hint: The model solution to this problem involved an additional helper.

;; PermComputation is (computationof (listof Character))
;; interp. a permutation of some input

;; String -> (listof String)
;; produce a list of all permutations of str
(define (perms str)
  (local [;; (listof Character) -> PermComputation
          (define (perm/eff chars) ; stub
            (return/eff empty))]
    (let ([char-lists (run/eff (perm/eff (string->list str)) #f)])
      (map list->string char-lists))))


