#lang plai
(require "../util/parsing.rkt")
(require "../util/test-diverge.rkt")

(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

;; Hammock: kinda like Hannah, but not.
;; A functional language with:
;; - arithmetic
;; - conditional expressions
;; - pairs/lists
;; - first-class functions
;; - ****non-strict semantics***   <--- This is the big difference!
;;    + call-by-name
;;    + non-strict pairs (sometimes called "lazy" pairs, but that word is
;;      fraught)

;; Named in memory of Summer  :: sniff! ::


;; HKID is Symbol
;; INVARIANT: an HKID cannot be equal to any Hammock keyword
(define (hkid? x)
  (local [(define KEYWORDS
            '(isnumz + - with if0 fun isfunz fix fixFun
                     pair ispairz left right mt ismtz))]
    (and (symbol? x)
         (not (member x KEYWORDS)))))

(define HKID0 'a)
(define HKID1 'b)


(define-type HK
  [num (n number?)]
  [isnumz (e HK?)]
  [add (lhs HK?) (rhs HK?)]
  [sub (lhs HK?) (rhs HK?)]
  [id (name hkid?)]
  [fun (param hkid?) (body HK?)]
  [isfunz (e HK?)]
  [app (rator HK?) (arg HK?)]
  [if0 (predicate HK?) (consequent HK?) (alternative HK?)]
  [fix (name hkid?) (body HK?)]
  [pair (left HK?) (right HK?)]
  [ispairz (e HK?)]
  [left (e HK?)]
  [right (e HK?)]
  [mt]
  [ismtz (e HK?)])
;; interp. expressions in a language that supports applying first-class
;; functions. Its syntax is defined by the following BNF:
;; <HK> ::=
;; (ARITHMETIC)
;;          <num>
;;        | {isnumz <HK>}
;;        | {+ <HK> <HK>}
;;        | {- <HK> <HK>}
;; (IDENTIFIERS)
;;        | {with {<id> <HK>} <HK>}
;;        | <id>
;; (CONDITIONALS)
;;        | {if0 <HK> <HK> <HK>}
;; (FUNCTIONS)
;;        | {<HK> <HK>}
;;        | {fun {<id>} <HK>}
;;        | {isfunz <HK>}
;; (RECURSION)
;;        | {fix <id> <HK>}
;;        | {fixFun <id> <id> <HK>}
;;        | {rec {<id> <HK>} <HK>}
;; (PAIRS/LISTS)
;;        | {pair <HK> <HK>}
;;        | {ispairz <HK>}
;;        | {left <HK>}
;;        | {right <HK>}
;;        | {mt}
;;        | {ismtz}
;; (SEQUENCING)
;;        | {seqn <HK> <HK>}
;; where
;; {with {x e1} e2} ≡ {{fun {x} e2} e1}
;; {rec {x e1} e2} ≡ {with {x {fix x e1}} e2}
;; {fixFun <id> {<id>} <FFWAE>} ≡ {fix <id> {fun {<id>} <FFWAE>}}

;; Syntactic Sugar
(define (with name named-expr body)
  (app (fun name body) named-expr))

(define (rec name named-expr body)
  (with name (fix name named-expr) body))

(define (fixFun f x body) (fix f (fun x body)))

;; Every AE program is a Hammock program
(define AE1 (num 4))
(define AE2 (add AE1 (num 5)))
(define AE3 (sub (num 6) (num 3)))


;; Every WAE program is a Hammock program
(define WAES4 '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})
(define WAE4 (with 'x (add (num 5) (num 5))
                   (with 'y (sub (id 'x) (num 3))
                         (add (id 'y) (id 'y)))))

(define WAES5 '{with {x 5} {+ x {with {x 3} 10}}})
(define WAE5 (with 'x (num 5) (add (id 'x) (with 'x (num 3) (num 10)))))

(define WAES6 '{with {x 5} {+ x {with {x 3} x}}})
(define WAE6 (with 'x (num 5) (add (id 'x) (with 'x (num 3) (id 'x)))))

(define WAES7 '{with {x 5} {+ x {with {y 3} x}}})
(define WAE7 (with 'x (num 5) (add (id 'x) (with 'y (num 3) (id 'x)))))


;; Every FWAE program is a Hammock program
(define FWAES1 '{fun {x} {+ x x}})
(define FWAE1 (fun 'x (add (id 'x) (id 'x))))

(define FWAES2 '{fun {x} {+ x 1}})
(define FWAE2 (fun 'x (add (id 'x) (num 1))))

(define FWAES3 '{fun {x} {+ x y}})
(define FWAE3 (fun 'x (add (id 'x) (id 'y))))

;; Every FFWAE program is a Hammock program
(define FFWAES1 '{fix f 4})
(define FFWAE1 (fix 'f (num 4)))
(define FFWAES2 '{fix f f})
(define FFWAE2 (fix 'f (id 'f)))
(define FFWAES3 '{fix a {fix b a}})
(define FFWAE3 (fix 'a (fix 'b (id 'a))))
(define FFWAES4
  '{with {down {fix f {fun {x} {if0 x 9 {f {- x 1}}}}}}
         {down 1}})
(define FFWAE4
  (app (fun 'down (app (id 'down) (num 1)))
       (fix 'f (fun 'x (if0 (id 'x)
                            (num 9)
                            (app (id 'f) (sub (id 'x) (num 1))))))))

(define FFWAES5
  '{rec {down {fun {x} {if0 x
                            9
                            {down {- x 1}}}}}
     {down 1}})
(define FFWAE5
  (app (fun 'down (app (id 'down) (num 1)))
       (fix 'down (fun 'x (if0 (id 'x)
                               (num 9)
                               (app (id 'down) (sub (id 'x) (num 1))))))))

(define FFWAES6
  '{with {down {fixFun f {x} {if0 x 9 {f {- x 1}}}}}
         {down 1}})

(define FFWAE6
  (app (fun 'down (app (id 'down) (num 1)))
       (fixFun 'f 'x (if0 (id 'x)
                          (num 9)
                          (app (id 'f) (sub (id 'x) (num 1)))))))


;; Mutation-free Hannah programs are also Hammock programs
(define HG1 (ismtz (mt)))
(define HG2 (ismtz (num 0)))
(define HG3 (isnumz (num 0)))
(define HG4 (isnumz (mt)))
(define HG5 (ispairz (pair (mt) (mt))))
(define HG6 (ispairz (num 0)))

(define HG7 (left (pair (mt) (num 7))))
(define HG8 (right (pair (mt) (num 7))))


;; Programs that produce values in Hammock but not in Hannah
;; (that's the only difference between the programs they have in common)

(define HK1 (with 'room-warmer (fix 'f (id 'f))
                  (num 7)))

(define HK2 (with 'pair-roulette (pair (num 1) (fix 'f (id 'f)))
                  (left (id 'pair-roulette))))

(define HK3S '{rec {ones {pair 1 ones}}
                {left ones}})
(define HK3 (rec 'ones (pair (num 1) (id 'ones))
                             (left (id 'ones))))



(define (fn-for-hk hk)
  (type-case HK hk
    [num (n) (... n)]
    [isnumz (e) (... (fn-for-hk e))]
    [add (l r) (... (fn-for-hk l)
                    (fn-for-hk r))]
    [sub (l r) (... (fn-for-hk l)
                    (fn-for-hk r))]
    [id (x) (... x)]
    [fun (x body) (... x
                       (fn-for-hk body))]
    [isfunz (e) (... (fn-for-hk e))]
    [app (rator rand) (... (fn-for-hk rator)
                           (fn-for-hk rand))]
    [if0 (p c a)
         (... (fn-for-hk p)
              (fn-for-hk c)
              (fn-for-hk a))]
    [fix (x body) (... x
                       (fn-for-hk body))]
    [pair (l r) (... (fn-for-hk l)
                     (fn-for-hk r))]
    [ispairz (e) (... (fn-for-hk e))]
    [left (e) (... (fn-for-hk e))]
    [right (e) (... (fn-for-hk e))]
    [mt () (...)]
    [ismtz (e) (... (fn-for-hk e))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hk-focused-sexp (hkfs) is one of:
;; - number
;; - `{isnumz ,hkfs}
;; - `{+ ,hkfs ,hkfs}
;; - `{- ,hkfs ,hkfs}
;; - `{with ,identifier ,hkfs ,hkfs}
;; -  HKID
;; - `{set-var ,identifier ,hkfs} ;; !!! NEW THING!!!
;; - `{fun ,identifier hkfs}
;; - `{isfunz ,hkfs}
;; - `{,hkfs ,hkfs}
;; - `{fix ,identifier ,hkfs}
;; - `{fixFun ,identifier {,identifier} ,hkfs}
;; - `{rec ,identifier ,hkfs ,hkfs}
;; - `{if0 ,hkfs ,hkfs ,hkfs}
;; - `{pair ,hkfs ,hkfs}
;; - `{ispairz ,hkfs}
;; - `{left ,hkfs}
;; - `{right ,hkfs}
;; - `{mt}
;; - `{ismtz ,hkfs}
;; - `{newbox ,hkfs}
;; - `{isboxz ,hkfs}
;; - `{setbox ,hkfs ,hkfs}
;; - `{openbox ,hkfs}
;; - "any other s-expression"
;; interp.  any s-expression, but with a focus on those that represent
;; HK expressions.


#;
(define (fn-for-hkfs sexp)
  (match sexp
    [`,n
     #:when (number? n)
     (... n)]
    [`{isnumz ,sexp1} (... (fn-for-hkfs sexp1))]
    [`{+ ,sexp1 ,sexp2}
     (... (fn-for-hkfs sexp1)
          (fn-for-hkfs sexp2))]
    [`{- ,sexp1 ,sexp2}
     (... (fn-for-hkfs sexp1)
          (fn-for-hkfs sexp2))]
    [`{with {,x ,sexp1} ,sexp2}
     #:when (identifier? x)
     (... x
          (fn-for-hkfs sexp1)
          (fn-for-hkfs sexp2))]
    [`,x
     #:when (identifier? x)
     (... x)]
    [`{setvar ,x ,sexp1} (... x (fn-for-hkfs sexp1))]
    [`{if0 ,sexp1 ,sexp2 ,sexp3}
     (... (fn-for-hkfs sexp1)
          (fn-for-hkfs sexp2)
          (fn-for-hkfs sexp3))]
    [`{fun {,x} ,sexp1}
     #:when (identifier? x)
     (... x
          (fn-for-hkfs sexp1))]
    [`{isfunz ,sexp1} (... (fn-for-hkfs sexp1))]
    [`{fix ,f ,sexp1}
     #:when (identifier? f)
     (... f
          (fn-for-hkfs sexp1))]
    [`{fixFun ,f {,x} ,sexp1}
     #:when (and (identifier? f) (identifier? x))
     (... f
          x
          (fn-for-hkfs sexp1))]
    [`{rec {,x ,sexp1} ,sexp2}
     #:when (identifier? x)
     (... x (fn-for-hkfs sexp1) (fn-for-hkfs sexp2))]
    [`{pair ,sexp1 ,sexp2}
     (... (fn-for-hkfs sexp1)
          (fn-for-hkfs sexp2))]
    [`{ispairz ,sexp1} (... (fn-for-hkfs sexp1))]
    [`{left ,sexp1} (... (fn-for-hkfs sexp1))]
    [`{right ,sexp1} (... (fn-for-hkfs sexp1))]
    [`{mt} (...)]
    [`{ismtz ,sexp1} (... (fn-for-hkfs sexp1))]
    [`{newbox ,sexp1} (... (fn-for-hkfs sexp1))]
    [`{isboxz ,sexp1} (... (fn-for-hkfs sexp1))]
    [`{setbox ,sexp1 ,sexp2}
     (... (fn-for-hkfs sexp1)
          (fn-for-hkfs sexp2))]
    [`{openbox ,sexp1} (... (fn-for-hkfs sexp1))]
    ;; Notice that application is now the last focused case...
    [`{,sexp1 ,sexp2}
     (... (fn-for-hkfs sexp1)
          (fn-for-hkfs sexp1))]
    [else (... sexp)] ))



;; parse-expr : s-expression -> HK
;; parse the given s-expression into a HK expression
;; EFFECT: signals an error on failure
(define (parse-expr sexp)
  (match sexp
    [`,n #:when (number? n) (num n)]
    [`{isnumz ,sexp1} (isnumz (parse-expr sexp1))]
    [`{+ ,lhs ,rhs} (add (parse-expr lhs) (parse-expr rhs))]
    [`{- ,lhs ,rhs} (sub (parse-expr lhs) (parse-expr rhs))]
    [`{with {,id ,named-exp} ,body}
     #:when (hkid? id)
     ;; desugar with into anonymous function application
     (with id (parse-expr named-exp) (parse-expr body))]
    [`,x #:when (hkid? x) (id x)]
    [`{if0 ,pred ,conseq ,altern}
     (if0 (parse-expr pred) (parse-expr conseq) (parse-expr altern))]
    ;; Notice that application is now last...
    [`{fun {,x} ,sexp1}
     #:when (hkid? x)
     (fun x (parse-expr sexp1))]
    [`{isfunz ,sexp1} (isfunz (parse-expr sexp1))]
    [`{fix ,f ,sexp1}
     #:when (hkid? f)
     (fix f (parse-expr sexp1))]
    [`{fixFun ,f {,x} ,sexp1}
     #:when (and (hkid? f) (hkid? x))
     ;; desugar to fix and fun
     (fixFun f x (parse-expr sexp1))]
    [`{rec {,x ,sexp1} ,sexp2}
     #:when (hkid? x)
     (rec x (parse-expr sexp1) (parse-expr sexp2))]
    [`{pair ,sexp1 ,sexp2}
     (pair (parse-expr sexp1)
           (parse-expr sexp2))]
    [`{ispairz ,sexp1} (ispairz (parse-expr sexp1))]
    [`{left ,sexp1} (left (parse-expr sexp1))]
    [`{right ,sexp1} (right (parse-expr sexp1))]
    [`{mt} (mt)]
    [`{ismtz ,sexp1} (ismtz (parse-expr sexp1))]
    [`{,rator-exp ,arg-exp}
     (app (parse-expr rator-exp) (parse-expr arg-exp))]
    [_ (error 'parse-expr "bad expression ~a" sexp)]))


(test (parse-expr WAES4) WAE4)
(test (parse-expr WAES5) WAE5)
(test (parse-expr WAES6) WAE6)
(test (parse-expr WAES7) WAE7)


