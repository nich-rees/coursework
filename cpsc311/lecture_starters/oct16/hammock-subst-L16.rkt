#lang plai
(require "../util/parsing.rkt")

;; hammock-subst-L19-factored - Substitution-based Interpreter

(require "hammock-common.rkt") ;; Parser and language data types

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Substitution-based Interpreter

;;
;; Interpretation Functions
;;

;; HK -> (listof HKID)
;; produce a list of names of free identifiers in hk
(define (free-ids hk)
  (type-case HK hk
    [num (n) empty]
    [isnumz (e) (free-ids e)]
    [add (l r) (set-union (free-ids l)
                          (free-ids r))]
    [sub (l r) (set-union (free-ids l)
                          (free-ids r))]
    [id (x) (list x)]
    [fun (x body) (set-remove (free-ids body) x)]
    [isfunz (e) (free-ids e)]
    [app (rator rand) (set-union (free-ids rator)
                                 (free-ids rand))]
    [if0 (p c a)
         (set-union (free-ids p)
                    (free-ids c)
                    (free-ids a))]
    [fix (x body) (set-remove (free-ids body) x)]
    [pair (l r) (set-union (free-ids l)
                           (free-ids r))]   
    [ispairz (e) (free-ids e)]
    [left (e) (free-ids e)]
    [right (e) (free-ids e)]
    [mt () empty]
    [ismtz (e) (free-ids e)]))

(test (free-ids (num 7)) empty)
(test (free-ids (add (id 'x) (num 7))) (list 'x))
(test (free-ids (fun 'x (id 'x))) empty)
(test (free-ids (fun 'x (id 'y))) (list 'y))
(test (free-ids (add (id 'x) (id 'y))) (list 'y 'x))
(test (free-ids (fix 'x (id 'x))) empty)
(test (free-ids (fix 'x (id 'y))) (list 'y))


;; HK symbol HK -> HK
;; produce the **capture-avoiding** substitution of sub-expr for x0 in expr1
(define (subst expr1 x0 sub-expr)
  (local [;; HKID HK -> (list HKID HK)
          ;; if necessary, 1) rename x to a fresh name and 2) substitute into hk
          (define (handle-binder x hk)
            (cond
              [(or
                ;; 1) x0 can only occur bound in body (like in subst)
                (symbol=? x x0) 
                ;; 2) x0 does not appear free in body
                (not (member x0 (free-ids hk))))
               ;; no cause to rename OR substitute in body
               (list x hk)]
              [;; 3) no risk of x capturing free identifiers in wae1
               (not (member x (free-ids sub-expr)))
               ;; no cause to rename BUT must still substitute
               (list x (recur hk))]
              [else 
               ;; (and (member x (free-ids subst-expr))
               ;;      (member x0 (free-ids hk))]
               ;; capture is imminent, so rename x before substituting
               (let ([g (gensym)])
                 (list g (recur (subst hk x (id g)))))]))
          ;; HK -> HK
          (define (recur expr1)
            (type-case HK expr1
              [num (n) (num n)]
              [isnumz (e) (isnumz (recur e))]
              [add (l r) (add (recur l)
                              (recur r))]
              [sub (l r) (sub (recur l)
                              (recur r))]
              [id (x) (if (symbol=? x x0)
                          sub-expr
                          (id x))] ;; INTERESTING
              [fun (x body)
                   (match-let ([(list x^ body^) (handle-binder x body)])
                     (fun x^ body^))] ;; INTERESTING
              [isfunz (e) (isfunz (recur e))]
              [app (rator rand) (app (recur rator)
                                     (recur rand))] ;; NOT INTERESTING
              [if0 (p c a)
                   (if0 (recur p)
                        (recur c)
                        (recur a))]
              [fix (x body)
                   (match-let ([(list x^ body^) (handle-binder x body)])
                     (fix x^ body^))] ;; INTERESTING
              [pair (l r) (pair (recur l) (recur r))]   
              [ispairz (e) (ispairz (recur e))]
              [left (e) (left (recur e))]
              [right (e) (right (recur e))]
              [mt () (mt)]
              [ismtz (e) (ismtz (recur e))]))]
    (recur expr1)))



;;
;; Interpreter Values - now a refinement of HK!
;;

;; Value is one of:
;; - (num Number)
;; - (fun Symbol HK)
;; - (pair HK HK) ;; <-- **non-strict pair values contain HK's, not Values**
;; - (mt)
;; interp.  Those HK values that count as runtime values

(define (fn-for-value v)
  (match v
    [(num n) (... n)]
    [(fun x e) (... x (fn-for-hk e))]
    [(pair e1 e2) (... (fn-for-hk e1)
                       (fn-for-hk e2))]
    [(mt) (...)]))


;; Value -> Number
;; produce the number represented by the given value
;; Effect: signal an error if the value does not represent a number
(define (value->num v)
  (match v
    [(num n) n]
    [else (error 'value->num "Bad number: ~a" v)]))

(test (value->num (num 6)) 6)
(test (value->num (num 9)) 9)
(test/exn (value->num (fun 'x (id 'x))) "Bad number")

;; (implicit type definition)
;; pair is (pair HK HK)

;; Value -> pair
;; produce the same v if it is a pair
;; Effect: signal an error if the value does not represent a pair
(define (value->pair v)
  (match v
    [(pair l r) v]
    [else (error 'value->num "Bad pair: ~a" v)]))

(test (value->pair (pair (num 0) (num 1))) (pair (num 0) (num 1)))
(test/exn (value->pair (num 6)) "Bad pair")


;; (Number Number -> Number) Value Value  -> Value
;; apply num-op to the numbers represented by v1 and v2
;; Effect: signal an error if either argument does not represent a number
(define (num-binop-value num-op v1 v2)
  (let ([n1 (value->num v1)]
        [n2 (value->num v2)])
    (num (num-op n1 n2))))

(test (num-binop-value * (num 5) (num 6)) (num 30))
(test/exn (num-binop-value * (num 5) (fun 'x (id 'x)))
          "Bad number")
(test/exn (num-binop-value * (fun 'x (id 'x)) (num 6))
          "Bad number")
(test/exn (num-binop-value * (fun 'x (id 'x))
                           (fun 'x (id 'x))) "Bad number")


;; Value Value -> Value
;; produce the sum of two numbers
;; Effect: signal an error if either argument does not represent a number
(define (add-value v1 v2)
  (num-binop-value + v1 v2))

(test (add-value (num 5) (num 6)) (num 11))
(test/exn (add-value (num 5) (fun 'x (id 'x))) "Bad number")
(test/exn (add-value (fun 'x (id 'x)) (num 6)) "Bad number")
(test/exn (add-value (fun 'x (id 'x))
                     (fun 'x (id 'x))) "Bad number")


;; Value Value -> Value
;; produce the difference of two numbers
;; Effect: signal an error if either argument does not represent a number
(define (sub-value v1 v2)
  (num-binop-value - v1 v2))

(test (sub-value (num 5) (num 6)) (num -1))
(test/exn (sub-value (num 5) (fun 'x (id 'x))) "Bad number")
(test/exn (sub-value (fun 'x (id 'x)) (num 6)) "Bad number")
(test/exn (sub-value (fun 'x (id 'x))
                     (fun 'x (id 'x))) "Bad number")


;; Value -> Boolean
;; produce true if v1 represents the number zero, else false
(define (zero-value? v)
  (match v
    [(num n) (zero? n)]
    [else #f]))

(test (zero-value? (num 7)) #f)
(test (zero-value? (num 0)) #t)
(test (zero-value? (fun 'x (id 'x))) #f)



;; Value HK -> Value
;; produce the result of applying v1 to e2
;; Effect: signal an error if v1 does not represent a function
(define (apply-value v1 e2)
  (match v1
    [(fun x body) (interp/hk (subst body x e2))]
    [else (error 'apply-value "Bad function: ~a" v1)]))

;; apply-value EXAMPLES below interp/hk because of mutual reference

;; Value -> Value
;; produce the value of the left member of a pair
;; Effect: Signal an error if the given value is not a pair
(define (left-value v)
  (match-let ([(pair e1 e2) (value->pair v)])
    (interp/hk e1)))

;; ***
;; left-value EXAMPLES below interp/hk because of mutual reference

;; Value -> Value
;; produce the value of the right member of a pair
;; Effect: Signal an error if the given value is not a pair
(define (right-value v)
  (match-let ([(pair e1 e2) (value->pair v)])
    (interp/hk e2)))

;; ***
;; right-value EXAMPLES below interp/hk because of mutual reference


;; Value -> Value
;; produce a 0 if the value is a number, otherwise 1
(define (isnumz-value v)
  (if (num? v) (num 0) (num 1)))

(test (isnumz-value (num 0)) (num 0))
(test (isnumz-value (fun 'x (id 'x))) (num 1))
(test (isnumz-value (pair (num 0) (num 0))) (num 1))
(test (isnumz-value (mt)) (num 1))


;; Value -> Value
;; produce a 0 if the value is a function, otherwise 1
(define (isfunz-value v)
  (if (fun? v) (num 0) (num 1)))

(test (isfunz-value (num 0)) (num 1))
(test (isfunz-value (fun 'x (id 'x))) (num 0))
(test (isfunz-value (pair (num 0) (num 0))) (num 1))
(test (isfunz-value (mt)) (num 1))


;; Value -> Value
;; produce a 0 if the value is a pair, otherwise 1
(define (ispairz-value v)
  (if (pair? v) (num 0) (num 1)))

(test (ispairz-value (num 0)) (num 1))
(test (ispairz-value (fun 'x (id 'x))) (num 1))
(test (ispairz-value (pair (num 0) (num 0))) (num 0))
(test (ispairz-value (mt)) (num 1))

;; Value -> Value
;; produce a 0 if the value is mt, otherwise 1
(define (ismtz-value v)
  (if (mt? v) (num 0) (num 1)))

(test (ismtz-value (num 0)) (num 1))
(test (ismtz-value (fun 'x (id 'x))) (num 1))
(test (ismtz-value (pair (num 0) (num 0))) (num 1))
(test (ismtz-value (mt)) (num 0))


;; HK -> Value
(define (interp/hk hk)
  (type-case HK hk
    [num (n) (num n)]
    [isnumz (e) (isnumz-value (interp/hk e))]
    [add (l r) (add-value (interp/hk l) (interp/hk r))]
    [sub (l r) (sub-value (interp/hk l) (interp/hk r))]
    [id (x) (error 'interp/hk "Unbound identifier: ~a" x)]
    [fun (x body) (fun x body)]
    [isfunz (e) (isfunz-value (interp/hk e))]
    [app (rator rand) (apply-value (interp/hk rator) rand)] ;; ***
    [if0 (p c a)
         (if (zero-value? (interp/hk p))
             (interp/hk c)
             (interp/hk a))]
    [fix (f body) (interp/hk (subst body f (fix f body)))]
    [pair (l r) (pair l r)] ;; ***
    [ispairz (e) (ispairz-value (interp/hk e))]
    [left (e) (left-value (interp/hk e))]
    [right (e) (right-value (interp/hk e))]
    [mt () (mt)]
    [ismtz (e) (ismtz-value (interp/hk e))]))


;; apply-value examples

(test (apply-value (fun 'x (add (id 'x) (num 7)))
                   (num 12))
      (num 19))


(test/exn (apply-value (num 12)
                       (fun 'x (add (id 'x) (num 7))))
          "Bad function")

;; left-value examples
(test (left-value (pair (add (num 5) (num -5)) (num 1))) (num 0))
(test/exn (left-value (num 6)) "Bad pair")

;; right-value examples
(test (right-value (pair (num 0) (num 1))) (num 1))
(test/exn (right-value (num 6)) "Bad pair")


;; interp/hk examples

(test (interp/hk (ismtz (mt))) (num 0))
(test (interp/hk (ismtz (num 0))) (num 1))
(test (interp/hk (isnumz (num 0))) (num 0))
(test (interp/hk (isnumz (mt))) (num 1))
(test (interp/hk (ispairz (pair (mt) (mt)))) (num 0))
(test (interp/hk (ispairz (num 0))) (num 1))

(test (interp/hk (left (pair (mt) (num 7)))) (mt))
(test (interp/hk (right (pair (mt) (num 7)))) (num 7))


;;;;;;;;
;; Observing/printing final Hammock values 

;; DeepValue is one of:
;; - (num number)
;; - (fun symbol HK)
;; - (pair DeepValue DeepValue) ;; ** force the non-strict pairs for viewing
;; - (mt)
;; interp.  Hammock values that contain no pending computations

;; Value -> DeepValue
;; force all pending computations in the given value
(define (force-value v)
  (match v
    [(num n) (num n)]
    [(fun x e) (fun x e)]
    [(pair e1 e2) (pair (force-value (interp/hk e1))
                        (force-value (interp/hk e2)))]
    [(mt) (mt)]))

(test (force-value (num 7)) (num 7))
(test (force-value (pair (add (num 6) (num 6))
                      (if0 (num 0)
                           (mt)
                           (num 9))))
      (pair (num 12) (mt)))




;;
;; PUTTING IT ALL TOGETHER  - an interpreter of files on disk
;;

;; S-expression -> Value
;; produce the result of interpreting the HK program represented as an sexp.
;; EFFECT: signals an error if interpretation signals a runtime error.
(define (interp-sexp sexp)
  (interp/hk
   (parse-expr sexp)))

(test/exn (interp-sexp '{with {y {- 10 x}}
                              {with {x 7}
                                    y}})
          "Unbound")


;; String -> Value
;; produce the result of interpreting the HK program in input
;; EFFECT: signals an error if interpretation signals a runtime error.
(define (interp-string pgm)
  (interp/hk
   (parse-expr
    (read-from-string pgm))))


;; String -> Value
;; produce the result of interpreting the HK stored in the file fname
;; EFFECT: signals an error if no file fname contains an AE representation
;;         or if interpretation signals a runtime error.
(define (interp-file fname)
  (interp/hk
   (parse-expr
    (read-from-file fname))))

(test (with-temporary-data-file "{+ 3 7}\n"
        (λ (fname) (interp-file fname)))
      (num 10))

(test (with-temporary-data-file "{+ {- 3 4} 7}\n"
        (λ (fname) (interp-file fname)))
      (num 6))

(test (with-temporary-data-file "{with {x 5} {+ x {with {y 3} x}}}"
        (λ (fname) (interp-file fname)))
      (num 10))


(test (with-temporary-data-file
          "{with {double {fun {x} {+ x x}}} {with {x 5} {double x}}}"
        (λ (fname) (interp-file fname)))
      (num 10))


(test (with-temporary-data-file
          "{with {* {fixFun mult {lhs} {fun {rhs}
                                {if0 rhs
                                     0
                                     {+ lhs {{mult lhs} {- rhs 1}}}}}}}
             {{* 20} 3}}"
        (λ (fname) (interp-file fname)))
      (num 60))

(test (with-temporary-data-file
          "{with {* {fix mult {fun {lhs} {fun {rhs}
                                {if0 rhs
                                     0
                                     {+ lhs {{mult lhs} {- rhs 1}}}}}}}}
             {{* 20} 3}}"
        (λ (fname) (interp-file fname)))
      (num 60))


(define DOUBLE-LIST
  "{with {dbl
          {fixFun recur {l} {if0 {ismtz l}
                               {mt}
                               {pair {+ {left l} {left l}}
                                     {recur {right l}}}}}}
    {dbl {pair 5 {pair 4 {pair 3 {pair 2 {pair 1 {mt}}}}}}}}
")

(test (with-temporary-data-file
          DOUBLE-LIST
        (λ (fname)
          (force-value ;; ***
           (interp-file fname))))
      (pair (num 10)
            (pair (num 8)
                  (pair (num 6)
                        (pair (num 4)
                              (pair (num 2)
                                    (mt)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some example code, mostly based on Chapter 7 of the textbook
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; take
(define TAKE
  '{fixFun take {n}
           {fun {ls}
                {if0 n
                     {mt}
                     {if0 {ismtz ls}
                          {mt}
                          {pair {left ls}
                                {{take {- n 1}} {right ls}}}}}}})

(test
 (force-value
  (interp-sexp
   `{with {take ,TAKE}
          {{take 3} {pair 4 {pair 5 {pair 6 {pair 7 {mt}}}}}}}))
 (pair (num 4) (pair (num 5) (pair (num 6) (mt)))))

;; ones
(define ONES
  '{fix f {pair 1 f}})

;; map
(define MAP
  '{fixFun map {fn}
           {fun {ls}
                {if0 {ismtz ls}
                     {mt}
                     {pair {fn {left ls}}
                           {{map fn} {right ls}}}}}})

;; add1
(define ADD1
  '{fun {n} {+ n 1}})

;; nats
(define NATS
  `{with {map ,MAP}
         {with {add1 ,ADD1}
               {fix f {pair 1 {{map add1} f}}}}})

(define FIRST-FIVE
  `{with {take ,TAKE}
         {{take 5} ,NATS}})

(test (force-value (interp-sexp `{left {right ,NATS}}))
      (num 2))

(test (force-value (interp-sexp FIRST-FIVE))
      (pair (num 1)
            (pair (num 2)
                  (pair (num 3)
                        (pair (num 4)
                              (pair (num 5)
                                    (mt)))))))

;; "cyclic" list generator 
(define CYCLE
  `{fun {ls0}
        {with {cycle
               {fixFun cycle {ls}
                       {if0 {ismtz ls}
                            {cycle ls0}
                            {pair {left ls}
                                  {cycle {right ls}}}}}}
              {cycle ls0}}})

(test (force-value (interp-sexp `{{,TAKE 5}
                               {,CYCLE {pair 3 {pair 2 {pair 1 {mt}}}}}}))
      (pair (num 3)
            (pair (num 2)
                  (pair (num 1)
                        (pair (num 3)
                              (pair (num 2)
                                    (mt)))))))

;; zipOp
(define ZIPOP
  '{fun {f}
        {fix recur
             {fun {ls1}
                  {fun {ls2}
                       {if0 {ismtz ls1}
                            {mt}
                            {if0 {ismtz ls2}
                                 {mt}
                                 {pair {{f {left ls1}} {left ls2}}
                                       {{recur {right ls1}} {right ls2}}}}}}}}})

(test (force-value (interp-sexp `{{{,ZIPOP {fun {a} {fun {b} {pair a b}}}}
                                {pair 1 {pair 2 {pair 3 {mt}}}}}
                               {pair 3 {pair 2 {pair 1 {mt}}}}}))
      (pair (pair (num 1) (num 3))
            (pair (pair (num 2) (num 2))
                  (pair (pair (num 3) (num 1)) (mt)))))

;; fibs
(define FIBS
  `{fix recur
        {pair 1
              {pair 1
                    {{{,ZIPOP {fun {a} {fun {b} {+ a b}}}}
                      recur}
                     {right recur}}}}})

(test (force-value (interp-sexp `{{,TAKE 12} ,FIBS}))
      (local [(define (for-cons n rlon) (pair (num n) rlon))
              (define for-empty (mt))]
        (foldr for-cons for-empty (list 1 1 2 3 5 8 13 21 34 55 89 144))))
