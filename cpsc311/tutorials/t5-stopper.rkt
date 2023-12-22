#lang plai

(print-only-errors #t)

;; Tutorial: Macros

;; Rather than run this file, you may want to copy and paste these into the
;; interaction window.

;; Prologue:  (sleep s) pauses execution for s seconds

"yawwn, so tired..."

(sleep 2)

"so well rested!"

;; We can use sleep to help us demonstrate that if does not evaluate all its
;; arguments: it is strict only in its first position

(if #t
    7
    (begin 
      (sleep 2)
      "false branch #1"))

(if #f
    7
    (begin 
      (sleep 2)
      "false branch #2"))

;; Let's abstract this sleeping capability.

;; natural X -> X
;; sleep for s seconds, then produce value v
(define (sleep-then-v s v)
  (begin    
    (sleep s)
    v))

;; Yeah, this doesn't quite work:

(sleep-then-v 3 (begin (display "starting\n")
                       (sleep 2)
                       (display "stopping\n")
                       9))

;; Okay we need to delay evaluation.  Well, we could require the user to provide
;; a *thunk* (zero-argument function)

;; natural ( -> X ) -> X
;; sleep for s seconds, then produce value v
(define (sleep-then-thaw s thunk)
  (begin    
    (sleep s)
    (thunk)))

(sleep-then-thaw 3
                 (λ () (begin (display "starting th\n")
                              (sleep 2)
                              (display "stopping th\n")
                              9)))


;; It sucks to have to write a thunk.  Is there another way? YES!

;; instead of using procedural abstraction, we use *syntactic abstraction*
;; in the form of a Racket macro.
(define-syntax sleep-then
  (syntax-rules ()
    [(_ s e) (begin (sleep s) e)]))


(sleep-then 3 (begin (display "starting\n")
                     (sleep 2)
                     (display "stopping\n")
                     9))

;; The Racket *compiler* rewrites (sleep-then s e) into (begin (sleep s) e)
;; during *macro expansion*, which completely precedes execution.


;;
;; Chapter 1: binary or
;;

(or #t (sleep-then 1000 "not getting run"))

;; Since or is short-circuiting in Racket, the sleep-then call is skipped. You
;; may already know that (or e1 e2) is equivalent to:
#;(let ([v1 e1])
    (if v1 v1 e2))

;; If you are wondering what the let is about, and why we didn't write
#;(if e1 e1 e2)

;; try running
#;
(or (begin (displayln "Cheese!") #t) #f)
;; and then translate it both ways above.

;; Given the above information, we can write our own binary-or macro

(define-syntax binary-or
  (syntax-rules ()
    [(_ e1 e2) (let ([v1 e1])
                 (if v1 v1 e2))]))

(binary-or #t (sleep-then 1000 "not getting run"))

;; EXERCISE:  Write a macro for binary and

(define-syntax binary-and
  (syntax-rules ()
    [(_ e1 e2) (let ([v1 e1])
                 (if v1 e2 v1))]))

(binary-and #f (sleep-then 1000 "not getting run"))
(binary-and (begin (displayln "Cheese!") #f) #t)

;;
;; Variable hygiene
;;

;; Consider the following expression:
#;(let ([v1 "cheese"])
    (binary-or #f v1))

;; what do you expect its would macro expand to?
;; what do you expect its output to be?
;; Try running it and see.

;; Naively:
#;(let ([v1 "cheese"])
    (let ([v1 #f])
      (if v1 v1 v1)))

;; Surprised?  

;; Racket Macros are "hygienic" in that they automatically rename bound
;; identifiers so that your macro definition cannot accidentally capture
;; identifier references from user code.  THIS IS WONDERFUL!  Your macros
;; are truly abstracted objects:  the details of names you chose in a macro's
;; implementation do not affect code that you call it with.  As it should be!
;; Hygienic macros are one of the greatest (and most underappreciated)
;; contributions of the Scheme/Racket programming languages to programming
;; language design.


;;
;; Self-referential macros: n-ary or
;;

;; Racket's or accepts an arbitrary number of arguments.  As you've learned,
;; we typically process arbitrary-arity data using self-reference (usually a
;; "natural recursion").  Macros have a form of natural recursion, but it's
;; slightly odd compared to recursive function calls.

;; Here is an n-ary or implementation.

(define-syntax n-ary-or
  (syntax-rules ()
    [(_) #f]
    [(_ e) e]
    [(_ e1 e2 e* ...)
     (let ([v1 e1])
       (if v1 v1 (n-ary-or e2 e* ...)))]))

;; before staring at this too much, consider the following examples:

(test (n-ary-or) #f)
(test (n-ary-or 33) 33)
(test (n-ary-or #f #f 55 #f) 55)

;; This is our first syntax-rules macro with multiple pattern matching cases.
;; syntax-rules patterns are different than match macros.  In particular,
;; the pattern "e ..." is a post-fix pattern that matches a list of e.
;; So our last pattern "(_ e1 e2 e* ...)"  matches a list of 2 or more items
;; where e* represents the list of all items after the second.

;; Now let's hand-expand (n-ary-or #f #f 55 #f), following the structure of the
;; syntax-rules macro:

;; 0
(n-ary-or #f #f 55 #f)

;; 1
(let ([v1 #f])
  (if v1 v1 (n-ary-or #f 55 #f)))

;; 2
(let ([v1 #f])
  (if v1
      v1
      (let ([v1 #f])
        (if v1
            v1 
            (n-ary-or 55 #f)))))

;; EXERCISE: Finish stepping
;; 3
(let ([v1 #f])
  (if v1
      v1
      (let ([v1 #f])
        (if v1
            v1 
            (let ([v1 55])
              (if v1
                  v1
                  (n-ary-or #f)))))))

;; 4
(let ([v1 #f])
  (if v1
      v1
      (let ([v1 #f])
        (if v1
            v1 
            (let ([v1 55])
              (if v1
                  v1
                  #f))))))


;; Question:  Why did we special case "1 argument" and "2 or more arguments"?

#;
(define-syntax n-ary-or
  (syntax-rules ()
    [(_) #f]
    [(_ e1 e* ...)
     (let ([v1 e1])
       (if v1 v1 (n-ary-or e* ...)))]))

;; 0
(n-ary-or 33)

;; 1
(let ([v1 33])
  (if v1
      v1
      (n-ary-or)))

;; 2
(let ([v1 33])
  (if v1
      v1
      #f))

;; The inner if expression is always equal to just v1.

;; Exercise:  Implement multi-arity and.

(define-syntax n-ary-and
  (syntax-rules ()
    [(_) #t]
    [(_ e) e]
    [(_ e1 e2 e* ...)
     (let ([v1 e1])
       (if v1 (n-ary-and e2 e* ...) v1))]))


;;
;; with
;;

;; The languages we have implemented support a "with" form
;; {with {x e1} e2}

;; which can be desugared to
;; {{fun {x} e2} e1}

;; Let's add it to Racket using macros, but also make it multi-identifier

(define-syntax with
  (syntax-rules ()
    [(_ ([x* e*] ...) e)
     ((λ (x* ...) e) e* ...)]))


(test (with ([x 1]
             [y 2]
             [z 3])
            (+ x y z))
      6)

;; EXERCISE:  Hand expand the above with-expression.

((λ (x y z) (+ x y z)) 1 2 3)


;; EXERCISE: (Advanced)
;; The Racket Expression
#;
(let* ([a  6]
       [a (+ a 1)]
       [a (+ a 3)])
  a)

;; is equivalent to
#;
(let ([a 6])
  (let ([a (+ a 1)])
    (let ([a (+ a 3)])
      a)))

;; introduce a macro called with*, implemented in terms of with, that does
;; the same thing.

(define-syntax with*
  (syntax-rules ()
    [(_ () e) e]
    [(_ ([x1 e1] [x* e*] ...) e)
     ((λ (x1) (with* ([x* e*] ...) e)) e1)]))

(test (with ([a 6])
            (with ([a (+ a 1)])
                  (with ([a (+ a 3)])
                        a)))
      10)


;; For more on macros, see:

;; The Scheme Programming Language, 4th Edition, Chapters 3.1 and Chapter 8
;; https://www.scheme.com/tspl4/

;; The Racket Guide, Chapter 16: Macros
;; https://docs.racket-lang.org/guide/macros.html
