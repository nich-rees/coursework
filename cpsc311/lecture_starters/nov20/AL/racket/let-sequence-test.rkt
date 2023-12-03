#lang racket

;;
;; let-sequence-test.rkt - tests for let-sequence.rkt
;;

(require "datatypes.rkt")
(require "let-sequence.rkt")


(module+ test
  (require rackunit))

(module+ test
  ;; Values
  (check-equal? (ee '7) (number-value 7))
  (check-equal? (ee '#t) (boolean-value #t))
  (check-equal? (ee '(lambda (x) 5))
                (procedure-value 'x (number-expr 5) '()))
  (check-equal? (ee '(box 5)) (box-value (box (number-value 5))))
  (check-pred cont-value? (ee '(letcc k k)))
  (check-pred procedure-value? (ee `(rec f (n) n)))
  
  ;; Simple Uses
  (check-equal? (ee '(* 7 8)) (number-value 56))
  (check-equal? (ee '(sub1 23)) (number-value 22))
  (check-equal? (ee '(zero? #t)) (boolean-value #f))
  (check-equal? (ee '(zero? 7)) (boolean-value #f))
  (check-equal? (ee '(zero? 0)) (boolean-value #t))
  (check-equal? (ee '(begin 7 8)) (number-value 8))
  (check-equal? (ee '(begin 7 8 #f)) (boolean-value #f))
  (check-equal? (ee `(if #t 1 2)) (number-value 1))
  (check-equal? (ee `(if (box 1) 1 2)) (number-value 1))
  (check-equal? (ee `(if #f 1 2)) (number-value 2))
  (check-equal? (ee '(let ([q 9]) (* q q)))
                (number-value 81))
  
  ;; Function applications
  (check-equal? (ee '((lambda (x) x) 7)) (number-value 7))
  (check-equal? (ee '((lambda (x) (sub1 x)) 7)) (number-value 6))
  (check-equal? (ee '(((lambda (x) (lambda (y) x)) 6) 9)) (number-value 6))
  (check-equal? (ee '((lambda (p) (p 70)) (lambda (x) (sub1 x))))
                (number-value 69))

  ;; continuations
  (check-equal? (ee '(letcc k (* 7 (throw k 80)))) (number-value 80))
  (check-equal? (ee '(letcc k (* (throw k 7) (throw k 80)))) (number-value 7))
  (check-equal? (ee '(sub1 (letcc k (zero? (throw k 7))))) (number-value 6))
  (check-equal? (ee '(sub1 (call/cc
                            (lambda (k) (zero? (k 7)))))) (number-value 6))
  (check-equal? (ee '((letcc k (lambda (x) (throw k (lambda (y) (* x y))))) 9))
                (number-value 81))
  
  ;; boxes
  (check-equal? (ee '(let ([b (box 5)])
                       (set-box! b #f)))
                (number-value 5))
  (check-equal? (ee '((lambda (b)
                        (begin (set-box! b #f)
                               (unbox b)))
                      (box 77)))
                (boolean-value #f))
  (check-equal? (ee '(let ([b (box 5)])
                       (let ([a (box b)])
                         (begin (set-box! b a)
                                (set-box! a 5)
                                (unbox (unbox b))))))
                (number-value 5))

  ;; call/cc meets box
  (check-equal? (ee '(let ([b (box #f)])
                       (let ([q (* (* 9
                                      (call/cc (lambda (k)
                                                 (begin
                                                   (set-box! b k)
                                                   10))))
                                   11)])
                         (let ([v (unbox b)])
                           (if v
                               (begin (set-box! b #f)
                                      (v 17))
                               q)))))
                (number-value 1683))
  
  ;; rec
  (check-equal?
   (ee `(let ([fact (rec f (n)
                      (if (zero? n)
                          1
                          (* n (f (sub1 n)))))])
          (fact 5)))
   (number-value (* 5 4 3 2 1)))

  ;; errors
  (check-exn exn:fail? (λ () (ee '(* 7 #t))))
  (check-exn exn:fail? (λ () (ee '(sub1 #f))))
  (check-exn exn:fail? (λ () (ee '(5 #f))))
  (check-exn exn:fail? (λ () (ee '(unbox 5))))
  (check-exn exn:fail? (λ () (ee '(throw #t 1))))
  (check-exn exn:fail? (λ () (ee '(letcc k (* 7 (k 80)))))))