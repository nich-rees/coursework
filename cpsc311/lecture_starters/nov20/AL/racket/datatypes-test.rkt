#lang racket

;;
;; datatypes-test.rkt - test cases for datatypes.rkt
;;

(require "datatypes.rkt")

(module+ test
  (require rackunit))

;; Parse-expr tests
(module+ test
  (check-equal? (parse-expr '7) (number-expr 7))
  (check-equal? (parse-expr '#t) (boolean-expr #t))
  (check-equal? (parse-expr 'pulse) (var-expr 'pulse))
  (check-equal? (parse-expr '(if #f 5 7)) (if-expr (boolean-expr #f)
                                                   (number-expr 5)
                                                   (number-expr 7)))
  (check-equal? (parse-expr '(* 4 (sub1 7)))
                (mult-expr (number-expr 4)
                           (sub1-expr (number-expr 7))))
  (check-equal? (parse-expr '(if (zero? 6) #t 24))
                            (if-expr (zero-expr (number-expr 6))
                                     (boolean-expr #t)
                                     (number-expr 24)))
  (check-equal? (parse-expr '(letcc k (throw k 9)))
                (letcc-expr 'k
                            (throw-expr (var-expr 'k)
                                        (number-expr 9))))
  (check-equal? (parse-expr '((lambda (x) (x x)) (lambda (y) (y y))))
                (app-expr
                 (lambda-expr 'x (app-expr (var-expr 'x)
                                           (var-expr 'x)))
                 (lambda-expr 'y (app-expr (var-expr 'y)
                                           (var-expr 'y)))))
  (check-equal? (parse-expr '(set-box! (unbox (box (box 7))) 12))
                (set-box-expr (unbox-expr (box-expr (box-expr (number-expr 7))))
                              (number-expr 12)))
  (check-equal? (parse-expr '(begin 7 8))
                (begin-expr (number-expr 7) (number-expr 8)))
  (check-equal? (parse-expr '(begin 7 8 9))
                (begin-expr (number-expr 7) (begin-expr (number-expr 8)
                                                        (number-expr 9))))
  (check-equal? (parse-expr '(let ([q 9]) (* q q)))
                (app-expr (lambda-expr 'q (mult-expr (var-expr 'q)
                                                     (var-expr 'q)))
                          (number-expr 9)))
  (check-equal? (parse-expr '(rec f (x) (f x)))
                (app-expr
                 (lambda-expr
                  'Y
                  (app-expr (var-expr 'Y)
                            (lambda-expr
                             'f
                             (lambda-expr
                              'x
                              (app-expr (var-expr 'f)
                                        (var-expr 'x))))))
                 (lambda-expr
                  'f
                  (app-expr
                   (lambda-expr
                    'w
                    (app-expr (var-expr 'w)
                              (var-expr 'w)))
                   (lambda-expr
                    'x
                    (app-expr
                     (var-expr 'f)
                     (lambda-expr
                      'a
                      (app-expr
                       (app-expr (var-expr 'x)
                                 (var-expr 'x))
                       (var-expr 'a)))))))))
  (void))

;; Environment Tests
(module+ test
  (check-exn exn:fail? (λ () (lookup-env empty-env 'x)))
  (check-exn exn:fail? (λ () (lookup-env (extend-env 'x 7 empty-env) 5)))
  (check-equal? 7 (lookup-env (extend-env 'x 7 empty-env) 'x))
  (check-equal? #f (lookup-env (extend-env 'y 3
                                           (extend-env 'x #f empty-env)) 'x))
  (check-equal? 3 (lookup-env (extend-env 'x 3
                                           (extend-env 'x #f empty-env)) 'x))
  (check-equal? 3 (lookup-env (extend-env 'x 3
                                           (extend-env 'y #f empty-env)) 'x))
  (check-exn exn:fail?
             (λ () (lookup-env (extend-env 'x 3
                                           (extend-env 'y #f empty-env)) 'z))))