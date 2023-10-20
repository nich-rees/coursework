Welcome to DrRacket, version 8.6 [cs].
Language: Determine language from source; memory limit: 128 MB.
> (quote hello)
'hello
> 'hello
'hello
> (symbol=? 'hello 'hello)
#t
> (symbol=? 'hello 'goodbye)
#f
> (string->symbol "ma man!")
'|ma man!|
> (string->symbol "frobnicator")
'frobnicator
> (define G1 (gensym))
> G1
'g262412
> (symbol=? G1 'g262412)
#f
> (symbol=? 'g262412 'g262412)




#t
> (symbol=? G1 G1)



#t
> 'G1
'G1
> (symbol=? G1 'G1)



#f
> (cons 5 6)
'(5 . 6)
> (first (cons 5 6))
. . first: contract violation
  expected: (and/c list? (not/c empty?))
  given: '(5 . 6)
> (rest (cons 5 6))
. . rest: contract violation
  expected: (and/c list? (not/c empty?))
  given: '(5 . 6)
> (car (cons 5 6))
5
> (cdr (cons 5 6))
6
> (car (cdr (cons 5 (cons 6 (cons 7 empty)))))
6
> (cadr (cons 5 (cons 6 (cons 7 empty))))
6
> (cons? (cons 5 6))
#t
> (empty? empty)
#t
> (quote (5 . 6))
'(5 . 6)
> (cons 5 6)
'(5 . 6)
> (quote (5 . (6 . (7 . empty))))
'(5 6 7 . empty)
> (list 5 6 7)
'(5 6 7)
> (cdddr (quote (5 . (6 . (7 . empty)))))
'empty
> quasiquote
. quasiquote: bad syntax in: quasiquote
> (quasiquote (5 . (6 . (7 . empty))))
'(5 6 7 . empty)
> (quasiquote (5 . (6 . (7 . (unquote empty)))))
'(5 6 7)
> (define L (list 8 9 10))
> (quasiquote (5 . (6 . (7 . (unquote L)))))
'(5 6 7 8 9 10)
> `(5 . 6 . 7 . ,L)
. read-syntax: illegal use of `.`
> `(5 . (6 . (7 . (,L))))
'(5 6 7 (8 9 10))
> `(5 . (6 . (7 . ,L)))
'(5 6 7 8 9 10)
> 