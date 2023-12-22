#lang plai

;; Stepping programs that use continuations.

;; to step {..._1 {letcc {k} e} ..._2}
;; -- determine the  context {..._1 • ..._2} surrounding the letcc
;; substitute {kont {..._1 • ..._2}} for k in e

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define MCA1 '{letcc {k}
                     {+ 5 3}})

'{+ 5 3}
'8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define MCA2 '{letcc {k}
                     {+ 5 {throwcc k 3}}})

'{+ 5 {throwcc {kont •} 3}}
'3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define MCA3 '{letcc {k}
                     {throwcc k {+ 5 3}}})

'{throwcc {kont •} {+ 5 3}}
'{throwcc {kont •} 8}
'8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the body and context of letcc are nontrivial.
(define MCA4 '{+ 5
                 {letcc {k}
                        {+ {throwcc k 3} {- 9 6}}}})

'{+ 5
    {+ {throwcc {kont {+ 5 •}} 3} {- 9 6}}}
'{+ 5 3}
'8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the body and context of letcc are nontrivial.
(define MCA4b '{+ {- 7 2}
                  {letcc {k}
                         {+ {throwcc k {+ 1 2}} {- 9 6}}}})

'{+ 5
    {letcc {k}
           {+ {throwcc k {+ 1 2}} {- 9 6}}}}
'{+ 5
    {+ {throwcc {kont {+ 5 •}} {+ 1 2}} {- 9 6}}}
'{+ 5
    {+ {throwcc {kont {+ 5 •}} 3} {- 9 6}}}
'{+ 5 3}
'8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define MCA5 '{with {x {letcc {k} k}}
                    {if0 x
                         22
                         {+ x {throwcc x 0}}}})

'{with {x {kont {with {x •}
                      {if0 x
                           22
                           {+ x {throwcc x 0}}}}}}
       {if0 x
            22
            {+ x {throwcc x 0}}}}

'{if0 {kont {with {x •}
                  {if0 x
                       22
                       {+ x {throwcc x 0}}}}}
      22
      {+ {kont {with {x •}
                     {if0 x
                          22
                          {+ x {throwcc x 0}}}}}
         {throwcc {kont {with {x •}
                              {if0 x
                                   22
                                   {+ x {throwcc x 0}}}}} 0}}}

'{+ {kont {with {x •}
                {if0 x
                     22
                     {+ x {throwcc x 0}}}}}
    {throwcc {kont {with {x •}
                         {if0 x
                              22
                              {+ x {throwcc x 0}}}}} 0}}

'{with {x 0}
       {if0 x
            22
            {+ x {throwcc x 0}}}}

'{if0 0
      22
      {+ 0 {throwcc 0 0}}}
'22

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "The following ... example ... is probably the most confusing Scheme program
;; of its size; it might be easy to guess what it returns, but it takes some
;; thought to figure out why."

(define CALLCC '{fun {p} {letcc {k}
                                {p {fun {v} {throwcc k v}}}}})

(define HOWTF `{{{,CALLCC {fun {k} k}} {fun {x} x}} 9})

;; 0 - the beginning
'{{{{fun {p} {letcc {k}
                    {p {fun {v} {throwcc k v}}}}}
    {fun {k} k}}
   {fun {x} x}}
  9}

;; 1 - absorb identity function
'{{{letcc {k}
          {{fun {k} k} {fun {v} {throwcc k v}}}}
   {fun {x} x}}
  9}

;; 2 - capture a continuation that will apply id then 9.
'{{{{fun {k} k}
    {fun {v} {throwcc {kont {{• {fun {x} x}} 9}} v}}}
   {fun {x} x}}
  9}

;; 3 - apply id 
'{{{fun {v} {throwcc {kont {{• {fun {x} x}} 9}} v}}
   {fun {x} x}}
  9}

;; 4 - apply big function TO id
'{{throwcc {kont {{• {fun {x} x}} 9}} {fun {x} x}}
  9}

;; 5 - throw id to the continuation
'{{{fun {x} x} {fun {x} x}} 9}

;; 6 - identity function applied to identity function
'{{fun {x} x} 9}

;; 7 - identity function applied to 9
'9