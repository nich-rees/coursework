#lang plai

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

(define FIBS
  `{fix myself
        {pair 1
              {pair 1
                    {{{,ZIPOP {fun {a} {fun {b} {+ a b}}}}
                      myself}
                     {right myself}}}}})

;; A little stepping-ish of FIBS

;; step 0
`{right {right {right {fix myself
      {pair 1
            {pair 1
                  {{{,ZIPOP {fun {a} {fun {b} {+ a b}}}}
                    myself}
                   {right myself}}}}}}}}

;; step 1
`{right {right {right {pair 1
      {pair 1
            {{{,ZIPOP {fun {a} {fun {b} {+ a b}}}}
              {fix myself
                   {pair 1
                         {pair 1
                               {{{,ZIPOP {fun {a} {fun {b} {+ a b}}}}
                                 myself}
                                {right myself}}}}}}
             {right {fix myself
                         {pair 1
                               {pair 1
                                     {{{,ZIPOP {fun {a} {fun {b} {+ a b}}}}
                                       myself}
                                      {right myself}}}}}}}}}}}}

;; step 2 -- really {right {right {right FIBS}}}
'{right {right {right {pair 1 {pair 1 ....}}}}}

;; step 3
'{right {right {pair 1 ...}}}

;; step 4
'{right ...}
'{right
  {{{,ZIPOP {fun {a} {fun {b} {+ a b}}}}
              {fix myself
                   {pair 1
                         {pair 1
                               {{{,ZIPOP {fun {a} {fun {b} {+ a b}}}}
                                 myself}
                                {right myself}}}}}}
             {right {fix myself
                         {pair 1
                               {pair 1
                                     {{{,ZIPOP {fun {a} {fun {b} {+ a b}}}}
                                       myself}
                                      {right myself}}}}}}}}
;; ...
