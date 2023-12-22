#lang plai

;;
;; By Jove, This Room Warmer is Exceptional!
;;

;; PROBLEM 2.
;; Consider the following program at STEP0. This program runs forever.
;; Step this program until you arrive at a step that is the same as STEP0
;; (So your last presented step should look exactly like STEP0)
;; and (equal? STEPN STEP0) gives #t)).

(define STEP0
  '{{fun {x}
         {{match/handle {x 7}
                        [q q]
                        [{raze E z} z]}
          x}}
    {fun {m}
         {raze E
               {fun {x}
                    {{match/handle {x 7}
                                   [q q]
                                   [{raze E z} z]}
                     x}}}}})


;; Wrap each step in the appropriate STEPN definition.  We have provided
;; a placeholder for STEP1 for you

#;
(define STEP1 (error "REPLACE error WITH A STEP"))


(define STEP1
  '{{match/handle {{fun {m}
                        {raze E
                              {fun {x}
                                   {{match/handle {x 7}
                                                  [q q]
                                                  [{raze E z} z]}
                                    x}}}}
                   7}
                  {q q}
                  [{raze E z} z]}
    {fun {m}
         {raze E
               {fun {x}
                    {{match/handle {x 7}
                                   [q q]
                                   [{raze E z} z]}
                     x}}}}})

(define STEP2
  '{{match/handle {raze E
                        {fun {x}
                             {{match/handle {x 7}
                                            [q q]
                                            [{raze E z} z]}
                              x}}}
                  [q q]
                  [{raze E z} z]}
    {fun {m}
         {raze E
               {fun {x}
                    {{match/handle {x 7}
                                   [q q]
                                   [{raze E z} z]}
                     x}}}}})

(define STEP3
  '{{fun {x}
         {{match/handle {x 7}
                        [q q]
                        [{raze E z} z]}
          x}}
    {fun {m}
         {raze E
               {fun {x}
                    {{match/handle {x 7}
                                   [q q]
                                   [{raze E z} z]}
                     x}}}}})

