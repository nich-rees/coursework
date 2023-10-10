#lang plai
(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

;; CPSC311 2023 Winter Term 1
;; Assignment 0: Designing Functions over Trees using PLAI data types

;; Released: Tuesday, September 12, 2023
;; Due: Sunday, September 17, 2023 at 11:59pm

;; This assignment is to be completed individually 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Name: Nicholas Rees
;; Student Number: 11848363
;; CWL: nrees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 2htdp/image) 

;;
;; Render Tree and Me
;;

;; A render tree lets you arrange images relative to one another.
;; (render-vertical rt*) renders each element of 

(define-type render-tree
  [img (i image?)]
  [vertical (rt* (listof render-tree?))]
  [horizontal (rt1 render-tree?) (rt2 render-tree?)])
;; interp.
;; layout instructions for a collection of images:
;; - (img i) is the image i
;; - (vertical rt*) renders the list of render-trees vertically,
;;   with the first image in the list above the last.  An empty list
;;   renders as the empty-image
;; - (horizontal rt1 rt2) renders rt1 to the left of rt2

(define RT0 (img (square 20 "solid" "red")))
(define RT1 (img (square 20 "solid" "blue")))
(define RT2 (horizontal RT0 (horizontal RT1 RT0)))
(define RT3 (horizontal RT1
                        (horizontal RT0
                                    (horizontal RT1
                                                (img empty-image)))))
(define RT4 (vertical (list RT2 RT3 RT2)))
(define RT5 (horizontal (img empty-image) (img empty-image)))
(define RT6 (vertical (list)))

;; CPSC110 BSL-Style Template (for reference, NOT FOR USE)
#;
(define (fn-for-rt rt)
  (cond [(img? rt) (... (img-i rt))]
        [(vertical? rt)
         (... (fn-for-lort (vertical-rt* rt)))]
        [else ; (horizontal? rt)
         (... (fn-for-rt (horizontal-rt1 rt))
              (fn-for-rt (horizontal-rt2 rt)))]))


;; CPSC311 PLAI-Style Template (USE THIS ONE)
#;
(define (fn-for-rt rt)
  (type-case render-tree rt
    [img (i) (... i)]
    [vertical (rt*) (... (fn-for-lort rt*))]
    [horizontal (rt1 rt2) (... (fn-for-rt rt1)
                               (fn-for-rt rt2))]))

;; structurally recursive list of render-tree template
#;
(define (fn-for-lort lort)
  (cond [(empty? lort) (...)]
        [else ; (cons? lort)]
         (... (fn-for-rt (first lort))
              (fn-for-lort (rest lort)))]))

;; example abstract function list of render-tree template 
#;
(define (fn-for-lort2 lort)
  (foldr ... ...
         (map ... lort)))


;; For the following problems, you are expected to follow the design recipes.
;; See https://www.students.cs.ubc.ca/~cs-311/current/syllabus.html#coding-style
;; and #examples.

;; Use the PLAI-Style render-tree template where appropriate, and either
;; the structurally recursive list template or a template that uses
;; abstract list functions, if you prefer.


;; Problem 1: Ho Ho Ho (in September?!?)
;; Whoever designed the render-tree data structure can't seem to decide between
;; elegant generality and spartan simplicity: vertical takes a list of
;; any number of render trees, but horizontal takes exactly two.  
;; But it would be nice if both could behave the same.  With a little
;; effort, we can build a more uniform interface to the data type.

;; Design a function, called horizontal* that accepts an
;; list of render-trees and produces a render-tree that
;; lays them out from left-to-right.  HINT: empty-image is your friend.

;; (listof RenderTree) -> RenderTree
;; produce a render tree that horizontally renders of the given render trees

;(define (horizontal* lort) (img empty-image)) ; stub

(define (horizontal* lort)
  (cond [(empty? lort) (img empty-image)]
        [else ; (cons? lort)]
         (horizontal (first lort)
              (horizontal* (rest lort)))]))

(test (horizontal* (list)) (img empty-image))
(test (horizontal* (list RT0)) (horizontal RT0 (img empty-image)))
(test (horizontal* (list RT2)) (horizontal RT2 (img empty-image)))
(test (horizontal* (list RT0 RT1 RT0)) (horizontal RT0 (horizontal RT1
                                                                   (horizontal RT0 (img empty-image)))))
(test (horizontal* (list RT4 RT0 RT3)) (horizontal RT4 (horizontal RT0
                                                                       (horizontal RT3 (img empty-image)))))


;; Problem 2: Paint Me A Picture
;; Design a function called render that renders the given render tree
;; as an image.  You will want to use the built-in above and beside functions.
;; Fix the signature and purpose of render/lort by replacing ??? with
;; the appropriate return type and purpose.

;; RenderTree -> Image
;; (listof RenderTree) -> Image
;; produce the proper rendering of the given render tree
;; produce vertically stacked renderings of the render trees, with the first element on top
; (define (render rt) empty-image) ; stub
; (define (render/lort lort) empty-image) ; stub

(define (render rt)
  (type-case render-tree rt
    [img (i) i]
    [vertical (rt*) (render/lort rt*)]
    [horizontal (rt1 rt2) (beside (render rt1)
                               (render rt2))]))

(define (render/lort lort)
  (cond [(empty? lort) empty-image]
        [else ; (cons? lort)]
         (above (render (first lort))
              (render/lort (rest lort)))]))


(test (render RT0) (square 20 "solid" "red"))
(test (render RT2) (beside (square 20 "solid" "red")
                           (beside (square 20 "solid" "blue")
                                   (square 20 "solid" "red"))))
(test (render RT4)
      (above
       (beside (square 20 "solid" "red")
               (beside (square 20 "solid" "blue") (square 20 "solid" "red")))
       (above (beside (square 20 "solid" "blue")
               (beside (square 20 "solid" "red") (square 20 "solid" "blue")))
              (beside (square 20 "solid" "red")
               (beside (square 20 "solid" "blue") (square 20 "solid" "red"))))))
(test (render RT5) empty-image)
(test (render RT6) empty-image)
(test (render/lort (list)) empty-image)
(test (render/lort (list RT0)) (square 20 "solid" "red"))
(test (render/lort (list RT1 RT0))
      (above (square 20 "solid" "blue") (square 20 "solid" "red")))
(test (render/lort (list RT4 RT0 RT2))
      (above
       (above
       (beside (square 20 "solid" "red")
               (beside (square 20 "solid" "blue") (square 20 "solid" "red")))
       (above (beside (square 20 "solid" "blue")
               (beside (square 20 "solid" "red") (square 20 "solid" "blue")))
              (beside (square 20 "solid" "red")
               (beside (square 20 "solid" "blue") (square 20 "solid" "red")))))
       (above (square 20 "solid" "red")
              (beside (square 20 "solid" "red")
                           (beside (square 20 "solid" "blue")
                                   (square 20 "solid" "red"))))))


;; Problem 3: Put the Render in the Blender
;; Sometimes, don't you just want to SHAKE THINGS UP?!?
;; Design a function called blend that, given a render tree, produces a new
;; render tree that horizontally renders all render trees that were vertically
;; rendered originally, and vertically renders all render trees
;; that were horizontally rendered originally.
;; Fix the signature and purpose of the blend/lort by replacing ??? with
;; the appropriate return type and purpose.
;; HINT: be mindful of the work you have already done.

;; RenderTree -> RenderTree
;; (listof RenderTree) -> RenderTree
;; produce a render tree that swaps all horizontal and vertical renderings
;; produce a rt that horizontally renders elements and swaps the horizontals/verticals
; (define (blend rt) (img empty-image)) ; stub
; (define (blend/lort lort) (...)) ; partial stub,

(define (blend rt)
  (type-case render-tree rt
    [img (i) rt]
    [vertical (rt*) (blend/lort rt*)]
    [horizontal (rt1 rt2) (vertical (list (blend rt1)
                               (blend rt2)))]))

(define (blend/lort lort)
  (cond [(empty? lort) (img empty-image)]
        [else ; (cons? lort)]
         (horizontal (blend (first lort))
              (blend/lort (rest lort)))]))


(test (blend RT0) RT0)
(test (blend RT2)
      (vertical (list RT0 (vertical (list RT1 RT0)))))
(test (blend RT4)
      (horizontal (blend RT2) ; we test (blend RT2) above, so this is fine
                  (horizontal (vertical (list RT1 (vertical (list RT0 ; blending RT3
                                                                  (vertical (list RT1 (img empty-image)))))))
                              (horizontal (blend RT2) (img empty-image)))))
(test (blend RT6) (img empty-image))
(test (blend RT5) (vertical (list (img empty-image) (img empty-image))))
;; for the above test case, even though it's vertical, I want empty lists to be empty-image
(test (blend/lort (list)) (img empty-image))
(test (blend/lort (list RT0)) (horizontal RT0 (img empty-image)))
;; the test case above is odd, but it's because blend/lort has to deal with an empty list
(test (blend/lort (list RT1 RT0))
      (horizontal RT1
                  (horizontal RT0 (img empty-image))))
(test (blend/lort (list RT4 RT0 RT2))
      (horizontal
       (blend RT4) ; we test (blend RT4) above, so this is fine
       (horizontal RT0
                   (horizontal (blend RT2) (img empty-image)))))