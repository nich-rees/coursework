#lang plai
(print-only-errors)
(define (... . args) (cons '... args)) ;; enables us to use ... in templates

(require 2htdp/image)



;;
;; Region and ListOfRegion data definitions provided.
;;

(define-type region
  [single (label string?) (weight natural?) (color image-color?)]
  [group (color image-color?) (subs (listof region?))])
;; interp.
;;  an arbitrary-arity tree of regions
;;  single regions have label, weight and color
;;  groups just have a color and a list of sub-regions
;;
;;  weight is a unitless number indicating how much weight
;;  given single contributes to whole tree
;; THIS IS OUR REPLACEMENT FOR TYPE COMMENTS
;; AND DATA DEFINITIONS


;; All the Ss and Gs are Regions
(define S1 (make-single "sss-one" 20 "red"))
(define S2 (make-single "sss-two" 40 "blue"))
(define S3 (make-single "sss-three" 60 "orange"))
(define S4 (make-single "sss-four" 30 "black"))
(define S5 (make-single "sss-five" 50 "purple"))
(define S6 (make-single "sss-six" 80 "yellow"))

(define G1 (make-group "red"  (list S1 S2 S3)))
(define G2 (make-group "blue" (list G1 S4)))
(define G3 (make-group "orange" (list S5 S6)))
(define G4 (make-group "black" (list G2 G3)))

(define LORE empty)
(define LOR123 (list S1 S2 S3))

(define (fn-for-region r)
  (cond [(single? r)
         (... (single-label r)
              (single-weight r)
              (single-color r))]
        [else ;(group? r)
         (... (group-color r)
              (fn-for-lor (group-subs r)))]))

(define (fn-for-lor lor)
  (cond [(empty? lor) (...)]
        [else
         (... (fn-for-region (first lor))
              (fn-for-lor (rest lor)))]))


;; *THIS IS THE TEMPLATE WE WILL MOSTLY USE*
(define (fn-for-region2 r)
  (type-case region r
    [single (l w c)
            (... l w c)]
    [group (c s)
           (... c
                (fn-for-lor s))]))
;; type-case is a rudimentary version of pattern matching
;; we just get to put all of the things in without the cond
;; and l w c instead of using selectors...
;; interpretors a lot more compact


;; For total-weight--lor we demonstrate a different function design.
;; It uses the following *abstract list function* template which composes
;; two abstract list functions:
(define (fn-for-lor2 lor)
     (foldr ... ...
            (map ... lor)))


;; Problem 1: Design a function that produces the total
;; weight of a region / list of region

;(@htdf total-weight--region total-weight--lor)
;(@signature Region -> Natural)
;(@signature ListOfRegion -> Natural)
;; produce total weight of region / list of region

;; Region -> Natural
;; produce the total weight of the given region
;(define (total-weight--region r) 0)


;; (listof Region) -> Natural
;; produce the cumulative weight of the given list of regions
;(define (total-weight--lor lor) 0)

#;
(define (total-weight--region r)
  (cond [(single? r)
              (single-weight r)]
        [else ;(group? r)
        (total-weight--lor (group-subs r))]))

(define (total-weight--region r)
  (type-case region r
    [single (l w c) w]
    [group (c s) (total-weight--lor s)]))
;; what is type-case doing here?
;; type-case calls region? ("region hunh?") and if its a region,
;; then it calls single?, and then it binds l to the first argument,
;; w to the second argument, and c to the third argument,
;; and so on for group

#;
(define (total-weight--lor lor)
  (cond [(empty? lor) 0]
        [else
         (+ (total-weight--region (first lor))
              (total-weight--lor (rest lor)))]))

(define (total-weight--lor lor)
  (foldr + 0 ; replacing cons with addition
         (map total-weight--region lor)))

;; In PLAI, examples have to come *after* the function definitions (sigh...)
(test (total-weight--region S1) 20)
(test (total-weight--region G1) 120)
(test (total-weight--region G4) 280)


;; ** ADD TEMPLATE TAGS, COPY TEMPLATES COMPLETE FUNCTION DESIGN **




;; Problem 2: Design a function that consumes a region and
;; produces a list of all the labels in the region.








;; Problem 3: Design a function that consumes a region and a string
;; and produces a list of all contained regions with the given color.
;; Include the root if it has that color.



;; Problem 4: Design a function that consumes a region and a string
;; and looks for a region with the given label.  If there is one
;; the function should produce the first one it finds.  If there is
;; not one it should produce false.  The signature for the function
;; is given below







;; Problem 5: Design a function that renders a region and its subregions as
;; nested boxes. The rendering does not have to be pretty, but it must somehow
;; reflect the coloring, the labels and the weights.  The border function below
;; may be helpful to you. 















(define BORDER-THICKNESS 5)

;; border : color image -> image
;; add a border of the given color around img
(define (border c img)
  (overlay img
           (rectangle (image-width img)
                      (image-height img)
                      "solid"
                      "white")
           (rectangle (+ (image-width img) BORDER-THICKNESS)
                      (+ (image-height img) BORDER-THICKNESS)
                      "solid"
                      c)))
           
(test (border "red" (rectangle 50 100 "solid" "blue"))
              (overlay (rectangle 50 100 "solid" "blue")
                       (rectangle 50 100 "solid" "white")
                       (rectangle (+ 50 BORDER-THICKNESS)
                                  (+ 100 BORDER-THICKNESS)
                                  "solid"
                                  "red")))
(test (border "orange" (rectangle 60 70 "solid" "blue"))
              (overlay (rectangle 60 70 "solid" "blue")
                       (rectangle 60 70 "solid" "white")
                       (rectangle (+ 60 BORDER-THICKNESS)
                                  (+ 70 BORDER-THICKNESS)
                                  "solid"
                                  "orange")))




