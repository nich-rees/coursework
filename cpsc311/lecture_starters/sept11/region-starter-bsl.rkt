;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname region-starter-bsl) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;(require spd/tags)

;;
;; Region and ListOfRegion data definitions provided.
;;

;(@htdd Region ListOfRegion)
(define-struct single (label weight color))
(define-struct group (color subs))
;; Region is one of:
;;  - (make-single String Natural Color)
;;  - (make-group Color ListOfRegion)
;; interp.
;;  an arbitrary-arity tree of regions
;;  single regions have label, weight and color
;;  groups just have a color and a list of sub-regions
;;
;;  weight is a unitless number indicating how much weight
;;  given single contributes to whole tree

;; ListOfRegion is one of:
;;  - empty
;;  - (cons Region ListOfRegion)
;; interp. a list of regions
;;
;; We have a self-ref in LoR, and mutual refs between Region and LoR
;; This helps us understand structure of the code we will have to write (recursion?)
;; If implementing a function on region, will probably have to implement on LoR

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




;; Problem 1: Design a function that produces the total
;; weight of a region / list of region

;(@htdf total-weight--region total-weight--lor)
;(@signature Region -> Natural)
;(@signature ListOfRegion -> Natural)
;; produce total weight of region / list of region

;; ** ADD EXAMPLES **
(check-expect (total-weight--region (make-single "sss-one" 20 "red")) 20)
(check-expect (total-weight--region (make-group "red" (list S1 S2 S3)))
              (+ (total-weight--region S1)
                 (total-weight--region S2)
                 (total-weight--region S3)))
(check-expect (total-weight--lor (list S1 S2 S3))
              (+ (total-weight--region S1)
                 (total-weight--region S2)
                 (total-weight--region S3)))

;(define (total-weight--region r) 0)
;(define (total-weight--lor lor) 0)


;; ** ADD TEMPLATE TAGS, COPY TEMPLATES COMPLETE FUNCTION DESIGN **
(define (total-weight--region r)
  (cond [(single? r)
              (single-weight r)]
        [else ;(group? r)
        (total-weight--lor (group-subs r))]))

(define (total-weight--lor lor)
  (cond [(empty? lor) 0]
        [else
         (+ (total-weight--region (first lor))
              (total-weight--lor (rest lor)))]))



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

;(@htdf find-region--region find-region--lor)
;(@signature String Region -> Region or false)
;(@signature String ListOfRegion -> Region or false)






;; Problem 5: Design a function that renders a region and its subregions as
;; nested boxes. The rendering does not have to be pretty, but it must somehow
;; reflect the coloring, the labels and the weights.  The border function below
;; may be helpful to you. 















(define BORDER-THICKNESS 5)

;(@htdf border)
;(@signature Color Image -> Image)
;; add a border of the given color around img
(check-expect (border "red" (rectangle 50 100 "solid" "blue"))
              (overlay (rectangle 50 100 "solid" "blue")
                       (rectangle 50 100 "solid" "white")
                       (rectangle (+ 50 BORDER-THICKNESS)
                                  (+ 100 BORDER-THICKNESS)
                                  "solid"
                                  "red")))
(check-expect (border "orange" (rectangle 60 70 "solid" "blue"))
              (overlay (rectangle 60 70 "solid" "blue")
                       (rectangle 60 70 "solid" "white")
                       (rectangle (+ 60 BORDER-THICKNESS)
                                  (+ 70 BORDER-THICKNESS)
                                  "solid"
                                  "orange")))

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
           






