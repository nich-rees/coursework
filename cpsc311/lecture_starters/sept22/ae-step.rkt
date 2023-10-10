#lang plai

;;
;; Hand-Stepping AE programs
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define AEFS1 4)

'4 ;; DONE
;; something weird about quotes:
;; with quote is s-expression data representation of AE program
;; without quote would be PLAI program
;; Am I talking about the meta langauge (PLAI) which is what we are coding in,
;; or the object language (AE) which is the object of study?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define AEFS2 `{+ ,AEFS1 5})

'{+ 4 5}

;; Step rules for addition: (pretty standard across languages)
;; if the first position can step, then step it
;; if the first position can't step, but the second can, then step that
;; if neither position can step, but both positions are numbers, do addition
'9

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define AEFS3 '{- 6 3})

'{- 6 3}
;; if the first position can step, then step it
;; if the first position can't step, but the second can, then step that
;; if neither position can step, but both positions are numbers, do subtraction
'3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define AEFS4 '{+ {- 6 3}
;                  {- 7 {+ 3 1}}})

'{+ {- 6 3} {- 7 {+ 3 1}}}
'{+ 3 {- 7 {+ 3 1}}}
'{+ 3 {- 7 4}}
'{+ 3 3}
'6