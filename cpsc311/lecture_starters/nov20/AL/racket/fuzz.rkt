#lang racket
(require redex)

(provide gen-terms-from-p gen-terms)

;; Concrete Syntax
;; e ::= n 
;;     | b
;;     | x
;;     | (if e e e)
;;     | (* e e)
;;     | (sub1 e)
;;     | (zero? e)
;;     | (letcc x e)
;;     | (call/cc e) == (letcc x (e (lambda (v) (throw x v)))
;;     | (throw e e)
;;     | (lambda (x) e)
;;     | (box e)
;;     | (set-box! x e)
;;     | (unbox e)
;;     | (e e)
(define-language AL
  (e ::= n (* e e) (zero? e) (sub1 e)
     b (if e e e)
     x (lambda (x) e) (e e)
     (letcc x e) (call/cc e) (throw e e)
     (box e) (set-box! x e) (unbox e))
  (n ::= integer)
  (b ::= #t #f)
  (x ::= variable-not-otherwise-mentioned)
  (Γ ::= () (x Γ))
  (p ::= (side-condition e_1 (judgment-holds (⊢ () e_1 : wf)))))


;; Type System
(define-judgment-form AL
  #:mode (⊢ I I I I)
  #:contract (⊢ Γ e : wf)
  
  [---------------
   (⊢ (x_1 Γ) x_1 : wf)    
   ]

  [(⊢ Γ x_2 : wf)
   (side-condition (different x_1 x_2))
   ---------------
   (⊢ (x_1 Γ) x_2 : wf)    
   ]

  
  [---------------
   (⊢ Γ #t : wf)]
  
  [---------------
   (⊢ Γ #f : wf)]
  
  [---------------
   (⊢ Γ n : wf)]
  
  [(⊢ Γ e_1 : wf)
   (⊢ Γ e_2 : wf)
   (⊢ Γ e_3 : wf)
   --------------
   (⊢ Γ (if e_1 e_2 e_3) : wf)]

  [(⊢ Γ e_1 : wf)
   (⊢ Γ e_2 : wf)
   -----------
   (⊢ Γ (* e_1 e_2) : wf)]

  [(⊢ Γ e : wf)
   -----------
   (⊢ Γ (sub1 e) : wf)]

  [(⊢ Γ e : wf)
   -----------
   (⊢ Γ (zero? e) : wf)]

  [(⊢ (x_1 Γ) e : wf)
   --------------
   (⊢ Γ (lambda (x_1) e) : wf)]

  [(⊢ Γ e_1 : wf)
   (⊢ Γ e_2 : wf)
   --------------
   (⊢ Γ (e_1 e_2) : wf)]

  [(⊢ (x_1 Γ) e : wf)
   --------------
   (⊢ Γ (letcc x_1 e) : wf)]

  [(⊢ Γ e : wf)
   --------------
   (⊢ Γ (call/cc e) : wf)]

  [(⊢ Γ e_1 : wf)
   (⊢ Γ e_2 : wf)
   --------------
   (⊢ Γ (throw e_1 e_2) : wf)]

  [(⊢ Γ e : wf)
   --------------
   (⊢ Γ (box e) : wf)]

  [(⊢ Γ e_1 : wf)
   (⊢ Γ e_2 : wf)
   --------------
   (⊢ Γ (set-box! e_1 e_2) : wf)]

  [(⊢ Γ e : wf)
   --------------
   (⊢ Γ (unbox e) : wf)]
  )

;; Using programs p to generate doesn't work very well
;; because generate-term ignores side-conditions (see docs).
(define (gen-terms-from-p n s)
  (for/list ([i (in-range n)]) (generate-term AL p s)))

;; Generating terms directly using the well-formedness judgment
;; exploits the judgment structure, so works way better!
(define (gen-terms n s)
  (define term-attempts
    (for/list ([i (in-range n)])
      (define jdg (generate-term AL #:satisfying (⊢ () e : wf) s))
      (if jdg
          (third jdg)
          'failed)))
  (filter (λ (t) (not (equal? t 'failed))) term-attempts))
 
