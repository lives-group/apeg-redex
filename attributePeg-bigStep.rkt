#lang racket
(require redex)
(require "attributePeg-syntax.rkt")
(provide (all-defined-out))

(define-judgment-form val-AttributePeg
  #:mode (eval I I I O)
  #:contract (eval ctx G P r)

  ;Terminal
  [-------------------------------- 
   (eval ctx G (natural_1 (natural_1 natural ...)) (natural ...))]

  [(side-condition (dismatch? natural_1 natural_2))
   --------------------------------
   (eval ctx G (natural_1 (natural_2 natural ...)) ⊥)]

  [--------------------------------
   (eval ctx G (natural_1 ()) ⊥)]

  ;Choice 
  [(eval ctx G (p_1 r) (natural ...))
   --------------------------------
   (eval ctx G ((/ p_1 p_2) r) (natural ...))]

  [(eval ctx G (p_1 r) ⊥)
   (eval ctx G (p_2 r) r_1)
   -------------------------------
   (eval ctx G ((/ p_1 p_2) r) r_1)]

  ;Sequence
  [(eval ctx G (p_1 r) (natural ...))
   (eval ctx G (p_2 (natural ...)) r_2)
   -------------------------------
   (eval ctx G ((• p_1 p_2) r) r_2)]

  [(eval ctx G (p_1 r) ⊥)
   ------------------------------
   (eval ctx G ((• p_1 p_2) r) ⊥)]

  ;Not
  [(eval ctx G (p r) (natural ...) )
   -------------------------------
   (eval ctx G ((! p) r) ⊥)]  

  [(eval ctx G (p r) ⊥)
   -------------------------------
   (eval ctx G ((! p) r) r)]

  ;Repetition
  [
   -------------------------------
   (eval ctx G ((* ε) r) ⊥)]
  
  [(eval ctx G (p r) ⊥)
   -------------------------------
   (eval ctx G ((* p) r) r)]

  [(eval ctx G (p r) (natural ...))
   (eval ctx G ((* p) (natural ...)) r_2)
   -------------------------------
   (eval ctx G ((* p) r) r_2)]

  ;Empty
  [-------------------------------
   (eval ctx G (ε r) r)]

  ;Non-Terminal
  ;-

  ;Update

  [ (eval ctx G (p r_1) s) ;; eu preciso salvar no x o resultado do p (expr) ou o termo todo? 
    (eval ctx G x s)
   ;(eval ctx G ((← x expr) ...) )
   ----------------------------------"Update"
   (eval ctx G (((← x p) ...) r_1) s)]

)

(define-metafunction val-AttributePeg
  [(dismatch? natural_1 natural_1) #f]
  [(dismatch? natural_1 natural_2) #t]) 

;TERMINAL
(judgment-holds (eval () ∅ (1 (1 2)) r) r)
(judgment-holds (eval () ∅ (1 (2 2)) r) r)
(judgment-holds (eval () ∅ (1 ()) r) r)
;CHOICE
(judgment-holds (eval () ∅ ((/ 1 2) (1 2)) r) r)
(judgment-holds (eval () ∅ ((/ 1 2) (2 1)) r) r)
(judgment-holds (eval () ∅ ((/ 1 2) (3 3)) r) r)
(judgment-holds (eval () ∅ ((/ 1 2) ()) r) r)
;SEQUENCE
(judgment-holds (eval () ∅ ((• 1 2) (1 2)) r) r)
(judgment-holds (eval () ∅ ((• 1 2) (1 2 2)) r) r)
(judgment-holds (eval () ∅ ((• 1 2) (2 2)) r) r)
(judgment-holds (eval () ∅ ((• 1 2) ()) r) r)
;NOT
(judgment-holds (eval () ∅ ((! 1) (1)) r) r)
(judgment-holds (eval () ∅ ((! 1) (2)) r) r)
(judgment-holds (eval () ∅ ((! 1) ()) r) r)
(judgment-holds (eval () ∅ ((! 1) (1 2)) r) r)
;REPETITION
(judgment-holds (eval () ∅ ((* 1) (1 1 1 1 2 3)) r) r)
(judgment-holds (eval () ∅ ((* 1) ()) r) r)
;(judgment-holds (eval () ∅ ((* ε) (1 2)) r) r) ;DA RUIM
;EMPTY
(judgment-holds (eval () ∅ (ε (1 2)) r) r)
;NON-TERMINAL
;--
;MIX
;(judgment-holds (eval () ∅ ((* (/ (• 1 2) (! 3))) (1 2 1 2)) r) r)
(judgment-holds (eval () ∅ ((/ (• 1 2) (! 3)) (1 2 3)) r) r)
(judgment-holds (eval () ∅ ((/ (• 1 2) (! 3)) (4)) r) r)




