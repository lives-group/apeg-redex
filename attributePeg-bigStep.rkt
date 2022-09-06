#lang racket
(require redex)
(require "attributePeg-syntax.rkt")
(require "attributeL-bigStep.rkt")
(provide (all-defined-out))

(define-judgment-form val-AttributePeg
  #:mode (parse I I I O)
  #:contract (parse ctx G P r)

  ;Terminal
  [-------------------------------- 
   (parse ctx G (natural_1 (natural_1 natural ...)) (natural ...))]

  [(side-condition (dismatch? natural_1 natural_2))
   --------------------------------
   (parse ctx G (natural_1 (natural_2 natural ...)) ⊥)]

  [--------------------------------
   (parse ctx G (natural_1 ()) ⊥)]

  ;Choice 
  [(parse ctx G (p_1 r) (natural ...))
   --------------------------------
   (parse ctx G ((/ p_1 p_2) r) (natural ...))]

  [(parse ctx G (p_1 r) ⊥)
   (parse ctx G (p_2 r) r_1)
   -------------------------------
   (parse ctx G ((/ p_1 p_2) r) r_1)]

  ;Sequence
  [(parse ctx G (p_1 r) (natural ...))
   (parse ctx G (p_2 (natural ...)) r_2)
   -------------------------------
   (parse ctx G ((• p_1 p_2) r) r_2)]

  [(parse ctx G (p_1 r) ⊥)
   ------------------------------
   (parse ctx G ((• p_1 p_2) r) ⊥)]

  ;Not
  [(parse ctx G (p r) (natural ...) )
   -------------------------------
   (parse ctx G ((! p) r) ⊥)]  

  [(parse ctx G (p r) ⊥)
   -------------------------------
   (parse ctx G ((! p) r) r)]

  ;Repetition
  [
   -------------------------------
   (parse ctx G ((* ε) r) ⊥)]
  
  [(parse ctx G (p r) ⊥)
   -------------------------------
   (parse ctx G ((* p) r) r)]

  [(parse ctx G (p r) (natural ...))
   (parse ctx G ((* p) (natural ...)) r_2)
   -------------------------------
   (parse ctx G ((* p) r) r_2)]

  ;Empty
  [-------------------------------
   (parse ctx G (ε r) r)]

  ;Non-Terminal
  ;-

  ;Update
  ;; eu preciso salvar no x o resultado do p (expr) ou o termo todo? 
  #;[ (parse ctx G (expr r_1) s)
    (parse ctx G x s)
   ;(parse ctx G ((← x expr) ...) )
    (eval ctx expr value)
   ----------------------------------"Update"
   (parse ((x value)...) G (((← x expr) ...) r_1) s)]

)

(define-metafunction val-AttributePeg
  [(dismatch? natural_1 natural_1) #f]
  [(dismatch? natural_1 natural_2) #t])


;TERMINAL
(judgment-holds (parse () ∅ (1 (1 2)) r) r)
(judgment-holds (parse () ∅ (1 (2 2)) r) r)
(judgment-holds (parse () ∅ (1 ()) r) r)
;CHOICE
(judgment-holds (parse () ∅ ((/ 1 2) (1 2)) r) r)
(judgment-holds (parse () ∅ ((/ 1 2) (2 1)) r) r)
(judgment-holds (parse () ∅ ((/ 1 2) (3 3)) r) r)
(judgment-holds (parse () ∅ ((/ 1 2) ()) r) r)
;SEQUENCE
(judgment-holds (parse () ∅ ((• 1 2) (1 2)) r) r)
(judgment-holds (parse () ∅ ((• 1 2) (1 2 2)) r) r)
(judgment-holds (parse () ∅ ((• 1 2) (2 2)) r) r)
(judgment-holds (parse () ∅ ((• 1 2) ()) r) r)
;NOT
(judgment-holds (parse () ∅ ((! 1) (1)) r) r)
(judgment-holds (parse () ∅ ((! 1) (2)) r) r)
(judgment-holds (parse () ∅ ((! 1) ()) r) r)
(judgment-holds (parse () ∅ ((! 1) (1 2)) r) r)
;REPETITION
(judgment-holds (parse () ∅ ((* 1) (1 1 1 1 2 3)) r) r)
(judgment-holds (parse () ∅ ((* 1) ()) r) r)
;(judgment-holds (parse () ∅ ((* ε) (1 2)) r) r) ;DA RUIM
;EMPTY
(judgment-holds (parse () ∅ (ε (1 2)) r) r)
;NON-TERMINAL
;--
;MIX
(judgment-holds (parse () ∅ ((* (/ (• 1 2) 3)) (1 2 1 2)) r) r)
(judgment-holds (parse () ∅ ((/ (• 1 2) (! 3)) (1 2 3)) r) r)
(judgment-holds (parse () ∅ ((/ (• 1 2) (! 3)) (4)) r) r)




