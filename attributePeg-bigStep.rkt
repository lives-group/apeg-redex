#lang racket
(require redex)
(require "attributePeg-syntax.rkt")
(require "attributeL-bigStep.rkt")
(provide (all-defined-out))

(define-judgment-form val-AttributePeg
  #:mode (parse I I I O O)
  #:contract (parse ctx G P r ctx)

  ;Terminal
  [-------------------------------- 
   (parse ctx G (natural_1 (natural_1 natural ...)) (natural ...) ctx)]

  [(side-condition (dismatch? natural_1 natural_2))
   --------------------------------
   (parse ctx G (natural_1 (natural_2 natural ...)) ⊥ ctx)]

  [--------------------------------
   (parse ctx G (natural_1 ()) ⊥ ctx)]

  ;Choice 
  [(parse ctx G (p_1 r) (natural ...) ctx)
   --------------------------------
   (parse ctx G ((/ p_1 p_2) r) (natural ...) ctx)]

  [(parse ctx G (p_1 r) ⊥ ctx)
   (parse ctx G (p_2 r) r_1 ctx)
   -------------------------------
   (parse ctx G ((/ p_1 p_2) r) r_1 ctx)]

  ;Sequence
  [(parse ctx G (p_1 r) (natural ...) ctx)
   (parse ctx G (p_2 (natural ...)) r_2 ctx)
   -------------------------------
   (parse ctx G ((• p_1 p_2) r) r_2 ctx)]

  [(parse ctx G (p_1 r) ⊥ ctx)
   ------------------------------
   (parse ctx G ((• p_1 p_2) r) ⊥ ctx)]

  ;Not
  [(parse ctx G (p r) (natural ...) ctx)
   -------------------------------
   (parse ctx G ((! p) r) ⊥ ctx)]  

  [(parse ctx G (p r) ⊥ ctx)
   -------------------------------
   (parse ctx G ((! p) r) r ctx)]

  ;Repetition
  [
   -------------------------------
   (parse ctx G ((* ε) r) ⊥ ctx)]
  
  [(parse ctx G (p r) ⊥ ctx)
   -------------------------------
   (parse ctx G ((* p) r) r ctx)]

  [(parse ctx G (p r) (natural ...) ctx)
   (parse ctx G ((* p) (natural ...)) r_2 ctx)
   -------------------------------
   (parse ctx G ((* p) r) r_2 ctx)]

  ;Empty
  [-------------------------------
   (parse ctx G (ε r) r ctx)]

  ;Non-Terminal
  ;-

  ;Update
  [(eval ((x_1 value_1)... (x value_3) (x_2 value_2)...) expr value)
   ----------------------------------"Update"
   (parse ((x_1 value_1)... (x value_3) (x_2 value_2)...) G (((← x expr) (← x_1 expr_1)...) s) s ((x_1 value_1)... (x value) (x_2 value_2)...))]

  )

(define-metafunction val-AttributePeg
  [(dismatch? natural_1 natural_1) #f]
  [(dismatch? natural_1 natural_2) #t])

(define-metafunction val-AttributePeg
  [(diff? x_1 x_2) #f]
  [(diff? x_1 x_2) #t])


;TERMINAL
;(judgment-holds (parse () ∅ (1 (1 2)) r ctx) r)
;(judgment-holds (parse () ∅ (1 (2 2)) r ctx) r)
;(judgment-holds (parse () ∅ (1 ()) r ctx) r)
;; CHOICE
;(judgment-holds (parse () ∅ ((/ 1 2) (1 2)) r ctx) r)
;(judgment-holds (parse () ∅ ((/ 1 2) (2 1)) r ctx) r)
;(judgment-holds (parse () ∅ ((/ 1 2) (3 3)) r ctx) r)
;(judgment-holds (parse () ∅ ((/ 1 2) ()) r ctx) r)
;; SEQUENCE
;(judgment-holds (parse () ∅ ((• 1 2) (1 2)) r ctx) r)
;(judgment-holds (parse () ∅ ((• 1 2) (1 2 2)) r ctx) r)
;(judgment-holds (parse () ∅ ((• 1 2) (2 2)) r ctx) r)
;(judgment-holds (parse () ∅ ((• 1 2) ()) r ctx) r)
;; NOT
;(judgment-holds (parse () ∅ ((! 1) (1)) r ctx) r)
;(judgment-holds (parse () ∅ ((! 1) (2)) r ctx) r)
;(judgment-holds (parse () ∅ ((! 1) ()) r ctx) r)
;(judgment-holds (parse () ∅ ((! 1) (1 2)) r ctx) r)
;; REPETITION
;(judgment-holds (parse () ∅ ((* 1) (1 1 1 1 2 3)) r ctx) r)
;(judgment-holds (parse () ∅ ((* 1) ()) r ctx) r)
;(judgment-holds (parse () ∅ ((* ε) (1 2)) r ctx) r) ;DA RUIM
; EMPTY
;(judgment-holds (parse () ∅ (ε (1 2)) r ctx) r)
;NON-TERMINAL
;--

;UPDATE
(judgment-holds (parse ((x 1) (y 2)) ∅ (((← x 3)) (1 1 1)) s ctx) ctx)
(judgment-holds (parse ((x 1) (y 2)) ∅ (((← x (+ 1 2))) (1 1 1)) s ctx) (s ctx))
(judgment-holds (parse ((x 1) (y 2)) ∅ (((← x (* 1 2))) (1 1 1)) s ctx) (s ctx))
;(judgment-holds (parse ((x 1) (y 2)) ∅ (((← x (/ 1 2))) (1 1 1)) s ctx) (s ctx)) ;;n funciona
(judgment-holds (parse ((x 1) (y 2)) ∅ (((← x (+ (+ 1 2) 2))) (1 1 1)) s ctx) (s ctx))
(judgment-holds (parse ((x 1) (y 2)) ∅ (((← x (+ (+ 1 2) (- 1 6)))) (1 1 1)) s ctx) (s ctx))
;(judgment-holds (parse ((x 1) (y 2)) ∅ (((← x (+ (+ 1 2) (- 1 6))) (← y (+ (+ 1 2) (- 1 6)))) (1 1 1)) s ctx) (s ctx)) ;; nao funciona
;(judgment-holds (parse ((x 1) (y 2)) ∅ (((← y (+ 5 2))) (1 1 1)) s ctx) (s ctx)) ;; nao funciona
(judgment-holds (parse ((x 1) (y 2)) ∅ (((← z (+ 2 2))) (1 1 1)) s ctx) (s ctx)) ;; nao sei se eh pra ter esse comportamento

;;testar e estudar o artigo pra veer como vai fazer o terminal
;MIX
;(judgment-holds (parse () ∅ ((* (/ (• 1 2) 3)) (1 2 1 2)) r ctx) r)
;(judgment-holds (parse () ∅ ((/ (• 1 2) (! 3)) (1 2 3)) r ctx) r)
;(judgment-holds (parse () ∅ ((/ (• 1 2) (! 3)) (4)) r ctx) r)




