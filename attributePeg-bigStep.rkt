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

)

(define-metafunction val-AttributePeg
  [(dismatch? natural_1 natural_1) #f]
  [(dismatch? natural_1 natural_2) #t]) 

(judgment-holds (eval () ∅ (1 (1 2)) r) r)
(judgment-holds (eval () ∅ (1 (2 2)) r) r)




