#lang racket
(require redex)
(require "attributePeg-syntax.rkt")
(provide (all-defined-out))

(define-judgment-form AttributePeg
  #:mode (eval I I O)
  #:contract (eval ctx p value)

  ;Terminal
  [-------------------------------- 
   (eval ctx (natural_1 (natural_1 natural ...)) (natural ...))]

  

)




