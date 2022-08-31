#lang racket

(require redex)
(require "attributeL-syntax.rkt")
(provide (all-defined-out))

(define-extended-language AttributePeg val-AttributeL ;; Peg syntax
  (p
   ((← x expr) ...) ;update
   natural
   (• p p)
   (/ p p)
   (* p)
   (! p)
   ε)
  (G ∅)
  )

(define-extended-language val-AttributePeg AttributePeg
  [P (p s)]         
  [s (natural ...)]
  [r s
     ⊥])  