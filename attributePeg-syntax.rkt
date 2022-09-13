#lang racket

(require redex)
(require "attributeL-syntax.rkt")
(provide (all-defined-out))

(define-extended-language AttributePeg val-AttributeL ;; Peg syntax
  (p
   ((← x expr) ...) ;update ;;where x is a variable name and expr is an attribute expression, using F functions.
   natural
   (• p p)
   (/ p p)
   (* p)
   (! p)
   ε)
  (x variable-not-otherwise-mentioned)
  (G ∅))

(define-extended-language val-AttributePeg AttributePeg
  [P (p s)]         
  [s (natural ...)]
  [r s
     ⊥])  