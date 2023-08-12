#lang racket

(require redex)
(require "attributeL-syntax.rkt")
(provide (all-defined-out))

(define-extended-language AttributePeg val-AttributeL
  (p
   (x (expr ...) (x ...))
   ((← x expr) ...)
   ((= x p) ...) ;added
   (? expr) ;added
   natural
   (• p p)
   (/ p p)
   (* p)
   (! p)
   ε)
  (x variable-not-otherwise-mentioned)
  (G (NT ...))
  (NT (x (x ...) (expr ...) p)))

(define-extended-language val-AttributePeg AttributePeg
  [P (p s)] 
  [s (natural ...)]
  [r s
     ⊥])