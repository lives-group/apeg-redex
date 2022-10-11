#lang racket

(require redex)
(require "attributeL-syntax.rkt")
(provide (all-defined-out))

(define-extended-language AttributePeg val-AttributeL ;; Peg syntax
  (p
   (x (expr ...) (x ...)) ;; 1x -> nome do NT // Lista de expr ->parametros do NT // Lista de x -> nomes dos retornos
   ((← x expr) ...) ;update ;;where x is a variable name and expr is an attribute expression, using F functions.
   natural
   (• p p)
   (/ p p)
   (* p)
   (! p)
   ε)
  (x variable-not-otherwise-mentioned)
  (G ((x p) ...)))

(define-extended-language val-AttributePeg AttributePeg
  [P (p s)] 
  [s (natural ...)]
  [r s
     ⊥])
