#lang racket

(require redex
         "attributeL-syntax.rkt")
(provide (all-defined-out))


(define-extended-language AttributePeg val-AttributeLType
  [p ::= (x (expr ...) (x ...))
   ((← x expr) ...)
   (= x p)
   (? expr)
   (• p p)
   (/ p p)
   (* p)
   (! p)
   natural
   ε]
  [G ::= (NT ...)]
  [NT ::= (x ((type x) ...) (expr ...) p)])


(define-extended-language val-AttributePeg AttributePeg
  [P ::= (p s)] 
  [s ::= (natural ...)]
  [r ::= s
     ⊥])