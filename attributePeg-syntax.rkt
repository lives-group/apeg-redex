#lang racket

(require redex
         "attributeL-syntax.rkt")
(provide (all-defined-out))


(define-extended-language AttributePeg vAttributeL
  [p ::= (x (expr ...) (x ...))	; nonterminal
   ((← x expr) ...)	; update
   (= x p)		; bind
   (? expr)		; constraint
   (• p p)		; sequence
   (/ p p)		; priorized-choice
   (* p)		; repetition
   (! p)		; not
   natural		; terminal
   ε]			; epsilon
  [G ::= (NT ...)]
  [NT ::= (x ((type x) ...) (expr ...) p)])


(define-extended-language vAttributePeg AttributePeg
  [s ::= (natural ...)]
  [r ::= s
     ⊥])