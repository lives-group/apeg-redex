#lang racket

(require redex)
(require "attributeL-syntax.rkt")
(provide (all-defined-out))

(define-extended-language AttributePeg AttributeL ;; Peg syntax
  (p 
   ((← x expr) ...) ;update
   natural
   (• p p)
   (/ p p)
   (* p)
   (! p)
   ε)
  )

;fazer o bigstep pra isso aqui tbm