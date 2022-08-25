#lang racket

(require redex)
(require rackcheck)
(require "attributeL-syntax.rkt")

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