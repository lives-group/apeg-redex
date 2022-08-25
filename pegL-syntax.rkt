#lang racket

(require redex)
(require rackcheck)
(require "attributeL-syntax.rkt")

(define-extended-language AttributePeg AttributeL ;; Peg syntax
  (p 
   (Update ...)
   natural
   (• p p)
   (/ p p)
   (* p)
   (! p)
   ε)
  )