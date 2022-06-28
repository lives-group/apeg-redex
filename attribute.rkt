#lang racket
(require redex)

; Syntax
(define-language AttributeL
  (update (attr ...)) 
  (attr (← x expr))
  (expr natural
        (+ expr expr)
        (* expr expr)
        (/ expr expr)
        (- expr expr)
        x)
  (x variable-not-otherwise-mentioned)
  (value natural))

(define-extended-language ctx-AttributeL AttributeL
  (ctx ((x value)...))
  (H value 
     (+ H expr)
     (* H expr)
     (/ H expr)
     (- H expr))
  (VS (H ctx)))

(define ctx-red
  (reduction-relation
   ctx-AttributeL
    #:domain VS
    (--> ((+ value expr) ctx) 
         (value ctx))))
