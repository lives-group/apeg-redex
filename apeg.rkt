#lang racket
(require redex)
(require rackcheck)
(require "attributeL-syntax.rkt")

(define-extended-language AttributePeg AttributeL
  (ctx (e ...))
  (H natural    ; Terminal
     (/ e H)     ; Choice
     (• e H)     ; Sequence
     (* H)       ; Repetition
     (! H)       ; Not complement
     ε           ; Empty
     x)          ; Non-Terminal 
    (x variable-not-otherwise-mentioned)
  (VS (H ctx)))
