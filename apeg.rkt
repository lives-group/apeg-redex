#lang racket
(require redex)
(require rackcheck)
(require "../redexAPEG/Redex-PEG/peg/lang/peg.rkt")

(define-extended-language AttributePeg Peg
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
