#lang racket

(require redex)
(provide (all-defined-out))

; Syntax
(define-language AttributeL
  (expr l
        (⇒ ((expr expr) ...))
        (get expr expr)
        (put expr expr expr)
        (: expr expr) ;; do lista
        nil           ;; lista empty
        (head expr)
        (tail expr)
        (+  expr expr)
        (*  expr expr)
        (÷  expr expr)
        (-  expr expr)
        x)
  (l number
     string)
  (x variable-not-otherwise-mentioned))

(define-extended-language val-AttributeL AttributeL
  (ctx ((x value)...))
  (value number
         string
         (⇒ ((string value) ...))
         (: value value)
         nil
         undef))