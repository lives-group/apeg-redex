#lang racket

(require redex)
(provide (all-defined-out))


(define-language AttributeL
  [expr ::= l
        (⇒ ((expr expr) ...))
        (get expr expr)
        (put expr expr expr)
        (: expr expr)
        nil
        (head expr)
        (tail expr)
        (+ expr expr)
        (* expr expr)
        (÷ expr expr)
        (- expr expr)
        (&& expr expr)
        (|| expr expr)
        (¬ expr)
        (== expr expr)
        (> expr expr)
        x]
  [l ::= boolean
     integer
     string]
  [x ::= variable-not-otherwise-mentioned])


(define-extended-language AttributeLType AttributeL
  [Γ ::= ((x type)...)]
  [type ::= type:boolean
        type:integer
        type:string
        (→ (type ...) (type ...))
        (⇒ type)
        (: type)])


(define-extended-language vAttributeL AttributeLType
  [ctx ::= ((x value)...)]
  [value ::= boolean
         number
         string
         (⇒ ((string_!_ value) ...))
         (: value value)
         nil])