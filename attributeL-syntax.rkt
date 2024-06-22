#lang racket

(require redex)
(provide (all-defined-out))


(define-language AttributeL
  [expr ::= l			; literals
        (⇒ ((expr expr) ...))	; map definition
        (get expr expr)		; map access
        (put expr expr expr)	; map extension
        (: expr expr)		; list definition
        nil			; empty list
        (head expr)		; list operation
        (tail expr)		; list operation
        (+ expr expr)		; add operation
        (* expr expr)		; mult operation
        (÷ expr expr)		; div operation
        (- expr expr)		; sub operation
        (&& expr expr)		; and operation
        (|| expr expr)		; or operation
        (¬ expr)		; not operation
        (== expr expr)		; equality operation
        (> expr expr)		; bigger-than operation
        x]
  [l ::= boolean
     integer
     string]
  [x ::= variable-not-otherwise-mentioned])


(define-extended-language AttributeLType AttributeL
  [Γ ::= ((x type)...)]		; context for typing 
  [type ::= type:boolean	; boolean type
        type:integer		; integer type
        type:string		; string type
        (⇒ type)		; map type
        (: type)		; list type
        (→ (type ...) (type ...))])	;nonterm type


(define-extended-language vAttributeL AttributeLType
  [ctx ::= ((x value)...)]
  [value ::= boolean
         integer
         string
         (⇒ ((string_!_ value) ...))
         (: value value)
         nil])