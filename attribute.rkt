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
  (value natural)
  (ctx ((x value)...)))

(define-judgment-form AttributeL
  #:mode (eval I I O)
  #:contract (eval ctx expr value)
  
  [-------------------------------- 
   (eval ctx number number)]

  [
   -------------------------------- 
   (eval ((x value)) x value)]

  [(eval ctx expr_1 value_1)
   (eval ctx expr_2 value_2)
   ------------------------------------
   (eval ctx (+ expr_1 expr_2) ,(+ (term value_1) (term value_2)))]

  [(eval ctx expr_1 value_1)
   (eval ctx expr_2 value_2)
   ------------------------------------
   (eval ctx (* expr_1 expr_2) ,(* (term value_1) (term value_2)))]
  )

(define-extended-language ctx-AttributeL AttributeL
  (VS (expr ctx))
  (ctx ((x value)...))
  (P (H ctx))
  (H (+ H expr)
     (+ value H)
     (* H expr)
     (* value H)
     hole)
  
  (value number))

(define ctx-red
  (reduction-relation
       ctx-AttributeL
       #:domain expr
       (--> (in-hole H (+ value_1 value_2) ) 
            (in-hole H ,(+ (term value_1) (term value_2)) )
            "add1")
   ))
