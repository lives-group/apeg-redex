#lang racket
(require redex)

; Syntax
(define-language AttributeL
  (update (attr ...)) 
  (attr (← x expr))
  (expr natural
        float
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

  [(eval ctx expr_1 value_1)
   (eval ctx expr_2 value_2)
   ------------------------------------
   (eval ctx (- expr_1 expr_2) ,(- (term value_1) (term value_2)))]

  [(eval ctx expr_1 value_1)
   (eval ctx expr_2 value_2)
   ------------------------------------
   (eval ctx (/ expr_1 expr_2) ,(/ (term value_1) (term value_2)))]
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

(define expr-red
  (reduction-relation
       ctx-AttributeL
       #:domain VS
       (--> ((in-hole H   (+  number_1        number_2)       ) ctx) 
            ((in-hole H  ,(+ (term number_1) (term number_2)) ) ctx)
            "add1")
       (--> ((in-hole H   x)     ( (x_1 value_1)... (x value) (x_2 value_2)... ) )  
            ((in-hole H  value ) ( (x_1 value_1)... (x value) (x_2 value_2)... ) )
            "var")
   ))

;(traces ctx-red (term (+ 1 (+ 1 1))))
;(judgment-holds (eval ((x 4)) (+ (* x 7) (* 1 3)) value) value)
;(judgment-holds (eval ((x 3)) (+ (* x 7) (/ x 3)) value) value)