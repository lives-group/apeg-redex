#lang racket
(require redex)

; Syntax
(define-language AttributeL
  (update (attr ...)) 
  (attr (← x expr))
  (expr number
        (+ expr expr)
        (* expr expr)
        (/ expr expr)
        (- expr expr)
        x)
  (x variable-not-otherwise-mentioned)
  (value number
         undef)
  (ctx ((x value)...)))
  
(define-extended-language ctx-AttributeL AttributeL
  (VS (expr ctx))
  (ctx ((x value)...))
  (H (+ H expr)
     (+ value H)
     (* H expr)
     (* value H)
     (- H expr)
     (- value H)
     (/ H expr)
     (/ value H)
     hole)
  (value number
         undef))


(define-judgment-form ctx-AttributeL
  #:mode (eval I I O)
  #:contract (eval ctx expr value)
  
  [-------------------------------- 
   (eval ctx number number)]

  [
   -------------------------------- 
   (eval ctx x (look x ctx))]

  [(eval ctx expr_1 number_1)
   (eval ctx expr_2 number_2)
   ------------------------------------
   (eval ctx (+ expr_1 expr_2) ,(+ (term number_1) (term number_2)))]

  [(eval ctx expr_1 number_1)
   (eval ctx expr_2 number_2)
   ------------------------------------
   (eval ctx (* expr_1 expr_2) ,(* (term number_1) (term number_2)))]

  [(eval ctx expr_1 number_1)
   (eval ctx expr_2 number_2)
   ------------------------------------
   (eval ctx (- expr_1 expr_2) ,(- (term number_1) (term number_2)))]

  [(eval ctx expr_1 number_1)
   (eval ctx expr_2 number_2)
   ------------------------------------
   (eval ctx (/ expr_1 expr_2) ,(/ (term number_1) (term number_2)))]
  )






(define-metafunction ctx-AttributeL
    look : x ((x value) ...) -> value
   [(look x ()) undef]
   [(look x ((x value) (x_1 value_1)...) ) value]
   [(look x ((x_1 value) (x_2 value_1)...) ) (look x ((x_2 value_1) ...)) ]
  )



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
   (--> ((in-hole H   (*  number_1        number_2)       ) ctx) 
        ((in-hole H  ,(* (term number_1) (term number_2)) ) ctx)
        "mult1")
   (--> ((in-hole H   (-  number_1        number_2)       ) ctx) 
        ((in-hole H  ,(- (term number_1) (term number_2)) ) ctx)
        "sub1")
   (--> ((in-hole H   (/  number_1        number_2)       ) ctx) 
        ((in-hole H  ,(/ (term number_1) (term number_2)) ) ctx)
        "div1")
       
   ))

;(traces expr-red (term ((* 1 2) () ) ) )
;(judgment-holds (eval ((x 4)) (+ (* x 7) (* 1 3)) value) value)
;(judgment-holds (eval ((x 3)) (+ (* x 7) (/ x 3)) value) value)
;(judgment-holds (eval ((x 4) (y 2)) (+ (* x 7) (* y 3)) value) value)
;(judgment-holds (eval ((x 4) (y 2)) (+ (* x 7) (* t 3)) value) value)

(traces expr-red (term ( Z ((X 10) (Y 20)))) )
#;(traces expr-red (term ((+ 1 2) () ) ) )
#;(traces expr-red (term ((+ 2 (+ 1 2)) ()) )) 
#;(traces expr-red (term ((+ (+ 1 1) 2) ())) )
(traces expr-red (term ((+ (+ X 1) 2) ((X 10)))) )
(traces expr-red (term ((+ (+ 2 4) (+ 1 2)) ()) ))


; The so-precious in-hole examaples ! 
;
#;(redex-match ctx-AttributeL (in-hole H (+ number_1 number_2)) (term (+ 5 (+ 1 (+ 2 3))) ) )
#;(redex-match ctx-AttributeL (in-hole H expr) (term ( + (+ 1 (+ 2 3)) 5) ) )
(redex-match ctx-AttributeL (in-hole H expr) (term ( + (+ 2 4) (+ 1 2)) ) ) 

