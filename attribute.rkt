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
  (x variable-not-otherwise-mentioned))

(define-extended-language ctx-AttributeL AttributeL
  (VS (expr ctx))
  (ctx ((x value)...))
  (H (+ expr H)
     (+ H expr) 
     hole)
  (value number
         undef))


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

(define-metafunction ctx-AttributeL
    look : x ((x value) ...) -> value
   [(look x ()) undef]
   [(look x ((x value) (x_1 value_1)...) ) value]
   [(look x ((x_1 value) (x_2 value_1)...) ) (look x ((x_2 value_1) ...)) ]
  )

(define tvar (traces expr-red (term ( Z ((X 10) (Y 20)))) ))
#;(traces expr-red (term ((+ 1 2) () ) ) )
#;(define t2 (traces expr-red (term ((+ 2 (+ 1 2)) ()) )) )
#;(define t3 (traces expr-red (term ((+ (+ 1 1) 2) ())) ))
(define t4 (traces expr-red (term ((+ (+ X 1) 2) ((X 10)))) ))
(traces expr-red (term ((+ (+ 2 4) (+ 1 2)) ()) ))

#;(redex-match ctx-AttributeL (in-hole H (+ number_1 number_2)) (term (+ 5 (+ 1 (+ 2 3))) ) )
#;(redex-match ctx-AttributeL (in-hole H expr) (term ( + (+ 1 (+ 2 3)) 5) ) )
(redex-match ctx-AttributeL (in-hole H expr) (term ( + (+ 2 4) (+ 1 2)) ) ) 