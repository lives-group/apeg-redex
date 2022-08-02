#lang racket
(require redex)

; Syntax
(define-language AttributeL
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

(define-extended-language PegL ctx-AttributeL
   (APeg  (Update ... )
          any)
   (Update (← x expr))
 
   (input (string natural (natural ...)))
   (r ok
      fail
      indef
      )
   (st (APeg ctx input r))
  )

(define-metafunction PegL
    len : string -> natural
   [(len  string) ,(= (string-length (term string) 1))]
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

#;(traces expr-red (term ( Z ((X 10) (Y 20)))) )
#;(traces expr-red (term ((+ 1 2) () ) ) )
#;(traces expr-red (term ((+ 2 (+ 1 2)) ()) )) 
#;(traces expr-red (term ((+ (+ 1 1) 2) ())) )
#;(traces expr-red (term ((+ (+ X 1) 2) ((X 10)))) )
#;(traces expr-red (term ((+ (+ 2 4) (+ 1 2)) ()) ))


; The so-precious in-hole examaples ! 
;
#;(redex-match ctx-AttributeL (in-hole H (+ number_1 number_2)) (term (+ 5 (+ 1 (+ 2 3))) ) )
#;(redex-match ctx-AttributeL (in-hole H expr) (term ( + (+ 1 (+ 2 3)) 5) ) )
#;(redex-match ctx-AttributeL (in-hole H expr) (term ( + (+ 2 4) (+ 1 2)) ) ) 


(define apeg-red
  (reduction-relation
   PegL
   #:domain st
   (--> ( ( (← x value) Update ...) ctx input r) 
        ( (Update ...) (update_val x value ctx) input r)
        "update")
   (--> ( ( (← x expr) Update ...) ctx input r) 
        ( ( (← x value) Update ...) ctx input r)
        (where #t (notSingleton expr))
        (where ((value ctx_1) (value_2 ctx_2)...) ,(apply-reduction-relation* expr-red (term (expr ctx)) ))
        "eval-expr")       
   ))


(define-metafunction ctx-AttributeL
    update_val : x value ctx -> ctx 
   [(update_val x value ()) ((x value))]
   [(update_val x value ((x value_2) (x_1 value_1)...) ) ((x value) (x_1 value_1)...)]
   [(update_val x value ((x_1 value_1) (x_2 value_2)...) ) ((x_1 value_1) (look x ((x_2 value_2) ...))) ]
  )

(define-metafunction PegL
    notSingleton : expr -> boolean 
   [(notSingleton value)  #f]
   [(notSingleton expr)   #t]
  )


;(traces apeg-red (term ( ((← A 1)) ((A 234)) ("a" 0 ()) indef) ))
(traces apeg-red (term ( ((← A (+ 1 A))) ((A 234)) ("a" 0 ()) indef) ))
(apply-reduction-relation* expr-red (term (  (+ 1 A)  ((A 234)) )  ))