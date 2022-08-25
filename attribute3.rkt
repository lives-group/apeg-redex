#lang racket
(require redex)
(require "syntax.rkt")

(define expr-red
  (reduction-relation ctx-AttributeL
  #:domain VS
   (--> ((in-hole H   (+  number_1        number_2)       ) ctx)
        ((in-hole H  ,(+ (term number_1) (term number_2)) ) ctx)
        "add1")
   (--> ((in-hole H   x)     ( (x_1 value_1)... (x value) (x_2 value_2)... ) )
        ((in-hole H   value ) ( (x_1 value_1)... (x value) (x_2 value_2)... ) )
        "var")
   (--> ((in-hole H   (*  number_1        number_2)       ) ctx)
        ((in-hole H  ,(* (term number_1) (term number_2)) ) ctx)
        "mult1")
   (--> ((in-hole H   (-  number_1        number_2)       ) ctx)
        ((in-hole H  ,(- (term number_1) (term number_2)) ) ctx)
        "sub1")
   (--> ((in-hole H   (÷ number_1        number_2)       ) ctx)
        ((in-hole H  ,(/ (term number_1) (term number_2)) ) ctx)
        "div1")
   (--> ((in-hole H  (get  (⇒ ((string_1 value_1)... (string_2 value_2) (string_3 value_3)...))        string_2)      )  ctx)
        ((in-hole H (map-get ((string_1 value_1)... (string_2 value_2) (string_3 value_3)...) string_2)) ctx)
        "get")
   (--> ((in-hole H (put (⇒ ((string_1 value_1)...)) string value)) ctx)
        ((in-hole H (⇒ ((string value) (string_1 value_1)...))) ctx)
        "put")
   (--> ((in-hole H (head (: value_1 value_2)))       ctx)
        ((in-hole H value_1)    ctx)
        "head")
   (--> ((in-hole H (tail (: value_1 value_2)))       ctx)
        ((in-hole H value_2)    ctx)
        "tail") ))

;(traces expr-red (term ((* 1 2) () ) ) )
;(judgment-holds (eval ((x 4)) (+ (* x 7) (* 1 3)) value) value)
;(judgment-holds (eval ((x 3)) (+ (* x 7) (÷ x 3)) value) value)
;(judgment-holds (eval ((x 4) (y 2)) (+ (* x 7) (* y 3)) value) value)
;(judgment-holds (eval ((x 4) (y 2)) (+ (* x 7) (* t 3)) value) value)

#;(traces expr-red (term ( Z ((X 10) (Y 20)))) )
#;(traces expr-red (term ((+ 1 2) () ) ) )
#;(traces expr-red (term ((+ 2 (+ 1 2)) ()) )) 
#;(traces expr-red (term ((+ (+ 1 1) 2) ())) )
#;(traces expr-red (term ((+ (+ X 1) 2) ((X 10)))) )
#;(traces expr-red (term ((+ (+ 2 4) (+ 1 2)) ()) ))
#;(traces expr-red (term ((get (⇒ (("1" (+ 1 2)) ("2" (+ 0 1)))) "2") ())))
#;(traces expr-red (term ((⇒ (("1" 1) ("2" (+ 1 2)))) ())))
#;(traces expr-red (term ((put (⇒ (("1" 1) ("2" (+ 1 2)))) "2" 1) ())))
#;(traces expr-red (term ((get (put (⇒ (("1" 1) ("2" (+ 1 2)))) "2" 1) "C") ())))
#;(traces expr-red (term ((: (+ 1 0) (: (+ 2 3) nil)) ()) ))
#;(traces expr-red (term ((head (: (+ 1 0) (: (+ 2 3) nil))) ()) ))
#;(traces expr-red (term ((tail (: (+ 1 0) (: (+ 2 3) nil))) ()) ))

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
        (where ((value ctx_1) (value_2 ctx_2)...)
               ,(apply-reduction-relation* expr-red (term (expr ctx)) ))
        "eval-expr")))


(define-metafunction ctx-AttributeL
    map-get : ((string value)...) string -> value
  [(map-get () string) undef]
  [(map-get ((string value) (string_1 value_1)...) string) value]
  [(map-get ((string value) (string_1 value_1)...) string_2) (map-get ((string_1 value_1)...) string_2)])

(define-metafunction ctx-AttributeL
    update_val : x value ctx -> ctx 
  [(update_val x value ()) ((x value))]
  [(update_val x value ((x value_2) (x_1 value_1)...) ) ((x value) (x_1 value_1)...)]
  [(update_val x value ((x_1 value_1) (x_2 value_2)...) ) ((x_1 value_1) (look x ((x_2 value_2) ...)))])

(define-metafunction PegL
    notSingleton : expr -> boolean 
  [(notSingleton value)  #f]
  [(notSingleton expr)   #t])


;(traces apeg-red (term ( ((← A 1)) ((A 234)) ("a" 0 ()) indef) ))
(traces apeg-red (term ( ((← A (+ 1 A))) ((A 234)) ("a" 0 ()) indef) ))
;(apply-reduction-relation* expr-red (term (  (+ 1 A)  ((A 234)) )  ))

;organizar o repo
;separar em duas linguagens
;trazer a sintaxe de peg pra cá
;attrPeg - Peg
;ler o artigo http://www.llp.dcc.ufmg.br/Publications/Journal2014/2014-scp-leonardo-formal-apeg.pdf
