#lang racket
(require redex)
(require "attributeL-syntax.rkt")
(provide (all-defined-out))

(define-judgment-form val-AttributeL
  #:mode (eval I I O)
  #:contract (eval ctx expr value)
  
  [-------------------------------- 
   (eval ctx number number)]

  [-------------------------------- 
   (eval ctx string string)]

  [
   -------------------------------- 
   (eval ((_ _)... (x value) (_ _)...) x value)]

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

  [(eval ctx expr_1 number_1)
   ------------------------------------
   (eval ctx (head (: expr_1 _)) number_1)]

  [(eval ctx expr_2 number)
   ------------------------------------
   (eval ctx (tail (: _ expr_2)) number)]

  [(eval ctx expr_1 (⇒ ((string_1 value_1))))
   (eval ctx expr_2 string_1)
   ------------------------------------
   (eval ctx (get expr_1 expr_2) value_1)]

  [
   -------------------------------------------------"map-empty"
   (eval ctx (⇒ () ) (⇒ () ))]
  

  [(eval ctx expr_1 string_1)
   (eval ctx expr_2 value_1)
   (eval ctx (⇒ ((expr_3 expr_4)...)) (⇒((value_3 value_4)...)))
   -------------------------------------------------"map-mult"
   (eval ctx (⇒ ((expr_1 expr_2) (expr_3 expr_4)...) ) (⇒ ((string_1 value_1) (value_3 value_4)...) ))]
   
  #;[ (eval ctx expr_1 string)
    (eval ctx expr_2 value)
    ------------------------------------"map-one"
    (eval ctx (⇒ ((expr_1 expr_2))) (⇒ ((string value))))]


  [(eval ctx expr_2 string)
   (eval ctx expr_1 (⇒ ((_ _)...  (string value) (_ _)...) ))
   ----------------------------------------------- "get"
   (eval ctx (get expr_1 expr_2) value )])

;FALTA O PUT
;TESTAR MESMO RESULTADO DO SMALL STEP COM O BIG STEP

(define-metafunction val-AttributeL
  look : x ((x value) ...) -> value
  [(look x ()) undef]
  [(look x ((x value) (x_1 value_1)...) ) value]
  [(look x ((x_1 value) (x_2 value_1)...) ) (look x ((x_2 value_1) ...)) ]
  )





;; am i testing this code or is it testing me?


(judgment-holds (eval () (⇒ (("1" 3) ("2" 1))) value) value)
(judgment-holds (eval () (⇒ (("1" 3))) value) value)
(judgment-holds (eval () (⇒ ()) value) value)

;(judgment-holds (eval ((x 4) (y 2)) (head (: (+ x 3) 4)) value) value)
;(judgment-holds (eval ((x 4) (y 2)) (tail (: (+ x 3) 4)) value) value)
;(judgment-holds (eval ((x 4) (y 2)) (tail (: (+ x 3) (- x 3))) value) value)
(judgment-holds (eval () (get (⇒ (("1" 1) ("2" 2))) "2") value) value)
(judgment-holds (eval () (get (⇒ (("1" 1) ("2" 2))) "1") value) value)
;(judgment-holds (eval () (put (⇒ (("1" 1) ("2" (+ 1 2)))) "2" 1) value) value)

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