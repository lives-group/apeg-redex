#lang racket
(require redex)
(require "attributeL-syntax.rkt")
(provide (all-defined-out))

(define-judgment-form val-AttributeL
  #:mode (eval I I O)
  #:contract (eval ctx expr value)

  [-------------------------- boolean
   (eval ctx boolean boolean)]
  
  [------------------------ number
   (eval ctx number number)]

  [------------------------ string
   (eval ctx string string)]

  [-------------------------------------------- variable
   (eval ((_ _)... (x value) (_ _)...) x value)]

  [(eval ctx expr_1 number_1)
   (eval ctx expr_2 number_2)
   ----------------------------------------------------------------- addition
   (eval ctx (+ expr_1 expr_2) ,(+ (term number_1) (term number_2)))]

  [(eval ctx expr_1 number_1)
   (eval ctx expr_2 number_2)
   ----------------------------------------------------------------- multiplication
   (eval ctx (* expr_1 expr_2) ,(* (term number_1) (term number_2)))]

  [(eval ctx expr_1 number_1)
   (eval ctx expr_2 number_2)
   ----------------------------------------------------------------- subtraction
   (eval ctx (- expr_1 expr_2) ,(- (term number_1) (term number_2)))]

  [(eval ctx expr_1 integer_1)
   (eval ctx expr_2 integer_2)
   -------------------------------------------------------------------------- division-integer ;added
   (eval ctx (÷ expr_1 expr_2) ,(quotient (term integer_1) (term integer_2)))]

  [(eval ctx expr_1 real_1)
   (eval ctx expr_2 real_2)
   (side-condition ,(not (or (exact-integer? (term real_1)) (exact-integer? (term real_2)))))
   ------------------------------------------------------------- division-real ;modified
   (eval ctx (÷ expr_1 expr_2) ,(/ (term real_1) (term real_2)))]

  [(eval ctx expr_1 #t)
   (eval ctx expr_2 boolean)
   ------------------------------------- and-first-success
   (eval ctx (&& expr_1 expr_2) boolean)]
  
  [(eval ctx expr_1 #f)
   -------------------------------- and-first-fail
   (eval ctx (&& expr_1 expr_2) #f)]

  [(eval ctx expr_1 #t)
   -------------------------------- or-first-success
   (eval ctx (|| expr_1 expr_2) #t)]

  [(eval ctx expr_1 #f)
   (eval ctx expr_2 boolean)
   ------------------------------------- or-first-fail
   (eval ctx (|| expr_1 expr_2) boolean)]

  [(eval ctx expr boolean)
   ----------------------------------------- not
   (eval ctx (¬ expr) ,(not (term boolean)))]

  [(eval ctx expr_1 number_1)
   (eval ctx expr_2 number_2)
   ------------------------------------------------------------------ equality
   (eval ctx (== expr_1 expr_2) ,(= (term number_1) (term number_2)))]

  [(eval ctx expr_1 number_1)
   (eval ctx expr_2 number_2)
   ----------------------------------------------------------------- bigger-then
   (eval ctx (> expr_1 expr_2) ,(> (term number_1) (term number_2)))]

  [------------------ nil
   (eval ctx nil nil)]

  [(eval ctx expr_1 value_1)
   (eval ctx expr_2 value_2)
   ---------------------------------------------- list
   (eval ctx (: expr_1 expr_2) (: value_1 value_2))]

  [(eval ctx expr (: value_head _))
   --------------------------------- head
   (eval ctx (head expr) value_head)]

  [(eval ctx expr (: _ value_tail))
   --------------------------------- tail
   (eval ctx (tail expr) value_tail)]

  [------------------------- empty-mapping
   (eval ctx (⇒ ()) (⇒ ()))]

  [(eval ctx (put (⇒ ((expr_keys expr_values)...)) expr_key expr_value) (⇒ ((string value)...)))
   ----------------------------------------------------------------------------------------- multiple-mapping 
   (eval ctx (⇒ ((expr_keys expr_values)... (expr_key expr_value))) (⇒ ((string value)...)))]

  [(eval ctx expr_2 string)
   (eval ctx expr_1 (⇒ ((_ _)... (string value) (_ _)...)))
   ------------------------------------ get
   (eval ctx (get expr_1 expr_2) value)]

  [(eval ctx expr_key string_key)
   (eval ctx expr_value value_value)
   (eval ctx expr_mapping (⇒ ((string_1 value_1)... (string_key value) (string_2 value_2)...)))
   ---------------------------------------------------------------------------------------------------------------------------- replace-put
   (eval ctx (put expr_mapping expr_key expr_value) (⇒ ((string_1 value_1)... (string_key value_value) (string_2 value_2)...)))]

  [(eval ctx expr_key string_key)
   (eval ctx expr_value value_value)
   (eval ctx expr_mapping (⇒ ((string value)...)))
   (side-condition (without-occurrence (string value)... string_key))
   -------------------------------------------------------------------------------------------------- append-put
   (eval ctx (put expr_mapping expr_key expr_value) (⇒ ((string value)... (string_key value_value))))])

(define-metafunction val-AttributeL
  without-occurrence : (string value)... string_key -> boolean
  [(without-occurrence (string_key value_1) (string value)... string_key) #f]
  [(without-occurrence (string_1 value_1) (string value)... string_key) (without-occurrence (string value)... string_key)]
  [(without-occurrence string_key) #t])