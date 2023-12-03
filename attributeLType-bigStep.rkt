#lang racket
(require redex
         "attributeL-syntax.rkt")
(provide (all-defined-out))

(define-judgment-form val-AttributeLType
  #:mode (types I I O)
  #:contract (types ctx expr type)

  [-------------------------------- boolean
   (types ctx boolean type:boolean)]

  [-------------------------------- integer
   (types ctx integer type:integer)]

  [------------------------------ string
   (types ctx string type:string)]

  [----------------------------------------------------------- attribute
   (types ((x_1 type_1) ... (x type) (x_2 type_2) ...) x type)]

  [(types ctx expr_1 type:integer)
   (types ctx expr_2 type:integer)
   ------------------------------------------ addition
   (types ctx (+ expr_1 expr_2) type:integer)]

  [(types ctx expr_1 type:integer)
   (types ctx expr_2 type:integer)
   ------------------------------------------ multiplication
   (types ctx (* expr_1 expr_2) type:integer)]

  [(types ctx expr_1 type:integer)
   (types ctx expr_2 type:integer)
   ------------------------------------------ subtraction
   (types ctx (- expr_1 expr_2) type:integer)]

  [(types ctx expr_1 type:integer)
   (types ctx expr_2 type:integer)
   ------------------------------------------ division
   (types ctx (÷ expr_1 expr_2) type:integer)]

  [(types ctx expr_1 type:boolean)
   (types ctx expr_2 type:boolean)
   ------------------------------------------- and
   (types ctx (&& expr_1 expr_2) type:boolean)]

  [(types ctx expr_1 type:boolean)
   (types ctx expr_2 type:boolean)
   ------------------------------------------- or
   (types ctx (|| expr_1 expr_2) type:boolean)]

  [(types ctx expr type:boolean)
   --------------------------------- not
   (types ctx (¬ expr) type:boolean)]

  [(types ctx expr_1 type:integer)
   (types ctx expr_2 type:integer)
   ------------------------------------------- equality
   (types ctx (== expr_1 expr_2) type:boolean)]

  [(types ctx expr_1 type:integer)
   (types ctx expr_2 type:integer)
   ------------------------------------------ bigger-then
   (types ctx (> expr_1 expr_2) type:boolean)]

  [(types ctx expr_head type)
   -------------------------------------- unitary-list
   (types ctx (: expr_head nil) (: type))]

  [(types ctx expr_head type)
   (types ctx expr_tail (: type))
   -------------------------------------------- multiple-list
   (types ctx (: expr_head expr_tail) (: type))]

  [(types ctx expr_key type:string) ...
   (types ctx expr_value type) ...
   (side-condition (equality type ...))
   -------------------------------------------------------------------------- mapping
   (types ctx (⇒ ((expr_key expr_value) ...)) (⇒ (get-first-type type ...)))]

  [(types ctx expr_list (: type))
   --------------------------------- head
   (types ctx (head expr_list) type)]

  [(types ctx expr_list type)
   --------------------------------- tail
   (types ctx (tail expr_list) type)]

  [(types ctx expr_key type:string)
   (types ctx expr_mapping (⇒ type))
   -------------------------------------------- get
   (types ctx (get expr_mapping expr_key) type)]

  [(types ctx expr_key type:string)
   (types ctx expr_value type)
   (types ctx expr_mapping (⇒ type))
   ----------------------------------------------------------- put
   (types ctx (put expr_mapping expr_key expr_value) (⇒ type))])


(define-metafunction val-AttributeLType
  equality : type ... -> boolean
  [(equality type_1 type_1 type_2 ...) (equality type_1 type_2 ...)]
  [(equality type_1 type_2 type_3 ...) #f]
  [(equality type) #t]
  [(equality) #f])


(define-metafunction val-AttributeLType
  get-first-type : type ... -> type
  [(get-first-type type_1 type_2 ...) type_1])