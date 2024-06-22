#lang racket
(require redex
         "attributeL-syntax.rkt")
(provide (all-defined-out))

(define-judgment-form AttributeLType
  #:mode (types I I O)
  #:contract (types Γ expr type)

  [------------------------------ boolean
   (types Γ boolean type:boolean)]

  [------------------------------ integer
   (types Γ integer type:integer)]

  [---------------------------- string
   (types Γ string type:string)]

  [----------------------------------------------------------- attribute
   (types ((x_1 type_1) ... (x type) (x_2 type_2) ...) x type)]

  [(types Γ expr_1 type:integer)
   (types Γ expr_2 type:integer)
   ---------------------------------------- addition
   (types Γ (+ expr_1 expr_2) type:integer)]

  [(types Γ expr_1 type:integer)
   (types Γ expr_2 type:integer)
   ---------------------------------------- multiplication
   (types Γ (* expr_1 expr_2) type:integer)]

  [(types Γ expr_1 type:integer)
   (types Γ expr_2 type:integer)
   ---------------------------------------- subtraction
   (types Γ (- expr_1 expr_2) type:integer)]

  [(types Γ expr_1 type:integer)
   (types Γ expr_2 type:integer)
   ---------------------------------------- division
   (types Γ (÷ expr_1 expr_2) type:integer)]

  [(types Γ expr_1 type:boolean)
   (types Γ expr_2 type:boolean)
   ----------------------------------------- and
   (types Γ (&& expr_1 expr_2) type:boolean)]

  [(types Γ expr_1 type:boolean)
   (types Γ expr_2 type:boolean)
   ----------------------------------------- or
   (types Γ (|| expr_1 expr_2) type:boolean)]

  [(types Γ expr type:boolean)
   ------------------------------- not
   (types Γ (¬ expr) type:boolean)]

  [(types Γ expr_1 type:integer)
   (types Γ expr_2 type:integer)
   ----------------------------------------- equality
   (types Γ (== expr_1 expr_2) type:boolean)]

  [(types Γ expr_1 type:integer)
   (types Γ expr_2 type:integer)
   ---------------------------------------- bigger-then
   (types Γ (> expr_1 expr_2) type:boolean)]

  [(types Γ expr_head type)
   ------------------------------------ unitary-list
   (types Γ (: expr_head nil) (: type))]

  [(types Γ expr_head type)
   (types Γ expr_tail (: type))
   ------------------------------------------ multiple-list
   (types Γ (: expr_head expr_tail) (: type))]

  [(types Γ expr_key type:string) ...
   (types Γ expr_value type) ...
   (side-condition (equality type ...))
   ------------------------------------------------------------------------ mapping
   (types Γ (⇒ ((expr_key expr_value) ...)) (⇒ (get-first-type type ...)))]

  [(types Γ expr_list (: type))
   ------------------------------- head
   (types Γ (head expr_list) type)]

  [(types Γ expr_list type)
   ------------------------------- tail
   (types Γ (tail expr_list) type)]

  [(types Γ expr_key type:string)
   (types Γ expr_mapping (⇒ type))
   ------------------------------------------ get
   (types Γ (get expr_mapping expr_key) type)]

  [(types Γ expr_key type:string)
   (types Γ expr_value type)
   (types Γ expr_mapping (⇒ type))
   ---------------------------------------------------------- put
   (types Γ (put expr_mapping expr_key expr_value) (⇒ type))])


(define-metafunction AttributeLType
  equality : type ... -> boolean
  [(equality type_1 type_1 type_2 ...) (equality type_1 type_2 ...)]
  [(equality type_1 type_2 type_3 ...) #f]
  [(equality type) #t]
  [(equality) #f])


(define-metafunction AttributeLType
  get-first-type : type ... -> type
  [(get-first-type type_1 type_2 ...) type_1])