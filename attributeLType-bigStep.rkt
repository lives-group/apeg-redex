#lang racket


(require redex
         "attributeL-syntax.rkt")


(provide (all-defined-out))


(define-judgment-form AttributeLType
  #:mode (types I I O)
  #:contract (types Γ e τ)

  [---------------- "Boolean"
   (types Γ b Bool)]

  [------------------- "Integer"
   (types Γ i Integer)]

  [------------------ "String"
   (types Γ s String)]

  [------------------------------- "Attribute"
   (types (_ ... (ϑ τ) _ ...) ϑ τ)]

  [(types Γ e1 Integer)
   (types Γ e2 Integer)
   --------------------------- "Addition"
   (types Γ (+ e1 e2) Integer)]

  [(types Γ e1 Integer)
   (types Γ e2 Integer)
   --------------------------- "Multiplication"
   (types Γ (× e1 e2) Integer)]

  [(types Γ e1 Integer)
   (types Γ e2 Integer)
   --------------------------- "Subtraction"
   (types Γ (- e1 e2) Integer)]

  [(types Γ e1 Integer)
   (types Γ e2 Integer)
   --------------------------- "Division"
   (types Γ (÷ e1 e2) Integer)]

  [(types Γ e1 Bool)
   (types Γ e2 Bool)
   ------------------------ "And"
   (types Γ (∧ e1 e2) Bool)]

  [(types Γ e1 Bool)
   (types Γ e2 Bool)
   ------------------------ "Or"
   (types Γ (∨ e1 e2) Bool)]

  [(types Γ e Bool)
   -------------------- "Not"
   (types Γ (¬ e) Bool)]

  [(types Γ e1 Bool)
   (types Γ e2 Bool)
   ------------------------- "Equality Boolean"
   (types Γ (== e1 e2) Bool)]

  [(types Γ e1 Integer)
   (types Γ e2 Integer)
   ------------------------- "Equality Integer"
   (types Γ (== e1 e2) Bool)]

  [(types Γ e1 String)
   (types Γ e2 String)
   ------------------------- "Equality String"
   (types Γ (== e1 e2) Bool)]

  [(types Γ e1 Integer)
   (types Γ e2 Integer)
   ------------------------ "Bigger Than"
   (types Γ (> e1 e2) Bool)]

  [(types Γ e_head τ)
   ------------------------------ "Unitary List"
   (types Γ (: e_head nil) (: τ))]

  [(types Γ e_head τ)
   (types Γ e_tail (: τ))
   --------------------------------- "List"
   (types Γ (: e_head e_tail) (: τ))]

  [(types Γ e_key String) ...
   (types Γ e_value τ) ...
   (side-condition (valid-map? τ ...))
   (where (τ_map _ ...) (τ ...))
   -------------------------------------------- "Map"
   (types Γ (⇒ (e_key e_value) ...) (⇒ τ_map))]

  [(types Γ e_list (: τ))
   ------------------------- "Head"
   (types Γ (head e_list) τ)]

  [(types Γ e_list (: τ))
   ----------------------------- "Tail"
   (types Γ (tail e_list) (: τ))]

  [(types Γ e_key String)
   (types Γ e_map (⇒ τ))
   ----------------------------- "Get"
   (types Γ (get e_map e_key) τ)]

  [(types Γ e_key String)
   (types Γ e_value τ)
   (types Γ e_map (⇒ τ))
   ----------------------------------------- "Put"
   (types Γ (put e_map e_key e_value) (⇒ τ))])


(define-metafunction AttributeLType
  valid-map? : τ ... -> b
  [(valid-map? τ1 τ1 τ2 ...) (valid-map? τ1 τ2 ...)]
  [(valid-map? _ _ _ ...) #f]
  [(valid-map? _) #t]
  [(valid-map?) #f])