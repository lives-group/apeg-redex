#lang racket


(require redex
         "attributeL-syntax.rkt")


(provide (all-defined-out))


(define-judgment-form vAttributeL
  #:mode (eval I I O)
  #:contract (eval Δ e v)

  [------------ "Boolean"
   (eval Δ b b)]
  
  [------------ "Integer"
   (eval Δ i i)]

  [------------ "String"
   (eval Δ s s)]

  [------------------------------ "Attribute"
   (eval (_ ... (ϑ v) _ ...) ϑ v)]

  [(eval Δ e1 i1)
   (eval Δ e2 i2)
   (where i ,[+ (term i1) (term i2)])
   -------------------- "Addition"
   (eval Δ [+ e1 e2] i)]

  [(eval Δ e1 i1)
   (eval Δ e2 i2)
   (where i ,[* (term i1) (term i2)])
   -------------------- "Multiplication"
   (eval Δ [× e1 e2] i)]

  [(eval Δ e1 i1)
   (eval Δ e2 i2)
   (where i ,[- (term i1) (term i2)])
   -------------------- "Subtraction"
   (eval Δ [- e1 e2] i)]

  [(eval Δ e1 i1)
   (eval Δ e2 i2)
   (where i ,[quotient (term i1) (term i2)])
   -------------------- "Division"
   (eval Δ [÷ e1 e2] i)]

  [(eval Δ e1 #t)
   (eval Δ e2 b)
   -------------------- "And 1"
   (eval Δ [∧ e1 e2] b)]
  
  [(eval Δ e1 #f)
   --------------------- "¬And 1"
   (eval Δ [∧ e1 e2] #f)]

  [(eval Δ e1 #t)
   --------------------- "Or 1"
   (eval Δ [∨ e1 e2] #t)]

  [(eval Δ e1 #f)
   (eval Δ e2 b)
   -------------------- "¬Or 1"
   (eval Δ [∨ e1 e2] b)]

  [(eval Δ e #f)
   ----------------- "Not"
   (eval Δ [¬ e] #t)]

  [(eval Δ e #t)
   ----------------- "¬Not"
   (eval Δ [¬ e] #f)]

  [(eval Δ e1 i1)
   (eval Δ e2 i2)
   (where b ,[equal? (term i1) (term i2)])
   --------------------- "Equality"
   (eval Δ [== e1 e2] b)]

  [(eval Δ e1 v1)
   (eval Δ e2 v2)
   (where b ,[> (term v1) (term v2)])
   -------------------- "Bigger Than"
   (eval Δ [> e1 e2] b)]

  [---------------- "Empty List"
   (eval Δ nil nil)]

  [(eval Δ e1 v1)
   (eval Δ e2 v2)
   ---------------------------- "List"
   (eval Δ [: e1 e2] [: v1 v2])]

  [(eval Δ e (: v_head _))
   ------------------------ "Head"
   (eval Δ (head e) v_head)]

  [(eval Δ e (: _ v_tail))
   ------------------------ "Tail"
   (eval Δ (tail e) v_tail)]

  [----------------- "Empty Map"
   (eval Δ (⇒) (⇒))]

  [(eval Δ [put (⇒ [e_keys e_vs] ...) e_key e_v] v)
   -------------------------------------------- "Map"
   (eval Δ [⇒ (e_keys e_vs) ... (e_key e_v)] v)]

  [(eval Δ e_key s)
   (eval Δ e_map [⇒ _ ... (s v) _ ...])
   ---------------------------- "Get"
   (eval Δ [get e_map e_key] v)]

  [(eval Δ e_key s)
   (eval Δ e_value v)
   (eval Δ e_map (⇒ (s1 v1) ... ))
   (where v2 (applay-put (⇒ (s1 v1) ... ) s v))
   ------------------------------------- "Put"
   (eval Δ [put e_map e_key e_value] v2)])