#lang racket


(require redex
         "attributePeg-syntax.rkt"
         "attributeL-bigStep.rkt")


(provide (all-defined-out))


(define-judgment-form vAttributePeg
  #:mode (parse I I I I O O)
  #:contract (parse Δ G p w o Δ)

  ;Terminal
  [----------------------------------- "Terminal"
   (parse Δ G t1 (t1 t ...) (t ...) Δ)]

  [--------------------------------- "¬Terminal 1"
   (parse Δ G t_!_ (t_!_ t ...) ⊥ Δ)]

  [-------------------- "¬Terminal 2"
   (parse Δ G t () ⊥ Δ)]

  ;Empty
  [------------------- "Empty"
   (parse Δ G ε w w Δ)]

  ;Choice 
  [(parse Δ G p1 w w1 Δ1)
   ----------------------------- "Choice 1"
   (parse Δ G (/ p1 p2) w w1 Δ1)]

  [(parse Δ G p1 w ⊥ Δ1)
   (parse Δ G p2 w w2 Δ2)
   ----------------------------- "Choice 2"
   (parse Δ G (/ p1 p2) w w2 Δ2)]

  [(parse Δ G p1 w ⊥ Δ1)
   (parse Δ G p2 w ⊥ Δ2)
   --------------------------- "¬Choice"
   (parse Δ G (/ p1 p2) w ⊥ Δ)]

  ;Sequence
  [(parse Δ G p1 w w1 Δ1)
   (parse Δ1 G p2 w1 w2 Δ2)
   ----------------------------- "Sequence"
   (parse Δ G (• p1 p2) w w2 Δ2)]

  [(parse Δ G p1 w ⊥ Δ1)
   --------------------------- "¬Sequence 1"
   (parse Δ G (• p1 p2) w ⊥ Δ)]

  [(parse Δ G p1 w w1 Δ1)
   (parse Δ1 G p2 w1 ⊥ Δ2)
   --------------------------- "¬Sequence 2"
   (parse Δ G (• p1 p2) w ⊥ Δ)]

  ;Negation
  [(parse Δ G p w ⊥ Δ1)
   ------------------------ "Negation"
   (parse Δ G (! p) w w Δ1)]

  [(parse Δ G p w w1 Δ1)
   ------------------------ "¬Negation"
   (parse Δ G (! p) w ⊥ Δ1)]

  ;Repetition
  [(parse Δ G p w w1 Δ1)
   (parse Δ1 G (* p) w1 o Δ2)
   ------------------------ "Repetition"
   (parse Δ G (* p) w o Δ2)]
  
  [(parse Δ G p w ⊥ Δ1)
   ----------------------- "¬Repetition"
   (parse Δ G (* p) w w Δ)]

  ;Constraint
  [(eval Δ e #t)
   ----------------------- "Constraint"
   (parse Δ G (? e) w w Δ)]
  
  [(eval Δ e #f)
   ----------------------- "¬Constraint"
   (parse Δ G (? e) w ⊥ Δ)]
  
  ;Bind
  [(parse Δ G p (t ... t1 ...) (t1 ...) Δ1)
   (where s ,(list->string (map integer->char (term (t ...)))))
   (where Δ2 (assign Δ1 ϑ s))
   ---------------------------------------------- "Bind"
   (parse Δ G (= ϑ p) (t ... t1 ...) (t1 ...) Δ2)]

  [(parse Δ G p w ⊥ Δ1)
   ------------------------- "¬Bind"
   (parse Δ G (= x p) w ⊥ Δ)]

  ;Nonterminal
  [(where (_ ... (NT ((_ ϑ1) ...) (e2 ...) p) _ ...) G)
   (eval Δ e1 v1) ...
   (parse () G ((← ϑ1 v1) ...) () () Δ1)
   (parse Δ1 G p w w1 Δ2)
   (eval Δ2 e2 v2) ...
   (parse Δ G ((← ϑ2 v2) ...) () () Δ_3)
   ------------------------------------------- "Nonterminal"
   (parse Δ G (NT (e1 ...) (ϑ2 ...)) w w1 Δ_3)]

  [(where (_ ... (NT ((_ ϑ1) ...) (e2 ...) p) _ ...) G)
   (eval Δ e1 v1) ...
   (parse () G ((← ϑ1 v1) ...) () () Δ1)
   (parse Δ1 G p w ⊥ Δ2)
   ---------------------------------------- "¬Nonterminal"
   (parse Δ G (NT (e1 ...) (ϑ2 ...)) w ⊥ Δ)]

  ;Update
  [(eval Δ e v)
   (where Δ1 (assign Δ ϑ v))
   (parse Δ1 G ((← ϑ1 e1) ...) w w Δ2)
   ------------------------------------------ "Update"
   (parse Δ G ((← ϑ e) (← ϑ1 e1) ...) w w Δ2)]

  [-------------------- "Empty Update"
   (parse Δ G () w w Δ)])