#lang racket


(require redex)


(provide (all-defined-out))


(define-language AttributeL
  [e e1 e2 e3 e4 ::= b
     i
     s
     (⇒ (e e) ...)
     (get e e)
     (put e e e)
     (: e e)
     nil
     (head e)
     (tail e)
     (+ e e)
     (× e e)
     (÷ e e)
     (- e e)
     (∧ e e)
     (∨ e e)
     (¬ e)
     (== e e)
     (> e e)
     ϑ]
  [b ::= boolean]
  [i i1 i2 ::= integer]
  [s s1 s2 ::= string]
  [ϑ ϑ1 ϑ2 ϑ3 ϑ4 ::= variable-not-otherwise-mentioned])


(define-extended-language AttributeLType AttributeL
  [τ τ1 τ2 τ3 τ4 ::= Bool
     Integer
     String
     (: τ)
     (⇒ τ)
     (→ (τ ...) (τ ...))]
  [γ ::= variable-not-otherwise-mentioned]
  [Γ Γ1 Γ2 ::= ((γ_!_ τ)...)])


(define-extended-language vAttributeL AttributeLType
  [v v1 v2 ::= b
     i
     s
     (⇒ (s_!_ v) ...)
     (: v v)
     nil]
  [Δ Δ1 Δ2 ::= ((ϑ_!_ v)...)])


(define-metafunction vAttributeL
  applay-put : (⇒ (s_!_ v) ...) s v -> (⇒ (s v) ...)
  [(applay-put (⇒ (s1 v1) ... (s _) (s2 v2) ...) s v)
   (⇒ (s1 v1) ... (s v) (s2 v2) ...)]
  [(applay-put (⇒ (s1 v1) ...) s v)
   (⇒ (s1 v1) ... (s v))])