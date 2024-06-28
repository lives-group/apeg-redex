#lang racket


(require redex
         "attributeL-syntax.rkt")


(provide (all-defined-out))


(define-extended-language AttributePeg vAttributeL
  [p p1 p2 p3 ::= (NT [e ...] [ϑ ...])
     ([← ϑ e] ...)
     (= ϑ p)
     (? e)
     (• p p)
     (/ p p)
     (* p)
     (! p)
     t
     ε]
  [t t1 t2 t3 n n1 n2 n3 m ::= natural]
  [NT ::= variable-not-otherwise-mentioned]
  [G ::= ((NT_!_ [(τ ϑ) ...] [e ...] p) ...)]
  [r r1 r2 ::= (NT [(τ ϑ) ...] [e ...] p)])


(define-extended-language vAttributePeg AttributePeg
  [w w1 w2 ::= (t ...)]
  [o ::= w
     ⊥])


(define-metafunction AttributePeg
  assign : Δ ϑ v -> Δ
  [(assign ((ϑ1 v1) ... (ϑ _) (ϑ2 v2) ...) ϑ v)
   ((ϑ1 v1) ... (ϑ v) (ϑ2 v2) ...)]
  [(assign ((ϑ1 v1) ...) ϑ v)
   ((ϑ1 v1) ... (ϑ v))])