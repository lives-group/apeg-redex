#lang racket


(require redex
         "attributeLType-bigStep.rkt"
         "attributePeg-syntax.rkt")


(provide (all-defined-out))


(define-judgment-form AttributePeg
  #:mode (typesPeg I I I O)
  #:contract (typesPeg Γ G p Γ)

  ;Terminal
  [------------------ "Terminal"
   (typesPeg Γ G t Γ)]

  ;Empty
  [------------------ "Empty"
   (typesPeg Γ G ε Γ)]

  ;Choice
  [(typesPeg Γ G p1 Γ1)
   (typesPeg Γ G p2 Γ2)
   (side-condition (same-contexts? Γ1 Γ2))
   --------------------------- "Choice"
   (typesPeg Γ G (/ p1 p2) Γ1)]

  ;Sequence
  [(typesPeg Γ G p1 Γ1)
   (typesPeg Γ1 G p2 Γ2)
   --------------------------- "Sequence"
   (typesPeg Γ G (• p1 p2) Γ2)]

  ;Negation
  [(typesPeg Γ G p Γ)
   (side-condition (it-not-is-negation p))
   ---------------------- "Negation 1"
   (typesPeg Γ G (! p) Γ)]

  [(typesPeg Γ1 G p Γ2)
   --------------------------- "Negation 2"
   (typesPeg Γ1 G (!(! p)) Γ2)]

  ;Repetition
  [(typesPeg Γ G p Γ)
   ---------------------- "Repetition"
   (typesPeg Γ G (* p) Γ)]

  ;Constraint
  [(types Γ e Bool)
   ---------------------- "Constraint"
   (typesPeg Γ G (? e) Γ)]

  ;Bind
  [(side-condition (ϑ∉Γ Γ ϑ))
   (typesPeg Γ G p Γ1)
   (where ((ϑ1 τ1) ... (ϑ String) (ϑ2 τ2) ...) Γ1)
   ------------------------- "Bind Declare 1"
   (typesPeg Γ G (= ϑ p) Γ1)]
  
  [(side-condition (ϑ∉Γ Γ ϑ))
   (typesPeg Γ G p ((ϑ1 τ1) ...))
   (side-condition (ϑ∉Γ ((ϑ1 τ1) ...) ϑ))
   (where Γ1 ((ϑ1 τ1) ... (ϑ String)))
   ------------------------- "Bind Declare 2"
   (typesPeg Γ G (= ϑ p) Γ1)]

  [(where (_ ... (ϑ String) _ ...) Γ)
   (typesPeg Γ G p Γ1)
   ------------------------- "Bind Update"
   (typesPeg Γ G (= ϑ p) Γ1)]

  ;Nonterminal
  [(where (_ ... (NT (→ (τ1 ...) (τ2 ...))) _ ...) ((ϑ τ) ...))
   (where (_ ... (NT ((τ1 ϑ1) ...) (e2 ...) p) _ ...) G)
   (types ((ϑ τ) ...) e1 τ1) ...
   (where Γ ((ϑ1 τ1) ...))
   (where (((ϑ3 τ3) ...) b) (create-S Γ () (ϑ2 ...) (τ2 ...)))
   (side-condition b)
   (where Γ1 ((ϑ τ) ... (ϑ3 τ3) ...))
   -------------------------------------------------- "Nonterminal"
   (typesPeg ((ϑ τ) ...) G (NT (e1 ...) (ϑ2 ...)) Γ1)]

  ;Update
  [(side-condition (ϑ∉Γ ((ϑ1 τ1) ...) ϑ))
   (types ((ϑ1 τ1) ...) e τ)
   (typesPeg ((ϑ1 τ1) ... (ϑ τ)) G ((← ϑ2 e2) ...) Γ1)
   ----------------------------------------------------- "Declare"
   (typesPeg ((ϑ1 τ1) ...) G ((← ϑ e) (← ϑ2 e2) ...) Γ1)]

  [(types Γ e τ)
   (where (_ ... (ϑ τ) _ ...) Γ)
   (typesPeg Γ G ((← ϑ1 e1) ...) Γ1)
   ----------------------------------------- "Update"
   (typesPeg Γ G ((← ϑ e) (← ϑ1 e1) ...) Γ1)]

  [------------------- "Empty Update"
   (typesPeg Γ G () Γ)])


(define-metafunction AttributePeg
  create-S : Γ Γ (ϑ ...) (τ ...) -> (Γ b)
  [(create-S ((ϑ1 τ1) ... (ϑ τ) (ϑ2 τ2) ...)
             ((ϑ3 τ3) ...)
             (ϑ ϑ4 ...)
             (τ τ4 ...))
   (create-S ((ϑ1 τ1) ... (ϑ τ) (ϑ2 τ2) ...)
             ((ϑ3 τ3) ...)
             (ϑ4 ...)
             (τ4 ...))]
  [(create-S (_ ... (ϑ _) _ ...) Γ (ϑ _ ...) _) (Γ #f)]
  [(create-S Γ
             ((ϑ1 τ1) ...)
             (ϑ ϑ2 ...)
             (τ τ2 ...))
   (create-S Γ
             ((ϑ1 τ1) ... (ϑ τ))
             (ϑ2 ...)
             (τ2 ...))]
  [(create-S _ Γ () ()) (Γ #t)])


(define-metafunction AttributePeg
  ϑ∉Γ : Γ ϑ -> b
  [(ϑ∉Γ (_ ... (ϑ _) _ ...) ϑ) #f]
  [(ϑ∉Γ Γ ϑ) #t])


(define-metafunction AttributePeg
  same-contexts? : Γ Γ -> b
  [(same-contexts? ((ϑ τ) (ϑ1 τ1) ...) ((ϑ2 τ2) ... (ϑ τ) (ϑ3 τ3) ...))
   (same-contexts? ((ϑ1 τ1) ...) ((ϑ2 τ2) ... (ϑ3 τ3) ...))]
  [(same-contexts? () ()) #t]
  [(same-contexts? _ _) #f])


(define-metafunction AttributePeg
  it-not-is-negation : p -> b
  [(it-not-is-negation (! _)) #f]
  [(it-not-is-negation _) #t])