#lang racket
(require redex         
         "attributeLType-bigStep.rkt"
         "attributePeg-syntax.rkt")
(provide (all-defined-out))


(define-judgment-form AttributePeg
  #:mode (typesPeg I I I O)
  #:contract (typesPeg Γ G p Γ)

  ;Terminal
  [------------------------ terminal
   (typesPeg Γ G natural Γ)]

  ;Empty
  [------------------ empty
   (typesPeg Γ G ε Γ)]

  ;Choice
  [(typesPeg Γ G p_1 Γ_1)
   (typesPeg Γ G p_2 Γ_2)
   (side-condition (equal-contexts? Γ_1 Γ_2))
   ------------------------------ choice
   (typesPeg Γ G (/ p_1 p_2) Γ_1)]

  ;Sequence
  [(typesPeg Γ G p_1 Γ_1)
   (typesPeg Γ_1 G p_2 Γ_2)
   ------------------------------ sequence
   (typesPeg Γ G (• p_1 p_2) Γ_2)]

  ;Negation
  [(typesPeg Γ G p Γ)
   (side-condition (it-not-is-negation p))
   ---------------------- negation_1
   (typesPeg Γ G (! p) Γ)]

  [(typesPeg Γ_1 G p Γ_2)
   ----------------------------- negation_2
   (typesPeg Γ_1 G (!(! p)) Γ_2)]

  ;Repetition
  [(typesPeg Γ G p Γ)
   ---------------------- repetition
   (typesPeg Γ G (* p) Γ)]

  ;Constraint
  [(types Γ expr type:boolean)
   ------------------------- constraint
   (typesPeg Γ G (? expr) Γ)]

  ;Bind
  [(side-condition (does-not-contain Γ x))
   (typesPeg Γ G p ((x_1 type_1) ... (x type:string) (x_2 type_2) ...))
   -------------------------------------------------------------------------- bind_declare_1
   (typesPeg Γ G (= x p) ((x_1 type_1) ... (x type:string) (x_2 type_2) ...))]
  
  [(side-condition (does-not-contain Γ x))
   (typesPeg Γ G p ((x_1 type_1) ...))
   (side-condition (does-not-contain ((x_1 type_1) ...) x))
   --------------------------------------------------------- bind_declare_2
   (typesPeg Γ G (= x p) ((x_1 type_1) ... (x type:string)))]
  
  [(typesPeg ((x_1 type_1) ... (x type:string) (x_2 type_2) ...) G p Γ_1)
   ---------------------------------------------------------------------------- bind_update
   (typesPeg ((x_1 type_1) ... (x type:string) (x_2 type_2) ...) G (= x p) Γ_1)]

  ;Non-terminal
  [(types ((x_1 type_1) ... (x_2 type_2) ...) expr_input type_input) ...
   (where Γ_s (create-S (x_output ...) (type_output ...) ((x_1 type_1) ... (x_2 type_2) ...) ()))
   (side-condition (types-of-∀x/x∋S (x_output ...) (type_output ...) Γ_s ((x_1 type_1) ... (x_2 type_2) ...)))
   ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- non-terminal
   (typesPeg ((x_1 type_1) ... (x (→ (type_input ...) (type_output ...))) (x_2 type_2) ...) (NT_1 ... (x ((type_input x_input) ...) (expr_output ...) p) NT_2 ...) (x (expr_input ...) (x_output ...)) (∪ ((x_1 type_1) ... (x (→ (type_input ...) (type_output ...))) (x_2 type_2) ...) Γ_s))]

  ;Update
  [(side-condition (does-not-contain ((x_Γ type_Γ) ...) x))
   (types ((x_Γ type_Γ) ...) expr type)
   (typesPeg ((x_Γ type_Γ) ... (x type)) G ((← x_rest expr_rest) ...) Γ_result)
   ------------------------------------------------------------------------------ update-declare
   (typesPeg ((x_Γ type_Γ) ...) G ((← x expr) (← x_rest expr_rest) ...) Γ_result)]

  [(types ((x_1 type_1) ... (x type) (x_3 type_3) ...) expr type)
   (typesPeg ((x_1 type_1) ... (x type) (x_3 type_3) ...) G ((← x_rest expr_rest) ...) Γ)
   ------------------------------------------------------------------------------------------------- update
   (typesPeg ((x_1 type_1) ... (x type) (x_3 type_3) ...) G ((← x expr) (← x_rest expr_rest) ...) Γ)]

  [------------------- update-empty
   (typesPeg Γ G () Γ)])


(define-metafunction AttributePeg
  ∩ : Γ Γ Γ -> Γ
  [(∩ ((x type) (x_1 type_1) ...) ((x_2 type_2) ... (x type) (x_3 type_3) ...) ((x_4 type_4) ...))
   (∩ ((x_1 type_1) ...) ((x_2 type_2) ... (x_3 type_3) ...) ((x_4 type_4) ... (x type)))]
  
  [(∩ ((x type) (x_1 type_1) ...) Γ_1 Γ_2) (∩ ((x_1 type_1) ...) Γ_1 Γ_2)]
  
  [(∩ () Γ_1 Γ_2) Γ_2])


(define-metafunction AttributePeg
  ∪ : Γ Γ -> Γ
  [(∪ ((x_1 type_1) ...) ((x_2 type_2) ...)) ((x_1 type_1) ... (x_2 type_2) ...)])


(define-metafunction AttributePeg
  create-S : (x ...) (type ...) Γ Γ -> Γ
  [(create-S (x x_1 ...) (type type_1 ...) ((x_2 type_2) ... (x type_anything) (x_3 type_3) ...) ((x_4 type_4) ...))
   (create-S (x_1 ...) (type_1 ...) ((x_2 type_2) ... (x type_anything) (x_3 type_3) ...) ((x_4 type_4) ...))]

  [(create-S (x x_1 ...) (type type_1 ...) ((x_2 type_2) ...) ((x_3 type_3) ...))
   (create-S (x_1 ...) (type_1 ...) ((x_2 type_2) ...) ((x_3 type_3) ... (x type)))]

  [(create-S () () Γ Γ_result) Γ_result])


(define-metafunction AttributePeg
  does-not-contain : Γ x -> boolean
  [(does-not-contain ((x_1 type_1) ... (x type) (x_2 type_2) ...) x) #f]
  [(does-not-contain Γ x) #t])


(define-metafunction AttributePeg
  same-types? : type type -> bool
  [(same-types? type type) #t]
  [(same-types? type_1 type_2) #f])


(define-metafunction AttributePeg
  types-of-∀x/x∋S : (x ...) (type ...) Γ Γ -> boolean
  [(types-of-∀x/x∋S (x x_1 ...) (type type_1 ...) ((x_2 type_2) ... (x type) (x_3 type_3) ...) Γ)
   (types-of-∀x/x∋S (x_1 ...) (type_1 ...) ((x_2 type_2) ... (x type) (x_3 type_3) ...) Γ)]

  [(types-of-∀x/x∋S (x x_1 ...) (type type_1 ...) Γ ((x_2 type_2) ... (x type_anything) (x_3 type_3) ...))
   (if (equal-type type type_anything)
       (types-of-∀x/x∋S (x_1 ...) (type_1 ...) Γ ((x_2 type_2) ... (x type_anything) (x_3 type_3) ...))
       #f)]

  [(types-of-∀x/x∋S () () Γ_1 Γ_2) #t])


(define-metafunction AttributePeg
  equal-contexts? : Γ Γ -> boolean
  [(equal-contexts? ((x_1 type_1) (x_2 type_2) ...) ((x_3 type_3) ... (x_1 type_1) (x_4 type_4) ...)) (equal-contexts? ((x_2 type_2) ...) ((x_3 type_3) ... (x_4 type_4) ...))]
  [(equal-contexts? () ()) #t]
  [(equal-contexts? Γ_1 Γ_2) #f])


(define-metafunction AttributePeg
  it-not-is-negation : p -> boolean
  [(it-not-is-negation (! p)) #f]
  [(it-not-is-negation p) #t])