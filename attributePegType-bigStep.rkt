#lang racket
(require redex         
         "attributeLType-bigStep.rkt"
         "attributePeg-syntax.rkt")
(provide (all-defined-out))


(define-judgment-form AttributePeg
  #:mode (typesPeg I I I O)
  #:contract (typesPeg ctx G p ctx)

  ;Terminal
  [---------------------- terminal
   (typesPeg ctx G natural ctx)]

  ;Empty
  [---------------------- empty
   (typesPeg ctx G ε ctx)]

  ;Choice
  [(typesPeg ctx G p_1 ctx_1)
   (typesPeg ctx G p_2 ctx_2)
   (side-condition (equal-contexts? ctx_1 ctx_2))
   ---------------------------------- choice
   (typesPeg ctx G (/ p_1 p_2) ctx_1)]

  ;Sequence
  [(typesPeg ctx G p_1 ctx_1)
   (typesPeg ctx_1 G p_2 ctx_2)
   ---------------------------------- sequence
   (typesPeg ctx G (• p_1 p_2) ctx_2)]

  ;Negation
  [(typesPeg ctx G p ctx)
   (side-condition (it-not-is-negation p))
   -------------------------- negation_1
   (typesPeg ctx G (! p) ctx)]

  [(typesPeg ctx_1 G p ctx_2)
   --------------------------------- negation_2
   (typesPeg ctx_1 G (!(! p)) ctx_2)]

  ;Repetition
  [(typesPeg ctx G p ctx)
   -------------------------- repetition
   (typesPeg ctx G (* p) ctx)]

  ;Constraint
  [(types ctx expr type:boolean)
   ----------------------------- constraint
   (typesPeg ctx G (? expr) ctx)]

  ;Bind
  [(side-condition (does-not-contain ctx x))
   (typesPeg ctx G p ((x_1 type_1) ... (x type:string) (x_2 type_2) ...))
   ---------------------------------------------------------------------------- bind_declare_1
   (typesPeg ctx G (= x p) ((x_1 type_1) ... (x type:string) (x_2 type_2) ...))]
  
  [(side-condition (does-not-contain ctx x))
   (typesPeg ctx G p ((x_1 type_1) ...))
   (side-condition (does-not-contain ((x_1 type_1) ...) x))
   ----------------------------------------------------------- bind_declare_2
   (typesPeg ctx G (= x p) ((x_1 type_1) ... (x type:string)))]
  
  [(typesPeg ((x_1 type_1) ... (x type:string) (x_2 type_2) ...) G p ctx_1)
   ------------------------------------------------------------------------------ bind_update
   (typesPeg ((x_1 type_1) ... (x type:string) (x_2 type_2) ...) G (= x p) ctx_1)]

  ;Non-terminal
  [(types ((x_1 type_1) ... (x_2 type_2) ...) expr_input type_input) ...
   (where ctx_s (create-S (x_output ...) (type_output ...) ((x_1 type_1) ... (x_2 type_2) ...) ()))
   (side-condition (types-of-∀x/x∋S (x_output ...) (type_output ...) ctx_s ((x_1 type_1) ... (x_2 type_2) ...)))
   --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- non-terminal
   (typesPeg ((x_1 type_1) ... (x (→ (type_input ...) (type_output ...))) (x_2 type_2) ...) (NT_1 ... (x ((type_input x_input) ...) (expr_output ...) p) NT_2 ...) (x (expr_input ...) (x_output ...)) (∪ ((x_1 type_1) ... (x (→ (type_input ...) (type_output ...))) (x_2 type_2) ...) ctx_s))]

  ;Update
  [(side-condition (does-not-contain ((x_ctx type_ctx) ...) x))
   (types ((x_ctx type_ctx) ...) expr type)
   (typesPeg ((x_ctx type_ctx) ... (x type)) G ((← x_rest expr_rest) ...) ctx_result)
   ------------------------------------------------------------------------------------ update-declare
   (typesPeg ((x_ctx type_ctx) ...) G ((← x expr) (← x_rest expr_rest) ...) ctx_result)]

  [(types ((x_1 type_1) ... (x type) (x_3 type_3) ...) expr type)
   (typesPeg ((x_1 type_1) ... (x type) (x_3 type_3) ...) G ((← x_rest expr_rest) ...) ctx)
   --------------------------------------------------------------------------------------------------- update
   (typesPeg ((x_1 type_1) ... (x type) (x_3 type_3) ...) G ((← x expr) (← x_rest expr_rest) ...) ctx)]

  [----------------------- update-empty
   (typesPeg ctx G () ctx)])


(define-metafunction val-AttributePeg
  ∩ : ctx ctx ctx -> ctx
  [(∩ ((x type) (x_1 type_1) ...) ((x_2 type_2) ... (x type) (x_3 type_3) ...) ((x_4 type_4) ...))
   (∩ ((x_1 type_1) ...) ((x_2 type_2) ... (x_3 type_3) ...) ((x_4 type_4) ... (x type)))]
  
  [(∩ ((x type) (x_1 type_1) ...) ctx_1 ctx_2) (∩ ((x_1 type_1) ...) ctx_1 ctx_2)]
  
  [(∩ () ctx_1 ctx_2) ctx_2])


(define-metafunction val-AttributePeg
  ∪ : ctx ctx -> ctx
  [(∪ ((x_1 type_1) ...) ((x_2 type_2) ...)) ((x_1 type_1) ... (x_2 type_2) ...)])


(define-metafunction val-AttributePeg
  create-S : (x ...) (type ...) ctx ctx -> ctx
  [(create-S (x x_1 ...) (type type_1 ...) ((x_2 type_2) ... (x type_anything) (x_3 type_3) ...) ((x_4 type_4) ...))
   (create-S (x_1 ...) (type_1 ...) ((x_2 type_2) ... (x type_anything) (x_3 type_3) ...) ((x_4 type_4) ...))]

  [(create-S (x x_1 ...) (type type_1 ...) ((x_2 type_2) ...) ((x_3 type_3) ...))
   (create-S (x_1 ...) (type_1 ...) ((x_2 type_2) ...) ((x_3 type_3) ... (x type)))]

  [(create-S () () ctx ctx_result) ctx_result])


(define-metafunction val-AttributePeg
  does-not-contain : ctx x -> boolean
  [(does-not-contain ((x_1 type_1) ... (x type) (x_2 type_2) ...) x) #f]
  [(does-not-contain ctx x) #t])


(define-metafunction val-AttributePeg
  same-types? : type type -> bool
  [(same-types? type type) #t]
  [(same-types? type_1 type_2) #f])


(define-metafunction val-AttributePeg
  types-of-∀x/x∋S : (x ...) (type ...) ctx ctx -> boolean
  [(types-of-∀x/x∋S (x x_1 ...) (type type_1 ...) ((x_2 type_2) ... (x type) (x_3 type_3) ...) ctx)
   (types-of-∀x/x∋S (x_1 ...) (type_1 ...) ((x_2 type_2) ... (x type) (x_3 type_3) ...) ctx)]

  [(types-of-∀x/x∋S (x x_1 ...) (type type_1 ...) ctx ((x_2 type_2) ... (x type_anything) (x_3 type_3) ...))
   (if (equal-type type type_anything)
       (types-of-∀x/x∋S (x_1 ...) (type_1 ...) ctx ((x_2 type_2) ... (x type_anything) (x_3 type_3) ...))
       #f)]

  [(types-of-∀x/x∋S () () ctx_1 ctx_2) #t])


(define-metafunction val-AttributePeg
  equal-contexts? : ctx ctx -> boolean
  [(equal-contexts? ((x_1 type_1) (x_2 type_2) ...) ((x_3 type_3) ... (x_1 type_1) (x_4 type_4) ...)) (equal-contexts? ((x_2 type_2) ...) ((x_3 type_3) ... (x_4 type_4) ...))]
  [(equal-contexts? () ()) #t]
  [(equal-contexts? ctx_1 ctx_2) #f])


(define-metafunction val-AttributePeg
  it-not-is-negation : p -> boolean
  [(it-not-is-negation (! p)) #f]
  [(it-not-is-negation p) #t])