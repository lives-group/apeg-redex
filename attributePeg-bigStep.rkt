#lang racket
(require redex)
(require "attributePeg-syntax.rkt")
(require "attributeL-bigStep.rkt")
(provide (all-defined-out))

(define-judgment-form val-AttributePeg
  #:mode (parse I I I I O O)
  #:contract (parse ctx G p s r ctx)

  ;Terminal
  [----------------------------------------------------------------- terminal-success
   (parse ctx G natural_1 (natural_1 natural ...) (natural ...) ctx)]

  [(side-condition (dismatch? natural_1 natural_2))
   ----------------------------------------------------- terminal-dismatch-fail
   (parse ctx G natural_1 (natural_2 natural ...) ⊥ ctx)]

  [-------------------------------- terminal-empty-fail
   (parse ctx G natural_1 () ⊥ ctx)]

  ;Choice 
  [(parse ctx G p_1 s s_1 ctx_1)
   ------------------------------------- choice-first-success
   (parse ctx G (/ p_1 p_2) s s_1 ctx_1)]

  [(parse ctx G p_1 s ⊥ ctx_1)
   (parse ctx G p_2 s s_1 ctx_2)
   ------------------------------------- choice-second-success
   (parse ctx G (/ p_1 p_2) s s_1 ctx_2)]

  [(parse ctx G p_1 s ⊥ ctx_1)
   (parse ctx G p_2 s ⊥ ctx_2)
   --------------------------------- choice-fail
   (parse ctx G (/ p_1 p_2) s ⊥ ctx)]

  ;Sequence
  [(parse ctx G p_1 s s_1 ctx_1)
   (parse ctx_1 G p_2 s_1 r ctx_2)
   ----------------------------------- sequence-second
   (parse ctx G (• p_1 p_2) s r ctx_2)]

  [(parse ctx G p_1 s ⊥ ctx)
   --------------------------------- sequence-fail
   (parse ctx G (• p_1 p_2) s ⊥ ctx)]

  ;Not
  [(parse ctx G p s s_1 ctx_1)
   ----------------------------- not-success
   (parse ctx G (! p) s ⊥ ctx_1)]

  [(parse ctx G p s ⊥ ctx_1)
   ----------------------------- not-fail
   (parse ctx G (! p) s s ctx_1)]

  ;Repetition
  [(parse ctx G p s ⊥ ctx_1)
   --------------------------- repetition-end
   (parse ctx G (* p) s s ctx)]

  [(parse ctx G p s s_1 ctx_1)
   (parse ctx_1 G (* p) s_1 r ctx_2)
   ----------------------------- repetition-next
   (parse ctx G (* p) s r ctx_2)]

  ;Empty
  [----------------------- empty
   (parse ctx G ε s s ctx)]

  ;Constraint
  [(eval ctx expr #t)
   ------------------------------ constraint-success ;added
   (parse ctx G (? expr) s s ctx)]
  
  [(eval ctx expr #f)
   ------------------------------ constraint-fail ;added
   (parse ctx G (? expr) s ⊥ ctx)]
  
  ;Bind

  [(parse ctx G p (number_consumed ... number ...) (number ...) ctx_any)
   (parse (bind-update ctx x ,(string-join (map ~a (term (number_consumed ...))))) G ((= x_rest p_rest) ...) (number_consumed ... number ...) s_1 ctx_1)
   ---------------------------------------------------------------------------------------- bind-multiple-success ;added
   (parse ctx G ((= x p) (= x_rest p_rest) ...) (number_consumed ... number ...) s_1 ctx_1)]

  [(parse ctx G p s ⊥ ctx_any)
   (parse (bind-remove ctx x) G ((= x_rest p_rest) ...) s s_1 ctx_1)
   --------------------------------------------------------- bind-multiple-fail ;added
   (parse ctx G ((= x p) (= x_rest p_rest) ...) s s_1 ctx_1)]

  ;Non-terminal
  [(parse (make-ctx (x_2 ...) (evalList ctx (expr ...))) (NT_1 ... (x_1 (x_2 ...) (expr_1 ...) p_1) NT_2 ...) p_1 s s_1 ctx_1)
   (parse ctx (NT_1 ... (x_1 (x_2 ...) (expr_1 ...) p_1) NT_2 ...) (make-ctx-update (x_3 ...) (evalList ctx_1 (expr_1 ...))) s_1 s_1 ctx_2)
   ------------------------------------------------------------------------------------------------------- non-terminal-success
   (parse ctx (NT_1 ... (x_1 (x_2 ...) (expr_1 ...) p_1) NT_2 ...) (x_1 (expr ...) (x_3 ...)) s s_1 ctx_2)]
  
  [(parse (make-ctx (x_2 ...) (evalList ctx (expr ...))) (NT_1 ... (x_1 (x_2 ...) (expr_1 ...) p_1) NT_2 ...) p_1 s ⊥ ctx_1)
   --------------------------------------------------------------------------------------------------- non-terminal-fail
   (parse ctx (NT_1 ... (x_1 (x_2 ...) (expr_1 ...) p_1) NT_2 ...) (x_1 (expr ...) (x_3 ...)) s ⊥ ctx)]

  ;Update
  [(eval ((x_1 value_1)... (x value_3) (x_2 value_2)...) expr value)
   (parse ((x_1 value_1)... (x value) (x_2 value_2)...) G ((← x_3 expr_1)...) s s_1 ctx)
   -------------------------------------------------------------------------------------------------- update-multiple
   (parse ((x_1 value_1)... (x value_3) (x_2 value_2)...) G ((← x expr) (← x_3 expr_1)...) s s_1 ctx)]

  [------------------------ update-empty
   (parse ctx G () s s ctx)]


  [(side-condition (insert? x_3 ((x_1 value_1)...)))
   (eval ((x_1 value_1)...) expr value)
   (parse ((x_1 value_1)... (x_3 value)) G ((← x_2 expr_1)...) s s_1 ctx_1)
   -------------------------------------------------------------------------- insert
   (parse ((x_1 value_1)...) G ((← x_3 expr) (← x_2 expr_1)...) s s_1 ctx_1)])

(define-metafunction val-AttributePeg
  set-entry : s r -> s
  [(set-entry s ⊥) s]
  [(set-entry s r) r])

(define-metafunction val-AttributePeg
  evalList : ctx (expr ...) -> (value ...)
  [(evalList ctx ()) ()]
  [(evalList ctx (expr_1 expr ...))
   ,(cons (car (judgment-holds (eval ctx expr_1 value) value)) (term (evalList ctx (expr ...))))])

(define-metafunction val-AttributePeg
  make-ctx-update : (x ...) (value ...) -> ((← x value) ...)
  [(make-ctx-update () ()) ()]
  [(make-ctx-update (x_1 x ...) (value_1 value ...)) ,(cons (term (← x_1 value_1)) (term (make-ctx-update (x ...) (value ...))))])

(define-metafunction val-AttributePeg
  make-ctx : (x ...) (value ...) -> ((x value) ...)
  [(make-ctx () ()) ()]
  [(make-ctx (x_1 x ...) (value_1 value ...)) ,(cons (term (x_1 value_1)) (term (make-ctx (x ...) (value ...))))])

(define-metafunction val-AttributePeg
  [(dismatch? natural_1 natural_1) #f]
  [(dismatch? natural_1 natural_2) #t])

(define-metafunction val-AttributePeg
  [(diff? x_1 x_2) #f]
  [(diff? x_1 x_2) #t])

(define-metafunction val-AttributePeg
  insert? : x ((x value) ...) -> boolean
  [(insert? x ()) #t]
  [(insert? x ((x value) (x_1 value_1)...) ) #f]
  [(insert? x ((x_1 value) (x_2 value_1)...) ) (insert? x ((x_2 value_1) ...)) ])

(define-metafunction val-AttributePeg ;added
  bind-update : ctx x value -> ctx
  [(bind-update ((x_1 value_1) ... (x value_any) (x_2 value_2) ...) x value) ((x_1 value_1) ... (x value) (x_2 value_2) ...)]
  [(bind-update ((x_1 value_1) ...) x value) ((x_1 value_1) ... (x value))])

(define-metafunction val-AttributePeg ;added
  bind-remove : ctx x -> ctx
  [(bind-remove ((x_1 value_1) ... (x value_any) (x_2 value_2) ...) x) ((x_1 value_1) ... (x_2 value_2) ...)]
  [(bind-remove ctx x) ctx])
