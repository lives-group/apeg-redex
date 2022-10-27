#lang racket
(require redex)
(require "attributePeg-syntax.rkt")
(require "attributeL-bigStep.rkt")
(provide (all-defined-out))

(define-judgment-form val-AttributePeg
  #:mode (parse I I I I O O)
  #:contract (parse ctx G p s r ctx)

  ;Terminal
  [-------------------------------- 
   (parse ctx G natural_1 (natural_1 natural ...) (natural ...) ctx)]

  [(side-condition (dismatch? natural_1 natural_2))
   --------------------------------
   (parse ctx G natural_1 (natural_2 natural ...) ⊥ ctx)]

  [--------------------------------
   (parse ctx G natural_1 () ⊥ ctx)]

  ;Choice 
  [(parse ctx G p_1 r (natural ...) ctx_1)
   --------------------------------
   (parse ctx G (/ p_1 p_2) r (natural ...) ctx_1)]

  [(parse ctx G p_1 r ⊥ ctx_1)
   (parse ctx G p_2 r r_1 ctx_2)
   -------------------------------
   (parse ctx G (/ p_1 p_2) r r_1 ctx_2)]

  ;Sequence
  [(parse ctx G p_1 r (natural ...) ctx_1)
   (parse ctx_1 G p_2 (natural ...) r_2 ctx_2)
   -------------------------------
   (parse ctx G (• p_1 p_2) r r_2 ctx_2)]

  [(parse ctx G p_1 r ⊥ ctx)
   ------------------------------
   (parse ctx G (• p_1 p_2) r ⊥ ctx)]

  ;Not
  [(parse ctx G p r (natural ...) ctx)
   -------------------------------
   (parse ctx G (! p) r ⊥ ctx)]

  [(parse ctx G p r ⊥ ctx)
   -------------------------------
   (parse ctx G (! p) r r ctx)]

  ;Repetition

  [(parse ctx G p r ⊥ ctx_1)
   -------------------------------
   (parse ctx G (* p) r r ctx)]

  [(parse ctx G p r (natural ...) ctx_1)
   (parse ctx_1 G (* p) (natural ...) r_2 ctx_2)
   -------------------------------
   (parse ctx G (* p) r r_2 ctx_2)]

  ;Empty
  [-------------------------------
   (parse ctx G ε r r ctx)]

  ;Non-Terminal
  ;; 1- lista (expr_1 ...) fazer o evalist com ctx_1 -> (value ...)
  ;; 2- Atualizar o (x_3 ...) com a (value ...) -> ((x_3 value) ...)
  ;; 3- Modificar o ctx com o zip do 2

  [;(eval (expr  ...) s (value ...))
   (parse (make-ctx (x_2 ...) (evalList ctx (expr ...))) () p_1 s s_1 ctx_1)
    (parse ctx_1 () (make-ctx-update (x_3 ...) (evalList ctx_1 (expr_1 ...))) s_1 s_2 ctx_2)
   ------------------------------------"Non-terminal"
   (parse ctx
          ((_ _ _ _)... (x_1 (x_2 ...) (expr_1 ...) p_1) (_ _ _ _)...)
          (x_1 (expr ...) (x_3 ...)) s s_2 ctx_2)]

  ;Update

  [(eval ((x_1 value_1)... (x value_3) (x_2 value_2)...) expr value)
   (parse ((x_1 value_1)... (x value) (x_2 value_2)...) G ((← x_3 expr_1)...) s s_1 ctx)
   ----------------------------------"Update"
   (parse ((x_1 value_1)... (x value_3) (x_2 value_2)...) G ((← x expr) (← x_3 expr_1)...) s s_1 ctx)]

  [----------------------------------"Update-one"
   (parse ctx G () s s ctx)]


  [(eval ((x_1 value_1)...) expr value)
   (parse ((x_3 value) (x_1 value_1)...) G ((← x_2 expr_1)...) s s_1 ctx_1)
   (side-condition (insert? x_3 ((x_1 value_1)...)))
   ----------------------------------"Insert-Rec"
   (parse
    ((x_1 value_1)...)
    G
    ((← x_3 expr) (← x_2 expr_1)...)
    s
    s_1
    ctx_1)])

(define-metafunction val-AttributePeg
  evalList : ctx (expr ...) -> (value ...)
  [(evalList ctx ()) ()]
  [(evalList ctx (expr_1 expr ...))
   ,(cons (car (judgment-holds (eval ctx expr_1 value) value)) (term (evalList ctx (expr ...))))])

(define-metafunction val-AttributePeg
  make-ctx-update : (x ...) (value ...) -> ((← x value) ...)
  [(make-ctx-update () ()) ()]
  [(make-ctx-update (x_1 x ...) (value_1 value ...))
   ,(cons (term (← x_1 value_1)) (term (make-ctx-update (x ...) (value ...))))])

(define-metafunction val-AttributePeg
  make-ctx : (x ...) (value ...) -> ((x value) ...)
  [(make-ctx () ()) ()]
  [(make-ctx (x_1 x ...) (value_1 value ...))
   ,(cons (term (x_1 value_1)) (term (make-ctx (x ...) (value ...))))])

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

;TERMINAL
;(judgment-holds (parse () () 1 (1 2) r ctx) r)
;(judgment-holds (parse () () 1 (2 2) r ctx) r)
;(judgment-holds (parse () () 1 () r ctx) r)
;; CHOICE
;(judgment-holds (parse () () (/ 1 2) (1 2) r ctx) r)
;(judgment-holds (parse () () (/ 1 2) (2 1) r ctx) r)
;(judgment-holds (parse () () (/ 1 2) (3 3) r ctx) r)
;(judgment-holds (parse () () (/ 1 2) () r ctx) r)
;; SEQUENCE
;(judgment-holds (parse () () (• 1 2) (1 2) r ctx) r)
;(judgment-holds (parse () () (• 1 2) (1 2 2) r ctx) r)
;(judgment-holds (parse () () (• 1 2) (2 2) r ctx) r)
;(judgment-holds (parse () () (• 1 2) () r ctx) r)
;; NOT
;(judgment-holds (parse () () (! 1) (1) r ctx) r)
;(judgment-holds (parse () () (! 1) (2) r ctx) r)
;(judgment-holds (parse () () (! 1) () r ctx) r)
;(judgment-holds (parse () () (! 1) (1 2) r ctx) r)
;; REPETITION
;(judgment-holds (parse () () (* 1) (1 1 1 1 2 3) r ctx) r)
;(judgment-holds (parse () () (* 1) () r ctx) r)
;(judgment-holds (parse () () (* ε) (1 2) r ctx) r) ;DA RUIM
; EMPTY
;(judgment-holds (parse () () ε (1 2) r ctx) r)
;NON-TERMINAL
;

;UPDATE
;(judgment-holds (parse ((x 1) (y 2)) () ((← x 3)) (1 1 1) s ctx) ctx)
;(judgment-holds (parse ((x 1) (y 2)) () ((← x (+ 1 2))) (1 1 1) s ctx) (s ctx))
;(judgment-holds (parse ((x 1) (y 2)) () ((← x (* 1 2))) (1 1 1) s ctx) (s ctx))
;(judgment-holds (parse ((x 1) (y 2)) () ((← x (÷ 1 2))) (1 1 1) s ctx) (s ctx)) 
;(judgment-holds (parse ((x 1) (y 2)) () ((← x (+ (+ 1 2) 2))) (1 1 1) s ctx) (s ctx))
;(judgment-holds (parse ((x 1) (y 2)) () ((← x (+ (+ 1 2) (- 1 6)))) (1 1 1) s ctx) (s ctx))
;(judgment-holds (parse ((x 1) (y 2)) () ((← x (+ (+ 1 2) (- 1 6))) (← y (+ (+ 1 2) (- 1 6)))) (1 1 1) s ctx) (s ctx)) ;; nao funciona
;(judgment-holds (parse ((x 1) (y 2)) () ((← y (+ 5 2))) (1 1 1) s ctx) (s ctx)) ;; nao funciona
;(judgment-holds (parse ((x 1) (y 2)) () ((← z (+ 2 2)) (← a (+ 2 2)) (← z (+ 2 3))) (1 1 1) s ctx) (s ctx)) ;; nao sei se eh pra ter esse comportamento

;;testar e estudar o artigo pra veer como vai fazer o terminal
;MIX
;(judgment-holds (parse () () (* (/ (• 1 2) 3)) (1 2 1 2) r ctx) r)
;(judgment-holds (parse () () (/ (• 1 2) (! 3)) (1 2 3) r ctx) r)
;(judgment-holds (parse () () (/ (• 1 2) (! 3)) (4) r ctx) r)

(judgment-holds
 (parse ()
        ((S (k) ((+ n 1)) (• ((← n k)) (* (• 1 ((← n (+ n 1))))))))
        (S (0) (m))
        (1 1 1)
        s ctx)
 (s ctx))



#;(judgment-holds
 (parse ()
        ((S (k) ((+ n 1)) (• ((← n k)) (* (• 1 ((← n (+ n 1))))))))
        (S (0) (m))
        (1 1 1)
        s ctx)
 (s ctx))

;(• ((← n k)) (* (• 1 ((← n (+ n 1)))) ) )
; evalList Metafunction tests
;(println "evalList tests")
;(term (evalList () (1 2)))
;(term (evalList () ((- 1 1) (+ 1 2))))
;(term (evalList () ((+ (+ 1 3) (* 1 5)) (* 1 2) (+ 1 2) (* 7 2))))

;(term (make-ctx (S T) (1 2)))
