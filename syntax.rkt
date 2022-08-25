#lang racket

(require redex)
(provide (all-defined-out))

; Syntax
(define-language AttributeL
  (expr l
        (⇒ ((expr expr) ...))
        (get expr expr)
        (put expr expr expr)
        (: expr expr) ;; do lista
        nil           ;; lista empty
        (head expr)
        (tail expr)
        (+  expr expr)
        (*  expr expr)
        (÷  expr expr)
        (-  expr expr)
        x)
  (l number
     string)
  (x variable-not-otherwise-mentioned))

(define-extended-language ctx-AttributeL AttributeL
  (VS (expr ctx))
  (ctx ((x value)...))
  (H (+  H expr)
     (+  value H)
     (*  H expr)
     (*  value H)
     (-  H expr)
     (-  value H)
     (÷  H expr)
     (÷  value H)
     (get H expr)
     (get value H)
     (put H expr expr)
     (put (⇒ ((string value)...)) H expr)
     (put (⇒ ((string value)...)) string H)
     (: H expr)
     (: value H)
     (head H)
     (tail H)
     (⇒ ((string value)... (H expr)   (expr expr)...))
     (⇒ ((string value)... (string H) (expr expr)...))
     hole)
  (value number
         string
         (⇒ ((string value) ...))
         (: value value)
         nil
         undef))

;;;;;;;;;;;;;;;;;;
;; Ainda nao chegamos aqui, ainda
;; CRIAR UMA SINTAXE DE PEG com nova construção (update)
; uma peg pode ser uma lista de updates
;AÇUCAR SINTATICO DO ATRIBUTED PEG
; atributos

(define-extended-language PegL ctx-AttributeL
  (APeg  (Update ... )
         any)
  (Update (← x expr)) ; n = n - 1
 
  (input (string natural (natural ...)))
  (r ok
     fail
     indef)
  (st (APeg ctx input r)))

(define-metafunction PegL
  len : string -> natural
  [(len  string) ,(= (string-length (term string) 1))])


(define-extended-language Peg PegL ;; Peg syntax
   (p
    (Update ...)
    (• expr expr)
    (/ expr expr)
    (* expr)
    (! expr)
    ε))
;;;;;;;;;;;
;;;