#lang racket

(require redex)
(provide (all-defined-out))
(require "attributeL-smallStep.rkt")


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

(define-metafunction PegL
    notSingleton : expr -> boolean 
  [(notSingleton value)  #f]
  [(notSingleton expr)   #t])

(define apeg-red
  (reduction-relation
   PegL
   #:domain st
   (--> ( ( (← x value) Update ...) ctx input r) 
        ( (Update ...) (update_val x value ctx) input r)
        "update")
   (--> ( ( (← x expr) Update ...) ctx input r) 
        ( ( (← x value) Update ...) ctx input r)
        (where #t (notSingleton expr))
        (where ((value ctx_1) (value_2 ctx_2)...)
               ,(apply-reduction-relation* expr-red (term (expr ctx)) ))
        "eval-expr")))

