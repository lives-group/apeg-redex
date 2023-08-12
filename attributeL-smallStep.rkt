#lang racket
(require redex)
(provide (all-defined-out))
(require "attributeL-syntax.rkt")

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
     (&&  H expr)
     (&&  value H)
     (||  H expr)
     (||  value H)
     (! H)
     (==  H expr)
     (==  value H)
     (>  H expr)
     (>  value H)
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

(define expr-red
  (reduction-relation ctx-AttributeL
  #:domain VS
   (--> ((in-hole H (+ number_1 number_2)) ctx)
        ((in-hole H ,(+ (term number_1) (term number_2))) ctx)
        "addition")
   
   (--> ((in-hole H x) ((x_1 value_1)... (x value) (x_2 value_2)... ))
        ((in-hole H value) ((x_1 value_1)... (x value) (x_2 value_2)... ))
        "variable")
   
   (--> ((in-hole H (* number_1 number_2)) ctx)
        ((in-hole H ,(* (term number_1) (term number_2))) ctx)
        "multiplication")
   
   (--> ((in-hole H (- number_1 number_2)) ctx)
        ((in-hole H ,(- (term number_1) (term number_2))) ctx)
        "subtraction")
   
   (--> ((in-hole H (÷ number_1 number_2)) ctx)
        ((in-hole H ,(/ (term number_1) (term number_2))) ctx)
        "division")

   (--> ((in-hole H (&& boolean_1 boolean_2)) ctx)
        ((in-hole H ,(and (term boolean_1) (term boolean_2))) ctx)
        "and") ;added

   (--> ((in-hole H (|| boolean_1 boolean_2)) ctx)
        ((in-hole H ,(or (term boolean_1) (term boolean_2))) ctx)
        "or") ;added

   (--> ((in-hole H (! boolean)) ctx)
        ((in-hole H ,(not (term boolean))) ctx)
        "not") ;added

   (--> ((in-hole H (== number_1 number_2)) ctx)
        ((in-hole H ,(= (term number_1) (term number_2))) ctx)
        "equality") ;added

   (--> ((in-hole H (> number_1 number_2)) ctx)
        ((in-hole H ,(> (term number_1) (term number_2))) ctx)
        "bigger-then") ;added
   
   (--> ((in-hole H (get (⇒ ((string value)...)) string_key)) ctx)
        ((in-hole H (mapping-get (⇒ ((string value)...)) string_key)) ctx)
        "get")
   
   (--> ((in-hole H (put (⇒ ((string value)...)) string_key value_value)) ctx)
        ((in-hole H (mapping-put (⇒ ((string value)...)) string_key value_value)) ctx)
        "put")
   
   (--> ((in-hole H (head (: value_1 value_2))) ctx)
        ((in-hole H value_1) ctx)
        "head")
   
   (--> ((in-hole H (tail (: value_1 value_2))) ctx)
        ((in-hole H value_2) ctx)
        "tail")
   
   (--> ((in-hole H (⇒ ((string value)...))) ctx)
        ((in-hole H (mapping (⇒ ((string value)...)))) ctx)
        (side-condition (term (has-duplicates (⇒ ((string value)...)))))
        "mapping")))

(define-metafunction ctx-AttributeL
  mapping : (⇒ ((string value)...)) -> (⇒ ((string value)...))
  [(mapping (⇒ ((string value)... (string_key value_value)))) (mapping-put (mapping (⇒ ((string value)...))) string_key value_value)]
  [(mapping (⇒ ())) (⇒ ())])

(define-metafunction ctx-AttributeL
  mapping-put : (⇒ ((string value)...)) string value -> (⇒ ((string value)...))
  [(mapping-put (⇒ ((string_before value_before)... (string_key value) (string_after value_after)...)) string_key value_value)
   (⇒ ((string_before value_before)... (string_key value_value) (string_after value_after)...))]
  [(mapping-put (⇒ ((string value)...)) string_key value_value) (⇒ ((string value)... (string_key value_value)))])

(define-metafunction ctx-AttributeL
  mapping-get : (⇒ ((string value)...)) string -> value
  [(mapping-get (⇒ ((string_before value_before)... (string_key value) (string_after value_after)...)) string_key) value])

(define-metafunction ctx-AttributeL
  has-duplicates : (⇒ ((string value)...)) -> boolean
  [(has-duplicates (⇒ ((string_before value_before)... (string_key value) (string_after value_after)... (string_key value_value)))) #t]
  [(has-duplicates (⇒ ((string value)... (string_key value_value)))) (has-duplicates (⇒ ((string value)...)))]
  [(has-duplicates (⇒ ())) #f])
