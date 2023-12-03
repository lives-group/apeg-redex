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
     (¬ H)
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
  (value integer
         string
         (⇒ ((string value) ...))
         (: value value)
         nil
         undef))

(define expr-red
  (reduction-relation ctx-AttributeL
  #:domain VS
   (--> ((in-hole H (+ integer_1 integer_2)) ctx)
        ((in-hole H ,(+ (term integer_1) (term integer_2))) ctx)
        "addition")
   
   (--> ((in-hole H x) ((x_1 value_1)... (x value) (x_2 value_2)... ))
        ((in-hole H value) ((x_1 value_1)... (x value) (x_2 value_2)... ))
        "variable")
   
   (--> ((in-hole H (* integer_1 integer_2)) ctx)
        ((in-hole H ,(* (term integer_1) (term integer_2))) ctx)
        "multiplication")
   
   (--> ((in-hole H (- integer_1 integer_2)) ctx)
        ((in-hole H ,(- (term integer_1) (term integer_2))) ctx)
        "subtraction")
   
   (--> ((in-hole H (÷ integer_1 integer_2)) ctx)
        ((in-hole H ,(quotient (term integer_1) (term integer_2))) ctx)
        "division")

   (--> ((in-hole H (&& #t boolean)) ctx)
        ((in-hole H boolean) ctx)
        "and-first-success")

   (--> ((in-hole H (&& #f expr)) ctx)
        ((in-hole H #f) ctx)
        "and-first-fail")

   (--> ((in-hole H (|| #t expr)) ctx)
        ((in-hole H #t) ctx)
        "or-first-success")

   (--> ((in-hole H (|| #f boolean)) ctx)
        ((in-hole H boolean) ctx)
        "or-first-fail")

   (--> ((in-hole H (¬ boolean)) ctx)
        ((in-hole H ,(not (term boolean))) ctx)
        "not")

   (--> ((in-hole H (== integer_1 integer_2)) ctx)
        ((in-hole H ,(= (term integer_1) (term integer_2))) ctx)
        "equality")

   (--> ((in-hole H (> integer_1 integer_2)) ctx)
        ((in-hole H ,(> (term integer_1) (term integer_2))) ctx)
        "bigger-then")
   
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
