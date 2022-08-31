#lang racket

(require redex)
(require rackunit)
(require "attributeL-bigStep.rkt")
(require "attributeL-smallStep.rkt")

(define (get-result list)
  (car (car list))
  )

(check-equal?
 (car (judgment-holds (eval () (get (⇒ (("1" 1) ("2" 2))) "2") value) value))
 (get-result
  (apply-reduction-relation*
   expr-red
   (term ((get (⇒ (("1" 1) ("2" 2))) "2") ()) ))))

