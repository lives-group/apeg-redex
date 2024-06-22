#lang racket
(require redex
         "../attributeLType-bigStep.rkt")


(test-equal (judgment-holds (types () #t type) type)
            '(type:boolean))
(test-equal (judgment-holds (types () 123 type) type)
            '(type:integer))
(test-equal (judgment-holds (types () "string" type) type)
            '(type:string))


(test-equal (judgment-holds (types () (+ 3 4) type) type)
            '(type:integer))
(test-equal (judgment-holds (types () (- 7 3) type) type)
            '(type:integer))
(test-equal (judgment-holds (types () (* 7 3) type) type)
            '(type:integer))
(test-equal (judgment-holds (types () (÷ 14 6) type) type)
            '(type:integer))


(test-equal (judgment-holds (types () (&& #t #t) type) type)
            '(type:boolean))
(test-equal (judgment-holds (types () (&& #t #f) type) type)
            '(type:boolean))
(test-equal (judgment-holds (types () (&& #f #f) type) type)
            '(type:boolean))
(test-equal (judgment-holds (types () (&& #f #t) type) type)
            '(type:boolean))
(test-equal (judgment-holds (types () (|| #t #t) type) type)
            '(type:boolean))
(test-equal (judgment-holds (types () (|| #t #f) type) type)
            '(type:boolean))
(test-equal (judgment-holds (types () (|| #f #f) type) type)
            '(type:boolean))
(test-equal (judgment-holds (types () (|| #f #t) type) type)
            '(type:boolean))
(test-equal (judgment-holds (types () (¬ #f) type) type)
            '(type:boolean))
(test-equal (judgment-holds (types () (¬ #t) type) type)
            '(type:boolean))
(test-equal (judgment-holds (types () (== 0 1) type) type)
            '(type:boolean))
(test-equal (judgment-holds (types () (== 32 32) type) type)
            '(type:boolean))
(test-equal (judgment-holds (types () (> 3 3) type) type)
            '(type:boolean))


(test-equal (judgment-holds (types ((x0 type:integer) (x1 type:integer)) (+ x0 x1) type) type)
            '(type:integer))
(test-equal (judgment-holds (types ((x0 type:integer) (x1 type:integer)) (- x0 x1) type) type)
            '(type:integer))
(test-equal (judgment-holds (types ((x0 type:integer) (x1 type:integer)) (* x0 x1) type) type)
            '(type:integer))
(test-equal (judgment-holds (types ((x0 type:integer) (x1 type:integer)) (÷ x0 x1) type) type)
            '(type:integer))


(test-equal (judgment-holds (types () (⇒ (("key1" 1) ("key2" 2) ("key3" 3))) type) type)
            '((⇒ type:integer)))
(test-equal (judgment-holds (types () (⇒ ()) type) type)
            '())


(test-equal (judgment-holds (types () (get (⇒ (("key1" 1) ("key2" 2) ("key3" 3))) "key1") type) type)
            '(type:integer))
(test-equal (judgment-holds (types () (get (⇒ (("key1" 1) ("key2" 2) ("key3" 3))) 3) type) type)
            '())
(test-equal (judgment-holds (types () (put (⇒ (("key1" 1) ("key2" 2) ("key3" 3))) "key4" 4) type) type)
            '((⇒ type:integer)))
(test-equal (judgment-holds (types () (put (⇒ (("key1" 1) ("key2" 2) ("key3" 3))) "key4" #t) type) type)
            '())
(test-equal (judgment-holds (types () (put (⇒ (("key1" 1) ("key2" 2) ("key3" 3))) 5 4) type) type)
            '())


(test-equal (judgment-holds (types () (: 1 nil) type) type)
            '((: type:integer)))
(test-equal (judgment-holds (types () (: 1 (: 2 nil)) type) type)
            '((: type:integer)))
(test-equal (judgment-holds (types () (: 1 (: 2 (: 3 nil))) type) type)
            '((: type:integer)))
(test-equal (judgment-holds (types () nil type) type)
            '())
(test-equal (judgment-holds (types () (: 1 (: 2 (: 3 4))) type) type)
            '())


(test-equal (judgment-holds (types () (head (: #t (: #f (: #t nil)))) type) type)
            '(type:boolean))
(test-equal (judgment-holds (types () (tail (: #t (: #f (: #t nil)))) type) type)
            '((: type:boolean)))


(test-equal (judgment-holds (types ((x (⇒ (: type:integer)))) (get x "key") type) type)
            '((: type:integer)))
(test-equal (judgment-holds (types ((x (⇒ (: type:integer)))) (head (get x "key")) type) type)
            '(type:integer))


(test-results)