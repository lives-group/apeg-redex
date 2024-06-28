#lang racket


(require redex
         "../attributeLType-bigStep.rkt")


(test-equal (judgment-holds (types () #t τ) τ)
            '(Bool))

(test-equal (judgment-holds (types () 123 τ) τ)
            '(Integer))

(test-equal (judgment-holds (types () "string" τ) τ)
            '(String))

(test-equal (judgment-holds (types () (+ 3 4) τ) τ)
            '(Integer))

(test-equal (judgment-holds (types () (- 7 3) τ) τ)
            '(Integer))

(test-equal (judgment-holds (types () (× 7 3) τ) τ)
            '(Integer))

(test-equal (judgment-holds (types () (÷ 14 6) τ) τ)
            '(Integer))

(test-equal (judgment-holds (types () (∧ #t #t) τ) τ)
            '(Bool))

(test-equal (judgment-holds (types () (∧ #t #f) τ) τ)
            '(Bool))

(test-equal (judgment-holds (types () (∧ #f #f) τ) τ)
            '(Bool))

(test-equal (judgment-holds (types () (∧ #f #t) τ) τ)
            '(Bool))

(test-equal (judgment-holds (types () (∨ #t #t) τ) τ)
            '(Bool))

(test-equal (judgment-holds (types () (∨ #t #f) τ) τ)
            '(Bool))

(test-equal (judgment-holds (types () (∨ #f #f) τ) τ)
            '(Bool))

(test-equal (judgment-holds (types () (∨ #f #t) τ) τ)
            '(Bool))

(test-equal (judgment-holds (types () (¬ #f) τ) τ)
            '(Bool))

(test-equal (judgment-holds (types () (¬ #t) τ) τ)
            '(Bool))

(test-equal (judgment-holds (types () (== 0 1) τ) τ)
            '(Bool))

(test-equal (judgment-holds (types () (== 32 32) τ) τ)
            '(Bool))

(test-equal (judgment-holds (types () (> 3 3) τ) τ)
            '(Bool))

(test-equal (judgment-holds (types ((x0 Integer) (x1 Integer)) (+ x0 x1) τ) τ)
            '(Integer))

(test-equal (judgment-holds (types ((x0 Integer) (x1 Integer)) (- x0 x1) τ) τ)
            '(Integer))

(test-equal (judgment-holds (types ((x0 Integer) (x1 Integer)) (× x0 x1) τ) τ)
            '(Integer))

(test-equal (judgment-holds (types ((x0 Integer) (x1 Integer)) (÷ x0 x1) τ) τ)
            '(Integer))

(test-equal (judgment-holds (types () (⇒ ("key1" 1) ("key2" 2) ("key3" 3)) τ) τ)
            '((⇒ Integer)))

(test-equal (judgment-holds (types () (⇒) τ) τ)
            '())

(test-equal (judgment-holds (types () (get (⇒ ("key1" 1) ("key2" 2) ("key3" 3)) "key1") τ) τ)
            '(Integer))

(test-equal (judgment-holds (types () (get (⇒ ("key1" 1) ("key2" 2) ("key3" 3)) 3) τ) τ)
            '())

(test-equal (judgment-holds (types () (put (⇒ ("key1" 1) ("key2" 2) ("key3" 3)) "key4" 4) τ) τ)
            '((⇒ Integer)))

(test-equal (judgment-holds (types () (put (⇒ ("key1" 1) ("key2" 2) ("key3" 3)) "key4" #t) τ) τ)
            '())

(test-equal (judgment-holds (types () (put (⇒ ("key1" 1) ("key2" 2) ("key3" 3)) 5 4) τ) τ)
            '())

(test-equal (judgment-holds (types () (: 1 nil) τ) τ)
            '((: Integer)))

(test-equal (judgment-holds (types () (: 1 (: 2 nil)) τ) τ)
            '((: Integer)))

(test-equal (judgment-holds (types () (: 1 (: 2 (: 3 nil))) τ) τ)
            '((: Integer)))

(test-equal (judgment-holds (types () nil τ) τ)
            '())

(test-equal (judgment-holds (types () (: 1 (: 2 (: 3 4))) τ) τ)
            '())

(test-equal (judgment-holds (types () (head (: #t (: #f (: #t nil)))) τ) τ)
            '(Bool))

(test-equal (judgment-holds (types () (tail (: #t (: #f (: #t nil)))) τ) τ)
            '((: Bool)))

(test-equal (judgment-holds (types ((x (⇒ (: Integer)))) (get x "key") τ) τ)
            '((: Integer)))

(test-equal (judgment-holds (types ((x (⇒ (: Integer)))) (head (get x "key")) τ) τ)
            '(Integer))


(test-results)