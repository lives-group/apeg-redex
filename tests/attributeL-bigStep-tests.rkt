#lang racket


(require redex
         "../attributeL-bigStep.rkt")


(test-equal (judgment-holds (eval () #t v) v)
            '(#t))

(test-equal (judgment-holds (eval () 123 v) v)
            '(123))

(test-equal (judgment-holds (eval () "string" v) v)
            '("string"))

(test-equal (judgment-holds (eval () (⇒ ("map1" 1) ("map2" 2) ("map3" 3)) v) v)
            '((⇒ ("map1" 1) ("map2" 2) ("map3" 3))))

(test-equal (judgment-holds (eval () (: 1 2) v) v)
            '((: 1 2)))

(test-equal (judgment-holds (eval () nil v) v)
            '(nil))

(test-equal (judgment-holds (eval () (+ 3 4) v) v)
            '(7))

(test-equal (judgment-holds (eval () (+ 314159 4) v) v)
            '(314163))

(test-equal (judgment-holds (eval () (- 7 3) v) v)
            '(4))

(test-equal (judgment-holds (eval () (- 7 314159) v) v)
            '(-314152))

(test-equal (judgment-holds (eval () (× 7 3) v) v)
            '(21))

(test-equal (judgment-holds (eval () (× 7 314159) v) v)
            '(2199113))

(test-equal (judgment-holds (eval () (÷ 14 6) v) v)
            '(2))

(test-equal (judgment-holds (eval () (÷ 70 314159) v) v)
            '(0))

(test-equal (judgment-holds (eval () (∧ #t #t) v) v)
            '(#t))

(test-equal (judgment-holds (eval () (∧ #t #f) v) v)
            '(#f))

(test-equal (judgment-holds (eval () (∧ #f #f) v) v)
            '(#f))

(test-equal (judgment-holds (eval () (∧ #f #t) v) v)
            '(#f))

(test-equal (judgment-holds (eval () (∨ #t #t) v) v)
            '(#t))

(test-equal (judgment-holds (eval () (∨ #t #f) v) v)
            '(#t))

(test-equal (judgment-holds (eval () (∨ #f #f) v) v)
            '(#f))

(test-equal (judgment-holds (eval () (∨ #f #t) v) v)
            '(#t))

(test-equal (judgment-holds (eval () (¬ #f) v) v)
            '(#t))

(test-equal (judgment-holds (eval () (¬ #t) v) v)
            '(#f))

(test-equal (judgment-holds (eval () (== 0 1) v) v)
            '(#f))

(test-equal (judgment-holds (eval () (== 32 32) v) v)
            '(#t))

(test-equal (judgment-holds (eval () (== 64 640) v) v)
            '(#f))

(test-equal (judgment-holds (eval () (> 3 45) v) v)
            '(#f))

(test-equal (judgment-holds (eval () (> 3 3) v) v)
            '(#f))

(test-equal (judgment-holds (eval () (> 45 3) v) v)
            '(#t))

(test-equal (judgment-holds (eval () (: 3 nil) v) v)
            '((: 3 nil)))

(test-equal (judgment-holds (eval () (head (: 3 nil)) v) v)
            '(3))

(test-equal (judgment-holds (eval () (head (: nil 3)) v) v)
            '(nil))

(test-equal (judgment-holds (eval () (head nil) v) v)
            '())

(test-equal (judgment-holds (eval () (tail (: 3 nil)) v) v)
            '(nil))

(test-equal (judgment-holds (eval () (tail (: nil 3)) v) v)
            '(3))

(test-equal (judgment-holds (eval () (tail nil) v) v)
            '())

(test-equal (judgment-holds (eval () (: 30 120) v) v)
            '((: 30 120)))

(test-equal (judgment-holds (eval () (head (: 30 120)) v) v)
            '(30))

(test-equal (judgment-holds (eval ((x 30)) (head (: x 120)) v) v)
            '(30))

(test-equal (judgment-holds (eval () (tail (: 30 120)) v) v)
            '(120))

(test-equal (judgment-holds (eval ((x 120)) (tail (: 30 x)) v) v)
            '(120))

(test-equal (judgment-holds (eval () (⇒) v) v)
            '((⇒)))

(test-equal (judgment-holds (eval () (⇒ ("k1" 1) ("k1" 2)) v) v)
            '((⇒ ("k1" 2))))

(test-equal (judgment-holds (eval () (⇒ ("key" 1)) v) v)
            '((⇒ ("key" 1))))

(test-equal (judgment-holds (eval () (⇒ ("key" (× 3 4))) v) v)
            '((⇒ ("key" 12))))

(test-equal (judgment-holds (eval () (put (⇒) "key" (× 3 4)) v) v)
            '((⇒ ("key" 12))))

(test-equal (judgment-holds (eval () (put (⇒ ("key" 15)) "key" (× 3 4)) v) v)
            '((⇒ ("key" 12))))

(test-equal (judgment-holds (eval () (put (⇒ ("k1" 1) ("k2" 2)) "k1" 2) v) v)
            '((⇒ ("k1" 2) ("k2" 2))))

(test-equal (judgment-holds (eval () (put (⇒ ("k1" 1) ("k2" 2)) "k2" 1) v) v)
            '((⇒ ("k1" 1) ("k2" 1))))

(test-equal (judgment-holds (eval ((x "key")) (put (⇒) x (× 3 4)) v) v)
            '((⇒ ("key" 12))))

(test-equal (judgment-holds (eval ((x 12)) (put (⇒) "key" x) v) v)
            '((⇒ ("key" 12))))

(test-equal (judgment-holds (eval () (get (⇒ ("key" (× 3 4))) "key") v) v)
            '(12))

(test-equal (judgment-holds (eval ((x "key")) (get (⇒ ("key" (× 3 4))) x) v) v)
            '(12))

(test-equal (judgment-holds (eval ((x "key")) (get (⇒ (x (× 3 4))) "key") v) v)
            '(12))

(test-equal (judgment-holds (eval ((x 12)) (get (⇒ ("key" x)) "key") v) v)
            '(12))

(test-equal (judgment-holds (eval ((x0 2) (x1 1)) (+ x0 x1) v) v)
            '(3))

(test-equal (judgment-holds (eval ((x0 2718281828) (x1 1)) (+ x0 x1) v) v)
            '(2718281829))

(test-equal (judgment-holds (eval ((x0 15) (x1 7)) (- x0 x1) v) v)
            '(8))

(test-equal (judgment-holds (eval ((x0 15) (x1 2718281828)) (- x0 x1) v) v)
            '(-2718281813))

(test-equal (judgment-holds (eval ((x0 15) (x1 7)) (× x0 x1) v) v)
            '(105))

(test-equal (judgment-holds (eval ((x0 15) (x1 2718281828)) (× x0 x1) v) v)
            '(40774227420))

(test-equal (judgment-holds (eval ((x0 30) (x1 14)) (÷ x0 x1) v) v)
            '(2))

(test-equal (judgment-holds (eval ((x0 150) (x1 2718281828)) (÷ x0 x1) v) v)
            '(0))

(test-equal (judgment-holds (eval ((y (: 127 128))) y v) v)
            '((: 127 128)))

(test-equal (judgment-holds (eval () (head (: 127 128)) v) v)
            '(127))

(test-equal (judgment-holds (eval ((y (: 127 128))) (head y) v) v)
            '(127))

(test-equal (judgment-holds (eval () (tail (: 127 128)) v) v)
            '(128))

(test-equal (judgment-holds (eval ((y (: 127 128))) (tail y) v) v)
            '(128))

(test-equal (judgment-holds (eval ((x (⇒ ("key" 1)))) x v) v)
            '((⇒ ("key" 1))))

(test-equal (judgment-holds (eval ((x (⇒))) (put x "key" (× 3 4)) v) v)
            '((⇒ ("key" 12))))

(test-equal (judgment-holds (eval ((x (⇒ ("key" 12)))) (get x "key") v) v)
            '(12))


(test-results)