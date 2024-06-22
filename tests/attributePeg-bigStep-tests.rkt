#lang racket
(require redex
         "../attributePeg-bigStep.rkt")


(test-equal (judgment-holds (parse () () ε (1 2 3) r ctx) (r ctx))
            '(((1 2 3) ())))
(test-equal (judgment-holds (parse () () ε () r ctx) (r ctx))
            '((() ())))


(test-equal (judgment-holds (parse () () 1 (1 2 3) r ctx) (r ctx))
            '(((2 3) ())))
(test-equal (judgment-holds (parse () () 2 (1 2 3) r ctx) (r ctx))
            '((⊥ ())))
(test-equal (judgment-holds (parse () () 2 () r ctx) (r ctx))
            '((⊥ ())))


(test-equal (judgment-holds (parse () () (• 1 2) (1 2 3) r ctx) (r ctx))
            '(((3) ())))
(test-equal (judgment-holds (parse () () (• 1 3) (1 2 3) r ctx) (r ctx))
            '((⊥ ())))
(test-equal (judgment-holds (parse () () (• 1 2) () r ctx) (r ctx))
            '((⊥ ())))
(test-equal (judgment-holds (parse () () (• 1 (• 2 3)) (1 2 3) r ctx) (r ctx))
            '((() ())))
(test-equal (judgment-holds (parse () () (• (• 1 2) 3) (1 2 3) r ctx) (r ctx))
            '((() ())))
(test-equal (judgment-holds (parse () () (• (• 1 2) (• 3 4)) (1 2 3) r ctx) (r ctx))
            '((⊥ ())))
(test-equal (judgment-holds (parse () () (• (• 1 2) (• 3 ε)) (1 2 3) r ctx) (r ctx))
            '((() ())))


(test-equal (judgment-holds (parse () () (/ 1 2) (1 2 3) r ctx) (r ctx))
            '(((2 3) ())))
(test-equal (judgment-holds (parse () () (/ 2 1) (1 2 3) r ctx) (r ctx))
            '(((2 3) ())))


#;(judgment-holds (parse () () (* ε) (1 1 1 2 3 4 5) r ctx))
(test-equal (judgment-holds (parse () () (* 1) (1 1 1 2 3 4 5) r ctx) (r ctx))
            '(((2 3 4 5) ())))
(test-equal (judgment-holds (parse () () (* (/ 1 (• 2 (• 3 (• 4 5))))) (1 1 1 2 3 4 5) r ctx) (r ctx))
            '((() ())))
(test-equal (judgment-holds (parse () () (* 2) (1 1 1 2 3 4 5) r ctx) (r ctx))
            '(((1 1 1 2 3 4 5) ())))


(test-equal (judgment-holds (parse () () (! 1) (1) r ctx) (r ctx))
            '((⊥ ())))
(test-equal (judgment-holds (parse () () (! 1) (2) r ctx) (r ctx))
            '(((2) ())))
(test-equal (judgment-holds (parse () () (! 1) () r ctx) (r ctx))
            '((() ())))
(test-equal (judgment-holds (parse () () (! 1) (1 2) r ctx) (r ctx))
            '((⊥ ())))
(test-equal (judgment-holds (parse () () (! (• 1 2)) () r ctx) (r ctx))
            '((() ())))
(test-equal (judgment-holds (parse () () (! (• 1 2)) (1 3 6 9) r ctx) (r ctx))
            '(((1 3 6 9) ())))
(test-equal (judgment-holds (parse () () (! (• 1 (• 2 3))) (1 2 3) r ctx) (r ctx))
            '((⊥ ())))


(test-equal (judgment-holds (parse ((x 1) (y 2)) () ((← x 3)) (1 2 3) r ctx) (r ctx))
            '(((1 2 3) ((x 3) (y 2)))))
(test-equal (judgment-holds (parse ((x 1) (y 2)) () ((← x (+ 1 2))) (1 2 3) r ctx) (r ctx))
            '(((1 2 3) ((x 3) (y 2)))))
(test-equal (judgment-holds (parse ((x 1) (y 2)) () ((← x (* 1 2))) (1 2 3) r ctx) (r ctx))
            '(((1 2 3) ((x 2) (y 2)))))
(test-equal (judgment-holds (parse ((x 1) (y 2)) () ((← x (÷ 5 2))) () r ctx) (r ctx)) 
            '((() ((x 2) (y 2)))))
(test-equal (judgment-holds (parse ((x 1) (y 2)) () ((← x (+ (+ 1 2) 2))) () r ctx) (r ctx))
            '((() ((x 5) (y 2)))))
(test-equal (judgment-holds (parse ((x 1) (y 2)) () ((← x (+ (+ 1 2) (- 1 6)))) () r ctx) (r ctx))
            '((() ((x -2) (y 2)))))
(test-equal (judgment-holds (parse () () ((← x (+ (+ 1 2) (- 1 6)))) () r ctx) (r ctx))
            '((() ((x -2)))))
#;(test-equal (judgment-holds (parse ((x 1) (x 2)) () ((← x (+ (+ 1 2) (- 1 6)))) () r ctx) (r ctx))
            '((() ((x -2)))))


#;(judgment-holds (parse () ((S () () (T () ()))
(T () () (• (B () ()) (* (B () ()))))
(B () () (/ 0 1))) (S () ()) (1 0 1 0 1) r ctx) (r ctx))


(test-equal (judgment-holds (parse () ((S () () (A () ()))
                                       (A () () (/ (• 0 (• (A () ()) (B () ()))) ε))
                                       (B () () 1)) (S () ()) (0 0 0 1 1 1 1 2 3 4 5) r ctx) (r ctx))
            '(((1 2 3 4 5) ())))
(test-equal (judgment-holds (parse () ((S () () (T () ()))
                                       (T () () (/ (• (B () ()) (T () ())) ε))
                                       (B () () (/ 0 1))) (S () ()) (1 0 1 0 1) r ctx) (r ctx))
            '((() ())))
(test-equal (judgment-holds (parse () ((S () () (T () ()))
                                       (T () () (• (B () ()) (B () ())))
                                       (B () () (/ 0 1))) (S () ()) (1 0 1 0 1) r ctx) (r ctx))
            '(((1 0 1) ())))
(test-equal (judgment-holds (parse () ((S () () (T () ()))
                                       (T () () (• (B () ()) (* (B () ()))))
                                       (B () () (/ 0 1))) (S () ()) (1 0 1 0 1) r ctx) (r ctx))
            '((() ())))
(test-equal (judgment-holds (parse () ((S () () (T () ()))
                                       (T () () (* (B () ())))
                                       (B () () (/ 0 1))) (S () ()) (1 0 1 0 1) r ctx) (r ctx))
            '((() ())))
(test-equal (judgment-holds (parse () ((S () () (! (• 1 (A () ()))))
                                       (A () () 2))
                                   (S () ()) (1 2 3 4 5) r ctx) (r ctx))
            '((⊥ ())))


(test-equal (judgment-holds (parse () ((S ((type:integer x0) (type:integer x1)) (x0) 1))
                                   (S (0 0) (decimal)) (1 0 1 0 1) r ctx) (r ctx))
            '(((0 1 0 1) ((decimal 0)))))

(test-equal (judgment-holds (parse () ((S ((type:integer count1)) (count1) (/ (• 1 (S ((+ count1 1)) (count1))) ε)))
                                   (S (0) (count1)) (1 1 1 1 1 1 1 1 1 1 2 3 4 5) r ctx) (r ctx))
            '(((2 3 4 5) ((count1 10)))))

(test-equal (judgment-holds (parse () ((S ((type:integer count1)) (count1) (* (• 1 (S ((+ count1 1)) (count1))))))
                                   (S (0) (count1)) (1 1 1 1 1 1 1 1 2 3 4 5) r ctx) (r ctx))
            '(((2 3 4 5) ((count1 8)))))

(test-equal (judgment-holds (parse () ((S ((type:integer count1)) (count1) (/ (• 1 (S ((+ count1 1)) (count1))) ε)))
                                   (S (0) (count1)) (2 3 4 5) r ctx) (r ctx))
            '(((2 3 4 5) ((count1 0)))))

(test-equal (judgment-holds (parse () ((S ((type:integer value)) (value) (/ (• 1 (S ((+ (* 2 value) 1)) (value))) (/ (• 0 (S ((* 2 value)) (value))) ε))))
                                   (S (0) (value)) (0 1 0 1) r ctx) (r ctx))
            '((() ((value 5)))))

(test-equal (judgment-holds (parse () ((S ((type:integer value)) (value) (/ (• 1 (S ((+ (* 2 value) 1)) (value))) (/ (• 0 (S ((* 2 value)) (value))) ε))))
                                   (S (0) (value)) (1 0 1 1 1) r ctx) (r ctx))
            '((() ((value 23)))))

(test-equal (judgment-holds (parse () ((S () (x0) (T (0) (x0)))
                                       (T ((type:integer x0)) (x0) (* (/ (• 1 (B (x0 1) (x0))) (• 0 (B (x0 0) (x0))))))
                                       (B ((type:integer x0) (type:integer x1)) ((+ (* 2 x0) x1)) ε))
                                   (S () (decimal)) (1 0 1 0 1) r ctx) (r ctx))
            '((() ((decimal 21)))))

(test-equal (judgment-holds (parse () ((S () (n) (/ (SUM () (n)) (N () (n))))
                                       (SUM () ((+ n0 n1)) (• (N () (n0)) (• 3 (S () (n1)))))
                                       (N () (x0) (T (0) (x0)))
                                       (T ((type:integer x0)) (x0) (* (/ (• 1 (B (x0 1) (x0))) (• 0 (B (x0 0) (x0))))))
                                       (B ((type:integer x0) (type:integer x1)) ((+ (* 2 x0) x1)) ε))
                                   (S () (result)) (1 0 1 0 1) r ctx) (r ctx))
            '((() ((result 21)))))

(test-equal (judgment-holds (parse () ((S () (x0 x1 3) (• (B (1) (x0)) (B (2) (x1))))
                                       (B ((type:integer n)) (n) (/ 1 0)))
                                   (S () (n0 n1 n2)) (1 0 1 0 1) r ctx) (r ctx))
            '(((1 0 1) ((n0 1) (n1 2) (n2 3)))))

(test-equal (judgment-holds (parse () ((S () (x0 x1 x2) (• (B (1) (x0)) (• (B (2) (x1)) (B (3) (x2)))))
                                       (B ((type:integer n)) (n) (/ 1 0)))
                                   (S () (n0 n1 n2)) (1 0 1 0 1) r ctx) (r ctx))
            '(((0 1) ((n0 1) (n1 2) (n2 3)))))

(test-equal (judgment-holds (parse () ((S () (n) (/ (• 1 (A () (n))) (/ (• 2 (B () (n))) (• 3 (C () (n))))))
                                       (A () (1) ε)
                                       (B () (2) ε)
                                       (C () (3) ε))
                                   (S () (result)) (1 0 1 0 1) r ctx) (r ctx))
            '(((0 1 0 1) ((result 1)))))

(test-equal (judgment-holds (parse () ((S () (n) (/ (• 1 (A () (n))) (/ (• 2 (B () (n))) (• 3 (C () (n))))))
                                       (A () (1) ε)
                                       (B () (2) ε)
                                       (C () (3) ε))
                                   (S () (result)) (2 0 1 0 1) r ctx) (r ctx))
            '(((0 1 0 1) ((result 2)))))

(test-equal (judgment-holds (parse () ((S () (n) (/ (• 1 (A () (n))) (/ (• 2 (B () (n))) (• 3 (C () (n))))))
                                       (A () (1) ε)
                                       (B () (2) ε)
                                       (C () (3) ε))
                                   (S () (result)) (3 0 1 0 1) r ctx) (r ctx))
            '(((0 1 0 1) ((result 3)))))

(test-equal (judgment-holds (parse () ((S () (n) (/ (• 1 (A () (n))) (/ (• 2 (B () (n))) (• 3 (C () (n))))))
                                       (A () (1) ε)
                                       (B () (2) ε)
                                       (C () (3) ε))
                                   (S () (result)) (4 0 1 0 1) r ctx) (r ctx))
            '((⊥ ())))

(test-equal (judgment-holds (parse () ((S () (n) (/ (A () (n)) (/ (B () (n)) (C () (n)))))
                                       (A () (1) 1)
                                       (B () (2) 2)
                                       (C () (3) 3))
                                   (S () (result)) (1 0 1 0 1) r ctx) (r ctx))
            '(((0 1 0 1) ((result 1)))))

(test-equal (judgment-holds (parse () ((S () (n) (/ (A () (n)) (/ (B () (n)) (C () (n)))))
                                       (A () (1) 1)
                                       (B () (2) 2)
                                       (C () (3) 3))
                                   (S () (result)) (2 0 1 0 1) r ctx) (r ctx))
            '(((0 1 0 1) ((result 2)))))

(test-equal (judgment-holds (parse () ((S () (n) (/ (A () (n)) (/ (B () (n)) (C () (n)))))
                                       (A () (1) 1)
                                       (B () (2) 2)
                                       (C () (3) 3))
                                   (S () (result)) (3 0 1 0 1) r ctx) (r ctx))
            '(((0 1 0 1) ((result 3)))))

(test-equal (judgment-holds (parse () ((S () (n) (/ (A () (n)) (/ (B () (n)) (C () (n)))))
                                       (A () (1) 1)
                                       (B () (2) 2)
                                       (C () (3) 3))
                                   (S () (result)) (4 0 1 0 1) r ctx) (r ctx))
            '((⊥ ())))

(test-equal (judgment-holds (parse () ((S () (n) (! (• 1 (A (1) (n)))))
                                       (A ((type:integer n)) ((* 2 n)) 2))
                                   (S () (result)) (1 2 3 4 5) r ctx) (r ctx))
              '((⊥ ())))

(test-equal (judgment-holds (parse () ((S () (n) (• 1 (A (1) (n))))
                                       (A ((type:integer n)) ((* 2 n)) 2))
                                   (S () (result)) (1 2 3 4 5) r ctx) (r ctx))
              '(((3 4 5) ((result 2)))))

;Exemplo importante
#;(judgment-holds (parse () ((S () (n) (! (• 1 (A (1) (n)))))
                                       (A (n) ((* 2 n)) 2))
                                   (S () (result)) (1 3 3 4 5) r ctx) (r ctx))

(test-equal (judgment-holds (parse () ((S () (n) (! (A (2) (n))))
                                       (A ((type:integer n)) ((* 2 n)) (• 1 2)))
                                   (S () (result)) (1 2 3 4 5) r ctx) (r ctx))
            '((⊥ ())))

(test-equal (judgment-holds (parse () ((S () (n) (A (2) (n)))
                                       (A ((type:integer n)) ((* 2 n)) (• 1 2)))
                                   (S () (result)) (1 2 3 4 5) r ctx) (r ctx))
            '(((3 4 5) ((result 4)))))

;factorial
(test-equal (judgment-holds (parse () ((S () (x) (/ (• 1 (A (1) (x))) ((← x 1))))
                                       (A ((type:integer y)) ((* y z)) (/ (• 1 (A ((+ y 1)) (z))) ((← z 1)))))
                                   (S () (result)) () r ctx) (r ctx))
            '((() ((result 1)))))

(test-equal (judgment-holds (parse () ((S () (x) (/ (• 1 (A (1) (x))) ((← x 1))))
                                       (A ((type:integer y)) ((* y z)) (/ (• 1 (A ((+ y 1)) (z))) ((← z 1)))))
                                   (S () (result)) (1 1 1 1 1 1 1) r ctx) (r ctx))
            '((() ((result 5040)))))

;fibonacci
(test-equal (judgment-holds (parse () ((S () (x) (/ (• 1 (A () (x))) ((← x nil))))
                                       (A () (x) (/ (• 1 (B () (x))) ((← x (: 0 nil)))))
                                       (B () (x) (/ (• 1 (C ((: (: 0 1) 1)) (x))) ((← x (: 0 1)))))
                                       (C ((type:integer x)) (y) (/ (• 1 (C ((: x (+ (tail x) (tail (head x))))) (y))) ((← y x)))))
                                   (S () (result)) () r ctx) (r ctx))
            '((() ((result nil)))))

(test-equal (judgment-holds (parse () ((S () (x) (/ (• 1 (A () (x))) ((← x nil))))
                                       (A () (x) (/ (• 1 (B () (x))) ((← x (: 0 nil)))))
                                       (B () (x) (/ (• 1 (C ((: (: 0 1) 1)) (x))) ((← x (: 0 1)))))
                                       (C ((type:integer x)) (y) (/ (• 1 (C ((: x (+ (tail x) (tail (head x))))) (y))) ((← y x)))))
                                   (S () (result)) (1) r ctx) (r ctx))
            '((() ((result (: 0 nil))))))

(test-equal (judgment-holds (parse () ((S () (x) (/ (• 1 (A () (x))) ((← x nil))))
                                       (A () (x) (/ (• 1 (B () (x))) ((← x (: 0 nil)))))
                                       (B () (x) (/ (• 1 (C ((: (: 0 1) 1)) (x))) ((← x (: 0 1)))))
                                       (C ((type:integer x)) (y) (/ (• 1 (C ((: x (+ (tail x) (tail (head x))))) (y))) ((← y x)))))
                                   (S () (result)) (1 1) r ctx) (r ctx))
            '((() ((result (: 0 1))))))

(test-equal (judgment-holds (parse () ((S () (x) (/ (• 1 (A () (x))) ((← x nil))))
                                       (A () (x) (/ (• 1 (B () (x))) ((← x (: 0 nil)))))
                                       (B () (x) (/ (• 1 (C ((: (: 0 1) 1)) (x))) ((← x (: 0 1)))))
                                       (C ((type:integer x)) (y) (/ (• 1 (C ((: x (+ (tail x) (tail (head x))))) (y))) ((← y x)))))
                                   (S () (result)) (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) r ctx) (r ctx))
            '((() ((result (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: 0 1) 1) 2) 3) 5) 8) 13) 21) 34) 55) 89) 144) 233) 377) 610) 987))))))

;x^y
(test-equal (judgment-holds (parse () ((S () (x) (A (0) (x)))
                                       (A ((type:integer x)) (y) (* (/ (• 1 (A ((+ (* 10 x) 1)) (y))) (/ (• 2 (A ((+ (* 10 x) 2)) (y))) (/ (• 3 (A ((+ (* 10 x) 3)) (y))) (/ (• 4 (A ((+ (* 10 x) 4)) (y))) (/ (• 5 (A ((+ (* 10 x) 5)) (y))) (/ (• 6 (A ((+ (* 10 x) 6)) (y))) (/ (• 7 (A ((+ (* 10 x) 7)) (y))) (/ (• 8 (A ((+ (* 10 x) 8)) (y))) (/ (• 9 (A ((+ (* 10 x) 9)) (y))) (/ (• 0 (A ((+ (* 10 x) 0)) (y))) (• 1000 (B (x) (y)))))))))))))))
                                       (B ((type:integer x)) (y) (/ (• 1 (C (x x) (y))) ((← y 0))))
                                       (C ((type:integer x) (type:integer y)) (z) (/ (• 1 (C ((* x y) y) (z))) ((← z x)))))
                                   (S () (result)) (0 0 1 2 1000 1 1 1 1 1) r ctx) (r ctx))
            '((() ((result 248832)))))

;x^2
(test-equal (judgment-holds (parse () ((S () (x) (! (! (A (0) (x)))))
                                       (A ((type:integer x)) (y) (/ (• 1 (A ((+ x 1)) (y))) ((← y (* x x))))))
                                   (S () (result)) (1 1 1 1 1 1 1) r ctx) (r ctx))
            '(((1 1 1 1 1 1 1) ((result 49)))))

;x^x
(test-equal (judgment-holds (parse () ((S () (y) (• (! (! (A (0) (x)))) (B (1 x) (y))))
                                       (A ((type:integer x)) (y) (/ (• 1 (A ((+ x 1)) (y))) ((← y x))))
                                       (B ((type:integer x) (type:integer y)) (z) (/ (• 1 (B ((* x y) y) (z))) ((← z x)))))
                                   (S () (result)) (1 1 1 1 1 1 1) r ctx) (r ctx))
            '((() ((result 823543)))))

#|
x/6
RESTO 0 - 000
	000|0 -> RESTO 0
	000|1 -> REST0 1
RESTO 1 - 001
	001|0 -> RESTO 2
	001|1 -> RESTO 3
RESTO 2 - 010
	010|0 -> RESTO 4
	010|1 -> RESTO 5
RESTO 3 - 011
	011|0 -> RESTO 0
	011|1 -> RESTO 1
RESTO 4 - 100
	100|0 -> RESTO 2
	100|1 -> RESTO 3
RESTO 5 - 101
	101|0 -> RESTO 4
	101|1 -> RESTO 5
|#
(test-equal (judgment-holds (parse () ((S () (n (÷ (- n r) 6) r) (RESTO/000-011 (0 0) (n r)))
                                       (RESTO/000-011 ((type:integer n) (type:integer r)) (n r) (/ (• 0 (RESTO/000-011 ((* 2 n) 0) (n r))) (/ (• 1 (RESTO/001-100 ((+ (* 2 n) 1) 1) (n r))) ε)))
                                       (RESTO/001-100 ((type:integer n) (type:integer r)) (n r) (/ (• 0 (RESTO/010-101 ((* 2 n) 2) (n r))) (/ (• 1 (RESTO/000-011 ((+ (* 2 n) 1) 3) (n r))) ε)))
                                       (RESTO/010-101 ((type:integer n) (type:integer r)) (n r) (/ (• 0 (RESTO/001-100 ((* 2 n) 4) (n r))) (/ (• 1 (RESTO/010-101 ((+ (* 2 n) 1) 5) (n r))) ε))))
                                   (S () (number quotient rest)) (1 0 1 0 0 1 1) r ctx) (r ctx))
            '((() ((number 83) (quotient 13) (rest 5)))))

(test-equal (judgment-holds (parse () ((S () () (• (! (! (• (A () ()) (! (b () ()))))) (• (* (a () ())) (• (B () ()) (! (/ 0 (/ 1 2)))))))
                                       (A () () (/ (• (a () ()) (• (A () ()) (b () ()))) ε))
                                       (B () () (/ (• (b () ()) (• (B () ()) (c () ()))) ε))
                                       (a () () 0)
                                       (b () () 1)
                                       (c () () 2))
                                   (S () ()) (0 0 0 1 1 1 2 2 2) r ctx) (r ctx))
            '((() ())))

(test-equal (judgment-holds (parse () ((S () () (• (! (! (• (A () ()) (! (b () ()))))) (• (* (a () ())) (• (B () ()) (! (/ 0 (/ 1 2)))))))
                                       (A () () (/ (• (a () ()) (• (A () ()) (b () ()))) ε))
                                       (B () () (/ (• (b () ()) (• (B () ()) (c () ()))) ε))
                                       (a () () 0)
                                       (b () () 1)
                                       (c () () 2))
                                   (S () ()) (0 0 0 1 1 1 2 2 2 2) r ctx) (r ctx))
            '((⊥ ())))

#;(judgment-holds (parse () ((S () () (• (a () ()) (• (A () ()) (• (B () ()) (e () ())))))
                                       (A () () (/ (• (A () ()) (• (b () ()) (c () ()))) (b () ())))
                                       (B () () (d () ()))
                                       (a () () 0)
                                       (b () () 1)
                                       (c () () 2)
                                       (d () () 3)
                                       (e () () 4))
                                   (S () ()) (0 1 1 2 3 4) r ctx) (r ctx))

;calculator
(test-equal (judgment-holds (parse () ((S () (x) (/ (addition () (x)) (/ (subtraction () (x)) (/ (multiplication () (x)) (/ (division () (x)) (number (0) (x)))))))
                                       (addition () ((+ x y)) (• (++ () ()) (• (S () (x)) (• (space () ()) (number (0) (y))))))
                                       (subtraction () ((- x y)) (• (-- () ()) (• (S () (x)) (• (space () ()) (number (0) (y))))))
                                       (multiplication () ((* x y)) (• (×× () ()) (• (S () (x)) (• (space () ()) (number (0) (y))))))
                                       (division () ((÷ x y)) (• (÷÷ () ()) (• (S () (x)) (• (space () ()) (number (0) (y))))))
                                       (number ((type:integer x)) (x) (/ (• 0 (number ((* 2 x)) (x))) (/ (• 1 (number ((+ (* 2 x) 1)) (x))) ε)))
                                       (++ () () 1100)
                                       (-- () () 1101)
                                       (×× () () 1110)
                                       (÷÷ () () 1111)
                                       (space () () 1000))
                                   (S () (result)) (1111 1110 1101 1100 1 0 1 0 1000 1 0 1 1000 1 0 0 1000 1 1 0 1000 1 0) r ctx) (r ctx)) ;(((((10) + 5) - 4) * 6)/2)
            '((() ((result 33)))))

;list denial
(test-equal (judgment-holds (parse () ((S () (n) (! (• (A () (n)) 2)))
                                      (A () (16) 1))
                                  (S () (result)) (1 2 3 4) r ctx) (r ctx))
            '((⊥ ())))

#;(test-equal (judgment-holds (parse () ((S () (n) (! (• (A () (n)) 2)))
                                      (A () (16) 1))
                                  (S () (result)) (1 3 4 2) r ctx) (r ctx))
            '(((1 3 4 2) ((result 16)))))

;W10W^-1
(test-equal (judgment-holds (parse () ((S () ((* 2 size)) (• (A (0) (size)) (! (/ 0 1))))
                                       (A ((type:integer size)) (size) (/ (• 0 (• (A ((+ size 1)) (size)) 0)) (/ (• 1 (• (A ((+ size 1)) (size)) 1)) 10))))
                                   (S () (result)) (1 0 0 0 1 1 0 1 10 1 0 1 1 0 0 0 1) r ctx) (r ctx))
            '((() ((result 16)))))

;1 1/2 1/4 1/8 1/16 ...
#;(test-equal (judgment-holds (parse () ((S () (list (+ summation 0.0)) (A () (list summation)))
                                       (A () (list summation) (/ (• 1 (B () (list summation))) ((← list (: 1.0 nil)) (← summation 1.0))))
                                       (B () (list summation) (/ (• 1 (C ((: 1.0 0.5) 1.5) (list summation))) ((← list (: 1.0 0.5)) (← summation 1.5))))
                                       (C (((: type:real) list) (type:real summation)) (list summation) (/ (• 1 (C ((: list (÷ (tail list) 2.0)) (+ summation (÷ (tail list) 2.0))) (list summation))) ε)))
                                   (S () (list summation)) () r ctx) (r ctx))
            '((() ((list (: 1.0 nil)) (summation 1.0)))))

#;(test-equal (judgment-holds (parse () ((S () (list (+ summation 0.0)) (A () (list summation)))
                                       (A () (list summation) (/ (• 1 (B () (list summation))) ((← list (: 1.0 nil)) (← summation 1.0))))
                                       (B () (list summation) (/ (• 1 (C ((: 1.0 0.5) 1.5) (list summation))) ((← list (: 1.0 0.5)) (← summation 1.5))))
                                       (C (((: type:real) list) (type:real summation)) (list summation) (/ (• 1 (C ((: list (÷ (tail list) 2.0)) (+ summation (÷ (tail list) 2.0))) (list summation))) ε)))
                                   (S () (list summation)) (1) r ctx) (r ctx))
            '((() ((list (: 1.0 0.5)) (summation 1.5)))))

#;(test-equal (judgment-holds (parse () ((S () (list (+ summation 0.0)) (A () (list summation)))
                                       (A () (list summation) (/ (• 1 (B () (list summation))) ((← list (: 1.0 nil)) (← summation 1.0))))
                                       (B () (list summation) (/ (• 1 (C ((: 1.0 0.5) 1.5) (list summation))) ((← list (: 1.0 0.5)) (← summation 1.5))))
                                       (C (((: type:real) list) (type:real summation)) (list summation) (/ (• 1 (C ((: list (÷ (tail list) 2.0)) (+ summation (÷ (tail list) 2.0))) (list summation))) ε)))
                                   (S () (list summation)) (1 1 1 1) r ctx) (r ctx))
            '((() ((list (: (: (: 1.0 0.5) 0.25) 0.125)) (summation 1.875)))))

#;(test-equal (judgment-holds (parse () ((S () (list (+ summation 0.0)) (A () (list summation)))
                                       (A () (list summation) (/ (• 1 (B () (list summation))) ((← list (: 1.0 nil)) (← summation 1.0))))
                                       (B () (list summation) (/ (• 1 (C ((: 1.0 0.5) 1.5) (list summation))) ((← list (: 1.0 0.5)) (← summation 1.5))))
                                       (C (((: type:real) list) (type:real summation)) (list summation) (/ (• 1 (C ((: list (÷ (tail list) 2.0)) (+ summation (÷ (tail list) 2.0))) (list summation))) ε)))
                                   (S () (list summation)) (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) r ctx) (r ctx))
            '((()
               ((list
                 (:
                  (:
                   (:
                    (:
                     (:
                      (:
                       (:
                        (:
                         (:
                          (:
                           (:
                            (:
                             (:
                              (:
                               (:
                                (:
                                 (:
                                  (:
                                   (:
                                    (:
                                     (:
                                      (:
                                       (:
                                        (: (: (: (: (: (: (: (: (: (: 1.0 0.5) 0.25) 0.125) 0.0625) 0.03125) 0.015625) 0.0078125) 0.00390625) 0.001953125) 0.0009765625)
                                        0.00048828125)
                                       0.000244140625)
                                      0.0001220703125)
                                     6.103515625e-5)
                                    3.0517578125e-5)
                                   1.52587890625e-5)
                                  7.62939453125e-6)
                                 3.814697265625e-6)
                                1.9073486328125e-6)
                               9.5367431640625e-7)
                              4.76837158203125e-7)
                             2.384185791015625e-7)
                            1.1920928955078125e-7)
                           5.960464477539063e-8)
                          2.9802322387695313e-8)
                         1.4901161193847656e-8)
                        7.450580596923828e-9)
                       3.725290298461914e-9)
                      1.862645149230957e-9)
                     9.313225746154785e-10)
                    4.656612873077393e-10)
                   2.3283064365386963e-10)
                  1.1641532182693481e-10))
                (summation 1.9999999998835847)))))

(test-equal (judgment-holds (parse () () (? #f) (1 2 3) r ctx) (r ctx))
            '((⊥ ())))

(test-equal (judgment-holds (parse () () (? #t) (1 2 3) r ctx) (r ctx))
            '(((1 2 3) ())))
(test-equal (judgment-holds (parse ((x #f)) () (? (&& x #t)) (1 2 3) r ctx) (r ctx))
            '((⊥ ((x #f)))))
(test-equal (judgment-holds (parse ((x #t)) () (? (&& x #t)) (1 2 3) r ctx) (r ctx))
            '(((1 2 3) ((x #t)))))

(test-equal (judgment-holds (parse () () (= x (• 1 (• 2 (• 3 4)))) (1 2 3 4 5) r ctx) (r ctx))
            '(((5) ((x "\u0001\u0002\u0003\u0004")))))
(test-equal (judgment-holds (parse () () (= x (• 1 (• 2 (• 3 4)))) (1 2 3 5 5) r ctx) (r ctx))
            '((⊥ ())))

;factorial
(test-equal (judgment-holds (parse () ((S () (n f) (• (N (0) (n)) (F (n) (f))))
                                       (N ((type:integer n)) (n) (/ (• 0 (N ((* 10 n)) (n)))
                                                     (/ (• 1 (N ((+ (* 10 n) 1)) (n)))
                                                        (/ (• 2 (N ((+ (* 10 n) 2)) (n)))
                                                           (/ (• 3 (N ((+ (* 10 n) 3)) (n)))
                                                              (/ (• 4 (N ((+ (* 10 n) 4)) (n)))
                                                                 (/ (• 5 (N ((+ (* 10 n) 5)) (n)))
                                                                    (/ (• 6 (N ((+ (* 10 n) 6)) (n)))
                                                                       (/ (• 7 (N ((+ (* 10 n) 7)) (n)))
                                                                          (/ (• 8 (N ((+ (* 10 n) 8)) (n)))
                                                                             (/ (• 9 (N ((+ (* 10 n) 9)) (n))) ε)))))))))))
                                       (F ((type:integer f)) (f) (/ (• (? (== f 0)) ((← f 1))) (• (F ((- f 1)) (y)) ((← f (* f y)))))))
                                   (S () (number factorial)) (6) r ctx) (r ctx))
            '((() ((number 6) (factorial 720)))))

;palindrome
(test-equal (judgment-holds (parse () ((S () (is-it-a-palindrome?) (/ (• (! (! (T (0) (size)))) (! (! (P (size) (is-it-a-palindrome?))))) ((← is-it-a-palindrome? "it's not a palindrome"))))
                                       (T ((type:integer size)) (size) (/ (• (/ 0 1) (T ((+ size 1)) (size))) ε))
                                       (P ((type:integer size)) (is-it-a-palindrome?) (/ (• (/ (? (== size 0)) (• (? (== size 1)) (/ 0 1))) ((← is-it-a-palindrome? "it's a palindrome")))
                                                                       (/ (• 0 (• (P ((- size 2)) (is-it-a-palindrome?)) 0))
                                                                          (• 1 (• (P ((- size 2)) (is-it-a-palindrome?)) 1))))))
                                   (S () (is-it-a-palindrome)) (0 0  0 1 1 0 1 1 0 0 0) r ctx) (r ctx))
            '(((0 0 0 1 1 0 1 1 0 0 0) ((is-it-a-palindrome "it's a palindrome")))))

(test-equal (judgment-holds (parse () ((S () (is-it-a-palindrome?) (/ (• (! (! (T (0) (size)))) (! (! (P (size) (is-it-a-palindrome?))))) ((← is-it-a-palindrome? "it's not a palindrome"))))
                                       (T ((type:integer size)) (size) (/ (• (/ 0 1) (T ((+ size 1)) (size))) ε))
                                       (P ((type:integer size)) (is-it-a-palindrome?) (/ (• (/ (? (== size 0)) (• (? (== size 1)) (/ 0 1))) ((← is-it-a-palindrome? "it's a palindrome")))
                                                                       (/ (• 0 (• (P ((- size 2)) (is-it-a-palindrome?)) 0))
                                                                          (• 1 (• (P ((- size 2)) (is-it-a-palindrome?)) 1))))))
                                   (S () (is-it-a-palindrome)) (1 1 0 1 0 0 0 0 1 0 1 1) r ctx) (r ctx))
            '(((1 1 0 1 0 0 0 0 1 0 1 1) ((is-it-a-palindrome "it's a palindrome")))))

(test-equal (judgment-holds (parse () ((S () (is-it-a-palindrome?) (/ (• (! (! (T (0) (size)))) (! (! (P (size) (is-it-a-palindrome?))))) ((← is-it-a-palindrome? "it's not a palindrome"))))
                                       (T ((type:integer size)) (size) (/ (• (/ 0 1) (T ((+ size 1)) (size))) ε))
                                       (P ((type:integer size)) (is-it-a-palindrome?) (/ (• (/ (? (== size 0)) (• (? (== size 1)) (/ 0 1))) ((← is-it-a-palindrome? "it's a palindrome")))
                                                                       (/ (• 0 (• (P ((- size 2)) (is-it-a-palindrome?)) 0))
                                                                          (• 1 (• (P ((- size 2)) (is-it-a-palindrome?)) 1))))))
                                   (S () (is-it-a-palindrome)) (1 1 0 1 1 0 0 0 1 0 1 1) r ctx) (r ctx))
            '(((1 1 0 1 1 0 0 0 1 0 1 1) ((is-it-a-palindrome "it's not a palindrome")))))

;fibonacci
(test-equal (judgment-holds (parse () ((S () (number list) (• (N (0) (number)) (F (0 number nil) (list))))
                                       (N ((type:integer n)) (n) (/ (• 0 (N ((* 10 n)) (n)))
                                                     (/ (• 1 (N ((+ (* 10 n) 1)) (n)))
                                                        (/ (• 2 (N ((+ (* 10 n) 2)) (n)))
                                                           (/ (• 3 (N ((+ (* 10 n) 3)) (n)))
                                                              (/ (• 4 (N ((+ (* 10 n) 4)) (n)))
                                                                 (/ (• 5 (N ((+ (* 10 n) 5)) (n)))
                                                                    (/ (• 6 (N ((+ (* 10 n) 6)) (n)))
                                                                       (/ (• 7 (N ((+ (* 10 n) 7)) (n)))
                                                                          (/ (• 8 (N ((+ (* 10 n) 8)) (n)))
                                                                             (/ (• 9 (N ((+ (* 10 n) 9)) (n))) ε)))))))))))
                                       (F ((type:integer count) (type:integer number) ((: type:integer) list)) (list) (/ (• (? (> number count))
                                                                           (/ (• (? (== count 0)) (F ((+ count 1) number list) (list)))
                                                                              (/ (• (? (== count 1)) (F ((+ count 1) number (: 0 nil)) (list)))
                                                                                 (/ (• (? (== count 2)) (F ((+ count 1) number (: 0 1)) (list)))
                                                                                    (• (? (== count 3)) (G ((+ count 1) number (: list 1)) (list))))))) ε))
                                       (G ((type:integer count) (type:integer number) ((: type:integer) list)) (list) (/ (• (? (> number count)) (G ((+ count 1) number (: list (+ (tail list) (tail (head list))))) (list))) ε)))
                                   (S () (number list)) (2 2) r ctx) (r ctx))
            '((()
               ((number 22)
                (list (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: 0 1) 1) 2) 3) 5) 8) 13) 21) 34) 55) 89) 144) 233) 377) 610) 987) 1597) 2584) 4181) 6765))))))

(test-equal (judgment-holds (parse () ((S () () (* (A () ())))
                                       (A () () 1))
                                   (= numbers-one (S () ())) (1 1 1 1 1 1 1 1 1 2 3 4 5 4 3 2 1) r ctx) (r ctx))
            '(((2 3 4 5 4 3 2 1) ((numbers-one "\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001")))))

(test-equal (judgment-holds (parse () ((S ((type:integer count)) (count) (* (A (count) (count))))
                                       (A ((type:integer count)) ((+ count 1)) 1))
                                   (= numbers-one (S (0) (count))) (1 1 1 1 1 1 1 1 1 2 3 4 5 4 3 2 1) r ctx) (r ctx))
            '(((2 3 4 5 4 3 2 1) ((count 9) (numbers-one "\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001")))))

(test-equal (judgment-holds (parse () ((S ((type:integer count)) (count) (* (A (count) (count))))
                                       (A ((type:integer count)) ((+ count 1)) 1))
                                   (= count (S (0) (count))) (1 1 1 1 1 1 1 1 1 2 3 4 5 4 3 2 1) r ctx) (r ctx))
            '(((2 3 4 5 4 3 2 1) ((count "\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001")))))

(test-equal (judgment-holds (parse () () (= numbers-one (* 1)) (1 1 1 1 1 1 1 1 1 2 3 4 5 4 3 2 1) r ctx) (r ctx))
            '(((2 3 4 5 4 3 2 1) ((numbers-one "\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001")))))

(test-equal (judgment-holds (parse () () (• (= one 1) (• (= two 2) (• (= three 3) (• (= four 4) (= five 5))))) (1 2 3 4 5) r ctx) (r ctx))
            '((() ((one "\u0001") (two "\u0002") (three "\u0003") (four "\u0004") (five "\u0005")))))


(test-results)