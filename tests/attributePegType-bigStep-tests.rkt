#lang racket


(require redex
         "../attributePegType-bigStep.rkt")


;TERMINAL
(test-equal (judgment-holds (typesPeg () () 1 Γ) Γ)
            '(()))

;SEQUENCE
(test-equal (judgment-holds (typesPeg () () (• (= test (* 1)) ((← x (+ 3 4)))) Γ) Γ)
            '(((test String) (x Integer))))

;CHOICE
(test-equal (judgment-holds (typesPeg () () (/ (• 0 ((← x 0))) (• 1 ((← x 1)))) Γ) Γ)
            '(((x Integer))))

(test-equal (judgment-holds (typesPeg () () (/ (• 0 ((← x 0))) (• 1 ((← x #t)))) Γ) Γ)
            '())

(test-equal (judgment-holds (typesPeg () () (/ (• 0 ((← x 0))) 1) Γ) Γ)
            '())

;NEGATION
(test-equal (judgment-holds (typesPeg () () (! (• 1 ((← x #f)))) Γ) Γ)
            '())

(test-equal (judgment-holds (typesPeg () () (! (! (• 1 ((← x #f))))) Γ) Γ)
            '(((x Bool))))

;REPRETITION
(test-equal (judgment-holds (typesPeg () () (* (/ (• 0 ((← x "even"))) (• 1 ((← x "odd"))))) Γ) Γ)
            '())

(test-equal (judgment-holds (typesPeg () () (• ((← number 0)) (* (/ (• 0 ((← number (× number 2)))) (• 1 ((← number (+ (× number 2) 1))))))) Γ) Γ)
            '(((number Integer))))

;BIND
(test-equal (judgment-holds (typesPeg () () (= x (• (• 1 2) ((← x "two") (← y 5) (← z (== y 5))))) Γ) Γ)
            '(((x String) (y Integer) (z Bool))))

(test-equal (judgment-holds (typesPeg () () (= x (• (• 1 2) ((← y 5) (← z (== y 5))))) Γ) Γ)
            '(((y Integer) (z Bool) (x String))))

(test-equal (judgment-holds (typesPeg () () (• ((← x "two")) (= x (• (• 1 2) ((← y 5) (← z (== y 5)))))) Γ) Γ)
            '(((x String) (y Integer) (z Bool))))

;NON-TERMINAL
(test-equal (judgment-holds (typesPeg ((S (→ (Integer String) (Bool))))
                                      ((S ((Integer x) (String y))
                                          ((> x z))
                                          (• ((← z 0)) (* (/ (• 0 ((← z (× 2 z)))) (• 1 ((← z (+ (× 2 z) 1)))))))))
                                      (= y (S (30 "anything") (result))) Γ) Γ)
            '(((S (→ (Integer String) (Bool))) (result Bool) (y String))))

;UPDATE
(test-equal (judgment-holds (typesPeg () () ((← x 1) (← x (+ x 5))) Γ) Γ)
            '(((x Integer))))

;MIX
(test-equal (judgment-holds (typesPeg () () (• ((← v-boolean (∧ #t (¬ #f)))) (? v-boolean)) Γ) Γ)
            '(((v-boolean Bool))))

(test-equal (judgment-holds (typesPeg () () (• (? v-boolean) ((← v-boolean (∧ #t (¬ #f))))) Γ) Γ)
            '())

(test-equal (judgment-holds (typesPeg ((S (→ (String) (Bool))))
                                      ((S ((String anything)) ((∧ #t (¬ #f))) 1)) (• (S ("anything") (v-boolean)) (? v-boolean)) Γ) Γ)
            '(((S (→ (String) (Bool))) (v-boolean Bool))))

(test-equal (judgment-holds (typesPeg ((S (→ (String) (Bool))))
                                      ((S ((String anything)) ((∧ #t (¬ #f))) 1)) (• (? v-boolean) (S ("anything") (v-boolean))) Γ) Γ)
            '())

(test-equal (judgment-holds (typesPeg ((A (→ () (Integer))))
                                      ((A () (n) (/ (• 0 ((← n 0))) (• 1 ((← n 1))))))
                                      (!(!(A () (number)))) Γ) Γ)
            '(((A (→ () (Integer))) (number Integer))))

(test-equal (judgment-holds (typesPeg ((A (→ (Integer) (Integer))))
                                      ((A ((Integer count1)) (count1) (/ (• 1 (A ((+ count1 1)) (count1))) ε)))
                                      (• (!(!(A (0) (x)))) (• ((← number 1)) (* (• 1 ((← number (× number x))))))) Γ) Γ)
            '(((A (→ (Integer) (Integer))) (x Integer) (number Integer))))

(test-equal (judgment-holds (typesPeg () () (* (/ (• 0 ((← odd #f))) (• 1 ((← odd #t))))) Γ) Γ)
            '())

(test-equal (judgment-holds (typesPeg ((A (→ () (Integer))))
                                      ((A () (number) (/ (• 0 ((← number 0))) (• 1 ((← number 1))))))
                                      (!(!(A () (number)))) Γ) Γ)
            '(((A (→ () (Integer))) (number Integer))))

(test-equal (judgment-holds (typesPeg () () (• (/ (• 1 ((← x 1))) (• 2 ((← x "two")))) ((← x #t))) Γ) Γ)
            '())

(test-results)