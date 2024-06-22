#lang racket

(require redex
         "../attributePegType-bigStep.rkt")

;TERMINAL
(test-equal (judgment-holds (typesPeg () () 1 Γ) Γ)
            '(()))

;SEQUENCE
(test-equal (judgment-holds (typesPeg () () (• (= test (* 1)) ((← x (+ 3 4)))) Γ) Γ)
            '(((test type:string) (x type:integer))))

;CHOICE
(test-equal (judgment-holds (typesPeg () () (/ (• 0 ((← x 0))) (• 1 ((← x 1)))) Γ) Γ)
            '(((x type:integer))))

(test-equal (judgment-holds (typesPeg () () (/ (• 0 ((← x 0))) (• 1 ((← x #t)))) Γ) Γ)
            '())

(test-equal (judgment-holds (typesPeg () () (/ (• 0 ((← x 0))) 1) Γ) Γ)
            '())

;NEGATION
(test-equal (judgment-holds (typesPeg () () (! (• 1 ((← x #f)))) Γ) Γ)
            '())

(test-equal (judgment-holds (typesPeg () () (! (! (• 1 ((← x #f))))) Γ) Γ)
            '(((x type:boolean))))

;REPRETITION
(test-equal (judgment-holds (typesPeg () () (* (/ (• 0 ((← x "even"))) (• 1 ((← x "odd"))))) Γ) Γ)
            '())

(test-equal (judgment-holds (typesPeg () () (• ((← number 0)) (* (/ (• 0 ((← number (* number 2)))) (• 1 ((← number (+ (* number 2) 1))))))) Γ) Γ)
            '(((number type:integer))))

;BIND
(test-equal (judgment-holds (typesPeg () () (= x (• (• 1 2) ((← x "two") (← y 5) (← z (== y 5))))) Γ) Γ)
            '(((x type:string) (y type:integer) (z type:boolean))))

(test-equal (judgment-holds (typesPeg () () (= x (• (• 1 2) ((← y 5) (← z (== y 5))))) Γ) Γ)
            '(((y type:integer) (z type:boolean) (x type:string))))

(test-equal (judgment-holds (typesPeg () () (• ((← x "two")) (= x (• (• 1 2) ((← y 5) (← z (== y 5)))))) Γ) Γ)
            '(((x type:string) (y type:integer) (z type:boolean))))

;NON-TERMINAL
(test-equal (judgment-holds (typesPeg ((S (→ (type:integer type:string) (type:boolean))))
                                      ((S ((type:integer x) (type:string y))
                                          ((> x z))
                                          (• ((← z 0)) (* (/ (• 0 ((← z (* 2 z)))) (• 1 ((← z (+ (* 2 z) 1)))))))))
                                      (= y (S (30 "anything") (result))) Γ) Γ)
            '(((S (→ (type:integer type:string) (type:boolean))) (result type:boolean) (y type:string))))

;UPDATE
(test-equal (judgment-holds (typesPeg () () ((← x 1) (← x (+ x 5))) Γ) Γ)
            '(((x type:integer))))

;MIX
(test-equal (judgment-holds (typesPeg () () (• ((← v-boolean (&& #t (¬ #f)))) (? v-boolean)) Γ) Γ)
            '(((v-boolean type:boolean))))

(test-equal (judgment-holds (typesPeg () () (• (? v-boolean) ((← v-boolean (&& #t (¬ #f))))) Γ) Γ)
            '())

(test-equal (judgment-holds (typesPeg ((S (→ (type:string) (type:boolean))))
                                      ((S ((type:string anything)) ((&& #t (¬ #f))) 1)) (• (S ("anything") (v-boolean)) (? v-boolean)) Γ) Γ)
            '(((S (→ (type:string) (type:boolean))) (v-boolean type:boolean))))

(test-equal (judgment-holds (typesPeg ((S (→ (type:string) (type:boolean))))
                                      ((S ((type:string anything)) ((&& #t (¬ #f))) 1)) (• (? v-boolean) (S ("anything") (v-boolean))) Γ) Γ)
            '())

(test-equal (judgment-holds (typesPeg ((A (→ () (type:integer))))
                                      ((A () (n) (/ (• 0 ((← n 0))) (• 1 ((← n 1))))))
                                      (!(!(A () (number)))) Γ) Γ)
            '(((A (→ () (type:integer))) (number type:integer))))

(test-equal (judgment-holds (typesPeg ((A (→ (type:integer) (type:integer))))
                                      ((A ((type:integer count1)) (count1) (/ (• 1 (A ((+ count1 1)) (count1))) ε)))
                                      (• (!(!(A (0) (x)))) (• ((← number 1)) (* (• 1 ((← number (* number x))))))) Γ) Γ)
            '(((A (→ (type:integer) (type:integer))) (x type:integer) (number type:integer))))

(test-equal (judgment-holds (typesPeg () () (* (/ (• 0 ((← odd #f))) (• 1 ((← odd #t))))) Γ) Γ)
            '())

(test-equal (judgment-holds (typesPeg ((A (→ () (type:integer))))
                                      ((A () (number) (/ (• 0 ((← number 0))) (• 1 ((← number 1))))))
                                      (!(!(A () (number)))) Γ) Γ)
            '(((A (→ () (type:integer))) (number type:integer))))

(test-equal (judgment-holds (typesPeg () () (• (/ (• 1 ((← x 1))) (• 2 ((← x "two")))) ((← x #t))) Γ) Γ)
            '())

(test-results)