#lang racket

(require redex
         "../attributePegType-bigStep.rkt")

;TERMINAL
(test-equal (judgment-holds (typesPeg () () 1 ctx) ctx)
            '(()))

;SEQUENCE
(test-equal (judgment-holds (typesPeg () () (• (= test (* 1)) ((← x (+ 3 4)))) ctx) ctx)
            '(((test type:string) (x type:integer))))

;CHOICE
(test-equal (judgment-holds (typesPeg () () (/ (• 0 ((← x 0))) (• 1 ((← x 1)))) ctx) ctx)
            '(((x type:integer))))

(test-equal (judgment-holds (typesPeg () () (/ (• 0 ((← x 0))) (• 1 ((← x #t)))) ctx) ctx)
            '())

(test-equal (judgment-holds (typesPeg () () (/ (• 0 ((← x 0))) 1) ctx) ctx)
            '())

;NEGATION
(test-equal (judgment-holds (typesPeg () () (! (• 1 ((← x #f)))) ctx) ctx)
            '())

(test-equal (judgment-holds (typesPeg () () (! (! (• 1 ((← x #f))))) ctx) ctx)
            '(((x type:boolean))))

;REPRETITION
(test-equal (judgment-holds (typesPeg () () (* (/ (• 0 ((← x "even"))) (• 1 ((← x "odd"))))) ctx) ctx)
            '())

(test-equal (judgment-holds (typesPeg () () (• ((← number 0)) (* (/ (• 0 ((← number (* number 2)))) (• 1 ((← number (+ (* number 2) 1))))))) ctx) ctx)
            '(((number type:integer))))

;BIND
(test-equal (judgment-holds (typesPeg () () (= x (• (• 1 2) ((← x "two") (← y 5) (← z (== y 5))))) ctx) ctx)
            '(((x type:string) (y type:integer) (z type:boolean))))

(test-equal (judgment-holds (typesPeg () () (= x (• (• 1 2) ((← y 5) (← z (== y 5))))) ctx) ctx)
            '(((y type:integer) (z type:boolean) (x type:string))))

(test-equal (judgment-holds (typesPeg () () (• ((← x "two")) (= x (• (• 1 2) ((← y 5) (← z (== y 5)))))) ctx) ctx)
            '(((x type:string) (y type:integer) (z type:boolean))))

;NON-TERMINAL
(test-equal (judgment-holds (typesPeg ((S (→ (type:integer type:string) (type:boolean))))
                                      ((S ((type:integer x) (type:string y))
                                          ((> x z))
                                          (• ((← z 0)) (* (/ (• 0 ((← z (* 2 z)))) (• 1 ((← z (+ (* 2 z) 1)))))))))
                                      (= y (S (30 "anything") (result))) ctx) ctx)
            '(((S (→ (type:integer type:string) (type:boolean))) (result type:boolean) (y type:string))))

;UPDATE
(test-equal (judgment-holds (typesPeg () () ((← x 1) (← x (+ x 5))) ctx) ctx)
            '(((x type:integer))))

;MIX
(test-equal (judgment-holds (typesPeg () () (• ((← v-boolean (&& #t (¬ #f)))) (? v-boolean)) ctx) ctx)
            '(((v-boolean type:boolean))))

(test-equal (judgment-holds (typesPeg () () (• (? v-boolean) ((← v-boolean (&& #t (¬ #f))))) ctx) ctx)
            '())

(test-equal (judgment-holds (typesPeg ((S (→ (type:string) (type:boolean))))
                                      ((S ((type:string anything)) ((&& #t (¬ #f))) 1)) (• (S ("anything") (v-boolean)) (? v-boolean)) ctx) ctx)
            '(((S (→ (type:string) (type:boolean))) (v-boolean type:boolean))))

(test-equal (judgment-holds (typesPeg ((S (→ (type:string) (type:boolean))))
                                      ((S ((type:string anything)) ((&& #t (¬ #f))) 1)) (• (? v-boolean) (S ("anything") (v-boolean))) ctx) ctx)
            '())

(test-equal (judgment-holds (typesPeg ((A (→ () (type:integer))))
                                      ((A () (n) (/ (• 0 ((← n 0))) (• 1 ((← n 1))))))
                                      (!(!(A () (number)))) ctx) ctx)
            '(((A (→ () (type:integer))) (number type:integer))))

(test-equal (judgment-holds (typesPeg ((A (→ (type:integer) (type:integer))))
                                      ((A ((type:integer count1)) (count1) (/ (• 1 (A ((+ count1 1)) (count1))) ε)))
                                      (• (!(!(A (0) (x)))) (• ((← number 1)) (* (• 1 ((← number (* number x))))))) ctx) ctx)
            '(((A (→ (type:integer) (type:integer))) (x type:integer) (number type:integer))))

(test-equal (judgment-holds (typesPeg () () (* (/ (• 0 ((← odd #f))) (• 1 ((← odd #t))))) ctx) ctx)
            '())

(test-equal (judgment-holds (typesPeg ((A (→ () (type:integer))))
                                      ((A () (number) (/ (• 0 ((← number 0))) (• 1 ((← number 1))))))
                                      (!(!(A () (number)))) ctx) ctx)
            '(((A (→ () (type:integer))) (number type:integer))))

(test-equal (judgment-holds (typesPeg () () (• (/ (• 1 ((← x 1))) (• 2 ((← x "two")))) ((← x #t))) ctx) ctx)
            '())

(test-results)