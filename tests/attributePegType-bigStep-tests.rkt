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
#;(test-equal (judgment-holds (typesPeg () () (/ (• ((← v-boolean (&& (|| (== 24 38) (> 71 56)) (¬ (|| #f #t)))) (← v-integer (+ 3 (÷ 4 5))) (← v-real (- 7.5 (* 3.14159 2.0)))) (= v-string (* 1))) ((← v-boolean #t) (← v-string "anything"))) ctx) ctx)
            '(((v-boolean type:boolean) (v-string type:string))))

;NOT
(test-equal (judgment-holds (typesPeg () () (! (• 1 ((← x #f)))) ctx) ctx)
            '(((x type:boolean))))

;REPRETITION
(test-equal (judgment-holds (typesPeg () () (* (• 1 ((← x "anything")))) ctx) ctx)
            '(((x type:string))))

;BIND
#;(test-equal (judgment-holds (typesPeg () () (= v-string ((← v-real 3.14159) (← v-integer 144) (← v-boolean #t))) ctx) ctx)
            '(((v-real type:real) (v-integer type:integer) (v-boolean type:boolean) (v-string type:string))))

;NON-TERMINAL
#;(test-equal (judgment-holds (typesPeg ((S (→ (type:integer type:string) (type:real type:boolean))))
                            ((S ((type:integer i) (type:string s))
                                (3.14159 #t) 1))
                            (= consumed (S (30 "racket") (pi valid))) ctx) ctx)
            '(((S (→ (type:integer type:string) (type:real type:boolean))) (pi type:real) (valid type:boolean) (consumed type:string))))

#;(test-equal (judgment-holds (typesPeg ((S (→ (type:real type:string) (type:real type:boolean))))
                            ((S ((type:integer i) (type:string s))
                                (3.14159 #t) 1))
                            (= consumed (S (7.5 "racket") (pi valid))) ctx) ctx)
            '())

#;(test-equal (judgment-holds (typesPeg ((S (→ (type:integer type:string) (type:real type:boolean))) (consumed type:string))
                            ((S ((type:integer i) (type:string s))
                                (3.14159 #t) 1))
                            (= consumed (S (30 "racket") (pi valid))) ctx) ctx)
            '(((S (→ (type:integer type:string) (type:real type:boolean))) (consumed type:string) (pi type:real) (valid type:boolean))))

#;(test-equal (judgment-holds (typesPeg ((S (→ (type:integer type:string) (type:real type:boolean))) (consumed type:real))
                            ((S ((type:integer i) (type:string s))
                                (3.14159 #t) 1))
                            (= consumed (S (30 "racket") (pi valid))) ctx) ctx)
            '())

;UPDATE
#;(test-equal (judgment-holds (typesPeg () () ((← v-boolean #t) (← v-integer 1) (← v-real 3.14159) (← v-string "anything")) ctx) ctx)
            '(((v-boolean type:boolean) (v-integer type:integer) (v-real type:real) (v-string type:string))))

#;(test-equal (judgment-holds (typesPeg ((v-string type:string)) () ((← v-boolean #t) (← v-integer 1) (← v-real 3.14159) (← v-string "anything")) ctx) ctx)
            '(((v-string type:string) (v-boolean type:boolean) (v-integer type:integer) (v-real type:real))))

#;(test-equal (judgment-holds (typesPeg ((v-string type:boolean)) () ((← v-boolean #t) (← v-integer 1) (← v-real 3.14159) (← v-string "anything")) ctx) ctx)
            '())

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

#;(test-equal (judgment-holds (typesPeg ((S (→ (type:string) (type:boolean))) (A (→ (type:boolean) (type:real))))
                                      ((S ((type:string anything)) ((&& #t (¬ #f))) 1) (A ((type:boolean anything)) (3.14159) 1))
                                      (• (S ("anything") (v-boolean)) (• (A (v-boolean) (v-real)) (? (== v-real 2.718281828)))) ctx) ctx)
            '(((S (→ (type:string) (type:boolean))) (A (→ (type:boolean) (type:real))) (v-boolean type:boolean) (v-real type:real))))


(test-results)