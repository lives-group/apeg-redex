#lang racket
(require redex
         "attributePeg-syntax.rkt")
(provide (all-defined-out))


#;(reduction-steps-cutoff 100)


(define-extended-language ctxAttributePeg vAttributePeg
  [C ::= (/ h p)
     (/ p h)
     (• h p)
     (• p h)
     (* h)
     (! h)
     (? h)
     (= x h s)
     (h p p p)
     (p h p p)
     (p p h p)
     attribution]
  [attribution ::= (AS ((← x expr) ...))]
  [AS ::= ▽ □ △]
  [BS ::= ▽ □]
  [E ::= (+ E expr)
     (+ value E)
     (* E expr)
     (* value E)
     (- E expr)
     (- value E)
     (÷ E expr)
     (÷ value E)
     (&& E expr)
     (&& value E)
     (|| E expr)
     (|| value E)
     (¬ E)
     (== E expr)
     (== value E)
     (> E expr)
     (> value E)
     (get E expr)
     (get value E)
     (put E expr expr)
     (put value E expr)
     (put value string E)
     (: E expr)
     (: value E)
     (head E)
     (tail E)
     (? E)
     ((← x E) (← x expr) ...)
     hole]
  [value ::= integer
         boolean
         string
         (⇒ ((string_!_ value) ...))
         (: value value)
         nil]
  [dir ::= □ ↓ ↑]
  [s ::= (natural ...)]
  [R ::= ⊥ ⊤]
  [state ::= ((G ctx ...) ⊢ (C ...) p dir s s R s)])


(define reduction
  (reduction-relation
   ctxAttributePeg
   #:domain state


   ;Terminal
   [--> ((G ctx ...) ⊢ (C ...) natural_1 ↓ (natural_1 natural_2 ...) (natural_3 ...) R (natural_4 natural_5 ...))
        ((G ctx ...) ⊢ (C ...) natural_1 ↑ (natural_2 ...) (natural_3 ... natural_1) ⊤ (,(+ (term natural_4) 1) natural_5 ...))
        "↓Terminal-success"]
   [--> ((G ctx ...) ⊢ (C ...) natural_1 ↓ (natural_2 natural_3 ...) s_1 R s_2)
        ((G ctx ...) ⊢ (C ...) natural_1 ↑ (natural_2 natural_3 ...) s_1 ⊥ s_2)
        (side-condition (term (dismatch? natural_1 natural_2)))
        "↓¬Terminal-dismatch"]
   [--> ((G ctx ...) ⊢ (C ...) natural ↓ () s_1 R s_2)
        ((G ctx ...) ⊢ (C ...) natural ↑ () s_1 ⊥ s_2)
        "↓¬Terminal-void"]


   ;Empty
   [--> ((G ctx ...) ⊢ (C ...) ε ↓ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) ε ↑ s_1 s_2 ⊤ s_3)
        "↓Empty"]


   ;Choice
   [--> ((G ctx_1 ctx_2 ...) ⊢ (C ...) (/ p_1 p_2) ↓ s_1	s_2 R (natural ...))
        ((G ctx_1 ctx_1 ctx_2 ...) ⊢ ((/ h p_2) C ...) p_1 ↓ s_1 s_2 R (0 natural ...))
        "↓Choice"]
   [--> ((G ctx_1 ctx_2 ctx_3 ...) ⊢ ((/ h p_2) C ...) p_1 ↑ s_1 s_2 ⊤ (natural_1 natural_2 natural_3 ...))
        ((G ctx_1 ctx_3 ...) ⊢ (C ...) (/ p_1 p_2) ↑ s_1 s_2 ⊤ (,(+ (term natural_1) (term natural_2)) natural_3 ...))
        "↑Choice_1"]
   [--> ((G ctx_1 ctx_2 ctx_3 ...) ⊢ ((/ h p_2) C ...) p_1 ↑ (natural_1 ...) (natural_2 ... natural_3) ⊥ (natural_4 natural_5 ...))
        ((G ctx_1 ctx_2 ctx_3 ...) ⊢ ((/ p_1 h) C ...) p_2 ↓ (natural_3 natural_1 ...) (natural_2 ...) ⊥ (,(- (term natural_4) 1) natural_5 ...))
        (side-condition (term (isnotzero? natural_4)))
        "↑¬Choice_1-restore"]
   [--> ((G ctx_1 ctx_2 ctx_3 ...) ⊢ ((/ h p_2) C ...) p_1 ↑ s_1 s_2 ⊥ (0 natural ...))
        ((G ctx_2 ctx_2 ctx_3 ...) ⊢ ((/ p_1 h) C ...) p_2 ↓ s_1 s_2 ⊥ (0 natural ...))
        "↑¬Choice_1"]
   #;[--> ((G ctx_1 ctx_2 ctx_3 ...) ⊢ ((/ p_1 h) C ...) p_2 ↑ s_1 s_2 ⊤ (natural_1 natural_2 natural_3 ...))
        ((G ctx_1 ctx_3 ...) ⊢ (C ...) (/ p_1 p_2) ↑ s_1 s_2 ⊤ (,(+ (term natural_1) (term natural_2)) natural_3 ...))
        "↑Choice_2"]
   #;[--> ((G ctx_1 ctx_2 ctx_3 ...) ⊢ ((/ p_1 h) C ...) p_2 ↑ (natural_1 ...) (natural_2 ... natural_3) ⊥ (natural_4 natural_5 ...))
        ((G ctx_1 ctx_2 ctx_3 ...) ⊢ ((/ p_1 h) C ...) p_2 ↑ (natural_3 natural_1 ...) (natural_2 ...) ⊥ (,(- (term natural_4) 1) natural_5 ...))
        (side-condition (term (isnotzero? natural_4)))
        "↑¬Choice_2-restore"]
   #;[--> ((G ctx_1 ctx_2 ctx_3 ...) ⊢ ((/ p_1 h) C ...) p_2 ↑ s_1 s_2 ⊥ (0 natural ...))
        ((G ctx_2 ctx_3 ...) ⊢ (C ...) (/ p_1 p_2) ↑ s_1 s_2 ⊥ (natural ...))
        "↑¬Choice_2"]
   [--> ((G ctx_1 ctx_2 ctx_3 ...) ⊢ ((/ p_1 h) C ...) p_2 ↑ s_1 s_2 R (natural_1 natural_2 natural_3 ...))
        ((G ctx_1 ctx_3 ...) ⊢ (C ...) (/ p_1 p_2) ↑ s_1 s_2 R (,(+ (term natural_1) (term natural_2)) natural_3 ...))
        "↑Choice_2"]


   ;Sequence
   [--> ((G ctx ...) ⊢ (C ...) (• p_1 p_2) ↓ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ ((• h p_2) C ...) p_1 ↓ s_1 s_2 R s_3)
        "↓Sequence"]
   [--> ((G ctx ...) ⊢ ((• h p_2) C ...) p_1 ↑ s_1 s_2 ⊤ s_3)
        ((G ctx ...) ⊢ ((• p_1 h) C ...) p_2 ↓ s_1 s_2 ⊤ s_3)
        "↑Sequence_1"]
   [--> ((G ctx ...) ⊢ ((• h p_2) C ...) p_1 ↑ s_1 s_2 ⊥ s_3)
        ((G ctx ...) ⊢ (C ...) (• p_1 p_2) ↑ s_1 s_2 ⊥ s_3)
        "↑¬Sequence_1"]
   [--> ((G ctx ...) ⊢ ((• p_1 h) C ...) p_2 ↑ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (• p_1 p_2) ↑ s_1 s_2 R s_3)
        "↑Sequence_2 & ↑¬Sequence_2"]


   ;Repetition
   [--> ((G ctx_1 ctx_2 ...) ⊢ (C ...) (* p) ↓ s_1 s_2 R (natural ...))
        ((G ctx_1 ctx_1 ctx_2 ...) ⊢ ((* h) C ...) p ↓ s_1 s_2 R (0 natural ...))
        "↓Repetition"]
   [--> ((G ctx_1 ctx_2 ctx_3 ...) ⊢ ((* h) C ...) p ↑ s_1 s_2 ⊤ (natural_1 natural_2 natural_3 ...))
        ((G ctx_1 ctx_1 ctx_3 ...) ⊢ ((* h) C ...) p ↓ s_1 s_2 ⊤ (0 ,(+ (term natural_1) (term natural_2)) natural_3 ...))
        "↑Repetition"]
   [--> ((G ctx_1 ctx_2 ctx_3 ...) ⊢ ((* h) C ...) p ↑ s_1 s_2 ⊥ (0 natural ...))
        ((G ctx_2 ctx_3 ...) ⊢ (C ...) (* p) ↑ s_1 s_2 ⊤ (natural ...))
        "↑¬Repetition"]
   [--> ((G ctx_1 ctx_2 ctx_3 ...) ⊢ ((* h) C ...) p ↑ (natural_1 ...) (natural_2 ... natural_3) ⊥ (natural_4 natural_5 ...))
        ((G ctx_1 ctx_2 ctx_3 ...) ⊢ ((* h) C ...) p ↑ (natural_3 natural_1 ...) (natural_2 ...) ⊥ (,(- (term natural_4) 1) natural_5 ...))
        (side-condition (term (isnotzero? natural_4)))
        "↑¬Repetition-restore"]


   ;Not
   [--> ((G ctx ...) ⊢ (C ...) (! p) ↓ s_1 s_2 R (natural ...))
        ((G ctx ...) ⊢ ((! h) C ...) p ↓ s_1 s_2 R (0 natural ...))
        "↓Not"]
   [--> ((G ctx ...) ⊢ ((! h) C ...) p ↑ s_1 s_2 ⊥ (0 natural ...))
        ((G ctx ...) ⊢ (C ...) (! p) ↑ s_1 s_2 ⊤ (natural ...))
        "↑Not"]
   [--> ((G ctx ...) ⊢ ((! h) C ...) p ↑ s_1 s_2 ⊤ (0 natural ...))
        ((G ctx ...) ⊢ (C ...) (! p) ↑ s_1 s_2 ⊥ (natural ...))
        "↑¬Not"]
   [--> ((G ctx ...) ⊢ ((! h) C ...) p ↑ (natural_1 ...) (natural_2 ... natural_3) R (natural_4 natural_5 ...))
        ((G ctx ...) ⊢ ((! h) C ...) p ↑ (natural_3 natural_1 ...) (natural_2 ...) R (,(- (term natural_4) 1) natural_5 ...))
        (side-condition (term (isnotzero? natural_4)))
        "↑Not-restore & ↑¬Not-restore"]


   ;Restriction
   [--> ((G ctx ...) ⊢ (C ...) (? expr) ↓ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (? expr) □ s_1 s_2 R s_3)
        "↓Restriction"]
   [--> ((G ctx ...) ⊢ (C ...) (? #t) □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (? #t) ↑ s_1 s_2 ⊤ s_3)
        "↑Restriction"]
   [--> ((G ctx ...) ⊢ (C ...) (? #f) □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (? #f) ↑ s_1 s_2 ⊥ s_3)
        "↑¬Restriction"]


   ;Bind
   [--> ((G ctx ...) ⊢ (C ...) (= x p) ↓ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ ((= x h s_1) C ...) p ↓ s_1 s_2 R s_3)
        "↓Bind"]
   [--> ((G ((x_1 value_1) ...) ctx ...) ⊢ ((= x h (natural_consumed ... natural_rest ...)) C ...) p ↑ (natural_rest ...) s_2 ⊤ s_3)
        ((G ((x_1 value_1) ... (x ,(list->string (map integer->char (term (natural_consumed ...)))))) ctx ...) ⊢ (C ...) (= x p) ↑ (natural_rest ...) s_2 ⊤ s_3)
        (side-condition (term (attribute∋ctx (x_1 ...) x)))
        "↑Bind-declare"]
   [--> ((G ((x_1 value_1) ... (x value) (x_2 value_2) ...) ctx ...) ⊢ ((= x h (natural_consumed ... natural_rest ...)) C ...) p ↑ (natural_rest ...) s_2 ⊤ s_3)
        ((G ((x_1 value_1) ... (x ,(list->string (map integer->char (term (natural_consumed ...))))) (x_2 value_2) ...) ctx ...) ⊢ (C ...) (= x p) ↑ (natural_rest ...) s_2 ⊤ s_3)
        "↑Bind-update"]
   [--> ((G ctx ...) ⊢ ((= x h s_1) C ...) p ↑ s_2 s_3 ⊥ s_4)
        ((G ctx ...) ⊢ (C ...) (= x p) ↑ s_2 s_3 ⊥ s_4)
        "↑¬Bind"]


   ;Nonterminal
   [--> (((NT_1 ... (x_1 ((type x_2) ...) (expr_1 ...) p_2) NT_2 ...) ctx ...) ⊢ (C ...) (x_1 (expr_2 ...) (x_3 ...)) ↓ s_1 s_2 R s_3)
        (((NT_1 ... (x_1 ((type x_2) ...) (expr_1 ...) p_2) NT_2 ...) () ctx ...) ⊢ ((h p_2 p_3 (x_1 (expr_2 ...) (x_3 ...))) C ...) p_1 ↓ s_1 s_2 R s_3)
        (where (p_1 p_2 p_3) (NT->ppp (x_1 ((type x_2) ...) (expr_1 ...) p_2) (x_1 (expr_2 ...) (x_3 ...))))
        "↓Nonterminal"]
   [--> ((G ctx ...) ⊢ ((h p_2 p_3 p_4) C ...) p_1 ↑ s_1 s_2 ⊤ s_3)
        ((G ctx ...) ⊢ ((p_1 h p_3 p_4) C ...) p_2 ↓ s_1 s_2 ⊤ s_3)
        "↑Nonterminal-▽"]
   [--> ((G ctx ...) ⊢ ((p_1 h p_3 p_4) C ...) p_2 ↑ s_1 s_2 ⊤ s_3)
        ((G ctx ...) ⊢ ((p_1 p_2 h p_4) C ...) p_3 ↓ s_1 s_2 ⊤ s_3)
        "↑Nonterminal"]
   [--> ((G ctx_1 ctx_2 ...) ⊢ ((p_1 h p_3 p_4) C ...) p_2 ↑ s_1 s_2 ⊥ s_3)
        ((G ctx_2 ...) ⊢ (C ...) p_4 ↑ s_1 s_2 ⊥ s_3)
        "↑¬Nonterminal"]
   [--> ((G ctx_1 ctx_2 ...) ⊢ ((p_1 p_2 h p_4) C ...) p_3 ↑ s_1 s_2 ⊤ s_3)
        ((G ctx_2 ...) ⊢ (C ...) p_4 ↑ s_1 s_2 ⊤ s_3)
        "↑Nonterminal-△"]


   ;Attribution
   [--> ((G ctx ...) ⊢ ((h p_1 p_2 p_3) C ...) ((← x expr) ...) ↓ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ ((▽ ((← x expr) ...)) (h p_1 p_2 p_3) C ...) ((← x expr) ...) ↓ s_1 s_2 R s_3)
        "↓Attribution-▽"]
   [--> ((G ctx ...) ⊢ (C ...) ((← x expr) ...) ↓ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ ((□ ((← x expr) ...)) C ...) ((← x expr) ...) ↓ s_1 s_2 R s_3)
        (side-condition (term (simplesattribution? C ...)))
        "↓Attribution-□"]
   [--> ((G ctx ...) ⊢ ((p_1 p_2 h p_3) C ...) ((← x expr) ...) ↓ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ ((△ ((← x expr) ...)) (p_1 p_2 h p_3) C ...) ((← x expr) ...) ↓ s_1 s_2 R s_3)
        "↓Attribution-△"]
   [--> ((G ctx ...) ⊢ ((AS p) C ...) () ↓ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) p ↑ s_1 s_2 ⊤ s_3)
        "↓Attribution-void-↓"]
   [--> ((G ctx ...) ⊢ ((AS p) C ...) () □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) p ↑ s_1 s_2 ⊤ s_3)
        "↓Attribution-void-□"]
   [--> ((G ctx ...) ⊢ ((AS p) C ...) ((← x_2 expr_1) (← x_3 expr_2) ...) ↓ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ ((AS p) C ...) ((← x_2 expr_1) (← x_3 expr_2) ...) □ s_1 s_2 R s_3)
        "↓Attribution"]
   [--> ((G ((x_1 value_1) ...) ctx ...) ⊢ ((BS p) C ...) ((← x_3 value_3) (← x_4 expr) ...) □ s_1 s_2 R s_3)
        ((G ((x_1 value_1) ... (x_3 value_3)) ctx ...) ⊢ ((BS p) C ...) ((← x_4 expr) ...) □ s_1 s_2 R s_3)
        (side-condition (term (attribute∋ctx (x_1 ...) x_3)))
        "↑Attribution-▽-declare & ↑Attribution-□-declare"]
   [--> ((G ((x_1 value_1) ... (x_2 value_2) (x_3 value_3) ...) ctx ...) ⊢ ((BS p) C ...) ((← x_2 value_5) (← x_5 expr) ...) □ s_1 s_2 R s_3)
        ((G ((x_1 value_1) ... (x_2 value_5) (x_3 value_3) ...) ctx ...) ⊢ ((BS p) C ...) ((← x_5 expr) ...) □ s_1 s_2 R s_3)
        "↑Attribution-▽-update & ↑Attribution-□-update"]
   [--> ((G ctx_1 ((x_1 value_1) ...) ctx_2 ...) ⊢ ((△ p) C ...) ((← x_3 value_3) (← x_4 expr) ...) □ s_1 s_2 R s_3)
        ((G ctx_1 ((x_1 value_1) ... (x_3 value_3)) ctx_2 ...) ⊢ ((△ p) C ...) ((← x_4 expr) ...) □ s_1 s_2 R s_3)
        (side-condition (term (attribute∋ctx (x_1 ...) x_3)))
        "↑Attribution-△-declare"]
   [--> ((G ctx_1 ((x_1 value_1) ... (x_2 value_2) (x_3 value_3) ...) ctx_2 ...) ⊢ ((△ p) C ...) ((← x_2 value_5) (← x_5 expr) ...) □ s_1 s_2 R s_3)
        ((G ctx_1 ((x_1 value_1) ... (x_2 value_5) (x_3 value_3) ...) ctx_2 ...) ⊢ ((△ p) C ...) ((← x_5 expr) ...) □ s_1 s_2 R s_3)
        "↑Attribution-△-update"]


   ;Expression
   [--> ((G ctx ...) ⊢ (C ...) (in-hole E (+ integer_1 integer_2)) □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (in-hole E ,(+ (term integer_1) (term integer_2))) □ s_1 s_2 R s_3)
        "addition"]
   [--> ((G ctx ...) ⊢ (C ...) (in-hole E (* integer_1 integer_2)) □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (in-hole E ,(* (term integer_1) (term integer_2))) □ s_1 s_2 R s_3)
        "multiplication"]
   [--> ((G ctx ...) ⊢ (C ...) (in-hole E (- integer_1 integer_2)) □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (in-hole E ,(- (term integer_1) (term integer_2))) □ s_1 s_2 R s_3)
        "subtraction"]
   [--> ((G ctx ...) ⊢ (C ...) (in-hole E (÷ integer_1 integer_2)) □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (in-hole E ,(quotient (term integer_1) (term integer_2))) □ s_1 s_2 R s_3)
        "division"]
   [--> ((G ctx ...) ⊢ (C ...) (in-hole E (&& #t expr)) □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (in-hole E expr) □ s_1 s_2 R s_3)
        "and-first-success"]
   [--> ((G ctx ...) ⊢ (C ...) (in-hole E (&& #f expr)) □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (in-hole E #f) □ s_1 s_2 R s_3)
        "and-first-fail"]
   [--> ((G ctx ...) ⊢ (C ...) (in-hole E (|| #t expr)) □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (in-hole E #t) □ s_1 s_2 R s_3)
        "or-first-success"]
   [--> ((G ctx ...) ⊢ (C ...) (in-hole E (|| #f expr)) □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (in-hole E expr) □ s_1 s_2 R s_3)
        "or-first-fail"]
   [--> ((G ctx ...) ⊢ (C ...) (in-hole E (¬ boolean)) □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (in-hole E ,(not (term boolean))) □ s_1 s_2 R s_3)
        "not"]
   [--> ((G ctx ...) ⊢ (C ...) (in-hole E (== integer_1 integer_2)) □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (in-hole E ,(= (term integer_1) (term integer_2))) □ s_1 s_2 R s_3)
        "equality"]
   [--> ((G ctx ...) ⊢ (C ...) (in-hole E (> integer_1 integer_2)) □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (in-hole E ,(> (term integer_1) (term integer_2))) □ s_1 s_2 R s_3)
        "less-than"]
   [--> ((G ctx ...) ⊢ (C ...) (in-hole E (get (⇒ ((string_!_ value) ...)) string)) □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (in-hole E (apply-get (⇒ ((string_!_ value) ...)) string)) □ s_1 s_2 R s_3)
        "get"]
   [--> ((G ctx ...) ⊢ (C ...) (in-hole E (put (⇒ ((string_!_ value_1) ...)) string value)) □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (in-hole E (apply-put (⇒ ((string_!_ value_1) ...)) string value)) □ s_1 s_2 R s_3)
        "put"]
   [--> ((G ctx ...) ⊢ (C ...) (in-hole E (head (: value_1 value_2))) □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (in-hole E value_1) □ s_1 s_2 R s_3)
        "head"]
   [--> ((G ctx ...) ⊢ (C ...) (in-hole E (tail (: value_1 value_2))) □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (in-hole E value_2) □ s_1 s_2 R s_3)
        "tail"]
   [--> ((G ctx ...) ⊢ (C ...) (in-hole E (⇒ ((expr_1 expr_2) ...))) □ s_1 s_2 R s_3)
        ((G ctx ...) ⊢ (C ...) (in-hole E (apply-mapping (expr_1 expr_2) ...)) □ s_1 s_2 R s_3)
        (side-condition (term (isnotvalue? (expr_1 expr_2) ...)))
        "mapping"]
   [--> ((G ((x_1 value_1) ... (x value) (x_2 value_2) ...) ctx ...) ⊢ (C ...) (in-hole E x) □ s_1 s_2 R s_3)
        ((G ((x_1 value_1) ... (x value) (x_2 value_2) ...) ctx ...) ⊢ (C ...) (in-hole E value) □ s_1 s_2 R s_3)
        (side-condition (term (not▽ C ...)))
        "variable"]
   [--> ((G ctx_1 ((x_1 value_1) ... (x value) (x_2 value_2) ...) ctx_2 ...) ⊢ ((▽ p) C ...) (in-hole E x) □ s_1 s_2 R s_3)
        ((G ctx_1 ((x_1 value_1) ... (x value) (x_2 value_2) ...) ctx_2 ...) ⊢ ((▽ p) C ...) (in-hole E value) □ s_1 s_2 R s_3)
        "parameter"]))


(define-metafunction ctxAttributePeg
  dismatch? : natural natural -> boolean
  [(dismatch? natural natural) #f]
  [(dismatch? natural_1 natural_2) #t])


(define-metafunction ctxAttributePeg
  isnotzero? : natural -> boolean
  [(isnotzero? 0) #f]
  [(isnotzero? natural) #t])


(define-metafunction ctxAttributePeg
  apply-get : (⇒ ((string_!_ value) ...)) string -> value
  [(apply-get (⇒ ((string_1 value_1) ... (string_2 value_2) (string_3 value_3) ...)) string_2) value_2])


(define-metafunction ctxAttributePeg
  apply-put : (⇒ ((string_!_ value) ...)) string value -> (⇒ ((string_!_ value) ...))
  [(apply-put (⇒ ((string_1 value_1) ... (string_input value_old) (string_2 value_2) ...)) string_input value_new) (⇒ ((string_1 value_1) ... (string_input value_new) (string_2 value_2) ...))]
  [(apply-put (⇒ ((string value) ...)) string_new value_new) (⇒ ((string value) ... (string_new value_new)))])


(define-metafunction ctxAttributePeg
  apply-mapping : (expr expr) ... -> expr
  [(apply-mapping (expr_key expr_value) (expr_1 expr_2) ...) (put expr_key expr_value (apply-mapping (expr_1 expr_2) ...))]
  [(apply-mapping) (⇒ ())])


(define-metafunction ctxAttributePeg
  isnotvalue? : expr -> boolean
  [(isnotvalue? value) #f]
  [(isnotvalue? expr) #t])


(define-metafunction ctxAttributePeg
  attribute∋ctx : (x ...) x -> boolean
  [(attribute∋ctx (x_1 ... x x_2 ...) x) #f]
  [(attribute∋ctx (x_1 ...) x_2) #t])


(define-metafunction ctxAttributePeg
  simplesattribution? : C ... -> boolean
  [(simplesattribution? attribution C ...) #f]
  [(simplesattribution? (h p_1 p_2 p_3) C ...) #f]
  [(simplesattribution? (p_1 p_2 h p_3) C ...) #f]
  [(simplesattribution? C ...) #t])


(define-metafunction ctxAttributePeg
  NT->ppp : NT (x (expr ...) (x ...)) -> (p p p)
  [(NT->ppp (x_1 ((type x_2) ...) (expr_1 ...) p) (x_3 (expr_2 ...) (x_4 ...))) (((← x_2 expr_2) ...) p ((← x_4 expr_1) ...))])


(define-metafunction ctxAttributePeg
  not▽ : C ... -> boolean
  [(not▽ (▽ p) C ...) #f]
  [(not▽ C ...) #t])


;Terminal
#;(apply-reduction-relation reduction (term ((() ()) ⊢ () 1 ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(apply-reduction-relation reduction (term ((() ()) ⊢ () 1 ↓ (2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(apply-reduction-relation reduction (term ((() ()) ⊢ () 1 ↓ () (5 6 7) ⊤ (1 2 3))))


;Empty
#;(traces reduction (term ((() ()) ⊢ () ε ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))


;Choice
#;(traces reduction (term ((() ()) ⊢ () (/ 1 2) ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(traces reduction (term ((() ()) ⊢ () (/ 2 1) ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(traces reduction (term ((() ()) ⊢ () (/ 2 3) ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))


;Sequence
#;(traces reduction (term ((() ()) ⊢ () (• 1 2) ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(traces reduction (term ((() ()) ⊢ () (• 2 1) ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(traces reduction (term ((() ()) ⊢ () (• 1 3) ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))


;Repetition
#;(traces reduction (term ((() ()) ⊢ () (* 1) ↓ (1 1 1 1 1 1 2 3 4) (5 6 7) ⊤ (1 2 3))))


;Not
#;(traces reduction (term ((() ()) ⊢ () (! (* 1)) ↓ (1 1 1 1 1 1 2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(traces reduction (term ((() ()) ⊢ () (! 2) ↓ (1 1 1 1 1 1 2 3 4) (5 6 7) ⊤ (1 2 3))))


;Restriction
#;(traces reduction (term ((() ()) ⊢ () (• (* 1) (• (? (&& #t (¬ (|| (== 2 4) (¬ #f))))) 2)) ↓ (1 1 1 1 1 1 2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(traces reduction (term ((() ()) ⊢ () (• (* 1) (• (? (&& #t (¬ (|| (== 2 4) (¬ #t))))) 2)) ↓ (1 1 1 1 1 1 2 3 4) (5 6 7) ⊤ (1 2 3))))


;Bind
#;(traces reduction (term ((() ()) ⊢ () (= x (• 1 (• 2 3))) ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(traces reduction (term ((() ((x "anything"))) ⊢ () (= x (• 1 (• 2 3))) ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(traces reduction (term ((() ()) ⊢ () (= x (• 1 (• 2 4))) ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))



;Nonterminal
#;(apply-reduction-relation* reduction (term ((((S () (n (÷ (- n r) 6) r) (RESTO/000-011 (0 0) (n r)))
                                                (RESTO/000-011 ((type:integer n) (type:integer r)) (n r) (/ (• 0 (RESTO/000-011 ((* 2 n) 0) (n r))) (/ (• 1 (RESTO/001-100 ((+ (* 2 n) 1) 1) (n r))) ε)))
                                                (RESTO/001-100 ((type:integer n) (type:integer r)) (n r) (/ (• 0 (RESTO/010-101 ((* 2 n) 2) (n r))) (/ (• 1 (RESTO/000-011 ((+ (* 2 n) 1) 3) (n r))) ε)))
                                                (RESTO/010-101 ((type:integer n) (type:integer r)) (n r) (/ (• 0 (RESTO/001-100 ((* 2 n) 4) (n r))) (/ (• 1 (RESTO/010-101 ((+ (* 2 n) 1) 5) (n r))) ε))))
                                               ()) ⊢ () (S () (number quotient rest)) ↓ (1 0 1 0 0 1 1) () ⊤ (0))))
	; number = 83, quotient = 13, rest = 5
#;(apply-reduction-relation* reduction (term ((((S () (x) (/ (addition () (x)) (/ (subtraction () (x)) (/ (multiplication () (x)) (/ (division () (x)) (number (0) (x)))))))
                                                (addition () ((+ x y)) (• (++ () ()) (• (S () (x)) (• (space () ()) (number (0) (y))))))
                                                (subtraction () ((- x y)) (• (-- () ()) (• (S () (x)) (• (space () ()) (number (0) (y))))))
                                                (multiplication () ((* x y)) (• (×× () ()) (• (S () (x)) (• (space () ()) (number (0) (y))))))
                                                (division () ((÷ x y)) (• (÷÷ () ()) (• (S () (x)) (• (space () ()) (number (0) (y))))))
                                                (number ((type:integer x)) (x) (/ (• 0 (number ((* 2 x)) (x))) (/ (• 1 (number ((+ (* 2 x) 1)) (x))) ε)))
                                                (++ () () 1100)
                                                (-- () () 1101)
                                                (×× () () 1110)
                                                (÷÷ () () 1111)
                                                (space () () 1000)) ()) ⊢ () (S () (result)) ↓ (1111 1110 1101 1100 1 0 1 0 1000 1 0 1 1000 1 0 0 1000 1 1 0 1000 1 0) () ⊤ (0))))
	; result = 33
#;(apply-reduction-relation* reduction (term ((((S () (number list) (• (N (0) (number)) (F (0 number nil) (list))))
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
                                                (G ((type:integer count) (type:integer number) ((: type:integer) list)) (list) (/ (• (? (> number count)) (G ((+ count 1) number (: list (+ (tail list) (tail (head list))))) (list))) ε))) ()) ⊢ () (S () (number list)) ↓ (2 2) () ⊤ (0))))
	; number = 22
	; list = (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: 0 1) 1) 2) 3) 5) 8) 13) 21) 34) 55) 89) 144) 233) 377) 610) 987) 1597) 2584) 4181) 6765)
#;(apply-reduction-relation* reduction (term ((((S () (is-it-a-palindrome?) (/ (• (! (! (T (0) (size)))) (! (! (P (size) (is-it-a-palindrome?))))) ((← is-it-a-palindrome? "it's not a palindrome"))))
                                                (T ((type:integer size)) (size) (/ (• (/ 0 1) (T ((+ size 1)) (size))) ε))
                                                (P ((type:integer size)) (is-it-a-palindrome?) (/ (• (/ (? (== size 0)) (• (? (== size 1)) (/ 0 1))) ((← is-it-a-palindrome? "it's a palindrome")))
                                                                                                  (/ (• 0 (• (P ((- size 2)) (is-it-a-palindrome?)) 0))
                                                                                                     (• 1 (• (P ((- size 2)) (is-it-a-palindrome?)) 1)))))) ()) ⊢ () (S () (is-it-a-palindrome)) ↓ (1 1 0 1 0 0 0 0 1 0 1 1) () ⊤ (0))))
	; is-it-a-palindrome = "it's a palindrome"