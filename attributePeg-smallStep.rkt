#lang racket


(require redex
         "attributePeg-syntax.rkt")


(provide (all-defined-out))


#;(reduction-steps-cutoff 100)


(define-extended-language ctxAttributePeg AttributePeg
  [C C1 C2 ::= (/ □ p)
     (/ p □)
     (• □ p)
     (• p □)
     (* □)
     (! □)
     (? □ e)
     (= ϑ □ w)
     (□ p p p)
     (p □ p p)
     (p p □ p)
     (▽ (← ϑ e) ...)
     (⊙ (← ϑ e) ...)
     (△ (← ϑ e) ...)]
  [E ::= (+ E e)
     (+ v E)
     (× E e)
     (× v E)
     (- E e)
     (- v E)
     (÷ E e)
     (÷ v E)
     (∧ E e)
     (∨ E e)
     (¬ E)
     (== E e)
     (== v E)
     (> E e)
     (> v E)
     (get E e)
     (get v E)
     (put E e e)
     (put v E e)
     (put v s E)
     (: E e)
     (: v E)
     (head E)
     (tail E)
     (? E)
     ((← ϑ E) (← ϑ e) ...)
     hole]
  [UT ::= ▽ ⊙ △]
  [D ::= ↓ □ ↑]
  [w w1 w2 w3 ::= (t ...)]
  [Δs ::= (Δ ...)]
  [Cs ::= (C ...)]
  [ns ::= (n ...)]
  [R ::= ⊥ ⊤]
  [state ::= ((G Δs) ⊢ Cs p D w w R ns)])


(define reduction
  (reduction-relation
   ctxAttributePeg
   #:domain state

   ;Terminal
   [--> ((G Δs) ⊢ Cs t ↓ (t t1 ...) (t2 ...) R (n1 n ...))
        ((G Δs) ⊢ Cs t ↑ (t1 ...) (t2 ... t) ⊤ (n2 n ...))
        (where n2 ,(+ (term n1) 1))
        "↓Termina l"]
   [--> ((G Δs) ⊢ Cs t1 ↓ (t2 t ...) w R ns)
        ((G Δs) ⊢ Cs t1 ↑ (t2 t ...) w ⊥ ns)
        (where (t_!_ t_!_) (t1 t2))
        "↓¬Terminal 1"]
   [--> ((G Δs) ⊢ Cs t ↓ () w R ns)
        ((G Δs) ⊢ Cs t ↑ () w ⊥ ns)
        "↓¬Terminal 2"]

   ;Empty
   [--> ((G Δs) ⊢ Cs ε ↓ w1 w2 R ns)
        ((G Δs) ⊢ Cs ε ↑ w1 w2 ⊤ ns)
        "↓Empty"]

   ;Choice
   [--> ((G [Δ1 Δ ...]) ⊢ (C ...) (/ p1 p2) ↓ w1 w2 R (n ...))
        ((G [Δ1 Δ1 Δ ...]) ⊢ ((/ □ p2) C ...) p1 ↓ w1 w2 R (0 n ...))
        "↓Choice"]

   [--> ((G [Δ1 Δ2 Δ ...]) ⊢ ((/ □ p2) C ...) p1 ↑ w1 w2 ⊤ (n1 n2 n ...))
        ((G [Δ1 Δ ...]) ⊢ (C ...) p1 ↑ w1 w2 ⊤ (m n ...))
        (where m ,(+ (term n1) (term n2)))
        "↑Choice 1"]

   [--> ((G Δs) ⊢ ((/ □ p2) C ...) p1 ↑ (t1 ...) (t2 ... t) ⊥ (n1 n ...))
        ((G Δs) ⊢ ((/ □ p2) C ...) p1 ↑ (t t1 ...) (t2 ...) ⊥ (n2 n ...))
        (side-condition (term (difference n1 0)))
        (where n2 ,(- (term n1) 1))
        "Restore Choice"]

   [--> ((G [Δ1 Δ2 Δ ...]) ⊢ ((/ □ p2) C ...) p1 ↑ w1 w2 ⊥ (0 n ...))
        ((G [Δ2 Δ2 Δ ...]) ⊢ ((/ p1 □) C ...) p2 ↓ w1 w2 ⊥ (0 n ...))
        "↑¬Choice 1"]

   [--> ((G [Δ1 Δ2 Δ ...]) ⊢ ((/ p1 □) C ...) p2 ↑ w1 w2 ⊤ (n1 n2 n ...))
        ((G [Δ1 Δ ...]) ⊢ (C ...) (/ p1 p2) ↑ w1 w2 ⊤ (m n ...))
        (where m ,(+ (term n1) (term n2)))
        "↑Choice 2"]

   [--> ((G [Δ1 Δ ...]) ⊢ ((/ p1 □) C ...) p2 ↑ w1 w2 ⊥ (n1 n2 n ...))
        ((G [Δ ...]) ⊢ (C ...) (/ p1 p2) ↑ w1 w2 ⊥ (m n ...))
        (where m ,(+ (term n1) (term n2)))
        "↑¬Choice 2"]

   ;Sequence
   [--> ((G [Δ1 Δ ...]) ⊢ (C ...) (• p1 p2) ↓ w1 w2 R ns)
        ((G [Δ1 Δ1 Δ ...]) ⊢ ((• □ p2) C ...) p1 ↓ w1 w2 R ns)
        "↓Sequence"]

   [--> ((G Δs) ⊢ ((• □ p2) C ...) p1 ↑ w1 w2 ⊤ ns)
        ((G Δs) ⊢ ((• p1 □) C ...) p2 ↓ w1 w2 ⊤ ns)
        "↑Sequence 1"]

   [--> ((G [Δ1 Δ ...]) ⊢ ((• □ p2) C ...) p1 ↑ w1 w2 ⊥ ns)
        ((G [Δ ...]) ⊢ (C ...) (• p1 p2) ↑ w1 w2 ⊥ ns)
        "↑¬Sequence 1"]

   [--> ((G [Δ1 Δ2 Δ ...]) ⊢ ((• p1 □) C ...) p2 ↑ w1 w2 ⊤ ns)
        ((G [Δ1 Δ ...]) ⊢ (C ...) (• p1 p2) ↑ w1 w2 ⊤ ns)
        "↑Sequence 2"]

   [--> ((G [Δ1 Δ ...]) ⊢ ((• p1 □) C ...) p2 ↑ w1 w2 ⊥ ns)
        ((G [Δ ...]) ⊢ (C ...) (• p1 p2) ↑ w1 w2 ⊥ ns)
        "↑¬Sequence 2"]

   ;Repetition
   [--> ((G [Δ1 Δ ...]) ⊢ (C ...) (* p) ↓ w1 w2 R (n ...))
        ((G [Δ1 Δ1 Δ ...]) ⊢ ((* □) C ...) p ↓ w1 w2 R (0 n ...))
        "↓Repetition"]

   [--> ((G [Δ1 Δ2 Δ ...]) ⊢ ((* □) C ...) p ↑ w1 w2 ⊤ (n1 n2 n ...))
        ((G [Δ1 Δ1 Δ ...]) ⊢ ((* □) C ...) p ↓ w1 w2 ⊤ (0 m n ...))
        (where m ,(+ (term n1) (term n2)))
        "↑Repetition"]

   [--> ((G Δs) ⊢ ((* □) C ...) p ↑ w1 w2 ⊥ (n1 n ...))
        ((G Δs) ⊢ ((* □) C ...) p ↑ w1 w2 ⊥ (n2 n ...))
        (side-condition (term (difference n1 0)))
        (where n2 ,(- (term n1) 1))
        "Restore Repetition"]

   [--> ((G [Δ1 Δ ...]) ⊢ ((* □) C ...) p ↑ w1 w2 ⊥ (0 n ...))
        ((G [Δ ...]) ⊢ (C ...) (* p) ↑ w1 w2 ⊤ (n ...))
        "↑¬Repetition"]

   ;Negation
   [--> ((G Δs) ⊢ (C ...) (! p) ↓ w1 w2 R (n ...))
        ((G Δs) ⊢ ((! □) C ...) p ↓ w1 w2 R (0 n ...))
        "↓Negation"]

   [--> ((G Δs) ⊢ ((! □) C ...) p ↑ (t1 ...) (t2 ... t) ⊥ (n1 n ...))
        ((G Δs) ⊢ ((! □) C ...) p ↑ (t t1 ...) (t2 ...) ⊥ (n2 n ...))
        (side-condition (term (difference n1 0)))
        (where n2 ,(- (term n1) 1))
        "Restore Negation"]

   [--> ((G Δs) ⊢ ((! □) C ...) p ↑ w1 w2 ⊥ (0 n ...))
        ((G Δs) ⊢ (C ...) (! p) ↑ w1 w2 ⊤ (n ...))
        "↑Negation"]

   [--> ((G Δs) ⊢ ((! □) C ...) p ↑ w1 w2 ⊤ (n1 n2 n ...))
        ((G Δs) ⊢ (C ...) (! p) ↑ w1 w2 ⊥ (m n ...))
        (where m ,(+ (term n1) (term n2)))
        "↑¬Negation"]

   ;Restrition
   [--> ((G Δs) ⊢ (C ...) (? e) ↓ w1 w2 R ns)
        ((G Δs) ⊢ ((? □ e) C ...) (? e) □ w1 w2 R ns)
        "↓Restrition"]

   [--> ((G Δs) ⊢ ((? □ e) C ...) (? #t) □ w1 w2 R ns)
        ((G Δs) ⊢ (C ...) (? e) ↑ w1 w2 ⊤ ns)
        "↑Restrition"]

   [--> ((G Δs) ⊢ ((? □ e) C ...) (? #f) □ w1 w2 R ns)
        ((G Δs) ⊢ (C ...) (? e) ↑ w1 w2 ⊥ ns)
        "↑¬Restrition"]

   ;Bind
   [--> ((G [Δ1 Δ ...]) ⊢ (C ...) (= ϑ p) ↓ w1 w2 R ns)
        ((G [Δ1 Δ1 Δ ...]) ⊢ ((= ϑ □ w1) C ...) p ↓ w1 w2 R ns)
        "↓Bind"]

   [--> ((G [Δ1 Δ2 Δ ...]) ⊢ ((= ϑ □ w) C ...) p ↑ w1 w2 ⊤ ns)
        ((G [(assign Δ1 ϑ s) Δ ...]) ⊢ (C ...) (= ϑ p) ↑ w1 w2 ⊤ ns)
        (where (t ... t1 ...) w)
        (where (t1 ...) w1)
        (where s ,(list->string (map integer->char (term (t ...)))))
        "↑Bind"]

   [--> ((G [Δ1 Δ ...]) ⊢ ((= ϑ □ w) C ...) p ↑ w1 w2 ⊥ ns)
        ((G [Δ ...]) ⊢ (C ...) (= ϑ p) ↑ w1 w2 ⊥ ns)
        "↑¬Bind"]

   ;Nonterminal
   [--> ((G [Δ ...]) ⊢ (C ...) (NT (e1 ...) (ϑ2 ...)) ↓ w1 w2 R ns)
        ((G [() Δ ...]) ⊢ (C1 C ...) p1 ↓ w1 w2 R ns)
        (where [_ ... (NT ((_ ϑ1) ...) (e2 ...) p2) _ ...] G)
        (where p1 ((← ϑ1 e1) ...))
        (where p3 ((← ϑ2 e2) ...))
        (where C1 (□ p2 p3 (NT (e1 ...) (ϑ2 ...))))
        "↓Nonterminal"]

   [--> ((G Δs) ⊢ ((□ p2 p3 p) C ...) p1 ↑ w1 w2 ⊤ ns)
        ((G Δs) ⊢ ((p1 □ p3 p) C ...) p2 ↓ w1 w2 ⊤ ns)
        "↑Nonterminal▽"]

   [--> ((G Δs) ⊢ ((p1 □ p3 p) C ...) p2 ↑ w1 w2 ⊤ ns)
        ((G Δs) ⊢ ((p1 p2 □ p) C ...) p3 ↓ w1 w2 ⊤ ns)
        "↑Nonterminal⊙"]

   [--> ((G [Δ1 Δ ...]) ⊢ ((p1 □ p3 p) C ...) p2 ↑ w1 w2 ⊥ ns)
        ((G [Δ ...]) ⊢ (C ...) p ↑ w1 w2 ⊥ ns)
        "↑¬Nonterminal⊙"]

   [--> ((G [Δ1 Δ ...]) ⊢ ((p1 p2 □ p) C ...) p3 ↑ w1 w2 ⊤ ns)
        ((G [Δ ...]) ⊢ (C ...) p ↑ w1 w2 ⊤ ns)
        "↑Nonterminal△"]

   ;Update
   [--> ((G Δs) ⊢ ((□ p2 p3 p) C ...) ((← ϑ e) ...) ↓ w1 w2 R ns)
        ((G Δs) ⊢ ((▽ (← ϑ e) ...) (□ p2 p3 p) C ...) ((← ϑ e) ...) □ w1 w2 R ns)
        "↓Update▽"]

   [--> ((G Δs) ⊢ (C ...) ((← ϑ e) ...) ↓ w1 w2 R ns)
        ((G Δs) ⊢ ((⊙ (← ϑ e) ...) C ...) ((← ϑ e) ...) □ w1 w2 R ns)
        (side-condition (term (not-▽-△ (C ...))))
        "↓Update⊙"]

   [--> ((G Δs) ⊢ ((p1 p2 □ p) C ...) ((← ϑ e) ...) ↓ w1 w2 R ns)
        ((G Δs) ⊢ ((△ (← ϑ e) ...) (p1 p2 □ p) C ...) ((← ϑ e) ...) □ w1 w2 R ns)
        "↓Update△"]

   [--> ((G [Δ1 Δ ...]) ⊢ ((UT (← ϑ1 e1) ...) C ...) ((← ϑ v) (← ϑ2 e2) ...) □ w1 w2 R ns)
        ((G [Δ2 Δ ...]) ⊢ ((UT (← ϑ1 e1) ...) C ...) ((← ϑ2 e2) ...) □ w1 w2 R ns)
        (side-condition (term (it-is-▽-⊙ UT)))
        (where Δ2 (assign Δ1 ϑ v))
        "↑Update▽ 1 & ↑Update⊙ 1"]

   [--> ((G [Δ1 Δ2 Δ ...]) ⊢ (C1 C ...) ((← ϑ v) (← ϑ2 e2) ...) □ w1 w2 R ns)
        ((G [Δ1 (assign Δ2 ϑ v) Δ ...]) ⊢ (C1 C ...) ((← ϑ2 e2) ...) □ w1 w2 R ns)
        (where (△ _ ...) C1)
        "↑Update△ 1"]

   [--> ((G Δs) ⊢ ((UT (← ϑ e) ...) C ...) () □ w1 w2 R ns)
        ((G Δs) ⊢ (C ...) ((← ϑ e) ...) ↑ w1 w2 ⊤ ns)
        "↑Update▽ 2 & ↑Update⊙ 2 & ↑Update△ 2"]

   ;Expression
   [--> ((G Δs) ⊢ Cs (in-hole E (+ i1 i2)) □ w1 w2 R ns)
        ((G Δs) ⊢ Cs (in-hole E i) □ w1 w2 R ns)
        (where i ,(+ (term i1) (term i2)))
        "Addition"]
   
   [--> ((G Δs) ⊢ Cs (in-hole E (× i1 i2)) □ w1 w2 R ns)
        ((G Δs) ⊢ Cs (in-hole E i) □ w1 w2 R ns)
        (where i ,(* (term i1) (term i2)))
        "Multiplication"]
   
   [--> ((G Δs) ⊢ Cs (in-hole E (- i1 i2)) □ w1 w2 R ns)
        ((G Δs) ⊢ Cs (in-hole E i) □ w1 w2 R ns)
        (where i ,(- (term i1) (term i2)))
        "Subtraction"]
   
   [--> ((G Δs) ⊢ Cs (in-hole E (÷ i1 i2)) □ w1 w2 R ns)
        ((G Δs) ⊢ Cs (in-hole E i) □ w1 w2 R ns)
        (where i ,(quotient (term i1) (term i2)))
        "Division"]
   
   [--> ((G Δs) ⊢ Cs (in-hole E (∧ #t e)) □ w1 w2 R ns)
        ((G Δs) ⊢ Cs (in-hole E e) □ w1 w2 R ns)
        "And 1"]
   
   [--> ((G Δs) ⊢ Cs (in-hole E (∧ #f e)) □ w1 w2 R ns)
        ((G Δs) ⊢ Cs (in-hole E #f) □ w1 w2 R ns)
        "¬And 1"]
   
   [--> ((G Δs) ⊢ Cs (in-hole E (∨ #t e)) □ w1 w2 R ns)
        ((G Δs) ⊢ Cs (in-hole E #t) □ w1 w2 R ns)
        "Or 1"]
   
   [--> ((G Δs) ⊢ Cs (in-hole E (∨ #f e)) □ w1 w2 R ns)
        ((G Δs) ⊢ Cs (in-hole E e) □ w1 w2 R ns)
        "¬Or 1"]
   
   [--> ((G Δs) ⊢ Cs (in-hole E (¬ #f)) □ w1 w2 R ns)
        ((G Δs) ⊢ Cs (in-hole E #t) □ w1 w2 R ns)
        "Not"]
   
   [--> ((G Δs) ⊢ Cs (in-hole E (¬ #t)) □ w1 w2 R ns)
        ((G Δs) ⊢ Cs (in-hole E #f) □ w1 w2 R ns)
        "¬Not"]
   
   [--> ((G Δs) ⊢ Cs (in-hole E (== i1 i2)) □ w1 w2 R ns)
        ((G Δs) ⊢ Cs (in-hole E b) □ w1 w2 R ns)
        (where b ,(= (term i1) (term i2)))
        "Equality"]
   
   [--> ((G Δs) ⊢ Cs (in-hole E (> i1 i2)) □ w1 w2 R ns)
        ((G Δs) ⊢ Cs (in-hole E b) □ w1 w2 R ns)
        (where b ,(> (term i1) (term i2)))
        "Bigger Than"]
   
   [--> ((G Δs) ⊢ Cs (in-hole E (⇒ (s v) ...)) □ w1 w2 R ns)
        ((G Δs) ⊢ Cs (in-hole E e) □ w1 w2 R ns)
        (side-condition (term (it-is-not-a-value (⇒ (s v) ...))))
        (where e (apply-map (⇒ (s v) ...)))
        "Map"]
   
   [--> ((G Δs) ⊢ Cs (in-hole E (get v s_key)) □ w1 w2 R ns)
        ((G Δs) ⊢ Cs (in-hole E v_value) □ w1 w2 R ns)
        [where (⇒ _ ... (s_key v_value) _ ...) v]
        "Get"]
   
   [--> ((G Δs) ⊢ Cs (in-hole E (put v1 s_key v_value)) □ w1 w2 R ns)
        ((G Δs) ⊢ Cs (in-hole E v2) □ w1 w2 R ns)
        (where (⇒ ((s_!_ v) ...)) v1)
        (where v2 (apply-put (⇒ ((s_!_ v) ...)) s_key v_value))
        "Put"]
   
   [--> ((G Δs) ⊢ Cs (in-hole E (head (: v1 v2))) □ w1 w2 R ns)
        ((G Δs) ⊢ Cs (in-hole E v1) □ w1 w2 R ns)
        "Head"]
   
   [--> ((G Δs) ⊢ Cs (in-hole E (tail (: v1 v2))) □ w1 w2 R ns)
        ((G Δs) ⊢ Cs (in-hole E v2) □ w1 w2 R ns)
        "Tail"]
   
   [--> ((G [Δ1 Δ ...]) ⊢ Cs (in-hole E ϑ) □ w1 w2 R ns)
        ((G [Δ1 Δ ...]) ⊢ Cs (in-hole E v) □ w1 w2 R ns)
        (side-condition (term (not-▽ Cs)))
        (where (_ ... (ϑ v) _ ...) Δ1)
        "Attribute"]
   
   [--> ((G [Δ1 Δ2 Δ ...]) ⊢ (C1 C ...) (in-hole E ϑ) □ w1 w2 R ns)
        ((G [Δ1 Δ2 Δ ...]) ⊢ (C1 C ...) (in-hole E v) □ w1 w2 R ns)
        (where (▽ _ ...) C1)
        (where (_ ... (ϑ v) _ ...) Δ2)
        "Parameter"]))


(define-metafunction ctxAttributePeg
  difference : n n -> b
  [(difference n n) #f]
  [(difference _ _) #t])


(define-metafunction ctxAttributePeg
  not-▽-△ : Cs -> b
  [(not-▽-△ ((□ _ _ _) _ ...)) #f]
  [(not-▽-△ ((_ _ □ _) _ ...)) #f]
  [(not-▽-△ _) #t])


(define-metafunction ctxAttributePeg
  not-▽ : Cs -> b
  [(not-▽ ((▽ _ ...) _ ...)) #f]
  [(not-▽ _) #t])


(define-metafunction ctxAttributePeg
  it-is-▽-⊙ : UT -> b
  [(it-is-▽-⊙ ▽) #t]
  [(it-is-▽-⊙ ⊙) #t]
  [(it-is-▽-⊙ _) #f])


(define-metafunction ctxAttributePeg
  it-is-not-a-value : e -> b
  [(it-is-not-a-value v) #f]
  [(it-is-not-a-value _) #t])


(define-metafunction ctxAttributePeg
  applay-map : (⇒ (s v) ...) -> e
  [(applay-map (⇒ (s v) (s1 v1) ...)) (put (applay-map (⇒ (s1 v1) ...)) s v)]
  [(applay-map (⇒)) (⇒)])


;Terminal
#;(apply-reduction-relation reduction (term ((() (())) ⊢ () 1 ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(apply-reduction-relation reduction (term ((() (())) ⊢ () 1 ↓ (2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(apply-reduction-relation reduction (term ((() (())) ⊢ () 1 ↓ () (5 6 7) ⊤ (1 2 3))))


;Empty
#;(traces reduction (term ((() (())) ⊢ () ε ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))


;Choice
#;(traces reduction (term ((() (())) ⊢ () (/ 1 2) ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(traces reduction (term ((() (())) ⊢ () (/ 2 1) ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(traces reduction (term ((() (())) ⊢ () (/ 2 3) ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))


;Sequence
#;(traces reduction (term ((() (())) ⊢ () (• 1 2) ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(traces reduction (term ((() (())) ⊢ () (• 2 1) ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(traces reduction (term ((() (())) ⊢ () (• 1 3) ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))


;Repetition
#;(traces reduction (term ((() (())) ⊢ () (* 1) ↓ (1 1 1 1 1 1 2 3 4) (5 6 7) ⊤ (1 2 3))))


;Not
#;(traces reduction (term ((() (())) ⊢ () (! (* 1)) ↓ (1 1 1 1 1 1 2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(traces reduction (term ((() (())) ⊢ () (! 2) ↓ (1 1 1 1 1 1 2 3 4) (5 6 7) ⊤ (1 2 3))))


;Restriction
#;(traces reduction (term ((() (())) ⊢ () (• (* 1) (• (? (∧ #t (¬ (∨ (== 2 4) (¬ #f))))) 2)) ↓ (1 1 1 1 1 1 2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(traces reduction (term ((() (())) ⊢ () (• (* 1) (• (? (∧ #t (¬ (∨ (== 2 4) (¬ #t))))) 2)) ↓ (1 1 1 1 1 1 2 3 4) (5 6 7) ⊤ (1 2 3))))


;Bind
#;(traces reduction (term ((() (())) ⊢ () (= x (• 1 (• 2 3))) ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(traces reduction (term ((() (((x "anything")))) ⊢ () (= x (• 1 (• 2 3))) ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))
#;(traces reduction (term ((() (())) ⊢ () (= x (• 1 (• 2 4))) ↓ (1 2 3 4) (5 6 7) ⊤ (1 2 3))))



;Nonterminal
#;(apply-reduction-relation* reduction (term ((((S () (n (÷ (- n r) 6) r) (RESTO/000-011 (0 0) (n r)))
                                                (RESTO/000-011 ((Integer n) (Integer r)) (n r) (/ (• 0 (RESTO/000-011 ((× 2 n) 0) (n r))) (/ (• 1 (RESTO/001-100 ((+ (× 2 n) 1) 1) (n r))) ε)))
                                                (RESTO/001-100 ((Integer n) (Integer r)) (n r) (/ (• 0 (RESTO/010-101 ((× 2 n) 2) (n r))) (/ (• 1 (RESTO/000-011 ((+ (× 2 n) 1) 3) (n r))) ε)))
                                                (RESTO/010-101 ((Integer n) (Integer r)) (n r) (/ (• 0 (RESTO/001-100 ((× 2 n) 4) (n r))) (/ (• 1 (RESTO/010-101 ((+ (× 2 n) 1) 5) (n r))) ε))))
                                               (())) ⊢ () (S () (number quotient rest)) ↓ (1 0 1 0 0 1 1) () ⊤ (0))))
	; number = 83, quotient = 13, rest = 5
#;(apply-reduction-relation* reduction (term ((((S () (x) (/ (addition () (x)) (/ (subtraction () (x)) (/ (multiplication () (x)) (/ (division () (x)) (number (0) (x)))))))
                                                (addition () ((+ x y)) (• (++ () ()) (• (S () (x)) (• (space () ()) (number (0) (y))))))
                                                (subtraction () ((- x y)) (• (-- () ()) (• (S () (x)) (• (space () ()) (number (0) (y))))))
                                                (multiplication () ((× x y)) (• (×× () ()) (• (S () (x)) (• (space () ()) (number (0) (y))))))
                                                (division () ((÷ x y)) (• (÷÷ () ()) (• (S () (x)) (• (space () ()) (number (0) (y))))))
                                                (number ((Integer x)) (x) (/ (• 0 (number ((× 2 x)) (x))) (/ (• 1 (number ((+ (× 2 x) 1)) (x))) ε)))
                                                (++ () () 1100)
                                                (-- () () 1101)
                                                (×× () () 1110)
                                                (÷÷ () () 1111)
                                                (space () () 1000)) (())) ⊢ () (S () (result)) ↓ (1111 1110 1101 1100 1 0 1 0 1000 1 0 1 1000 1 0 0 1000 1 1 0 1000 1 0) () ⊤ (0))))
	; result = 33
#;(apply-reduction-relation* reduction (term ((((S () (number list) (• (N (0) (number)) (F (0 number nil) (list))))
                                                (N ((Integer n)) (n) (/ (• 0 (N ((× 10 n)) (n)))
                                                                        (/ (• 1 (N ((+ (× 10 n) 1)) (n)))
                                                                           (/ (• 2 (N ((+ (× 10 n) 2)) (n)))
                                                                              (/ (• 3 (N ((+ (× 10 n) 3)) (n)))
                                                                                 (/ (• 4 (N ((+ (× 10 n) 4)) (n)))
                                                                                    (/ (• 5 (N ((+ (× 10 n) 5)) (n)))
                                                                                       (/ (• 6 (N ((+ (× 10 n) 6)) (n)))
                                                                                          (/ (• 7 (N ((+ (× 10 n) 7)) (n)))
                                                                                             (/ (• 8 (N ((+ (× 10 n) 8)) (n)))
                                                                                                (/ (• 9 (N ((+ (× 10 n) 9)) (n))) ε)))))))))))
                                                (F ((Integer count) (Integer number) ((: Integer) list)) (list) (/ (• (? (> number count))
                                                                                                                      (/ (• (? (== count 0)) (F ((+ count 1) number list) (list)))
                                                                                                                         (/ (• (? (== count 1)) (F ((+ count 1) number (: 0 nil)) (list)))
                                                                                                                            (/ (• (? (== count 2)) (F ((+ count 1) number (: 0 1)) (list)))
                                                                                                                               (• (? (== count 3)) (G ((+ count 1) number (: list 1)) (list))))))) ε))
                                                (G ((Integer count) (Integer number) ((: Integer) list)) (list) (/ (• (? (> number count)) (G ((+ count 1) number (: list (+ (tail list) (tail (head list))))) (list))) ε))) (())) ⊢ () (S () (number list)) ↓ (2 2) () ⊤ (0))))
	; number = 22
	; list = (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: (: 0 1) 1) 2) 3) 5) 8) 13) 21) 34) 55) 89) 144) 233) 377) 610) 987) 1597) 2584) 4181) 6765)
#;(apply-reduction-relation* reduction (term ((((S () (is-it-a-palindrome?) (/ (• (! (! (T (0) (size)))) (! (! (P (size) (is-it-a-palindrome?))))) ((← is-it-a-palindrome? "it's not a palindrome"))))
                                                (T ((Integer size)) (size) (/ (• (/ 0 1) (T ((+ size 1)) (size))) ε))
                                                (P ((Integer size)) (is-it-a-palindrome?) (/ (• (/ (? (== size 0)) (• (? (== size 1)) (/ 0 1))) ((← is-it-a-palindrome? "it's a palindrome")))
                                                                                                  (/ (• 0 (• (P ((- size 2)) (is-it-a-palindrome?)) 0))
                                                                                                     (• 1 (• (P ((- size 2)) (is-it-a-palindrome?)) 1)))))) (())) ⊢ () (S () (is-it-a-palindrome)) ↓ (1 1 0 1 0 0 0 0 1 0 1 1) () ⊤ (0))))
	; is-it-a-palindrome = "it's a palindrome"