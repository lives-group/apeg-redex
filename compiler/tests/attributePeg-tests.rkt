#lang racket


(require "../../attributePeg-bigStep.rkt"
         "../parser.rkt"
         "../compiler.rkt"
         redex)


(define (file->grammar filepath)
  (peg-compiler (PTVar "Grammar" (run-parse-from "Grammar" (file->string filepath)))))


(define (input->integer-list filepath)
  (map char->integer (string->list (file->string filepath))))


(define (r->output r)
  (match r
    [(list values ...) (list->string (map integer->char values))]
    [any any]))


;worddrow
#;(let* [(grammar (file->grammar "grammars/worddrow/grammar.txt")) (start (second grammar)) (grammar (first grammar))]
  (begin [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/worddrow/sample 1.txt") r ctx) (,(r->output (term r)) ctx))
                              '(("" ((consumed "abcdabbbbadcba"))))]
         [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/worddrow/sample 2.txt") r ctx) (,(r->output (term r)) ctx))
                              '((⊥ ()))]
         [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/worddrow/sample 3.txt") r ctx) (,(r->output (term r)) ctx))
                              '(("" ((consumed "abcdabbaabbadcba"))))]))


;palindrome
#;(let* [(grammar (file->grammar "grammars/palindrome/grammar.txt")) (start (second grammar)) (grammar (first grammar))]
  (begin [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/palindrome/sample 1.txt") r ctx) (,(r->output (term r)) ctx))
                     '(("" ((consumed "abcdabbbbadcba"))))]
         [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/palindrome/sample 2.txt") r ctx) (,(r->output (term r)) ctx))
                     '(("" ((consumed "abcdabbabbadcba"))))]
         [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/palindrome/sample 3.txt") r ctx) (,(r->output (term r)) ctx))
                     '(("" ((consumed "abcdabbaabbadcba"))))]
         [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/palindrome/sample 4.txt") r ctx) (,(r->output (term r)) ctx))
                     '((⊥ ()))]))


;arithmetic
#;(let* [(grammar (file->grammar "grammars/arithmetic/grammar.txt")) (start (second grammar)) (grammar (first grammar))]
  (begin [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/arithmetic/sample 1.txt") r ctx) (,(r->output (term r)) ctx))
                     '(("" ((result 36))))]
         [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/arithmetic/sample 2.txt") r ctx) (,(r->output (term r)) ctx))
                     '(("" ((result 121))))]
         [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/arithmetic/sample 3.txt") r ctx) (,(r->output (term r)) ctx))
                     '(("" ((result 144))))]))


;logic
#;(let* [(grammar (file->grammar "grammars/logic/grammar.txt")) (start (second grammar)) (grammar (first grammar))]
  (begin [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/logic/sample 1.txt") r ctx) (,(r->output (term r)) ctx))
                     '(("" ((result 1))))]
         [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/logic/sample 2.txt") r ctx) (,(r->output (term r)) ctx))
                     '(("" ((result 0))))]
         [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/logic/sample 3.txt") r ctx) (,(r->output (term r)) ctx))
                     '(("" ((result 0))))]))

;let
#;(let* [(grammar (file->grammar "grammars/let/grammar.txt")) (start (second grammar)) (grammar (first grammar))]
  (begin [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/let/sample 1.txt") r ctx) (,(r->output (term r)) ctx))
                     '(("" ((result 3))))]
         [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/let/sample 2.txt") r ctx) (,(r->output (term r)) ctx))
                     '(("" ((result 6241))))]
         [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/let/sample 3.txt") r ctx) (,(r->output (term r)) ctx))
                     '(("" ((result 1))))]))

;ifelse
#;(let* [(grammar (file->grammar "grammars/ifelse/grammar.txt")) (start (second grammar)) (grammar (first grammar))]
  (begin [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/ifelse/sample 1.txt") r ctx) (,(r->output (term r)) ctx))
                     '(("" ((result #t))))]
         [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/ifelse/sample 2.txt") r ctx) (,(r->output (term r)) ctx))
                     '(("" ((result #f))))]
         [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/ifelse/sample 3.txt") r ctx) (,(r->output (term r)) ctx))
                     '(("" ((result #t))))]
         [test-equal (judgment-holds (parse () ,grammar ,start ,(input->integer-list "grammars/ifelse/sample 4.txt") r ctx) (,(r->output (term r)) ctx))
                     '(("" ((result #f))))]))


(test-results)