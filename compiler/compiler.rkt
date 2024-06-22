#lang racket
(require "./parser.rkt"
         redex)


(provide peg-compiler)


;(string-join (map (lambda (input) (~a (integer->char input))) (range 300)))
;(first '(1 2 3 4 5))
;(second '(1 2 3 4 5))


(define (peg-compiler input)
  (match input
    [(PTVar "Grammar" (PTList (list values ... value))) (list (term ,(map peg-compiler values)) (term ,(peg-compiler value)))]
    [(PTVar "Production" (PTList (list value_1 values_1 ... (PTSym #\}) values_2 ... value_2))) (term (,(peg-compiler value_1) ,(map peg-compiler values_1) ,(map peg-compiler values_2) ,(peg-compiler value_2)))]
    [(PTVar "ProductionInput" (PTList (list value_1 value_2))) (term (,(peg-compiler value_1) ,(peg-compiler value_2)))]
    [(PTVar "Type" (PTList values)) (Type->term values)]

    
    [(PTVar "Bind" (PTList values)) (Bind->term values)]
    [(PTVar "Empty" (PTList '())) (term ε)]
    [(PTVar "Nonterminal" (PTStr value)) (term ,(string->symbol value))]
    [(PTVar "NonterminalCall" (PTList (list value values_1 ... (PTSym #\}) values_2 ...))) (term (,(peg-compiler value) ,(map peg-compiler values_1) ,(map peg-compiler values_2)))]
    [(PTVar "PegExpression" (PTList (list value))) (peg-compiler value)]
    [(PTVar "PegExpression" (PTList (list value_1 value_2))) (term (/ ,(peg-compiler value_1) ,(peg-compiler value_2)))]
    [(PTVar "PegFactor" value) (peg-compiler value)]
    [(PTVar "PegParenthesesExpression" (PTList (list value))) (peg-compiler value)]
    ;[(PTVar "PegPrefix" (PTList (list value))) (peg-compiler value)] ;old version
    [(PTVar "PegPrefix" (PTList (list (PTSym #\!) value))) (term (! ,(peg-compiler value)))]
    [(PTVar "PegPrefix" value) (peg-compiler value)] ;modified
    [(PTVar "PegSequence" (PTList values)) (PegSequence->term values)]
    [(PTVar "PegUnaryOperation" (PTList (list value))) (peg-compiler value)]
    [(PTVar "PegUnaryOperation" (PTList (list value_1 (PTSym symbol)))) (let [(value_2 (peg-compiler value_1))] (match symbol [#\* (term (* ,value_2))] [#\+ (term (• ,value_2 (* ,value_2)))] [#\? (term (/ ,value_2 ε))]))]
    [(PTVar "Range" (PTList (list (PTStr value)))) (RangeList->term (map char->integer (string->list value)))]
    [(PTVar "Range" (PTList (list (PTSym value_1) (PTSym value_2)))) (let* ([value_3 (char->integer value_1)] [value_4 (char->integer value_2)]) (if (< value_3 value_4) (Range->term value_3 value_4) (error "invalid range:" (list->string (list #\[ #\' value_1 #\' #\- #\' value_2 #\' #\])))))]
    [(PTVar "Range" (PTList (list (PTSym #\\) (PTSym value_1) (PTSym value_2)))) (let* ([value_3 (escape-character->integer value_1)] [value_4 (char->integer value_2)]) (if (< value_3 value_4) (Range->term value_3 value_4) (error "invalid range:" (list->string (list #\[ #\' #\\ value_1 #\' #\- #\' value_2 #\' #\])))))]
    [(PTVar "Range" (PTList (list (PTSym value_1) (PTSym #\\) (PTSym value_2)))) (let* ([value_3 (char->integer value_1)] [value_4 (escape-character->integer value_2)]) (if (< value_3 value_4) (Range->term value_3 value_4) (error "invalid range:" (list->string (list #\[ #\' value_1 #\' #\- #\' #\\ value_2 #\' #\])))))]
    [(PTVar "Range" (PTList (list (PTSym #\\) (PTSym value_1) (PTSym #\\) (PTSym value_2)))) (let* ([value_3 (escape-character->integer value_1)] [value_4 (escape-character->integer value_2)]) (if (< value_3 value_4) (Range->term value_3 value_4) (error "invalid range:" (list->string (list #\[ #\' #\\ value_1 #\' #\- #\' #\\ value_2 #\' #\])))))]
    [(PTVar "Restriction" (PTList (list value))) (term (? ,(peg-compiler value)))]
    [(PTVar "Terminal" (PTList (list (PTSym value)))) (term ,(char->integer value))]
    [(PTVar "Terminal" (PTList (list (PTSym #\\) (PTSym value)))) (term ,(escape-character->integer value))]
    [(PTVar "Terminal" (PTStr value)) (term ,(String->term (map char->integer (string->list value))))]
    [(PTVar "Update" (PTList values)) (term ,(map peg-compiler values))]
    [(PTVar "UpdateValue" (PTList (list value_1 value_2))) (term (← ,(peg-compiler value_1) ,(peg-compiler value_2)))]

    
    [(PTVar "AdditionExpression" (PTList values)) (AdditionExpression->term values)]
    [(PTVar "AndConditional" (PTList values)) (AndConditional->term values)]
    [(PTVar "Attribute" (PTStr value)) (term ,(string->symbol value))]
    [(PTVar "Boolean" (PTStr "true")) (term #t)]
    [(PTVar "Boolean" (PTStr "false")) (term #f)]
    [(PTVar "ConditionalExpression" (PTList (list value))) (peg-compiler value)]
    [(PTVar "ConditionalExpression" (PTList (list value_1 (PTStr symbol) value_2))) (ConditionalExpression->term (peg-compiler value_1) symbol (peg-compiler value_2))]
    ;[(PTVar "ConditionalExpression" (PTList (list value_1 (PTStr "==") value_2))) (term (== ,(peg-compiler value_1) ,(peg-compiler value_2)))]
    ;[(PTVar "ConditionalExpression" (PTList (list value_1 (PTSym #\>) value_2))) (term (> ,(peg-compiler value_1) ,(peg-compiler value_2)))]
    [(PTVar "Expression" (PTList (list value))) (peg-compiler value)]
    [(PTVar "Factor" (PTList (list (PTSym #\-) value))) (term (- 0 ,(peg-compiler value)))]
    [(PTVar "Factor" (PTList (list (PTSym #\~) value))) (term (¬ ,(peg-compiler value)))]
    [(PTVar "Factor" value) (peg-compiler value)]
    [(PTVar "Get" (PTList (list value_1 value_2))) (term (get ,(peg-compiler value_1) ,(peg-compiler value_2)))]
    [(PTVar "Head" (PTList (list value))) (term (head ,(peg-compiler value)))]
    [(PTVar "ListHandler" value) (peg-compiler value)]
    [(PTVar "LiteralList" (PTList '())) (term nil)]
    [(PTVar "LiteralList" (PTList (list value_1 (PTSym #\:) value_2))) (term (: ,(peg-compiler value_1) ,(peg-compiler value_2)))]
    [(PTVar "LiteralList" (PTList values)) (LiteralList->term values)]
    [(PTVar "LiteralMapping" (PTList '())) (term (⇒ ()))]
    [(PTVar "LiteralMapping" (PTList values)) (term (⇒ ,(map peg-compiler values)))]
    [(PTVar "MappingHandler" value) (peg-compiler value)]
    [(PTVar "MappingValue" (PTList (list value_1 value_2))) (term (,(peg-compiler value_1) ,(peg-compiler value_2)))]
    [(PTVar "Integer" (PTStr value)) (term ,(string->number value))]
    [(PTVar "Parentheses" (PTList (list value))) (peg-compiler value)]
    [(PTVar "Primary" value) (peg-compiler value)]
    [(PTVar "Put" (PTList (list value_1 value_2 value_3))) (term (put ,(peg-compiler value_1) ,(peg-compiler value_2) ,(peg-compiler value_3)))]
    [(PTVar "OrConditional" (PTList values)) (OrConditional->term values)]
    [(PTVar "String" (PTStr value)) (term ,value)]
    [(PTVar "Tail" (PTList (list value))) (term (tail ,(peg-compiler value)))]
    [(PTVar "Term" (PTList values)) (Term->term values)]))


(define (AdditionExpression->term input)
  (match input
    [(list value) (peg-compiler value)]
    [(list value (PTSym symbol) values ...) (term (,(if (equal? symbol #\+) (term +) (term -)) ,(peg-compiler value) ,(AdditionExpression->term values)))]))


(define (AndConditional->term input)
  (match input
    [(list value) (peg-compiler value)]
    [(list value values ...) (term (&& ,(peg-compiler value) ,(AndConditional->term values)))]))


(define (Bind->term input)
  (match input
    [(list value_1 value_2) (term (= ,(peg-compiler value_1) ,(peg-compiler value_2)))]
    [(list value_1 value_2 values ...) (term (• (= ,(peg-compiler value_1) ,(peg-compiler value_2)) ,(Bind->term values)))]))


(define (ConditionalExpression->term value_1 symbol value_2)
  (match symbol
    ["==" (term (== ,value_1 ,value_2))]
    ["/=" (term (¬ (== ,value_1 ,value_2)))]
    [">" (term (> ,value_1 ,value_2))]
    [">=" (term (|| (> ,value_1 ,value_2) (== ,value_1 ,value_2)))]
    ["<" (term (¬ (> ,value_1 ,value_2)))]
    ["<=" (term (¬ (> ,value_1 ,value_2)))]))

            
(define (escape-character->integer input)
  (match input
    [#\b 8]
    [#\t 9]
    [#\n 10]
    [#\r 13]
    [value (char->integer value)]))


(define (escape-character-inside-string->integer input)
  (match input
    [98 8]
    [116 9]
    [110 10]
    [114 13]
    [value value]))


(define (PegSequence->term input)
  (match input
    [(list value) (peg-compiler value)]
    [(list value values ...) (term (• ,(peg-compiler value) ,(PegSequence->term values)))]))


(define (Range->term value_1 value_2)
  (term (/ ,value_1 ,(let ([value_3 (+ value_1 1)]) (if (= value_3 value_2) value_2 (Range->term value_3 value_2))))))


(define (RangeList->term input)
  (match input
    [(list value) (term ,value)]
    [(list 92 value) (term ,(escape-character-inside-string->integer value))]
    [(list 92 values ...) (term (/ ,(escape-character-inside-string->integer (car values)) ,(RangeList->term (cdr values))))]
    [(list value values ...) (term (/ ,value ,(RangeList->term values)))]))


(define (LiteralList->term input)
  (match input
    ['() (term nil)]
    [(list value values ...) (term (: ,(peg-compiler value) ,(LiteralList->term values)))]))


(define (OrConditional->term input)
  (match input
    [(list value) (peg-compiler value)]
    [(list value values ...) (term (|| ,(peg-compiler value) ,(OrConditional->term values)))]))


(define (String->term input)
  (match input
    ['() (term ε)]
    [(list value) (term ,value)]
    [(list 92 value) (term ,(escape-character-inside-string->integer value))]
    [(list 92 values ...) (term (• ,(escape-character-inside-string->integer (car values)) ,(String->term (cdr values))))]
    [(list value values ...) (term (• ,value ,(String->term values)))]))


(define (Term->term input)
  (match input
    [(list value) (peg-compiler value)]
    [(list value (PTSym symbol) values ...) (term (,(if (equal? symbol #\*) (term *) (term ÷)) ,(peg-compiler value) ,(Term->term values)))]))


(define (Type->term input)
  (match input
    [(list (PTSym #\<) values ...) (term (⇒ ,(Type->term values)))]
    [(list (PTSym #\[) values ...) (term (: ,(Type->term values)))]
    [(list (PTStr "boolean") values ...) (term type:boolean)]
    [(list (PTStr "int") values ...) (term type:integer)]
    [(list (PTStr "float") values ...) (term type:real)]
    [(list (PTStr "string") values ...) (term type:string)]))


;REUNIÃO 10/11/2023

;(peg-compiler (PTVar "Expression" (run-parse-from "Expression" "12/=3")))
;(peg-compiler (PTVar "Expression" (run-parse-from "Expression" "12>=3")))
;(peg-compiler (PTVar "Expression" (run-parse-from "Expression" "12>3")))
;(peg-compiler (PTVar "Expression" (run-parse-from "Expression" "12<3")))
;(peg-compiler (PTVar "Expression" (run-parse-from "Expression" "12<=3")))
;(peg-compiler (PTVar "Expression" (run-parse-from "Expression" "12==3")))


;(peg-compiler (PTVar "NonterminalCall" (run-parse-from "NonterminalCall" "Test     {    x   ,    5    ,    true   ,  3.14  ,  \"anything\"  }  {  y  ,  w  ,  z  }")))
;(peg-compiler (PTVar "Range" (run-parse-from "Range" "[ '1' -   '3' ]   ")))
;(peg-compiler (PTVar "Range" (run-parse-from "Range" "[ '1' , '2'   ,   '3' ]   ")))
;(run-parse-from "Type" "int        ")
;(run-parse-from "Type" "<   int    >    ")
;(run-parse-from "Type" "[  <   int    >  \n ]   ")


;GRAMMAR


;(peg-compiler (PTVar "Grammar" (run-parse-from "Grammar" "Symbol{int::a,<<[[float]]>>::b,[string]::c,boolean::d}{~true&&false&&(true&&false||~false&&true)&&30/10-5==-22+4*5*a,\"anything\"}<--'a''b'/'c'('a'/'b');Name{}{}<--['A'-'B']['a'-'b']+;start:Symbol{0,[],\"abcdef\",true}{x,y}")))
;(peg-compiler (PTVar "Grammar" (run-parse-from "Grammar" "Symbol{int::a,<<[[float]]>>::b,[string]::c,boolean::d}{~true&&false&&(true&&false||~false&&true)&&30/10-5==-22+4*5*a,\"anything\"}<--'a''b'/'c'('a'/'b');start:Symbol{0,[],\"abcdef\",true}{x,y}")))
;(peg-compiler (PTVar "Production" (run-parse-from "Production" "Symbol{int::a,<<[[float]]>>::b,[string]::c,boolean::d}{~true&&false&&(true&&false||~false&&true)&&30/10-5==-22+4*5*a,\"anything\"}<--'a''b'/'c'('a'/'b')")))
;(peg-compiler (PTVar "ProductionInput" (run-parse-from "ProductionInput" "<<[[int]]>>::list")))
;(peg-compiler (PTVar "Type" (run-parse-from "Type" "<<[[int]]>>")))


;PEG EXPRESSIONS


;(peg-compiler (PTVar "Empty" (run-parse-from "Empty" "ε")))
;(peg-compiler (PTVar "Nonterminal" (run-parse-from "Nonterminal" "Value")))
;(peg-compiler (PTVar "Nonterminal" (run-parse-from "Nonterminal" "_value")))
;(peg-compiler (PTVar "NonterminalCall" (run-parse-from "NonterminalCall" "Test{x,5,true,3.14,\"anything\"}{y,w,z}")))
;(peg-compiler (PTVar "PegExpression" (run-parse-from "PegExpression" "!'a'+'b'/'c'")))
;(peg-compiler (PTVar "PegExpression" (run-parse-from "PegExpression" "!'a'+('b'/'c')")))
;(peg-compiler (PTVar "PegExpression" (run-parse-from "PegExpression" "{abs=('a'/'b')*,cds=('c'/'d')+}")))
;(peg-compiler (PTVar "PegPrefix" (run-parse-from "PegPrefix" "!'a'*")))
;(peg-compiler (PTVar "PegPrefix" (run-parse-from "PegPrefix" "'a'?")))
;(peg-compiler (PTVar "PegUnaryOperation" (run-parse-from "PegUnaryOperation" "'a'*")))
;(peg-compiler (PTVar "PegUnaryOperation" (run-parse-from "PegUnaryOperation" "'a'+")))
;(peg-compiler (PTVar "PegUnaryOperation" (run-parse-from "PegUnaryOperation" "'a'?")))
;(peg-compiler (PTVar "Range" (run-parse-from "Range" "['\\\\'-'A']")))
;(peg-compiler (PTVar "Range" (run-parse-from "Range" "['\\t'-'\\n']")))
;(peg-compiler (PTVar "Range" (run-parse-from "Range" "['\\r'-'\\n']")))
;(peg-compiler (PTVar "Range" (run-parse-from "Range" "['a','b','c','d','\\n']")))
;(peg-compiler (PTVar "Range" (run-parse-from "Range" "['a'-'f']")))
;(peg-compiler (PTVar "Range" (run-parse-from "Range" "['f'-'\\n']")))
;(peg-compiler (PTVar "Range" (run-parse-from "Range" "['f'-'a']")))
;(peg-compiler (PTVar "Restriction" (run-parse-from "Restriction" "{?30==10}")))
;(peg-compiler (PTVar "Terminal" (run-parse-from "Terminal" "\'a\'")))
;(peg-compiler (PTVar "Terminal" (run-parse-from "Terminal" "\'\\n\'")))
;(peg-compiler (PTVar "Terminal" (run-parse-from "Terminal" "\"\"")))
;(peg-compiler (PTVar "Terminal" (run-parse-from "Terminal" "\"a\"")))
;(peg-compiler (PTVar "Terminal" (run-parse-from "Terminal" "\"\\n\"")))
;(peg-compiler (PTVar "Terminal" (run-parse-from "Terminal" "\"abc\"")))
;(peg-compiler (PTVar "Update" (run-parse-from "Update" "{}")))
;(peg-compiler (PTVar "Update" (run-parse-from "Update" "{name<-\"Gabriel\",surname<-\"Pires Ferreira\",age<-22,single<-true}")))


;EXPRESSIONS


;(peg-compiler (PTVar "AdditionExpression" (run-parse-from "AdditionExpression" "3")))
;(peg-compiler (PTVar "AdditionExpression" (run-parse-from "AdditionExpression" "3*4/5-6*4+10/3")))
;(peg-compiler (PTVar "AndConditional" (run-parse-from "AndConditional" "true&&false&&true&&false")))
;(peg-compiler (PTVar "ConditionalExpression" (run-parse-from "ConditionalExpression" "20*3-7==3*4/5-6*4+10/abc3")))
;(peg-compiler (PTVar "ConditionalExpression" (run-parse-from "ConditionalExpression" "20*3-7>3*4/5-6*4+10/abc3")))
;(peg-compiler (PTVar "Expression" (run-parse-from "Expression" "true&&false&&false||true&&false&&20*3+4==31")))
;(peg-compiler (PTVar "Factor" (PTList (list (PTSym #\-) (PTVar "Primary" (PTVar "Number" (PTStr "-3")))))))
;(peg-compiler (PTVar "Factor" (PTVar "Primary" (PTVar "Attribute" (PTStr "abc")))))
;(peg-compiler (PTVar "Factor" (PTVar "Primary" (PTVar "String" (PTStr "abc")))))
;(peg-compiler (PTVar "LiteralList" (run-parse-from "LiteralList" "[]")))
;(peg-compiler (PTVar "LiteralList" (run-parse-from "LiteralList" "[0:[1,2,3]]")))
;(peg-compiler (PTVar "LiteralList" (run-parse-from "LiteralList" "[1,2,3]")))
;(peg-compiler (PTVar "LiteralMapping" (run-parse-from "LiteralMapping" "<<>>")))
;(peg-compiler (PTVar "LiteralMapping" (run-parse-from "LiteralMapping" "<<\"key_1\"->1,\"key_2\"->2,\"key_3\"->3>>")))
;(peg-compiler (PTVar "Primary" (PTVar "Number" (PTStr "7"))))
;(peg-compiler (PTVar "Term" (run-parse-from "Term" "123*3*4")))
;(peg-compiler (PTVar "Term" (run-parse-from "Term" "123*3/4")))


;EXPRESSIONS WITH WHITE SPACE


;(peg-compiler (PTVar "Expression" (run-parse-from "Expression" "  20  *  3 -     7   ==  3  *  4  /  5  -  6  *  4  +   10   /   abc3")))
;(peg-compiler (PTVar "Expression" (run-parse-from "Expression" "  20  *  (   3 - 8 +  \n 5 + 9  -    7 )   ==  3  *  4  /  5  -  6  *  4  +   10   /   abc3")))
;(peg-compiler (PTVar "Expression" (run-parse-from "Expression" "    [  0  :   [   1   ,   2  \n \t  ,  3   ]   ]    ")))
;(peg-compiler (PTVar "Expression" (run-parse-from "Expression" "  head tail [  0  :   [   1   ,   2  \n \t  ,  3   ]   ]    ")))
;(peg-compiler (PTVar "Expression" (run-parse-from "Expression" "    get    put   <<    \"key_1\"   ->     1   ,   \"key_2\"   ->    2    ,   \"key_3\"   ->   3   >> \"key_4\" \n \t 4 \"key_2\"   ")))