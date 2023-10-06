#lang racket

(require redex)
(provide (all-defined-out))


(define-language AttributeL
  [expr ::= l
        (⇒ ((expr expr) ...))		; representação de um mapeamento: a primeira expressão deve reduzir para uma string, a segunda expressão deve reduzir para um valor
        (get expr expr)			; representação da operação de acesso a um mapeamento: a primeira expressão deve reduzir para um mapeamento, a segunda expressão deve reduzir para uma string
        (put expr expr expr)		; representação da operação de inserção em um mapeamento: a primeira expressão deve reduzir para um mapeamento, a segunda expressão deve reduzir para uma string, a terceira expressão deve reduzir para um valor
        (: expr expr)			; representação de lista
        nil				; representação de lista vazia
        (head expr)			; representação da operação utilizada para obter a "cabeça" (o primeiro elemento) de uma lista: a expressão deve reduzir para uma lista, a operação "emperra" (não reduz) quando atua sobre lista vazia
        (tail expr)			; representação da operação utilizada para obter a "cauda" (todos os demais elementos, exceto o primeiro) de uma lista: a expressão deve reduzir para uma lista, a operação "emperra" (não reduz) quando atua sobre lista vazia
        (+ expr expr)			; representação da operação de soma: ambas as expressões devem reduzir para literais números
        (* expr expr)			; representação da operação de multiplicação: ambas as expressões devem reduzir para literais números
        (÷ expr expr)			; representação da operação de divisão: ambas as expressões devem reduzir para literais números
        (- expr expr)			; representação da operação de subtração: ambas as expressões devem reduzir para literais números
        (&& expr expr)			; representação da operação de conjunção: ambas as expressões devem reduzir para literais booleanos
        (|| expr expr)			; representação da operação de disjunção: ambas as expressões devem reduzir para literais booleanos
        (¬ expr)			; representação da operação de negação: ambas as expressões devem reduzir para literais booleanos
        (== expr expr)			; representação da operação de igualdade: ambas as expressões devem reduzir para literais números
        (> expr expr)			; representação da operação de maior que: ambas as expressões devem reduzir para literais números
        x]				; representação da operação de acesso à variável: a operação emperra caso a variável não esteja definida no contexto da avaliação
  [l ::= boolean			; representação de literal boolean
     number				; representação de literal número
     string]				; representação de literal string
  [x ::= variable-not-otherwise-mentioned]); representação de variável


(define-extended-language val-AttributeL AttributeL
  [ctx ::= ((x value)...)]		; representação de contexto
  [value ::= boolean			; representação de literal boolean
         number				; representação de literal número
         string				; representação de literal string
         (⇒ ((string value) ...))	; representação de literal mapeamento
         (: value value)		; representação de literal lista
         nil				; representação de literal lista vazia
         undef])			; representação de literal indefinido


(define-extended-language val-AttributeLType val-AttributeL
  [ctx ::= .... ((x type)...)]		; representação de contexto
  [type ::= type:boolean		; representação de tipo boolean
        type:integer			; representação de tipo inteiro
        type:real			; representação de tipo real
        type:string			; representação de tipo string
        (→ (type ...) (type ...))
        (⇒ type)
        (: type)])