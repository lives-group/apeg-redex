# _Parsing Expression Grammar with Syntatic Attributes_ (PEGwSA): uma formalização em PLT _Redex_
PEGwSA é um formalismo usado para descrever analisadores sintáticos _top-down_ que estende _Parsing Expression Grammar_ (PEG) com atributos sintáticos e operadores para manipulalos. PEGwSA serve como base para _Adaptable Parsing Expression Grammar_ (APEG), formalismo que estende PEGwSA com mecanismos para manipular dinâmicamente as regras de produção que constituem a gramática.

Essa formalização é constituída por quatro artefatos: sintaxe, sistema de tipos e semânticas operacionais _small-step_ e _big-step_. À exceção da semântica operacional _small-step_, todos esses artefatos são dividios em dois arquivos: um arquivo que abarca as expressões de atributos e outro, as expressões de _parsing_.

> Os arquivos _attributeL-syntax.rkt_ e _attributePeg-syntax.rkt_ contém a especificação da sintaxe de PEGwSA.
>
> Os arquivos _attributeLType-bigStep.rkt_ e _attributePegType-bigStep.rkt_ contém a especificação do sistema de tipos de PEGwSA.
>
> Os arquivos _attributeL-bigStep.rkt_ e _attributePeg-bigStep.rkt_ contém a especificação da semântica operacional _big-step_ de PEGwSA.
>
> O arquivo _attributeL-bigStep.rkt_ contém a especificação da semântica operacional _small-step_ de PEGwSA.

Além disso, efetuamos alguns testes sobre a formalização de PEGwSA, esses testes podem ser encontrados na pasta _tests_.
