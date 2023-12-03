#lang peg-parser


Grammar <-- (Production ~";")+ ~WS ~"start" ~WS ~':' ~WS NonterminalCall;


Production <-- ~WS Nonterminal ~'{' ~WS (ProductionInput (~',' ~WS ProductionInput)*)? '}' ~WS ~'{' (Expression (~',' Expression)*)? ~'}' ~WS ~"<--" PegExpression;
ProductionInput <-- Type ~"::" ~WS Attribute;


PegExpression <-- ~WS PegSequence (~'/' PegExpression)?;
PegSequence <-- PegPrefix+;
PegPrefix <-- '!' ~WS PegPrefix / PegUnaryOperation;
PegUnaryOperation <-- PegFactor (('*' / '+' / '?') ~WS)?;
PegFactor <-- Terminal / NonterminalCall / Range / PegParenthesesExpression / Empty / Restriction / Update / Bind;


Terminal <-- ^Character / -String;
Character <-- ((~'\'' ^CharacterAuxiliary ~'\'') ~WS);
CharacterAuxiliary <-- ((!('\' / '\'' / '\"' / '\b' / '\n' / '\r' / '\t' ) .) / ('\' ('\' / '\'' / '\"' / 'b' / 'n' / 'r' / 't')));
String <-- -((~'\"' CharacterAuxiliary* ~'\"') ~WS);


NonterminalCall <-- Nonterminal ~'{' ~WS (Expression (~',' Expression)*)? '}' ~WS ~'{' ~WS (Attribute (~',' ~WS Attribute)*)? ~'}' ~WS;
Nonterminal <-- -((['A'-'Z'] / '_') (['a'-'z'] / ['0'-'9'] / ['A'-'Z'] / '_')* ~WS);


Range <-- ~'[' ~WS (^Character ~'-' ~WS ^Character / -RangeList) ~']' ~WS;
RangeList <-- ^Character (~',' ~WS ^Character)*;


PegParenthesesExpression <-- ~'(' PegExpression ~')' ~WS;


Empty <-- ~('ε' WS);


Restriction <-- ~'{' ~WS ~'?' Expression ~'}' ~WS;
Update <-- ~'{' (UpdateValue (~',' UpdateValue)*)? ~'}' ~WS;
UpdateValue <-- ~WS Attribute ~"<-" Expression;
Bind <-- ~'{' ~WS Attribute ~'=' PegExpression (~',' ~WS Attribute ~'=' PegExpression)* ~'}' ~WS;


Type <-- (-BasicType / ('<' ~WS ^Type ~'>') / ('[' ~WS ^Type ~']')) ~WS;
BasicType <-- "int" / "float" / "boolean" / "string";


Expression <-- ~WS OrConditional;
OrConditional <-- AndConditional (~"||" ~WS AndConditional)*;
AndConditional <-- ConditionalExpression (~"&&" ~WS ConditionalExpression)*;
ConditionalExpression <-- AdditionExpression (-("==" / "/=" / (('<' / '>') '='?)) ~WS AdditionExpression)?;
AdditionExpression <-- Term (('+' / '-') ~WS Term)*;
Term <-- Factor (('*' / '/') ~WS Factor)*;
Factor <-- ('-' / '~') ~WS Factor / Primary;
Primary <-- Attribute / Integer / String / Parentheses / Boolean / LiteralList / ListHandler / LiteralMapping / MappingHandler;


Attribute <-- -((!(("head" / "tail" / "get" / "put" / Boolean / BasicType)!(['a'-'z'] / ['0'-'9'] / ['A'-'Z'] / '_')) ['a'-'z'] (['a'-'z'] / ['0'-'9'] / ['A'-'Z'] / '_')*) ~WS);


Integer <-- -(('-'? (['1'-'9'] ['0'-'9']*) / '0') ~WS);


Parentheses <-- ~'(' Expression ~')' ~WS;


Boolean <-- -(("true" / "false") ~WS);


LiteralList <-- (~'[' (Expression (~',' Expression)*)? ~']' / ~'[' Expression ':' Expression ~']') ~WS;
ListHandler <-- Head / Tail;
Head <-- ~"head" Expression;
Tail <-- ~"tail" Expression;


MappingValue <-- Expression ~"->" Expression;
LiteralMapping <-- ~"<<" (MappingValue (~',' MappingValue)*)? ~">>" ~WS;
MappingHandler <-- Get / Put;
Get <-- ~"get" Expression Expression;
Put <-- ~"put" Expression Expression Expression;


WS <-- ([' ', '\n', '\t', '\r'] / LineComment / BlockComment)*;
LineComment <-- "\\" (!'\n' .)*;
BlockComment <-- '\' '*' (!('*' '\') .)* '*' '\';


start: PegExpression