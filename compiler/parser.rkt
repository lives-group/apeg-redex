#lang peg-parser


Grammar <-- (Production ~";")+ ~"start" ~':' NonterminalCall;


Production <-- Nonterminal ~'{' (ProductionInput (~',' ProductionInput)*)? '}' ~'{' (Expression (~',' Expression)*)? ~'}' ~"<--" PegExpression;
ProductionInput <-- Type ~"::" Attribute;


PegExpression <-- PegSequence (~'/' PegExpression)?;
PegSequence <-- PegPrefix+;
PegPrefix <-- ('!')? PegUnaryOperation;
PegUnaryOperation <-- PegFactor ('*' / '+' / '?')?;
PegFactor <-- Terminal / NonterminalCall / Range / PegParenthesesExpression / Empty / Restriction / Update / Bind;


Terminal <-- ^Character / -String;
Character <-- ((~'\'' ^CharacterAuxiliary ~'\'') ~WS);
CharacterAuxiliary <-- ((!('\' / '\'' / '\"' / '\b' / '\n' / '\r' / '\t' ) .) / ('\' ('\' / '\'' / '\"' / 'b' / 'n' / 'r' / 't')));
String <-- -((~'\"' CharacterAuxiliary* ~'\"') ~WS);


NonterminalCall <-- Nonterminal ~'{' (Expression (~',' Expression)*)? '}' ~'{' (Attribute (~',' Attribute)*)? ~'}';
Nonterminal <-- -((['A'-'Z'] / '_') (['a'-'z'] / ['0'-'9'] / ['A'-'Z'] / '_')*);


Range <-- ~'[' (^Character ~'-' ^Character / -RangeList) ~']';
RangeList <-- ^Character (~',' ^Character)*;


PegParenthesesExpression <-- ~'(' PegExpression ~')';


Empty <-- ~'ε';


Restriction <-- ~'{' ~'?' Expression ~'}';
Update <-- ~'{' (UpdateValue (~',' UpdateValue)*)? ~'}';
UpdateValue <-- Attribute ~"<-" Expression;
Bind <-- ~'{' Attribute ~'=' PegExpression (~',' Attribute ~'=' PegExpression)* ~'}';


Type <-- -BasicType / ('<' ^Type ~'>') / ('[' ^Type ~']');
BasicType <-- "int" / "float" / "boolean" / "string";


Expression <-- ~WS OrConditional;
OrConditional <-- AndConditional (~"||" ~WS AndConditional)*;
AndConditional <-- ConditionalExpression (~"&&" ~WS ConditionalExpression)*;
ConditionalExpression <-- AdditionExpression ((-"==" / '>' !'>') ~WS AdditionExpression)?;
AdditionExpression <-- Term (('+' / '-') ~WS Term)*;
Term <-- Factor (('*' / '/') ~WS Factor)*;
Factor <-- '-' ~WS Primary / '~' ~WS Primary / Primary;
Primary <-- Attribute / Number / String / Parentheses / Boolean / LiteralList / ListHandler / LiteralMapping / MappingHandler;


Attribute <-- -((!(("head" / "tail" / "get" / "put" / Boolean / BasicType)!(['a'-'z'] / ['0'-'9'] / ['A'-'Z'] / '_')) ['a'-'z'] (['a'-'z'] / ['0'-'9'] / ['A'-'Z'] / '_')*) ~WS);


Number <-- -((Real / Integer) ~WS);
Integer <-- ('-'? (['1'-'9'] ['0'-'9']*) / '0');
Real <-- '-'? (['1'-'9'] ['0'-'9']* / '0') '.' ['0'-'9']+;


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


WS <-- ([' ', '\n', '\t'] / LineComment / BlockComment)*;
LineComment <-- "\\" (!'\n' .)*;
BlockComment <-- '\' '*' (!('*' '\') .)* '*' '\';


start: PegExpression