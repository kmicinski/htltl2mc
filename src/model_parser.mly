%{
 module M = Model
%}

%type <Model.model_specification list> main
%token <int> INT
%token <string> STR
%token LBRACE RBRACE COMMA LPAREN RPAREN TO EOF START ALPHABET L1 LN
%start main
%%
main:
 rules EOF { $1 }
;

rules:
             { [] }
| rule rules { $1::$2 }

rule: 
| START ints                 { Model.Start $2 }
| INT TO ints                { Model.To ($1, $3) }
| ALPHABET strs              { Model.Alphabet $2 }
| L1 LPAREN INT RPAREN strs  { Model.L1 ($3, $5) }
| LN LPAREN ints RPAREN strs { Model.Ln ($3, $5) }
;

ints: 
| LBRACE ints RBRACE { $2 }
| INT                { [$1] }
| INT COMMA ints     { $1 :: $3 }
;

strs: 
| LBRACE strs RBRACE { $2 }
|                    { [] }
| STR                { [$1] }
| STR COMMA strs     { $1 :: $3 }
;
