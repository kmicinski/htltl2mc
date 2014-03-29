%{
  open Formulas
%}

%token <int> INT
%token <string> STR
%token A TO E EOF LBRACKET RBRACKET NOT OR X U TRUE FALSE COMMA AND G F IMPLIES
%token LPAREN RPAREN
%start main
%type <Formulas.formula> main ae psi phi
%type <Formulas.formula list> phis
%right COMMA
%right OR
%right U
%%
main:
 ae { $1 }
;

ae: 
  A TO INT E TO INT psi { AE($3,$6,$7) }
;

psi: 
| LPAREN psi RPAREN        { $2 }
| STR                    { Atom $1 }  
| LBRACKET phis RBRACKET { Multi $2 }
| NOT psi                  { Not $2 }
| G psi                    { G $2 }
| F psi                    { F $2 }    
| psi OR psi                 { Or ($1, $3) }
| psi TO psi                 { And ($1, $3) }
| psi IMPLIES psi            { Or (Not($1),$3) }
| X psi                    { X $2 }
| psi U psi                  { U ($1, $3) }

phis:
|              { [] }
| phi            { [$1] }
| phi COMMA phis { $1::$3 }
;

phi: 
| TRUE    { True }
| FALSE   { False } 
| STR     { Atom $1 }
| NOT STR { Not (Atom $2) }
| phi OR phi  { Or ($1, $3) }
;
