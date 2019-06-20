%{
%}

%token <int> NUM
%token <string> ID
%token PLUS SUB MUL DIV LT GT EQ NOT AND SKIP ASN IF ELSE THEN WHILE DO OD
%token SEQ WIDTH ANY AT LP RP LBRACK RBRACK COM HIGH LOW EOF PAR
%left PLUS SUB
%right MUL DIV LT GT EQ AND SEQ
%nonassoc NOT
%start program
%type <Program.cmd> cmd
%type <Program.program> program
%%

program:
| LBRACK cfglst RBRACK cmd EOF { Program.mk_program $2 $4 }
;

cfglst:
| cfg COM cfglst { $1 :: $3 }
| cfg            { [$1] }
|                { [] }
;

cfg:
| LP ID ASN NUM AT lev WIDTH EQ NUM RP 
    { ($2, $6, $9, Program.Specific $4) }
| LP ID ASN ANY AT lev WIDTH EQ NUM RP 
	{ ($2, $6, $9, Program.Any) }
;

lev:
| HIGH { Program.H }
| LOW  { Program.L }
;

cmd:
| SKIP                     { Program.Skip }
| ID ASN exp               { Program.Asn ($1, $3) }
| cmd SEQ cmd              { Program.Seq ($1, $3) }
| IF exp THEN cmd ELSE cmd { Program.If ($2, $4, $6) }
| IF exp THEN cmd          { Program.If ($2, $4, Program.Skip) }
| WHILE exp DO cmd OD      { Program.While ($2, $4) }
| LBRACK cmds RBRACK       { Program.Choice $2 }
;

cmds:
| cmd                      { [ $1 ] }
| cmd PAR cmds             { $1 :: $3 }
;

exp:
| NUM                 { Program.Const $1 }
| ID                  { Program.Id $1 }
| exp PLUS exp        { Program.Plus ($1, $3) }
| exp SUB exp         { Program.Sub ($1, $3) }
| exp MUL exp         { Program.Mul ($1, $3) }
| exp DIV exp         { Program.Div ($1, $3) }
| exp LT exp          { Program.Lt ($1, $3) }
| exp GT exp          { Program.Gt ($1, $3) }
| exp EQ exp          { Program.Eq ($1, $3) }
| NOT exp             { Program.Not $2 }
| exp AND exp         { Program.And ($1, $3) }
| LP exp RP           { $2 }
;

