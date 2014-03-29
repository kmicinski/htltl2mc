{
  open Model_parser
  exception Eof
}
rule token = parse
|  [' ' '\t' '\n' '\r']  { token lexbuf }
| "," { COMMA }
| "(" { LPAREN }
| ")" { RPAREN }
| "{" { LBRACE }
| "}" { RBRACE }
| "->" { TO }
| "start" { START }
| "alphabet" { ALPHABET }
| "L1" { L1 }
| "Ln" { LN }
| '"' [^'"']* '"' as lxm { STR(String.sub lxm 1 ((String.length lxm) - 2)) }
| ['0'-'9']+ as lxm { INT(int_of_string lxm) }
| eof { EOF }
| _ as lxm { Printf.printf "Illegal character %c" lxm; failwith "Bad input" }
