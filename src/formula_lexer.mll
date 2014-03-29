{
  open Formula_parser
  exception Eof
}
rule token = parse
|  [' ' '\t' '\n' '\r']  { token lexbuf }
| "," { COMMA }
| "A" { A }
| "E" { E }
| "X" { X }
| "U" { U }
| "T" { TRUE }
| "G" { G }
| "F" { F }
| "False" { FALSE }
| "^" { TO }
| "|" { OR }
| "(" { LPAREN }
| ")" { RPAREN }
| "[" { LBRACKET }
| "]" { RBRACKET }
| "->" { IMPLIES }
| "~" { NOT }
| '"' [^'"']* '"' as lxm { STR(String.sub lxm 1 ((String.length lxm) - 2)) }
| ['0'-'9']+ as lxm { INT(int_of_string lxm) }
| eof { EOF }
| _ as lxm { Printf.printf "Illegal character %c" lxm; failwith "Bad input" }
