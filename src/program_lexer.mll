{
  open Program_parser
  exception Eof
}
rule token = parse
|  [' ' '\t' '\n' '\r']  { token lexbuf }
| ";" { SEQ }
| "+" { PLUS }
| "-" { SUB }
| "*" { MUL }
| "(" { LP }
| ")" { RP }
| ":=" { ASN }
| "high" { HIGH }
| "low"  { LOW }
| "width" { WIDTH }
| "at" { AT }
| "any" { ANY }
| "," { COM }
| "/" { DIV }
| "{" { LBRACK }
| "}" { RBRACK }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "while" { WHILE }
| "do" { DO }
| "od" { OD }
| "=" { EQ }
| "||" { PAR }
| "!" { NOT }
| "&&" { AND }
| ">" { GT } 
| "<" { LT }
| "skip" { SKIP }
| ['a'-'z']+ as lxm { ID(lxm) }
| ['0'-'9']+ as lxm { NUM(int_of_string lxm) }
| eof            { EOF }
| _ as lxm { Printf.printf "Illegal character %c" lxm; failwith "Bad input" }
