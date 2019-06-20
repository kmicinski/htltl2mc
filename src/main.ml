(*
  Driver for program generator
*)

let pp = Printf.printf
module I = Imp_to_lts
module U = Utils

let main _ : unit = match (Array.length Sys.argv) with
  | 3 -> 
    let parse_program fname = 
      let str = U.read_file fname in
      let lexbuf = Lexing.from_string str in
      Program_parser.program (Program_lexer.token) lexbuf
    in
    (* pp "Generating model from input program.\n";*)
    let input_file = Sys.argv.(1) in
    let output_file = Sys.argv.(2) in
    let output_mod =
      I.build_model_from_program (parse_program input_file) output_file in
    ()
    (* pp "done...\n"; *)
  | _ -> 
    pp "./model_checker input.program output.lts\n";;

main ()
