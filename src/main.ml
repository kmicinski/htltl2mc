(*
  The main driver for the model checker.
*)

let pp = Printf.printf
module C = Checker
module P = Program
module U = Utils

open Formulas

let main _ = match (Array.length Sys.argv) with
  | 4 ->
    let _ = pp "Running the model checker with %s as the input file..\n" (Sys.argv.(1))
    in
    let _ = pp "Verifying property:\n%s\n" Sys.argv.(2) in
    let input_file = Sys.argv.(1) in
    let regex      = Str.regexp ".*/?\(.*\).mod$" in
    let model_name = Str.search_forward regex input_file 0; 
      Str.matched_group 0 input_file in
    pp "Model name: %s\n" model_name;
    C.check_formula_and_model input_file Sys.argv.(2) model_name Sys.argv.(3);
    pp "Finished checking all properties.\n"
  | _ ->
    pp "./model_checker input.model formula path-to-goal\n";;

main ()
