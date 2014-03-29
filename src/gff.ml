(*
  Interface to the GOAL solver.
*)

let pp = Printf.printf

module U = Unix
module S = String

let write_to_file ~filename:name ~contents:contents = 
  try
    let cout = open_out name in
    let co = Format.formatter_of_out_channel cout in
    Format.fprintf co "%s\n" contents;
    close_out cout
  with Sys_error _ as e ->
    Format.printf "Cannot open file \"%s\": %s\n" name (Printexc.to_string e)

let check_containment a1 a2 goal_executable = 
  let phi_f = "model_phi.gff" in
  let not_phi_f = "model_not_phi.gff" in
  write_to_file "model_phi.gff" a1;
  write_to_file "model_not_phi.gff" a2; 
  pp "Written to files...\n";
  pp "Calling GOAL's solver...\n";
  let in_ch = U.open_process_in (S.concat " " [goal_executable; "containment"; not_phi_f; phi_f;]) in
  pp "Goal says...\n%s\n\n" (input_line in_ch)

let check_empty a1 goal_executable = 
  write_to_file "maybe_empty.gff" a1;
  pp "Written to files...\n";
  pp "Calling GOAL's solver...\n";
  let in_ch = U.open_process_in (S.concat " " [goal_executable; "containment"; "maybe_empty.gff"; "empty_aut.gff";]) in
  pp "Goal says...\n%s\n\n" (input_line in_ch)
  ;
