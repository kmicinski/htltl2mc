(* 
   Parsing of models.
*)

module A = Atom
module U = Utils
module L = List
module M = Model

let read_file filename = 
  let lines = ref "" in
  let chan = open_in filename in
  try
    while true; do
      lines := !lines ^ "\n" ^ (input_line chan)
    done; ""
  with End_of_file ->
    close_in chan;
    !lines

(* Parse the model and get a self composed model out. *)
let parse_model pname = 
  let str = read_file fname in
  let lexbuf = Lexing.from_string str in
  let program = Program_parser.main (Model_lexer.token) lexbufn in
  (*
  let model_specs = Model_parser.main (Model_lexer.token) lexbuf in
  let states = 
    U.uniqueify (=) (L.flatten (L.map (function | M.To (i,_) -> [i] | _ -> []) model_specs)) in
  let start_states = 
    L.flatten (L.flatten (L.map (function | M.Start il -> [il] | _ ->  []) model_specs)) in
  let transition =
    L.flatten
      (L.map 
	 (function M.To (i,is) -> (L.map (fun i' -> (i,i')) is)
	   | _        -> [])
	 model_specs)
  in
  let l1 = 
    A.eta_expand_alist 
      (fun s -> (L.flatten 
		(L.flatten 
		   (L.map (function | M.L1 (i,sl) when i = s -> [sl] | _ -> []) model_specs))))
  in
  let ln_size = 
    L.hd (L.flatten (L.map (function | M.Ln (sl,_) -> [L.length sl] | _ -> []) model_specs))
  in
  let (ln : int list -> A.atoms) = 
    A.eta_expand_alist 
      (fun s -> (L.flatten
		(L.map (function | M.Ln (is,sl') when is = s -> sl' | _ -> []) model_specs)))
  in
  let alphabet_1 = 
    U.uniqueify (=) (L.flatten 
		       (L.flatten 
			  (L.map (function | M.L1 (is,sl) -> [sl] | _ -> []) model_specs)))
  in
  let alphabet_n = 
    U.uniqueify (=) (L.flatten 
		       (L.flatten 
			  (L.map (function | M.Ln (is,sl) -> [sl] | _ -> []) model_specs)))
  in
  let atomic_model = 
    M.AtomicModel.mkmodel [1] (* (L.hd start_states) *)
      states
      transition 
      l1
      (A.from_list alphabet_1)
  in
  M.SelfComposedAtomic.mkmodel atomic_model ln ln_size (A.from_list alphabet_n)
  *)
