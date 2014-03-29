(*
  The main checker, which grinds the knobs and cranks the equations.
*)

module M = Model
module A = Atom
module L = List
module G = Gff
module U = Utils
module F = Formulas
module P = Parse_model
module AT = Automata

let pp = Printf.printf

(* Check, once we have parsed the formula and model. *)
let check psi self_composed_model model_name goal_executable = 
  let F.AE (k,j,psi_1) = psi in
  pp "Simplified formula: %s\n" (F.to_string psi);
  pp "Parsing model...\n";
  (* The negation of the formula. *)
  let atoms_1 = M.SelfComposedAtomic.valuations_1 self_composed_model in
  let atoms_n = M.SelfComposedAtomic.valuations_n self_composed_model in
  let psi_1_n = F.simplify (F.Not psi_1) in
      
  (* Calculate ms set. *)
  let ms = F.ms psi_1 in
  pp "The maximally consistent sets are:\n";
  pp "{\n";
  List.map (fun fs -> pp "%s,\n" (F.string_of_formulas fs)) ms;
  pp "}\n";
  
  (* Parameters of the model, A^k E^j, and sets of atoms, L_1 and L_n *)
  let module A_psi_params =
      struct
	let n = k+j
	let atoms_1 = atoms_1
	let atoms_n = atoms_n
      end
  in
      
  (* A module specific to the formula that is required: this allows us
     to take a formula to a generalized Buchi automaton.  *)
  let module MA_Psi_GBA : sig
    include AT.GBA
    val to_automaton : Formulas.formulas list -> Formulas.formula -> t
  end with type a = A.atoms list
	= AT.FormulaAutomaton(A_psi_params)
  in
  
  (* Now take the generalized Buchi automaton to a regular Buchi
     automaton. *)
  let module MA_Psi : sig
    include AT.BUCHI
    val generalized_to_buchi : MA_Psi_GBA.t -> t
  end with type a = A.atoms list
	= AT.GBAtoBA (MA_Psi_GBA)
  in
  
  let module IA = 
	(AT.IntersectOneAllAccepting (AT.SelfCompositionAutomaton) (MA_Psi)
	   : sig
	     include AT.BUCHI
	     val mkintersection : AT.SelfCompositionAutomaton.t -> MA_Psi.t -> t
	   end with type a = A.atoms list)
  in
  pp "Constructing automata for formula...\n";
  

  (* The automata for the formula and its negation. *)
  let a_psi     = MA_Psi.generalized_to_buchi (MA_Psi_GBA.to_automaton ms psi_1) in
  let a_not_psi = MA_Psi.generalized_to_buchi (MA_Psi_GBA.to_automaton ms psi_1_n) in

  (* This is a hack, it is the empty language: GOAL doesn't provide
     evidence on its own for the emptiness command.  Instead we hack
     it so that we check containment of L in the empty langauge. *)
  let empty_aut = MA_Psi.generalized_to_buchi (MA_Psi_GBA.to_automaton [] psi_1) in

  (* Dump the formulae automata to files. *)
  pp "Writing formulae automata to files...\n";
  let module DF = AT.BuchiToGffXml (MA_Psi) (struct 
    let state_to_int m s = Utils.getpos (MA_Psi.states m) s
  end)
  in
  G.write_to_file "a_psi.gff" (DF.to_gff_xml a_psi);
  G.write_to_file "a_not_psi.gff" (DF.to_gff_xml a_not_psi);
  (*G.write_to_file "empty_aut.gff" (DF.to_gff_xml empty_aut);*)
  
  let print_state s = Printf.printf "%s\n" (MA_Psi.State.to_string s) in
  L.map print_state (MA_Psi.accepting_conditions a_psi);

  pp "The constructed automaton has %d states.\n"
    (L.length (MA_Psi.states a_psi));
  
  pp "Creating automaton for the model...\n";
  let model_a = 
    AT.SelfCompositionAutomaton.mkautomaton 
      self_composed_model (k+j) atoms_1 atoms_n
  in
  
  pp "The constructed automaton has %d states.\n"
    (L.length (AT.SelfCompositionAutomaton.states model_a));
  match (k,j) with
    | (n,0) ->
      (* A case *)
      pp "(n,0) case: \n";
      let intersect_aut = IA.mkintersection model_a a_not_psi in
      let module IGFF = AT.BuchiToGffXml (IA) (struct 
	let state_to_int m s = Utils.getpos (IA.states m) s
      end) in
      let module EGFF = AT.BuchiToGffXml (AT.AtomAutomaton) (struct 
	let state_to_int m s = Utils.getpos (AT.AtomAutomaton.states m) s
      end) in
      G.write_to_file "int_aut.gff" (IGFF.to_gff_xml intersect_aut);
      pp "Now to check the formula, we check emptiness of A^n_M \\cap A_{\\lnot \\psi}\n";
      G.check_empty
	(IGFF.to_gff_xml intersect_aut)
	goal_executable
    | (0,n) ->
      (* E case. *)
      ()
    | k,j ->
      (* AE case. *)
      pp "Checking consistency of this formula with the model:\n%s\n"
	(F.to_string psi);
      
      let module MGFF = AT.BuchiToGffXml (AT.SelfCompositionAutomaton) (struct 
	let state_to_int m s = Utils.getpos (AT.SelfCompositionAutomaton.states m) s
      end) in
      
      G.write_to_file "sc_model.gff" (MGFF.to_gff_xml model_a);

      pp "Creating intersection automaton for formula and model\n";
      let module PA = AT.ProjectionAutomaton (IA) in
      let module TG = AT.BuchiToGffXml (PA) (struct 
	let state_to_int m s = Utils.getpos (PA.states m) s
      end)
      in
      pp "Projecting automata\n";
      let ia_psi_proj     = PA.mkprojection (IA.mkintersection model_a a_psi) k in
      pp "intersection automaton...\n\n";
      let num = ref 0 in
      let ia_not_psi_proj = PA.mkprojection (IA.mkintersection model_a a_not_psi) k in
      pp "The constructed automaton has %d states.\n" 
	(L.length (PA.states ia_psi_proj));
      pp "Now to check the formula, we check containment of these two automata\n";
      G.check_containment 
	(TG.to_gff_xml ia_psi_proj) 
	(TG.to_gff_xml ia_not_psi_proj)
	goal_executable

(* Run the checker with a model derived from a program's semantics. *)
let check_program_and_model program formula goal_executable = 
  check formula program "" goal_executable
    
let check_formula_and_model
    ~model_filename:model_name
    ~formula_str:psi_str
    ~model_name:model_name
    ~goal_executable:goal_executable = 
  let lexbuf = Lexing.from_string psi_str in
  match 
    ((try
      Some 
        (F.simplify
              (Formula_parser.main 
                 (Formula_lexer.token) lexbuf))
    with _ -> 
      pp "Error: could not parse formula %s\n" psi_str;
      None),
     (try
	Some (P.parse_model model_name)
      with _ -> 
	pp "Error: could not parse model in file %s\n" model_name;
	None))
  with
    | (Some psi),(Some self_composed_model)  ->
      check psi self_composed_model model_name goal_executable
    | _                                    -> ()
