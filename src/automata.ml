(* 

   Omega automata constructions

   The automata constructions implemented in this module are eager.
   That is to say, their state space and transitions are built when
   the automata are constructed, rather than when they are queried.
   By contrast, it may be advantageous to have lazy automata
   constructions for some operations: this may (for example) help
   eliminate dead states that are never used.
   
   However, because our model checker actually punts checking to
   another solver, using an eager approach makes sense.  Many of the
   more efficient constructions seem to be lazy, but aren't
   implemented in our checker.

*)

module L = List
module A = Atom
module U = Utils
module F = Formulas
module M = Model
module X = Xml

exception NoState 

module type STATE = sig
  type t
  val to_string : t -> string
end

(** Representation of an automaton. *)
module type S = 
sig
  type t 

  (** Module to contain operations on states. *)
  module State : STATE

  (** A state. *)
  type state = State.t
    
  (** \Sigma, the alphabet of the automaton. *)
  type a
    
  (** Get the list of states. *)
  val states : t -> state list

  (** Make an empty automaton. *)
  (*val mkempty : t*)

  (** Get the alphabet. *)
  val gen_alphabet : t -> a list

  (** Turn a letter into a string. *)
  val string_of_letter : a -> string

  (** Get the transition from state on `a` *)
  val transition : t -> state -> a -> state list

  (** Get the transition relation *)
  val transitions : t -> state -> (a * state) list

  (** Get the set of initial states. *)
  val initial_states : t -> state list

  (** Dump to an association list. *)
  val to_graph : t -> (string * string) list

  (** Convert an automata to an XML format which can be recognized 
      and manipulated by the GOAL system.  We use this for pretty 
      printing automata, checking containment, etc..
  *)
  val to_gff_xml : t -> string (* Xml.xml*)

  (** Are two automata equal? *)
  val eq : t -> t -> bool
end

(** 
    Representation of a Buchi automaton.  In this case the acceptance 
    condition is a state.
*)
module type BUCHI = sig
  include S

  (** The type of the accepting condition, this type changes based on the type of omega automata:
        - state: Buchi
        - state set: Generalized Buchi
        - {E}/{F}: Rabin
        - Scott,
        - etc...
  *)
  type accepting_cond = state

  (** Get the list of accepting conditions. *)
  val accepting_conditions : t -> accepting_cond list
end

(** Generalized Buchi Automaton: acceptance conditions are sets of
    states.
*)
module type GBA = sig
  include S

  type accepting_cond = state list

  (** Get the list of accepting conditions. *)
  val accepting_conditions : t -> accepting_cond list
end

exception UnexpectedForm

module type AtomAutomaton =
sig 
  include BUCHI
  val mkautomaton : Model.AtomicModel.model -> t
end

module M1 = M.SelfCompositionModel(M.AtomicModel)

(*
  TODO: with type hell for keeping consistency between modules 
   when taking intersect.
*)

module type SelfCompositionAutomaton =
sig 
  include BUCHI
    (** Go from an atomic model to an automaton for that model. *)
  val mkautomaton : Model.SelfComposedAtomic.model -> int -> A.atoms -> A.atoms -> t
end with type a = Atom.atoms list

(* Compose an atomic model [n] times to get a model which applies over some 
   n-trace. *)
module SelfCompositionAutomaton =
struct
  let numstates = 0
    
  module SCM = M.SelfComposedAtomic

  module State = struct
    type t = 
      | InitialState
      | ModelState of SCM.S.t
    let to_string = function 
      | InitialState -> "iota"
      | ModelState s -> SCM.S.to_string s
  end
    
  open State 
    
  (* We need a self composed atomic model, plus Atoms_1 and Atoms_n.
     TODO: Punt this requirement to SelfCompositionAtomic rather than
     requiring we carry it around in the type here.
  *)
  type t = SCM.model * int * A.atoms * A.atoms
      
  (* States are just cross products of the underlying model's states *) 
  type state = State.t
      
  type accepting_cond = state

  (* The alphabet is just sets of atoms, where propositions are
     pointwise elements of the interpretation function L_1 *)
  type a = SCM.interpretation
      
  (*
    Get the set of states in the model; just cross the atomic model 
    with itself n times, *plus* the initial state.
  *)
  let states (m,_,_,_)  = InitialState::(L.map (fun x -> ModelState x) (SCM.states m))
    
  (* Get the transition relation out of state `st`.  Take the lower
     level model, *)
  let transitions (m,_,_,n) = function 
    | InitialState -> 
      L.map (fun state -> (SCM.interpret m state), ModelState state) 
	(SCM.initial_states m)
    | ModelState s -> L.map (fun x -> (SCM.interpret m x), ModelState x) (SCM.next_states m s)
      
  let transition (m,_,_,_) st a = []
    
  (* All states are accepting. *)
  let initial_states _ = [InitialState]
    
  let accepting_conditions = states
    
  let mkautomaton m n atoms_1 atoms_n = (m,n,atoms_1,atoms_n)
    
  let to_graph m = []
    
  let string_of_letter alpha = 
    let sep = "2" in
    List.fold_left (fun acc x -> acc ^ sep ^ (A.string_of_atoms_minimal x)) "" alpha
      
  (* Todo; don't copy this. *)
  let gen_alphabet (m,n,atoms_1,atoms_n) =
    let crossed_sets = 
      U.crossn (List.map (fun x -> A.from_list x) (U.pset (A.to_list atoms_1))) n
    in
    let (p_atoms_n : A.atoms list) = 
      List.map (fun x -> A.from_list x) (U.pset (A.to_list atoms_n)) in
    List.flatten (List.map (fun subset_atoms_n -> 
      List.map (fun p_atoms_1_n -> p_atoms_1_n @ [subset_atoms_n]) crossed_sets)
		    p_atoms_n)

  (* To do the transformation, we set up a bijection between states and 
     integers.  It's not completely clear how to set up a bijection 
     efficiently, but the obvious one is to identify the state with it's
     position in the state list.  For now I'm just using that.
  *)
  let to_gff_xml m = 
    let states = 
      states m 
    in
    (* A helper to go from states to numbers. *)
    let state_to_number = U.getpos states in

    (* States are just regarded as integers. *)
    let states_as_str = 
      L.map string_of_int (U.genseq (L.length states))
    in

    (* We use the modified string_of_letter here, which gives us output 
       suitable for transitions in the GFF format.
    *)
    let alphabet  = L.map string_of_letter (gen_alphabet m) in
    
    let transitions = 
      L.flatten (L.map (fun st -> 
	let transitions_st = transitions m st in
	L.map (fun (alpha,st') -> (st,alpha,st')) transitions_st) states)
    in

    let structure_params = 
      []
    in
    
    (* We map states to their position in the states list, and again apply 
       the transformation to GFF printable output for $\Sigma$. 
    *)
    let transitions_char =
      L.map (fun (s,a,s') -> (string_of_int (state_to_number s), 
			   string_of_letter a,
			   string_of_int (state_to_number s'))) transitions
    in
    
    let alphabet = 
      X.Element ("alphabet", ["type","classical"],
		 L.map (fun alpha_rep -> 
		   X.Element ("symbol",[],[X.PCData alpha_rep])) alphabet)
    in
    let states = 
      X.Element ("stateSet", [],
		 L.map (fun state_rep ->
		   X.Element ("state", ["sid",state_rep],[])) states_as_str)
    in
    let rec gen_transition i acc = function 
      | []                           -> acc
      | (state_s, alpha_s, state_s')::tl -> 
	let node = 
	  X.Element("transition", ["tid",string_of_int i],
		    [X.Element("from",[],[X.PCData state_s])
		    ;X.Element("to",[],[X.PCData state_s'])
		    ;X.Element("read",[],[X.PCData alpha_s])]
	  )
	in
	gen_transition (i+1) (node::acc) tl
    in
    X.to_string_fmt
      (X.Element ("structure", structure_params,
		  [alphabet;
		   states;
		   (X.Element ("transitionSet", [],
			       (gen_transition 0 [] transitions_char)))]))
      
  let eq a1 a2 = false
end

(* 
   To test:

   SelfCompositionAutomaton.mkautomaton cross_ex 2 
   (Atom.from_list ["p";"q"])
   (Atom.from_list ["LE"]);;

*)

module type FAUTOMATON =
  functor
    (P : sig
      val n : int
      val atoms_1 : Atom.atoms
      val atoms_n : Atom.atoms
    end) ->
sig
  include GBA
  val to_automaton : Formulas.formulas list -> Formulas.formula -> t
end with type a = A.atoms list

(* Representation of an automaton for a formula.
   
   Parameterized on: 
   n (the number of paths about which the formula speaks),
   Atoms_1
   Atoms_n
*)

module FormulaAutomaton (P : sig 
  val n : int
  val atoms_1 : A.atoms
  val atoms_n : A.atoms
end) = 
struct
  (* As states, we have sets of formulas.  These are the maximally
     consistent sets, generated from ms(.) *)
  module State : STATE with type t = F.formulas = struct
    type t = F.formulas
    (* Convert a set of sets of formulae to a string. *)
    let to_string fs = 
      "{\n" ^ F.string_of_formulas fs ^ "\n}"
  end
    
  type state = State.t

  (* (ms) *)
  type t = state list * F.formula
      
  type a = A.atoms list

  type accepting_cond = state list

  let to_graph _ = []

  (* The initial state will just be the empty set.  This is a little dirty, 
     but since the empty set should never be maximally consistent, I think 
     this is okay. *)
  let initial_states _ = [[]]

  (*
    For each 
    psi_1 U psi_2 List.mem cl(psi) : {s List.mem S_psi | ...}
  *)
  let accepting_conditions (ms,psi) =
    (* Collect up the until forms. *)
    let until_forms = 
      L.filter  (function | F.U _ -> true | _ -> false) (F.closure psi) in
    (* Two sets of sets s1 and s2 are equal when for all sets s1' in
       s1, there exists a set s2' in s2 such that s1' and s2' are
       equal sets, and mutatis mutandi. *)
    let equal_set_of_sets s1 s2 = 
      (List.for_all (fun s1_i -> (List.exists (fun s2_j -> U.equal_sets s1_i s2_j) s2)) s1) &&
	(List.for_all (fun s2_i -> (List.exists (fun s1_j -> U.equal_sets s2_i s1_j)) s1) s2) in
    (* Filter out empty sets and uniquify. *)
    (fun ss -> U.uniqueify equal_set_of_sets (L.filter ((<>) []) ss))
      (* Take each until form to a set of states containing their negation. *)
      (L.map
	 (function F.U (psi_1, psi_2) ->
	   (* The set of states in ms that satisfy the requirement
	      for psi_1 U psi_2 *)
	   L.filter (fun state -> 
	     (List.mem (F.simplify (F.Not (F.U (psi_1, psi_2)))) state)
	     || (List.mem psi_2 state)) ms
	   | _ -> []) (F.closure psi))
      
  let transition _ _ _ = []
    
  (* Hack on the initial states to the maximally consistent states of the model. *)
  let states (m,_) = []::m
    
  let to_automaton fs phi = (fs,phi)
    
  (* Take everything in Atoms_1^n and add everything in Atoms_n to its end. *)
  let gen_alphabet _ =
    let crossed_sets = 
      U.crossn (List.map (fun x -> A.from_list x) (U.pset (A.to_list P.atoms_1))) P.n
    in
    let (p_atoms_n : A.atoms list) = 
      List.map (fun x -> A.from_list x) (U.pset (A.to_list P.atoms_n)) in
    List.flatten (List.map (fun subset_atoms_n -> 
      List.map (fun p_atoms_1_n -> p_atoms_1_n @ [subset_atoms_n]) crossed_sets)
		    p_atoms_n)
      
  (* TODO: I use string_of_letter in the GffToXml module.  In reality 
     the functor should also be parameterized by a function whih takes an alpha to 
     a representation suitable for the automata.
     
     But because I'm lazy, it's just like this for now.
  *)
  let string_of_letter (alpha : a) = 
    let sep = "2" in
    List.fold_left (fun acc x -> acc ^ sep ^ (A.string_of_atoms_minimal x)) "" alpha
      
      (*
    let a' = L.map (fun x -> A.string_of_atoms x) alpha in
    "{" ^ (L.fold_left (fun acc x -> acc ^ "," ^ x) "" a') ^ "}"
      *)

  (* 
     Calculate the transition relation, definitionally.
     
     Rules:

     - For all $p \in Atoms_1$, if it's in $s_2$, then it has to be
     at the corresponding position in $\alpha$

     - For all $p \in Atoms_n$, if it's in $s_2$, then it has to be in
     the last position in $\alpha$.
     
     - The last statement is iff, so if it's not in $s_2$, it *can't$ be
     in $\alpha$.
     
     - If $X \phi \in s_1$, then $\phi$ has to be in $s_2$.

     - Handle U and R as in paper.
  *)
  let transitions m s =
    let (ms,phi) = m in
    (* Does there exist a transition between (s,alpha,s2)?  *)
    let transition_exists alpha s2 = 
      (* Check requirements for Atoms_1: if we find it in the state,
	 we have to find it in the thing that got us to the state.  If
	 we don't find it in the state, we better not find it in the
	 state.
      *)
      let rec check_atoms alpha s2 = match s2 with 
	| [] -> true
	| F.Multi (ls) :: tl -> 
	  if (List.for_all (fun x -> x) (L.map (fun (x,y) -> 
	    match x with 
	      (* Must be in s2 at the corresponding position. *)
	      | F.Atom a         -> (List.mem a (A.to_list y))
	      | F.Not (F.Atom a) -> not (List.mem a (A.to_list y))
	      | F.True           -> true
	      | F.False          -> false
	      | _                -> raise UnexpectedForm
	  ) (U.zip (ls, alpha)))) then
	    check_atoms alpha tl
	  else
	    false
	| _ :: tl -> 
	  check_atoms alpha tl 
      in

      (* Check that:
	 for all p in Atoms_n
	 p in s2  -> p in prj_n(alpha)
	 ~p in s2 -> p not in prj_n(alpha)
      *)
      let check_atoms_n (alpha :a) (s2 : F.formula list) =
	let alpha_n = A.to_list (L.nth alpha (L.length alpha - 1)) in
	let positive_s2 = 
	  L.fold_left (fun acc f -> match f with 
	    | F.Atom p -> p::acc
	    | _ -> acc
	  ) [] s2
	in
	let negative_s2 =
	  L.fold_left (fun acc f -> match f with 
	    | F.Not F.Atom p -> p::acc
	    | _ -> acc
	  ) [] s2
	in
	U.equal_sets 
	  (U.intersect positive_s2 alpha_n)
	  positive_s2
	&&  
	  (U.intersect negative_s2 alpha_n = [])
      in
      
      (* Check that: 
	 If X Phi_1 in s_1 then Phi_1 in s_2
      *)
      let check_next =
	List.for_all (function 
	  | F.X phi -> (List.mem (F.simplify phi) s2)
	  | _       -> true
	) s
      in
      
      (* 
	 Check the Phi_1 U Phi_2 and Phi_1 R Phi_2 cases.
	 TODO: Possible error on R here, check again.
      *)
      let check_until = 
	List.for_all (fun x -> match x with 
	  | F.U (p1, p2)         -> 
	    if (not (List.mem (F.simplify p2) s)) 
	    then 
	      (List.mem (F.simplify x) s2) 
	    else 
	      true
	  | F.Not (F.U (p1, p2)) -> 
	    if ((List.mem (F.simplify p1) s) || (F.simplify p1) = F.True) 
	    then
	      (List.mem (F.simplify x) s2) 
	    else
	      true 
	  | _                    -> true
	) s
      in
      
      if (List.mem s (initial_states m))
      then
	check_atoms alpha s2 && check_atoms_n alpha s2 && (List.mem (F.simplify phi) s2)
      else
	check_atoms alpha s2 && check_atoms_n alpha s2 && check_next && check_until
    in
    L.flatten (
      (L.map
	 (fun s2 -> 
	   (L.map (fun alpha -> (alpha, s2)) 
	      (L.filter (fun alpha -> transition_exists alpha s2) (gen_alphabet m))))
	 ms))
      
  let to_gff_xml _ = "" (* X.PCData ""*)
    
  let eq m1 m2 = false
end



module BuchiToGffXml (P : BUCHI) (OP : sig
  val state_to_int : P.t -> P.State.t -> int
end)
  = struct
    let to_gff_xml m = 
      let states = 
	P.states m 
      in
      (* A helper to go from states to numbers. *)
      let state_to_number = OP.state_to_int m in
      
      (* States are just regarded as integers. *)
      let states_as_str = 
	L.map string_of_int (U.genseq (L.length states))
      in

      (* We use the modified string_of_letter here, which gives us
	 output suitable for transitions in the GFF format.  *)
      let alphabet  = L.map P.string_of_letter (P.gen_alphabet m) in
      
      let transitions = 
	L.flatten (L.map (fun st -> 
	  let transitions_st = P.transitions m st in
	  L.map (fun (alpha,st') -> (st,alpha,st')) transitions_st) states)
      in

      let structure_params = 
	[("label-on","transition");("type","fa")]
      in
      
      (* We map states to their position in the states list, and
	 again apply the transformation to GFF printable output for
	 $\Sigma$.  *)
      let transitions_char =
	L.map (fun (s,a,s') -> (string_of_int (state_to_number s), 
			     P.string_of_letter a,
			     string_of_int (state_to_number s'))) 
	  transitions
      in
      
      let alphabet = 
	X.Element ("alphabet", ["type","classical"],
		   L.map (fun alpha_rep -> 
		     X.Element 
		       ("symbol",[],[X.PCData alpha_rep])) alphabet)
      in
      let states = 
	X.Element ("stateSet", [],
		   L.map (fun state_rep ->
		     X.Element 
		       ("state", ["sid",state_rep],[])) states_as_str)
      in
      let rec gen_transition i acc = function 
	| []                           -> acc
	| (state_s, alpha_s, state_s')::tl -> 
	  let node = 
	    X.Element("transition", ["tid",string_of_int i],
		      [X.Element("from",[],[X.PCData state_s])
		      ;X.Element("to",[],[X.PCData state_s'])
		      ;X.Element("read",[],[X.PCData alpha_s])]
	    )
	  in
	  gen_transition (i+1) (node::acc) tl
      in
      let state_sid_list states = 
	L.map 
	  (fun st -> 
	    X.Element("stateID",[],[X.PCData 
				       (string_of_int
					  (state_to_number st))]))
	  states
      in
      let initial_states = 
	X.Element ("initialStateSet",[],
		   state_sid_list (P.initial_states m))
      in
      let accepting_states = 
	X.Element ("acc",["type","buchi"],
		   state_sid_list (P.accepting_conditions m))
      in
      X.to_string_fmt
	(X.Element ("structure", structure_params,
		    [alphabet;
		     states;
		     (X.Element ("transitionSet", [],
				 (gen_transition 0 [] transitions_char)));
		     initial_states;
		     accepting_states
		    ]))
  end
  
module Play : sig 
  include S
  val to_automaton : Formulas.formulas list -> Formulas.formula -> t
end with type a = Atom.atoms list = ((FormulaAutomaton(struct 
  let n = 2
  let atoms_1 = A.from_list ["p";"q"]
  let atoms_n = A.from_list ["LE"]
end) ))

let play_ex = Play.to_automaton (F.Examples.es) (F.Examples.e)

let state_of_int m s = Utils.getpos (Play.states m) s;;

let string_of_letter alpha =
  let sep = "2" in
  List.fold_left (fun acc x -> acc ^ sep ^ (Atom.string_of_atoms_minimal x)) "" alpha;;

(*
let to_int m s = Utils.getpos (Play.states m) s;;
struct state_to_int = to_int end
*)

(* Print the transitions from some state. *)
let print_transitions ms state_num = 
  let state = L.nth (Play.states ms) state_num in
  let print_alpha a = Printf.printf "%s, " (Play.string_of_letter a) in
  let print_state s = Printf.printf "%d\n" (U.getpos (Play.states play_ex) s) in
  ignore (L.map (fun (x,y) -> print_alpha x; print_state y)
	    (Play.transitions play_ex state))
    
let print_states a = 
  ignore 
    (List.map (fun x -> Printf.printf "%s\n" (Play.State.to_string x)) (Play.states a))

(*map (fun (x,y) -> (Printf.printf "%s\n" (Play.string_of_letter x)); (Printf.printf "%s\n" (Play.State.to_string y))) (Play.transitions play_ex (hd (Play.states play_ex)));;*)

(* Play.transitions play_ex (hd (Play.states play_ex));;*)

(*
  module ProjectionAutomaton 

  module Play = FormulaAutomaton(struct 
  let n = 2
  let atoms_1 = A.from_list ["p";"q"]
  let atoms_n = A.from_list ["LE"]
  end)

  Play.to_automaton (Formulas.ms [[Formulas.Examples.e]]);;

  Play.to_automaton Formulas.Examples.es Formulas.Examples.e;;

*)

let gen_form_mod n atoms_n atoms_1 = 
  let module S : sig
    val n : int
    val atoms_1 : A.atoms
    val atoms_n : A.atoms
  end
      = struct
	let n = n 
	let atoms_1 = atoms_n
	let atoms_n = atoms_1
      end
  in
  let module FormulaMod = FormulaAutomaton(S) in 
  FormulaMod.to_automaton (F.ms F.Examples.e) F.Examples.e

(* 
   Lift an atomic model to an automaton.
*)
module AtomAutomaton : AtomAutomaton = struct
  module AM = M.AtomicModel
    
  module State = struct
    type t = 
      | InitialState
      | ModelState of AM.S.t
    let to_string = function 
      | InitialState -> "iota"
      | ModelState s -> AM.S.to_string s
  end
    
  (* States are just integers. *)
  type state = State.t
      
  (* We have strings as our labels of formulae *)
  type a = AM.interpretation
      
  (* Representation of automata. *)
  type t = AM.model

  type accepting_cond = state

  (*
    let mkempty = 
    {numstates = 0;
    transition = (fun _ -> []);
    initial = [];
    final = [];
    }
  *)
      
  (* Grab the transition relation. *)
  let transitions m = function 
    | State.InitialState -> 
      L.map (fun s -> (A.from_list [], State.ModelState s)) (AM.initial_states m)
    | State.ModelState s -> 
      List.map 
	(fun x -> ((AM.interpret m x), State.ModelState x))
	(AM.next_states m s)
	
  let transition _ _ _ = []
    
  (* let transitions a = a.transition *)

  (* Generate the set of states. *)
  let states m = 
    State.InitialState :: (List.map (fun x -> State.ModelState x) (AM.states m))

  (* All states are accepting. *)
  let accepting_conditions = states
    
  let initial_states a = [State.InitialState]
    
  let mkautomaton m = m
    
  (* *)
  let to_graph (a :t) = []
  (*L.map (fun s ->
    L.map State.to_string 
    L.filter (fun (x,_) -> x = s) (transitions a s))
  *)

  let string_of_letter alpha = A.string_of_atoms_minimal alpha
    
  let gen_alphabet m = L.map (fun x -> A.from_list x)
    (U.pset (A.to_list (AM.valuations m)))

  (* Create a gff rendering of this.. *)
  let to_gff_xml m = ""
      
  let eq a1 a2 = false
end

(* The automata accepting the empty language:
   XXX: Buggy
*)
let empty_language = AtomAutomaton.mkautomaton 
  (M.AtomicModel.mkmodel [0] [0] [] (fun _ -> A.from_list []) (A.from_list []))

(* 
   To test.. 

   Printf.printf "%s\n" (AtomAutomaton.to_gff_xml (AtomAutomaton.mkautomaton (Examples.atomic_ex)));;
*)

(* Intersection *)

module IntersectOneAllAccepting (S1 : BUCHI) (S2 : BUCHI with type a = S1.a) = 
struct
  module State = struct
    type t = 
	S1.state * S2.state
    let to_string ((s1,s2) : t) = 
      "(" ^ S1.State.to_string s1
      ^ "," ^ S2.State.to_string s2 ^ ")"
  end

  type state = State.t
  type a = S1.a
  type t = S1.t * S2.t
  type accepting_cond = state
      
  (* Inefficient, but get the set of states. *)
  let states (m1,m2) : state list = 
    U.cross (S1.states m1) (S2.states m2)
      
  let mkintersection (a1 : S1.t) (a2 : S2.t) : t = (a1,a2)
  
  let initial_states (a1,a2) =
    assert (L.tl (S1.initial_states a1) = []);
    assert (L.tl (S2.initial_states a2) = []);
    [(L.hd (S1.initial_states a1)),(L.hd (S2.initial_states a2))]
    
  let accepting_conditions (a1,a2) = 
    L.concat (L.map (fun x -> L.map (fun y -> (x,y)) (S2.accepting_conditions a2))
	      (S1.states a1))
  
  let to_graph _ = []

  let transitions (a1,a2) (s1,s2) =
    let transitions_a1 = (S1.transitions a1 s1) in
    let transitions_a2 = (S2.transitions a2 s2) in
    let alphas = U.union (L.map fst transitions_a1) (L.map fst transitions_a2) in
    L.concat (L.concat (L.map (fun alpha -> 
      L.map (function ((alpha1,x1),(alpha2,x2)) -> if (alpha1 = alpha2) then [(alpha1, (x1,x2))] else [])
	(U.cross transitions_a1 transitions_a2)) alphas))
      
  let transition _ _ _ = []
    
  let string_of_letter = S1.string_of_letter
    
  let gen_alphabet (a1,_) = S1.gen_alphabet a1
    
  let to_gff_xml _ = ""
    
  let eq a1 a2 = false
end
      
(* Intersecting two automata to create a product automaton. *)

(* XXX: This is currently buggy.  I was getting different results 
   than what I had expected when testing this on larger automata.
*)
(* TODO: Fix intersection in module. *)
module IntersectAutomaton (S1 : BUCHI) (S2 : BUCHI with type a = S1.a) = 
struct
  module State = struct
    type t = 
	S1.state * S2.state * int
    let to_string ((s1,s2,n) : t) = 
      "(" ^ S1.State.to_string s1
      ^ "," ^ S2.State.to_string s2
      ^ "," ^ string_of_int n ^ ")"
  end
  
  (* Types are just the cross product. *)
  type state = State.t
  
  (* The types of the transition letters. *)
  type a = S1.a

  (* Since this is a Buchi automaton, our accepting conditions are
     just states. *)
  type accepting_cond = state

  (* The automaton is just the constituent functions from the lower level automata. *)
  type t = S1.t * S2.t
      
  (* Inefficient, but get the set of states. *)
  let states (m1,m2) : state list = 
    L.flatten 
      (L.flatten 
	 (L.map
	    (fun s1 ->
	      L.map (fun s2 -> (L.map (fun n -> (s1,s2,n)) [1;2]))
		(S2.states m2))
	    (S1.states m1)))
      
  let mkintersection (a1 : S1.t) (a2 : S2.t) : t = (a1,a2)
  
  let initial_states (a1,a2) =
  L.concat (L.map (fun x -> L.map (fun y -> (x,y,1)) (S2.initial_states a2)) 
	      (S1.initial_states a1))
    
  let accepting_conditions (a1,a2) = 
  L.concat (L.map (fun x -> L.map (fun y -> (x,y,2)) (S2.accepting_conditions a2)) 
	      (S1.states a1))
  
  let to_graph _ = []

  let transitions (a1,a2) (s1,s2,i) =
    let transitions_a1 = S1.transitions a1 s1 in
    let transitions_a2 = S2.transitions a2 s2 in
    let alphas = U.union (U.lfst transitions_a1) (U.lfst transitions_a2) in
    L.flatten 
      (L.map 
	 (fun alpha -> 
	   let states_a1' = L.flatten (L.map (fun (alpha',s) -> if alpha = alpha' then [s] else []) 
					 transitions_a1) 
	   in
	   let states_a2' = L.flatten (L.map (fun (alpha',s) -> if alpha = alpha' then [s] else []) 
					 transitions_a2)
	   in
	   L.map 
	     (fun (a1',a2') -> 
	       let swap_to = match i with
		 | 1 -> if (List.mem s1 (S1.accepting_conditions a1)) then 2 else 1
		 | 2 -> if (List.mem s2 (S2.accepting_conditions a2)) then 1 else 2
	       in
	       (alpha,(a1', a2', swap_to))
	     )
	     (U.cross states_a1' states_a2'))
	 alphas)
      
  let transition _ _ _ = []
    
  let string_of_letter = S1.string_of_letter
    
  let gen_alphabet (a1,_) = S1.gen_alphabet a1
    
  let to_gff_xml _ = ""
    
  let eq a1 a2 = false
end

(* Intersecting two automata to create a product automaton. *)
module ProjectionAutomaton (A : BUCHI with type a = Atom.atoms list) = 
struct
  module State = A.State
  
  (* Types are just the cross product. *)
  type state = A.State.t
  
  (* The types of the transition letters. *)
  type a = A.a
  
  (* The automaton is just the constituent functions from the lower level automata. *)
  type t = A.t * int

  type accepting_cond = state

  let states a : state list = A.states (fst a)
      
  let mkprojection (a : A.t) (k : int) : t = a,k
  
  let initial_states a = A.initial_states (fst a)
    
  let accepting_conditions a = A.accepting_conditions (fst a)

  let to_graph _ = []
    
  let project k alpha = 
    let rec h k alpha' = 
      match alpha' with 
	| []     -> []
	| hd::tl -> if k = 0
	  then []
	  else hd::(h (k-1) tl)
    in
    h k alpha

  (* Project the automata labels. *)
  let transitions (m,k) alpha = 
    let trans = A.transitions m alpha in
    L.map (fun (alpha, s) -> (project k alpha),s) trans
      
  let transition _ _ _ = []
    
  let string_of_letter = A.string_of_letter
    
  let gen_alphabet (a,k) = 
    L.map (project k) (A.gen_alphabet a)
      
  let to_gff_xml _ = ""
    
  let eq a1 a2 = false
end

(* Transform a generalized Buchi to a regular Buchi. *)
module GBAtoBA (A : GBA) = 
struct
  module State = struct
    type t = A.state * int
    let to_string (s,n) =
      "(" ^ A.State.to_string s
      ^ "," ^ string_of_int n ^ ")"
  end
  
  (* Types are just the cross product. *)
  type state = State.t
  
  (* The types of the transition letters. *)
  type a = A.a
      
  (* Since this is a Buchi automaton, our accepting conditions are
     just states. *)
  type accepting_cond = state

  (* The automaton is the Generalized Buchi Automata: we change the
     definition of the states. *)
  type t = A.t
      
  (* Inefficient, but get the set of states. *)
  let states (a : t) = 
    U.cross (A.states a) (U.genseq (L.length (A.accepting_conditions a)))
    
  (* S_0 x {0} *)
  let initial_states a =
    U.cross (A.initial_states a) [0]
      
  (* F_0 x {0} *)
  let accepting_conditions a = 
    U.cross (L.nth (A.accepting_conditions a) 0) [0]
  
  let to_graph _ = []

  (*
    ∆' = { ( (q,i), a, (q',j) ) 
             | (q,a,q') ∈ ∆ 
                and if q ∈ Fi then j=(i mod n)+1 else j=i }
  *)
  let transitions a (s,i) =
    let delta = A.transitions a s in
    if (List.mem s (L.nth (A.accepting_conditions a) i)) then
      List.map (fun (alpha,delta') -> (alpha,(delta',i mod (L.length (A.accepting_conditions a))))) delta
    else
      List.map (fun (alpha,delta') -> (alpha,(delta',i))) delta
	
  let transition _ _ _ = []
    
  let string_of_letter = A.string_of_letter
    
  let gen_alphabet = A.gen_alphabet
    
  let to_gff_xml _ = ""
    
  let eq a1 a2 = false
    
  let generalized_to_buchi = fun x -> x
end

(*

  let cross_a = SelfCompositionAutomaton.mkautomaton cross_ex 2
  (Atom.from_list ["p";"q"]) (Atom.from_list ["LE"]);;

  module IA = IntersectAutomaton (Play) (SelfCompositionAutomaton);;
  A.mkintersection play_ex cross_a;;
*)

(*

  type state = int

(* Alphabet is just a list of propositions. *)
  type a = string list 

(* Transitions, and accepting states. *)
  type t = 
  {transition: state -> a -> state list,
  acc: state list,
  init: state list}
  
  let transition a s c = (t.transition) s c

  let accepting_states = t.acc

  let initial_states = t.init

  let intersect ( t2 =);
  

  module Intersect = 
  functor (S1 : S) (S2 : S) -> S
  end
*)

(* Unit test cases for this module. *)
module Test = struct

end
