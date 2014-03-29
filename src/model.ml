(* 
   Models in HyperLTL: just simple transition systems for now.
   
   Note that throughout our presentation here, we use lowercase `l` to
   refer to the interpretation function, rather than an uppercase `L`.  This
   is because OCaml's naming conventions require that we use the
   lowercase letter to refer to functions rather than constructors.
   
*)

module A = Atom
module L = List
module H = Hashtbl
module U = Utils

(*
  module ModelGraph = Graph.Persistent.ConcreteBidirectional(ModelNode) 
*)

(* States are atoms that are true in those states. *)
module AtomsState = struct 
  (*let flat = List.fold_left (fun a c -> c ^ a) ""*)
  type t = int
  let compare = (-)
  let hash = H.hash
  let equal = (=)
  let rec to_string = string_of_int
end

(* Interpretations. *)
(*module type INTERPRETATION (S : STATE) = 
sig
  type interpretation : S.t -> atom list
end
*)

module type STATE = 
sig
  type t
  val compare : t -> t -> int
  val to_string : t -> string
  val hash : t -> int
  val equal : t -> t -> bool
end

module type INTERPRETATION = 
  functor (S : STATE) -> 
sig
  type t = S.t -> A.atoms
end

module type MODEL =
sig
  module S : STATE
  (*module I : INTERPRETATION*)
  type model
  type state = S.t
  type states = state list
  type atoms = A.atoms
  type interpretation
    
  val states : model -> states 
  val initial_states : model -> state list
  val next_states : model -> state -> states
  val interpret : model -> state -> interpretation
  val eq : model -> model -> bool
end

module type AMODEL = (sig
  include MODEL
  val mkmodel : int list -> int list ->  (int * int) list -> (int -> Atom.atoms) 
    -> Atom.atoms -> model
  val state_of_int : int -> state
  val int_of_state : state -> int
  val valuations   : model -> Atom.atoms
end) with type interpretation = A.atoms

module type COMPMODEL = 
  functor (A : AMODEL) -> sig
    include MODEL
    val mkmodel : A.model -> (int list -> Atom.atoms) -> int -> Atom.atoms -> model
    val getdimension : model -> int
    val valuations_1 : model -> Atom.atoms
    val valuations_n : model -> Atom.atoms
  end

module Interpretation (S : STATE) = 
struct
  type t = S.t -> A.atoms
end

(* A simple model which has as states simply atomic propositions. *)
module AtomicModel : AMODEL = struct
  module S = AtomsState
  (* module I = Interpretation *)
    
  type state = int
  type states = state list
  type atoms = A.atoms
  type interpretation = atoms

  (* Initial state, set of states, transition relation,
     interpretation function, and range of possible valuations. *)
  type model = 
      state list * state list * (state * state) list 
        * (state -> interpretation) * interpretation
      
  let initial_states (x,_,_,_,_) = x
  let states         (_,s,_,_,_) = (s : states)
  let next_states    (_,_,d,_,_) s = L.map snd (L.filter (fun (s',_) -> s' = s) d)
  let to_graph       (_,_,d,_,_) = L.map (fun (x,y) -> 
    ((string_of_int x) ^ "," ^ (string_of_int y))) d
  let mkmodel il sl delta l vs = (il,sl,delta,l,vs)
  let interpret      (_,_,_,l,_) = l
  let valuations     (_,_,_,_,a) = a
  let state_of_int   i = i
  let int_of_state   i = i
    
  (* Are two models equal?

     Models are equal when their state spaces are equal, transition
     sets are equal, and interpertations are equal pointwise.  
  *)
  let eq m1 m2 = 
    let equal_state_spaces = 
      U.equal_sets (states m1) (states m2)
    in
    List.for_all (fun s -> U.equal_sets (next_states m1 s) (next_states m2 s))
      (states m1)
    && 
      List.for_all (fun s -> A.equal (interpret m1 s) (interpret m2 s))
      (states m1)
end

module SelfCompositionModel (A1 : AMODEL) = (*  *)
struct
  (* 
     Raise this if someone tries to do an operation with the wrong 
     arity of operations for the model.
     
     expected * got
  *)
  exception ComposedModelWrongArity of int * int

  module S : (STATE with type t = A1.state list) = struct
    type t = A1.state list
    let compare a b = (H.hash a) - (H.hash b)
    let hash = H.hash
    let equal = (=)
    let rec to_string s = 
      L.fold_left (^) "" (L.map A1.S.to_string s)
  end
    
  type atoms = A.atoms
  type state = A1.state list
  type states = state list
  type interpretation = atoms list
      
  (* Underlying atomic model, interpretation in Atoms_n, and n. *)
  type model = A1.model * (state -> atoms) * int * Atom.atoms
      
  (* Check the arity of the underlying list. *)
  let checkarity (l : 'a list) = function (_,_,i,_) ->
    if (L.length l <> i) then raise (ComposedModelWrongArity (i, (L.length l))) else l
      
  let states         (m,_,n,_) = U.crossn (A1.states m) n
  let initial_states (m,_,n,_) = U.crossn (A1.initial_states m) n
  let next_states   m s = 
    let (m',_,n,_) = m in
    let next_states_componentwise = L.map (fun x -> A1.next_states m' x) (s) 
    in
    (U.cross_n_lists next_states_componentwise)
      
  (* At first I ran into an error because I didn't have A1.state:
     instead I had int. *)

  (* Make a model from an atomic model *)
  let mkmodel       (m :A1.model) (l : int list -> Atom.atoms) (n : int) (valuations_n : Atom.atoms) : model = 
    (m,(fun s -> l (L.map (fun (x : A1.state) -> A1.(int_of_state x)) s)),n,valuations_n)
    
  (* let to_graph      m = [] *) (* L.map (fun x -> 
			      (S.to_string x, (next_states m x))) (states m) *)
    
  (* Interpret each state in A1 and then append interpretation in S^n *)
  let interpret     (m : model) s =
    match m with
      | (m',l,n,_) -> (List.map (fun s' -> (A1.interpret m') s') (checkarity s m)) @ [(l s)]
      
  (* Get the dimension of the model, the cardinality of the sets of
     states. *)
  let getdimension  (_,_,n,_) = n
    
  let eq _ _ = false

  let valuations_1 (m,_,_,_) = A1.valuations m

  let valuations_n (_,_,_,v) = v
end

(* The canonical self composed model. *)
module SelfComposedAtomic = SelfCompositionModel(AtomicModel)

(* 
   Example models.
*)
module Examples = struct 
  let m = AtomicModel.mkmodel [1] [1;2;3] [(1,2); (1,3); (2,2); (3,3)]
  let atomic_ex =
    let interp = function 
      | 1 -> A.from_list []
      | 2 -> A.from_list ["p"; "q"]
      | 3 -> A.from_list ["q"]
    in
    m interp (A.from_list ["p";"q"])
      
  let atomic_ex_2 =
    let interp = function 
      | 1 -> A.from_list []
      | 2 -> A.from_list ["p"; "q"]
      | 3 -> A.from_list ["p"; "q"]
    in
    m interp (A.from_list ["p";"q"])
  
  let interp_pairs = A.eta_expand_alist 
    (function 
      | [1;1] -> ["LE"]
      | [2;2] -> ["LE"]
      | [3;3] -> ["LE"]
      | [2;3] -> ["LE"]
      | [3;2] -> ["LE"]
      | _     -> [])

  let cross_ex = SelfComposedAtomic.mkmodel atomic_ex interp_pairs 2 (A.from_list ["LE"]);;
end

(*let lexer = make_lexer ["{","}","start","to","alphabet", "is", "has",","]*)

(*let p l =
  | parser [< `Start; `Int n >] -> Start n
  | parser [< `Int n; `To; `Int n' >] -> To (n, n')
  | parser [< `Alphabet; `Is; `LB; `Int n; `Comma; p >] -> *)

(*module SCAtomic = SelfCompositionModel(AtomicModel)*)

(* An example M^2 model. *)
(*let sc = SCAtomic.mkcomposed m*)

(* Dump it to a graph..*)
(*M.to_graph sc*)

(*
Output...

[("1,1", [(2, 2); (3, 2); (2, 3); (3, 3)]); ("2,1", [(2, 2); (2, 3)]);
 ("3,1", [(3, 2); (3, 3)]); ("1,2", [(2, 2); (3, 2)]); ("2,2", [(2, 2)]);
 ("3,2", [(3, 2)]); ("1,3", [(2, 3); (3, 3)]); ("2,3", [(2, 3)]);
 ("3,3", [(3, 3)])]

*)

(*module Make (St : STATE) (At : ATOM) : MODEL =
struct
  type model = St.t list * St.t list * (St.t -> St.t list) * int
  type state = St.t
  type states = state list
  type atom = At.t
  type atoms = atom list
  let initial_states (i,_,_,_) = i
  let next_states (_,_,f,_) = f
    
end
*)

(* Convert a list of states, association list (representing transition relation),
   and initial state, to a model *)
(*
let tomodel = 
  let transition_fn = fun st ->
    L.map snd (L.filter (fun (x,y) -> St.equals
*)

(* module for creating dot-files *)
(*module Graph = 
  functor (M : MODEL) -> 
    G.Graphviz.Dot(
      struct
	include Graph.Persistent.Digraph.ConcreteBidirectional(M.S)
	let edge_attributes _ = []
	let default_edge_attributes _ = []
	let get_subgraph _ = None
	let vertex_attributes _ = [`Shape `Box]
	let vertex_name v = ""
	let default_vertex_attributes _ = []
	let graph_attributes _ = []
      end)
*)

type model_specification = 
  | Start of int list 
  | To of int * int list
  | Alphabet of string list
  | L1 of int * string list
  | Ln of int list * string list

(* Tests for model building. *)
module Test = struct
  open OUnit
    
  (* Assert that a self composed model was properly built from the
     atomic model. *)
  (* let assert_self_composed_model_correct amodel scmodel =*)

end
