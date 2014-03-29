(**
   Definition of models as state transition systems.
*)

(** {2 State representation}
    E.g., we might just have states regraded as integers, or we may have 
    states which represent tuples.
*)

module type STATE = 
sig
  type t
  val compare : t -> t -> int
  val to_string : t -> string
  val hash : t -> int
  val equal : t -> t -> bool
end

(** Abstract type for what a state within a model should be.

    Roughly speaking, we need to be able to display it as a vertex
    within a graph (for output) and uniquely identify it.  Since
    OCamlGraph also uses hasing, etc.., we support these operations.
*)

module type INTERPRETATION = 
  functor (S : STATE) -> 
sig
  type t = S.t -> Atom.atoms
end


(*
(** Atoms, where strings are the identifiers of atoms. *)
module type ATOM = 
  sig
    type t
    val eq : t -> t -> bool
  end
*)

(** {2 Models} *)

(** Abstract definition of models.  

    Models are represented by:
     - states,
     - interpretations of those states (Atoms_i)
     - sets of initial / final states
     - transition relations between states.
*)
    
module type MODEL =
sig
  (** The type of a model. *)
  module S : STATE
  (*module I : INTERPRETATION*)
    
  type model
  type state = S.t
  type states = state list
  type atoms = Atom.atoms
  type interpretation 
    
  (** Get the set of states. *)
  val states : model -> states 
    
  (** Get the set of initial state from a model. *)
  val initial_states : model -> state list
    
  (** Get the set of next states for a given state. *)
  val next_states : model -> state -> states
    
  (** The interpretation function for this model.  
      The interpretation of a state is the set of atoms that hold of it.
      
      As an example, consider this is an atomic model: then this will map 
      states to sets of atomic propositions that hold in that state.
      
      As another example, consider a self composed two trace model.  This will
      map sets of two states to sets of atomic propositions that hold over those
      two states.
      
      We hold the type of interpretation abstract for now.
  *)
  val interpret : model -> state -> interpretation
    
  (** Equality of two models: mainly for testing purposes. *)
  val eq : model -> model -> bool
end

(** The type of an "atomic" model, which includes a mechanism for 
    creating a model from a bunch of data. *)
module type AMODEL = (sig
  include MODEL
  (** 
      Create a model from raw data:
      - An initial state
      - A set of states
      - A transition relation
      - A valuation from states to sets of atoms.
  *)
  val mkmodel : int list -> int list ->  (int * int) list -> (int -> Atom.atoms) 
    -> Atom.atoms -> model
    
  (** In an atomic model, we have states represented by integers.  We
      also include two functions to convert back and forth between these two
      representations.
  *)
  val state_of_int : int -> state
  val int_of_state : state -> int
  (** Get the range of possible valuations, e.g., {p,q}. *)
  val valuations   : model -> Atom.atoms
end) with type interpretation = Atom.atoms
  
(** The concrete implementation of an atomic model, using states 
    represented by integers.
*)
module AtomicModel : AMODEL

(** 
   This model represents an atomic model which has been composed with
   itself n times.  Currently the way I wrote it handles only the two
   path case, generalizing it to n paths is something to be done soon.
   
   We lift L_1 to operate on arbitrary sets of formulae in this case.
   
   Because ML isn't dependently typed, we enforce size by assertions.
   
*)
module type COMPMODEL = 
  functor (A : AMODEL) -> sig
    include MODEL

    (** Produce a self-composed model out of an atomic model.
	- The atomic model
	- The interpretation function that says out to create multi-state
	  labelings out of sets of states.
	- The order (i.e., dimension) of the model.
	- The set of multi-state propositions.
    *)
    val mkmodel      : A.model -> (int list -> Atom.atoms) -> int -> Atom.atoms -> model

    (**
	Get the dimension of the model.  Every 
    *)
    val getdimension : model -> int
      
    (** 
	Get the range of L1: the L1 alphabet for this model.
    *)
    val valuations_1 : model -> Atom.atoms

    (** 
	Get the range of Ln: the Ln alphabet for this model.
    *)
    val valuations_n : model -> Atom.atoms
  end

(** Concrete self composed model, given as a functor parameterized 
    by a concrete model. *)
module SelfCompositionModel : COMPMODEL

module SelfComposedAtomic : sig
  include MODEL with type interpretation = Atom.atoms list
  val mkmodel : AtomicModel.model -> (int list -> Atom.atoms) -> int -> Atom.atoms -> model
  val valuations_1 : model -> Atom.atoms
  val valuations_n : model -> Atom.atoms
end

(* module SelfComposedAtomic*)

(** 
    Examples of models. 
*)
module Examples : sig
  val atomic_ex : AtomicModel.model
  val atomic_ex_2 : AtomicModel.model
  val cross_ex : SelfComposedAtomic.model
end

(** {2 Parsing models} *)

(** 
    Simplified model format.
*)

type model_specification = 
  | Start of int list 
  | To of int * int list
  | Alphabet of string list
  | L1 of int * string list
  | Ln of int list * string list

(** Unit tests for this module. *)
module Test : sig
    
end
