(**
   Representation of various Omega automaton
*)

(*module Graph = G*)

(*module G = ConcreteBidirectionalLabeled (V) (E)*)

module type STATE = sig
  type t
  val to_string : t -> string
end

(** Representation of a bare automaton, without accepting
    conditions.
*)
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

(** Generalized Buchi Automaton: acceptance conditions are sets of
    states.
*)
module type GBA = sig
  include S

  type accepting_cond = state list

  (** Get the list of accepting conditions. *)
  val accepting_conditions : t -> accepting_cond list
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

(** {2 Common automata } *)

(** Representation of atomic automaton.
   
    This is the analogue of an atomic model, it is what we 
    get when we take an atomic model to an automaton.

    "Atomic" means that the automaton has only the single-state
    labels, but none of the multi-state labels.
*)
module type AtomAutomaton =
  sig 
    include BUCHI
    (** Go from an atomic model to an automaton for that model. *)
    val mkautomaton : Model.AtomicModel.model -> t
  end

(** Concrete instance of an automaton for atomic models. *)
module AtomAutomaton : AtomAutomaton

(** Automaton for self composed atomic models.
    
    The $n$-trace version of [AtomAutomaton].
*)
module type SelfCompositionAutomaton =
sig 
  include BUCHI

  (** Go from an atomic model to an automaton for that model. 
       - Self composed model 
       - n, where this is an n-fold composition 
       - set of single-state labels 
       - set of multi-state labels *)
  val mkautomaton : Model.SelfComposedAtomic.model -> int -> Atom.atoms -> Atom.atoms -> t
end with type a = Atom.atoms list

(*  sig 
    include S
    (** Go from an atomic model to an automaton for that model. *)
    val mkautomaton : Model.SelfComposedAtomic.model -> int -> Atom.atoms -> Atom.atoms -> t
  end
*)

module SelfCompositionAutomaton : SelfCompositionAutomaton

(** {2 Automata for formulas}
    
    Module for turning formulae into automata for representations 
    of formulae as Buchi automaton.
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
  (** Create an automaton that represents a formula phi.
       - The set of maximally consistent sets for phi.
       - phi *)
  val to_automaton : Formulas.formulas list -> Formulas.formula -> t
end with type a = Atom.atoms list

module FormulaAutomaton : FAUTOMATON

val gen_form_mod : int -> Atom.atoms -> Atom.atoms -> Formulas.formula list list * Formulas.formula

(** {1 Constructions } *)

(** Create the intersection of two automata.
    
    Warning: currently buggy implementation of this automata
    construction.  Needs further testing of the transition relation.
    
    A1 and A2 must have the same alphabet (not just type, but identical
    alphabets).

*)
module IntersectAutomaton : 
  functor (A1 : BUCHI) ->
    functor (A2 : BUCHI with type a = A1.a) -> (sig
      include BUCHI
      val mkintersection : A1.t -> A2.t -> t
    end with type a = A1.a)

(** Create the intersection of two automata, where the first automata
    has the condition that all states are accepting.
    
    Constraints: All constraints of [IntersectAutomaton], plus the
    constraint that every state in [A1] must be accepting, and both
    automata must have exactly one initial state.
    
*)
module IntersectOneAllAccepting : 
  functor (A1 : BUCHI) ->
    functor (A2 : BUCHI with type a = A1.a) -> (sig
      include BUCHI
      val mkintersection : A1.t -> A2.t -> t
    end with type a = A1.a)

(* 
   module GeneralizedBuchiToBuchi :
  functor (A : S with type 
*)

(** Create the projection of one automata *)
module ProjectionAutomaton :
  functor (A : BUCHI with type a = Atom.atoms list) ->
    (sig 
      include BUCHI
      (** Create the projection automaton for {% A |_k %}  *) 
      val mkprojection : A.t -> int -> t
     end with type a = Atom.atoms list)


(** Turn a Generalized Buchi Automaton into a regular Buchi
    automaton. *)
      
module GBAtoBA :
  functor (A : GBA) -> 
    (sig 
      include BUCHI
      (** Take a generalized Buchi automaton to a regular Buchi
	  automata recognizing the same langauge. *)
      val generalized_to_buchi : A.t -> t
     end with type a = A.a)

(** {2 Rendering automata} *)

(** 
    A functor which allows you to take an arbitrary automata and render it 
    as a GFF file, fit for feeding into GOAL and manipulating.
    
    Technically speaking, we should probably have this paramterized on
    another module which renders accepting states: because I only dump
    Buchi automata instead I just hardwire this to work with Buchi
    automata.  If we wanted to dump arbitrary automata we would
    parameterize this on another module which rendered the accepting
    states.
*)
module BuchiToGffXml : 
  functor (P : BUCHI) -> 
    functor (OP : sig
      val state_to_int : P.t -> P.state -> int
    end) ->
sig
  val to_gff_xml : P.t -> string
end


(** {2 Examples} *)

module Play : 
sig 
  include S
  val to_automaton : Formulas.formulas list -> Formulas.formula -> t
end with type a = Atom.atoms list

val play_ex : Play.t

val print_transitions : Play.t -> int -> unit

val print_states : Play.t -> unit

(** {2 Common Automata} *)

val empty_language : AtomAutomaton.t

(** 
    Create a projection automaton. 
    
    Take in an automaton, and create a projected version of it.
*)
(*module type PROJAUTOMATON = 
  functor (P : S with a = Formulas.formulas) ->
    functor (N : sig val n : int end) ->
sig
  include S
    
end
*)
(*
module type IntersectionAutomaton (A1 : S) (A2 : S) = 
sig 
  include S
end
*)

(** A set of tests for this module. *)
module Test : sig

end
