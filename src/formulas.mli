(**
   Syntax definitions for HyperLTL without the purge operator, along 
   with associated operations on formulae.
*)

(** An exception representing that a formula is invalid if its
    constituent pieces aren't well formed. *)
exception InvalidFormula

(** Identifiers in HyperLTL are simply strings. *)
type id = string

(** {2 Formulas} *)

(** Formulas in HyperLTL.  Note that here we do not restrict syntactic
    domains, or include the purge operator. *)
type formula = 
  | AE of int * int * formula  (* foralls, existentials, formula *)
  | Not of formula 
  | Or of formula * formula
  | And of formula * formula
  | Multi of formula list (* formulas for each path *) (* now called focus *)
  | X of formula
  | U of formula * formula
  | F of formula
  | G of formula
  | True
  | False
  | Atom of id

(** A set of [formula]s.  Many operations manipulate formulas, such
    as [closure] and [ms].*)

type formulas = formula list

(** Simplify this formula (to NNF). *)
val simplify : formula -> formula

(** Negate and simplify the formula. *)
val negate : formula -> formula

(** Decide if a formula is well formed. *)
val well_formed : formula -> bool

(** Generate a string representation of this formula. *)
val to_string : formula -> string

(** Convert a set of formulas to a string. *)
val string_of_formulas : formulas -> string

(** Print out a set of formulas. *)
val print_formulas : formulas -> unit

(** Generate the closure of a formula. *)
val closure : formula -> formulas

val generator : formula -> formula list -> formula list * formula list list

val decompose_fm : formula list -> formula list list

val gen_neg_multi: formula -> formula list

(** Generate the maximally consistent subsets from a formula. *)
val ms :  formula -> formula list list (*formulas list -> (formulas * formulas) list (* formula list -> formula list *) *)

val invert : formula list -> int -> formulas

(** Generate the negated form of focus formulas *)
val gen_negated_pred : formula -> formula

val generate : formula -> formulas

(** {2 Transformation to automata.} *)

(** If you give me a representation of automaton, I will give you a formula which will
    generate one. *)

module Examples : sig
  val t1 : formula
  val e  : formula
  val es : formula list list
end

(*val to_automaton : formula -> Automata.S.t*)

(** {2 Sets of formulae}
    
    Sets of formulae are handled in a special way.  There are several 
    places in the model checker where we need to deal with sets of 
    formulas which represent (e.g.,) sets of states, etc...
    
    To handle these we need efficient operations for checking
    membership and inserting elements.  This strcuture does so handing
    an enumerator to return successive elements of the set of
    formulas.
*)

(*
module FormulaSet : sig
  (** The type of a set of formulas *)
  type t
    
  (** Elements of a formula set. *)
  type e 
    
  (** Lookup a formula from a set of formulas. *)
  val get_elt : t -> e -> formula
    
  (** Is [f] a member of the formula set. *)
  val mem : t -> formula -> bool
    
  (** Add [f] to the set of formulas. *)
  val add : t -> formula -> t

  (** The type of an enumerator, to grab more elements from the set of formulas. *)
  type enumerator
      
  (** Enumerate the elements of the set. *)
  val enumerate : t -> enumerator -> (e * enumerator option)
    
  (** Grab the set as a list. *)
  val to_list : formula list
end
*)
