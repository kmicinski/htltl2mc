(**
   Definition of atomic propositions.
   
   Throughout the model checker, atomic propositions are regarded as
   sorted lists of strings.  This allows us to identify states with
   the same lists.
*)

(** The type of atomic propositions. *)
type t

(** The type of a set of atomic propositions. *)
type atoms

(** Take a single string to an atom. *)
val to_atom : string -> t

(** Convert from a list of strings to a set of atoms. *)    
val from_list : string list -> atoms

(** Convert from a set of atoms to a list. *)
val to_list : atoms -> string list

(** Is this set of atoms equal? *)
val equal : atoms -> atoms -> bool

(** Is the specified atom in the set (true, vs. false) *)
val is_true : t -> atoms -> bool

(** Convert a set of atoms to a string. *)
val string_of_atoms : atoms -> string

(** Convert a set of atoms to a minimal string, suitable for conversion to GFF format. *)
val string_of_atoms_minimal : atoms -> string

(** Wrap a function so that it goes to atoms rather than string list. *)
val eta_expand_alist : ('a -> string list) -> ('a -> atoms)
