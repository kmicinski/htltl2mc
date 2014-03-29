(**
   Utility functions.
*)

(** Lift a function to apply it across a product domain. *)
val lift2 : ('a -> 'b) -> (('a * 'a) -> ('b * 'b))

(** iterate a computation n times, throwing away the result *)
(*val iterateu : ('a -> ()) -> int -> ()*)

(** Generate a list of n elements, each with value i *)
val genlst : 'a -> int -> 'a list

(** Generate a constant function from a value. *)
val const : 'a -> ('b -> 'a)

(** Turn a pair of strings into its representation. *)
val string_of_pair : string * string -> string

(** Take a cross product from two lists. *)
val cross : 'a list -> 'b list -> ('a * 'b) list

(** Take an n-cross product of a list of things. *)
val crossn : 'a list -> int -> 'a list list

(** Take n lists and create lists of length n corresponding to the pointwise 
    cross product. *)
val cross_n_lists : 'a list list -> 'a list list

(** Take the power set of a set. *)
val pset : 'a list -> 'a list list

(** 0, 1, ..., n-1 *)
val genseq : int -> int list

(** Zip up two lists. *)
val zip : 'a list * 'b list -> ('a * 'b) list

(** Unzip a list. *)
val unzip : ('a * 'b) list -> 'a list * 'b list

(** Is set [l] disjoint to list [l'] *)
val disjoint : 'a list -> 'a list -> bool

(** Invert a function. *)
val fnot : ('a -> bool) -> ('a -> bool)

(** Get every first element in a list of pairs. *)
val lfst : ('a * 'b) list -> 'a list

(** Get every second element in a list of pairs. *)
val lsnd : ('a * 'b) list -> 'b list

(** Is list [a] a subset of list [b], regarded as sets. *)
val subset : 'a list -> 'a list -> bool

(** Is list [a] a strict superset of [b], regarded as sets. *)
val ssuperset : 'a list -> 'a list -> bool

(** Create A \ B. *)
val set_minus : 'a list -> 'a list -> 'a list

(** Take the set intersection. *)
val intersect : 'a list -> 'a list -> 'a list

(** Union of l and l' *)
val union : 'a list -> 'a list -> 'a list

(** Union of a set of lists. *)
val unions : 'a list list -> 'a list

(** Are the sets (regarded as lists) l and l' the same. *)
val equal_sets : 'a list -> 'a list -> bool

(** Get the index of [a] in [ls] *)
val getpos : 'a list -> 'a -> int

(** Make a set unique with respect to the equality measure eq *)
val uniqueify : ('a -> 'a -> bool) -> 'a list -> 'a list

(** Read in the set of lines from a file *)
val read_file : string -> string

(** Look up the position of element [e] in list [l] *)
val index_of : 'a -> 'a list -> int

