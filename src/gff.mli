(**
   Interface to the GOAL solver.
   
   After generating formulae, we use the GOAL solver to check emptiness 
   between two automata.  This interface allows us to shell out to it.
*)

(**
   Write string [contents] to file [filename].
*)
val write_to_file : filename:string -> contents:string -> unit

(**
   Check the containment of $L(a_1)$ in $L(a_2)$.
   
   Accepts (as its last argument) the executable to use for the GOAL solver.
*)
val check_containment : string -> string -> string -> unit

(**
   Check the emptiness of $L(a_1)$.
   
   Accepts (as its last argument) the executable to use for the GOAL solver.
*)
val check_empty : string -> string -> unit
