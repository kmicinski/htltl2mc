(**
  Module for transforming states to labeled transition systems.
  
  We use the definition of kripke structures given in the {@Model} 
  module to give the translation.
  
  Currently, the translation works by specifying an input program, 
  along with label assignments to the different levels of variables 
  in the heap.
  
  Note this code is pretty preliminary.
*)

open Program

module Examples : sig
(*  val ex1 : cmd
  val ex2 : cmd

  val p1 : program
    
  val store1 : store
    
  val cfg1 : configuration
    
  (** Examples from the paper. *)
  val program1 : program
  val program2 : program
  val program3 : program
  val program4 : program
*)
end

(** Render an expression as a string. *)
val e_to_string : exp -> string

(** Render a command as a string. *)
val to_string : cmd -> string

(** A program execution graph. *)
type execution_transition_graph

(** Enumerate state space and build a program. *)
(*val translate_program : program -> execution_transition_graph*)

(** Build a model from the program. *)
val build_model_from_program : program -> string -> unit
