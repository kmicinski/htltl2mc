(**
   Run the model checker, building automata from formulae and models.
*)

(** 
    Check a program.
*)
val check_program_and_model :
  Model.SelfComposedAtomic.model -> 
  Formulas.formula -> 
  string ->
  unit

(** Run the whole model checking procedure for [module_filename] on the
	formula [formula_str] ({% \Psi %}).

	This constructs a formula using the appropriate constructions, then 
	uses the containment checking algorithm in GOAL to discharge the obligations.
 *)

val check_formula_and_model : 
  model_filename:string -> 
  formula_str:string -> 
  model_name:string -> 
  goal_executable:string -> 
  unit
