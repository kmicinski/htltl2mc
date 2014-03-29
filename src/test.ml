(** Testing infrastructure for the model checker. *)
open OUnit

module M = Model
module P = Parse_model

(* Input test files *)
let input_model_1 = "../test_models/1.mod"
let input_model_2 = "../test_models/2.mod"

(* Test model functionality *)

(* A model should be equal to itself *)
let model_equals_itself_1 _ = 
  assert_equal 
    (M.AtomicModel.eq M.Examples.atomic_ex M.Examples.atomic_ex)
    true

(* A model should not equal something to which it's not equal *)
let model_not_equals_1 _ =
  assert_equal 
    (M.AtomicModel.eq M.Examples.atomic_ex_2 M.Examples.atomic_ex)
    false

(* Parsing models. *)
(* let parsing_well_formed _ = *)

let test_model_1 _ =
  let model =
    P.parse_model input_model_1
  in
  assert_equal (M.SelfComposedAtomic.eq model M.Examples.cross_ex) true

let model_parsing_suite =
  ["model equals itself" >:: model_equals_itself_1;
   "model does not equal something different" >:: model_not_equals_1;
   "parsing sample model 1" >:: test_model_1]

(* Run all the tests *)
let _ = 
  run_test_tt_main
    ("Model checker test suite" >::: model_parsing_suite)
