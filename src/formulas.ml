(* 
   Machinery for manipulating formulae
*)

module L = List
module P = Printf
module S = String

open Utils

exception InvalidFormula

let pp = fun _ _ -> () (*Printf.printf*)
    
type id = string
type formula = 
  | AE of int * int * formula  (* foralls, existentials, formula *)
  | Not of formula 
  | Or of formula * formula
  | And of formula * formula
  | Multi of formula list (* formulas for each path *)
  | X of formula
  | U of formula * formula
  | F of formula
  | G of formula
  | True
  | False
  | Atom of id

(* Print out a formula: pretty hacky, prints extraneous things.. *)
let rec to_string = function
  | AE (i,j,f) -> (L.fold_left (^) "" (genlst "A " i))
                 ^ (L.fold_left (^) "" (genlst "E " j))
                 ^ (to_string f)
  | Not f      -> "~ (" ^ (to_string f) ^ ")"
  | Or (f,g)   -> (to_string f) ^ " v " ^ (to_string g)
  | And (f,g)  -> (to_string f) ^ " ^ " ^ (to_string g)
  | Multi fs   -> "(" ^ (L.fold_left (^) "" (L.map (fun f -> (to_string f) ^ ",") fs)) ^ ")"
  | X f        -> "X " ^ to_string f
  | U (f,g)    -> (to_string f) ^ " U " ^ (to_string g)
  | F f        -> "F " ^ to_string f
  | G f        -> "G " ^ to_string f
  | True       -> "T"
  | False      -> "F" 
  | Atom id    -> id

type formulas = formula list

let string_of_formulas fs = 
  "{" ^ (List.fold_left (fun acc f -> acc ^ ("\t(" ^ (to_string f) ^ "),\n")) "" fs) ^ "}"

let print_formulas fs = 
  pp "%s\n" (string_of_formulas fs)

(*
  Generate a long sequence of formulas with the different 
  possible set of values: 
  
  0 0 0 0 
  0 0 0 1
  0 0 1 0 
  ...
  
  This is used as a utility in gen_negated_pred and the closure
  construction.
*)

let rec decompose_fm psi_set = 
  let rec h = function
    | hd::[] -> [[hd];[Not hd]]
    | hd::tl ->
      List.map (fun tl -> (Not hd) :: tl) (h tl)
      @ List.map (fun tl -> hd :: tl) (h tl) 
    | _ -> raise InvalidFormula 
  in
  L.filter (fun x -> (x <> psi_set)) (h psi_set)

(* Turn 
   ~ (a,b)
   into 
   (~a,b)
   (a,~b)
   (~a,~b)
*)   
let rec gen_negated_pred = function
    | Not (Multi fs) -> 
      let formula_sets = (List.filter (fun x -> (x <> fs)) (decompose_fm fs)) in
      L.fold_left (fun acc x -> Or ((Multi x), acc)) False formula_sets
    | _ -> raise InvalidFormula
      
(* 
   Simplificiation to NNF.
   
   Simplification works simply enough: iterating to a fixed point on
   the structure of formula using specific rewrite rules for each
   case.
   
   One of the tricky things is in handling idempotency of /\ and \/.
*)
let rec simplify f = 
  (* stupid hack to handle or elimination case.. *)
  let rec simplify_or exprs f = match f with
    | Or (g, h) -> 
      let r = (fun x y -> if (List.mem x exprs) then True else (simplify_or (x::exprs) y)) in
      Or (r g h, r h h)
    | _ -> f
  in
  let rec simplify' psi = 
    match psi with
    | AE (i,j,f)     -> AE (i, j, (simplify f))
    | Not (Not f)    -> f
    | Not True       -> False
    | Not False      -> True
    | Not Or (f,g)   -> (And (Not f, Not g))
    | Not And (f,g)  -> (Or (Not f, Not g))
    | Not Multi fs   -> (gen_negated_pred (Not (Multi fs)))
    | Not f          -> Not (simplify f)
    | And (f,g)      -> And(simplify f, simplify g)
    | Or (False,f')  -> f'
    | Or (f',False)  -> f'
    | Or (f,g)       -> (Or (simplify (simplify_or [] f), (simplify (simplify_or [] g))))
    | Multi fs       -> let fs' = L.map simplify fs in
		       if (List.exists ((=) False) fs') then False else Multi fs'
    | X f            -> X (simplify f)
    | U (f1,f2)      -> U (simplify f1, simplify f2)
    | F f            -> (U (True,f))
    | G f            -> Not (U (True,Not f))
    | True           -> True
    | False          -> False
    | Atom a         -> Atom a
  in
  let rec iterate f = 
    if (simplify' f) = f then
      f
    else
      iterate (simplify' f)
  in
  iterate f

let negate f = simplify (Not f)

(* 
   Gen \overline{\langle psi_1, ..., psi_n \rangle}
*)

let rec gen_neg_multi = function
  | Multi l -> L.map (fun x -> simplify (Multi x)) (decompose_fm l)
  | _ -> raise InvalidFormula
    
(* 
   Utility functions.
   
   TODO:
   I haven't had the time to actually go and make a proper set 
   class, or use the OCaml one, because I've just been using lists.
   I think that would make for a cleaner interface.
*)

(* Union of l and l' *)
let union l l' = 
  L.fold_left (fun acc x -> if (List.mem x acc) then acc else (x::acc)) l' l

(* Union of a set of lists. *)
let unions l = List.fold_left (fun acc x -> union x acc) [] l

(* Does some set in ls contain f? *)
let exists_in_some_set ls f = 
  List.exists (fun x -> (List.mem f x)) ls

(* Calculate the fixed point of some function which generates 
   new formulas.
*)
let rec fixed_point f = 
  (* Does l contain l' *)
  let contains l l' = 
    List.for_all (fun x -> (List.mem x l')) l in
  (* Calculate the set of generated things. *)
  let nl s = L.concat (L.map f s) in
  (* TODO *)
  (function s -> if contains (nl s) s then 
      ((ignore (pp "Done.\n")); s)
    else (fixed_point f (
      let u = union (nl s) s in
      List.map (fun x -> pp "%s\n" (to_string x)) u;
      u)))

(* Invert the nth term in the set of formulas l *)
let rec invert l n = match (l,n) with
  | ([],_)   -> []
  | (h::t,0) -> (Not h) :: t
  | (h::t,n) -> h :: (invert t (n-1))

(* The function which generates new terms from old terms. *)
let generate f =
  let t = match f with 
    | Multi ls -> 
      (Multi ls) :: (L.map (fun x -> simplify (Multi x)) (decompose_fm ls))
    | And (f,g) -> [f; g]
    | Or (f,g)  -> [f; g]
    | X f       -> [f]
    | U (f,g)   -> [f;g]
    | _         -> []
  in 
  ignore (List.map (fun x -> pp "generated: %s\n" (to_string x)) t);
  t

(* 
   Calculate the closure of some formula f in NNF. 
   
   We also compute this as a fixed point.  The type of this function is
   complex, and represents the following:
   
   (what we have * what we're working on * new things we generate
   that we need to work on)
   
*)
let closure f = 
  (* If this clause isn't Multi, generate it's negation and work on
     that. *)
  let gen_neg f =
      match f with
	| Multi fs -> []
	| _        -> [Not f]
  in
  let g f = 
    List.map simplify ((gen_neg f) @ (generate f))
  in
  (fixed_point g) [f]

(* The generator for a single formula.
   
   formula ->  (formula set, formula set set)
   ^^^^^^                      ^^^^^^^^^^^
   discovered               disjoint subsets
   
   This formula will give:
   - A set of things which *must* be in a subset of ms({f})
   - Sets of things which *cannot* be in a subset of ms({f})
*)
let generator f fs = 
    (* If a formula is not of the form (..) then generate its
       complement. *)
  let gen_neg = match f with
    | Multi fs -> []
    | _        -> [[simplify (Not f)]]
  in
  let saturate_set = match f with 
    | And (f1,f2)   -> ([f1;f2],[])
    | Or  (f1,f2)   -> ([f1],[f2::fs])
    | U   (f1,f2)   -> ([f1],[f2::fs])
    | Multi fs      -> ([f],(L.map (fun x -> (Multi x)::fs) (decompose_fm fs)))
    | Not U (f1,f2) -> ([simplify (Not f2)],[])
    | _             -> ([],[])
  in
  let (a,b) = saturate_set 
  in
  (fs@a,b@ gen_neg)

(* 
   Generate the maximally consistent subsets of closure(psi)
   
   Note that this is wildly inefficient.
   
   We generate powersets of the closure and then check them for the
   ms(.) properties.  We also filter sets we generate containing
   False, and remove True from formula sets.
*)
let ms psi =
  (* Is this set a candidate for being an ms(.) set? 
     Does it violate any conditions for being maximally satisfiable.
  *)
  let check_ms (set :formula list) = 
    List.for_all (fun psi -> 
      (match psi with 
	| Multi psi' -> 
	  disjoint (gen_neg_multi psi) set
	| _        -> (not (List.mem (negate psi) set)))
      && 
	(match psi with
	  | And   (psi1,psi2) -> (List.mem psi1 set) && (List.mem psi2 set)
	  | Or    (psi1,psi2) -> (List.mem psi1 set) || (List.mem psi2 set)
	  | U     (psi1,psi2) -> (List.mem psi1 set) || (List.mem psi2 set)
	  | Not U (psi1,psi2) -> (List.mem (negate psi2) set)
          | _             -> true))
      set
  in
  let ssuperset l l' = subset l' l && l <> l' in
  let filter_subsets subsets =
    List.filter (fun subset ->
      (not (List.exists (fun subset' -> ssuperset subset' subset) subsets)))
      subsets
  in
  let rec calculate_fp x = 
    if (equal_sets (filter_subsets x) x)
    then
      x
    else
      calculate_fp (filter_subsets x)
  in
  L.map (L.filter (fun x -> x <> True))
    (calculate_fp (L.filter (fun set -> not (List.mem False set))
		     (L.filter check_ms (pset (closure psi)))))

(* 
	
(* psi*)
  (* Takes f and generates two sets:
       {f', ~f'}

     - f' is the set of things which must be in the same set 
     of ms(psi) as f,

     - ~f' is the set of things which *cannot* be in the 
     same set of ms(psi) as f,
  *)
  (*let generate_new_sets f =
    let (@@) (a1,a2) (b1,b2) = (a1@b1, a2@b2) in
    (match f with
      | Multi fs -> (simplify (Not f),[])
      | _ -> ([],[]))
    @@ 
  *)
  (* let rec accumulate_ms    *)
    (sets : formula list list) =
  let set_and_disjoints fs = 
    let x = L.map (fun x -> generator x fs) fs in
    (unions (lfst x), unions (lsnd x))
  in
  let new_sets_new_disjoints = 
    (List.map (fun x -> set_and_disjoints x) sets)
  in
  (* we have a set for each formula, and then a set of sets for each 
     set of formulas. 
  *)
  let gen_lsts = 
    lfst new_sets_new_disjoints @ (unions (lsnd new_sets_new_disjoints))
  in
  let sets' = (uniqueify equal_sets (L.map (fun subst -> 
    (uniqueify (=) (List.map simplify subst))) gen_lsts))
  in 
  (* Remove sets of formulas that contain False. *)
  let sets'' = List.filter (fun x -> not (List.mem False x)) sets' in
  (* Remove True within sets of formulas. *)
  let sets''' = List.map (fun x -> (List.filter (function True -> false | _ -> true) x)) sets'' in
  (* Filter the maximally consistent sets so that we merge subsets
     together. *)
  L.filter (fun x -> not (List.exists (fun y -> ssuperset y x) sets')) sets'''
    
*)

let well_formed f = true

(* Examples of formulae in HyperLTL *)

module Examples = 
struct 
  (* Statement of two trace interference property. *)
  let t1 = AE (1, 1, (G (And ((Multi [True; Not (Atom "p")]), (Atom "LE")))))
  let e  = Not (U (True,
		   (Or 
		      ((Multi [True; Atom "p"]),
		       Not (Atom "LE")))))
  let es = 
    List.map (fun x -> List.map (fun y -> simplify y) x) (
    [[e; And ((Multi [True; Not (Atom "p")]), Atom "LE"); 
      Multi[True; Not (Atom "p")]; Atom "LE"];
     [Not e; Or (Multi[True; (Atom "p")], Not (Atom "LE"));
      (Multi[True; (Atom "p")]); Not (Atom "LE")];
     [Not e; Or (Multi[True; (Atom "p")], Not (Atom "LE")); 
      (Multi[True; (Atom "p")]); (Atom "LE")];
     [Not e; Or (Multi[True; (Atom "p")], Not (Atom "LE")); 
     (Multi[True; Not (Atom "p")]); Not (Atom "LE")];
     [Not e; And (Multi[True; Not (Atom "p")], Atom "LE"); 
      Multi[True; Not (Atom "p")]; Atom "LE"]])
end
