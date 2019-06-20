(*
  Module for transforming states to labeled transition systems.
  
  We use the definition of kripke structures given in the {@Model} 
  module to give the translation.
  
  Currently, the translation works by specifying an input program, 
  along with label assignments to the different levels of variables 
  in the heap.
  
  Note this code is pretty preliminary.
*)

module L = List
module U = Utils

open Utils
open Program

(* Lookup a variable from a store. *)
let rec lookup_store id = function
  | [] -> raise (NoId id)
  | x::tl -> let (i,_,_,v) = x in if i = id then v else lookup_store id tl

let rec lookup_width id = function
  | [] -> raise (NoId id)
  | x::tl -> let (i,_,w,_) = x in if i = id then w else lookup_store id tl

(* Write to the store. *)
let rec write_store id value : store -> store = function
  | [] -> raise (NoId id)
  | x::tl -> let (i,l,w,v) = x in 
	    if i = id then (i,l,w,value)::tl else x::(write_store id value tl)

let truncate i w = 
  i mod int_of_float (2.0 ** (float_of_int w))

(* Given the store, width of the output, and expression, perform the operation. *)
let rec eval_expr (store : store) w e = 
  let arith_op e1 e2 op : int =
    let e1' = (eval_expr store w e1) in
    let e2' = (eval_expr store w e2) in
    truncate (op e1' e2') w
  in
  let logical_op e1 e2 op : int =
    let e1' = (eval_expr store w e1) in
    let e2' = (eval_expr store w e2) in
    (op e1' e2')
  in
  match e with 
    | Const i    -> i
    | Id i       -> lookup_store i store
    | Plus (e1,e2) ->
      arith_op e1 e2 (+)
    | Sub (e1,e2)  -> 
      arith_op e1 e2 (-)
    | Mul (e1,e2)  -> 
      arith_op e1 e2 (fun x y -> x*y)
    | Div (e1,e2)  -> 
      arith_op e1 e2 (/)
    | Lt (e1,e2)   -> 
      logical_op e1 e2 (fun x y -> if x<y then 1 else 0)
    | Gt (e1,e2)   -> 
      logical_op e1 e2 (fun x y -> if x>y then 1 else 0)
    | Eq (e1,e2)   -> 
      logical_op e1 e2 (fun x y -> if x=y then 1 else 0)
    | And (e1,e2)  -> 
      logical_op e1 e2 (fun x y -> match x,y with (_,0) -> 0 | (0,_) -> 0 | _ -> 1)
    | Not e -> 
      if (eval_expr store w e) == 0 then
	1
      else
	0

module Examples = struct 
(*  let ex1 = 
    Seq
      ((Asn ("x",Const 3)),
       While (
	 (Gt ((Id "x"), (Const 0))),
	 (Asn ("x", (Sub ((Id "x"),Const 1))))))

  let ex2 =
    Seq
      ((Asn ("x",Const 3)),
       While (
	 (Gt ((Id "x"), (Const 0))),
	 (Asn ("x", (Sub ((Id "x"),Const 1))))))
      
  let store1 = 
    ["x",H,3,0]

  let cfg1 = 
    ["x",H,3]

  let p1 = cfg1,ex1

  (* First example from paper. *)
  (*let config1 = [("l",L,2);("h",H,2)]*)
    
  (* *)
  let cmd1 = 
    Asn ("l", Id "h")
      
  let config1 = [("l",L,1);("h",H,1)]
    
  let cmd2 = 
    If ((Eq ((Id "h"),Const 0)),(Asn (("l"),Const 1)),(Asn (("l"),Const 0)))
      
  let cmd3 = 
    Choice
      [Asn ("l", Id "h"); Asn ("l", Const 0); Asn ("l", Const 1)]
      
  let program1 = config1,cmd1
  let program2 = config1,cmd2
  let program3 = config1,cmd3
  let program4 = config1,cmd1
*)    
end

let string_of_store s = 
  let rec h = function
    | [] -> ""
    | (id,_,_,v)::t -> id ^ " = " ^ (string_of_int v) ^ ", " ^ (h t)
  in
  "< " ^ h s ^ " >"
    
let rec e_to_string = function 
  | Const i -> string_of_int i
  | Id x    -> x
  | Plus (e1,e2) -> "(" ^ (e_to_string e1) ^ ") + (" ^ (e_to_string e2) ^ ")"
  | Sub (e1,e2) -> "(" ^ (e_to_string e1) ^ ") - (" ^ (e_to_string e2) ^ ")"
  | Mul (e1,e2) -> "(" ^ (e_to_string e1) ^ ") * (" ^ (e_to_string e2) ^ ")"
  | Div (e1,e2) -> "(" ^ (e_to_string e1) ^ ") / (" ^ (e_to_string e2) ^ ")"
  | Lt (e1,e2) -> "(" ^ (e_to_string e1) ^ ") < (" ^ (e_to_string e2) ^ ")"
  | Gt (e1,e2) -> "(" ^ (e_to_string e1) ^ ") > (" ^ (e_to_string e2) ^ ")"
  | Eq (e1,e2) -> "(" ^ (e_to_string e1) ^ ") == (" ^ (e_to_string e2) ^ ")"
  | And (e1,e2) -> "(" ^ (e_to_string e1) ^ ") && (" ^ (e_to_string e2) ^ ")"
  | Not e -> "!" ^ (e_to_string e) ^ ")"

let rec to_string = function 
  | Skip -> ";\n"
  | Asn (x,e) -> x ^ " := " ^ (e_to_string e) ^ ";\n"
  | Seq (c1,c2) -> (to_string c1) ^ (to_string c2)
  | If (e,c1,c2) -> "if (" ^ (e_to_string e) ^ ") {\n" ^ (to_string c1) ^
    "} else {\n" ^ (to_string c2) ^ "}\n"
  | While (e,c) -> "while (" ^ (e_to_string e) ^ ") {\n" ^ (to_string c) ^ "}\n"
  | Choice es ->
    let rec h cmds acc = match cmds with
      | [] -> acc ^ "}"
      | hd::tl -> h tl (acc ^ (to_string hd) ^ "|| ")
    in
    h es "{ "

let pp x = Printf.printf "%s" x

(* Step the store using a small step semantics: the initial
   configuration is iterated to a fixed point.
   
   XXX: This assumes the store has homogenous width.
*)
let rec step store cmd = 
  match cmd with
    | Asn (id,e)    -> 
      [((write_store id (eval_expr store (lookup_width id store) e) store),Skip)]
    | Seq (Skip,c2) -> step store c2
    | Seq (c1,c2)   -> 
      L.map (function (store',c1') -> (store',Seq (c1',c2))) (step store c1)
    | If (e,c1,c2)  -> 
      if (eval_expr store ((fun (_,_,w,_) -> w) (L.hd store)) e) <> 0 then
	[store,c1]
      else
	[store,c2]
    | While (e,c)   ->
      [(store, If (e,Seq (c, cmd),Skip))]
    | Choice cs     -> 
      L.flatten (L.map (fun c -> step store c) cs)
    | Skip -> []
      
(* type graph = 
   (cmd * (store list) * cmd * (store list)) list
*)

let empty_graph = []
  
let insert_execution_edge edge graph = 
  []
  (* if L.exists graph edge then graph else edge::graph*)

(* Generate a CBV fixed point over a graph. *)
let fix_graph f =
  let rec h g = 
    if equal_sets (f g) g then g else h (f g)
  in
  h

(* Nodes, with and without PCs.  This should be a module or typeclass
   in an ideal world, but I'm too lazy to do it. *)
type node = 
  | Init
  | Node of cmd * store
  | Done of store

let pp = Printf.printf "%s"

let string_of_node = function
  | Init -> "Init\n"
  | Node (c,s) -> (to_string c) ^ ", " ^ (string_of_store s) ^ "\n"
  | Done s -> "Done: " ^ (string_of_store s)

type pclessnode = 
  | PInit
  | PNode of store
  | PDone of store

let string_of_pclessnode = function
  | PInit -> "Init\n"
  | PNode s -> (string_of_store s) ^ "\n"
  | PDone s -> "Done: " ^ (string_of_store s)

(* Execution graphs are edge lists. *)

type execution_transition_graph =
    (node * node) list

let nodes_from (g : ('a * 'b ) list) (n : 'a) : 'b list = 
  L.map snd (L.filter (fun x -> (fst x) = n) g)

let print_graph g =
  ignore (L.map (function (a,b) -> pp (string_of_node a); pp (string_of_node b)) g)

(* Generate the initial graph, which is the graph that steps from the
   initial node Init, to the set of all possible starting
   configurations, as specified by the configuration (cfg).  We will
   iterate on this initial graph to build the execution graph for the
   program.
*)
let initial_graph cfg cmd = 
  (*pp "Building program execution graph...\n";*)
  let rec build_initial_state_space cfg acc = match cfg with
    | [] -> acc
    | (id, l, w, Specific i)::tl ->
      build_initial_state_space tl
	(L.map (fun x -> (id,l,w,truncate i w)::x) acc)
    | (id, l, w, Any)::tl ->
      build_initial_state_space tl
	(L.flatten
	   (L.map
	      (fun x ->
		(L.map
		   (fun n -> (id,l,w,n)::x)
		   (genseq ((int_of_float (2.0 ** float_of_int w))))))
	      acc))
  in
  (* Build the possible set of initial states. *)
  let initial_nodes = 
    L.map (fun store -> Node (cmd,store))
      (build_initial_state_space cfg [[]])
  in
  let initial_graph = 
    L.map (fun init_node -> (Init, init_node)) initial_nodes in
  initial_graph

let generate_transition_graph cfg cmd =
  uniqueify (=)
    ((fix_graph (fun graph ->
      uniqueify (=)
	(L.flatten
	   (L.map
	      (fun edge -> match edge with
		| (_,(Node (Skip,s))) ->
		  [Node (Skip,s), Done s]
		| (_,(Node (c,s))) ->
		    (L.map (fun (s',c') -> 
		      (Node (c,s)), (Node (c',s'))) (step s c))
		| _ -> [edge])
	      graph)) @ graph))
	(initial_graph cfg cmd))

(* Add the transitions from the empty store to a potential new store. *)

exception NoSuchState

let state_to_num state_map state = 
  let rec h l n =
    match l with 
      | []     -> raise NoSuchState
      | hd::tl -> if hd = state then n else h tl (n+1)
  in
  h state_map 0

let state_map_to_int_map pmap mapping = 
  List.map (function (x,y) -> (state_to_num mapping x,state_to_num mapping y)) pmap

(* If you give me a state, I'll hand you back the set of single trace
   propositions it satisfies. *)
let state_to_props (state: pclessnode) : string list = 
  (* Get the set of propositions that are true given a store. *)
  let h s =
      L.flatten
      (L.map 
	 (fun ((id,_,w,v)) -> 
	   L.flatten
	     (L.map
		(fun bit -> 
		  if (((v lsr bit) land 0x01) = 1) then [id ^ string_of_int bit]
		  else [])
		(genseq w)))
	 s)
  in
  match state with 
    | PInit -> []
    | PNode s -> h s
    | PDone s -> "done"::(h s)

(* If you give me two states, I'll tell you whether or not they're LE. *)
let low_equivalent ((_,store1) : cmd * store) ((_,store2) : cmd * store) =
  List.for_all (fun x -> x) 
    ((L.map 
	(function (id,L,w,v) -> 
	  lookup_store id store2 = v)
	(L.filter (function (_,L,_,_) -> true | _ -> false) store1)))

exception UnexpectedArity
exception LookupError

let build_model_from_program (cfg,cmd) output_file =
  let project = function
    | Init -> PInit
    | Node (cmd,s) -> PNode s
    | Done s -> PDone s
  in
  
  let execution_graph = 
    L.map (fun (x,y) -> (project x, project y))
      (generate_transition_graph cfg cmd)
  in
  
  let states : pclessnode list =
    (uniqueify (=)
       (L.flatten (L.map (fun (x,y) -> [x;y]) execution_graph)))
  in
  
  (* Convert each state in the graph to an integer *)
  let node_to_num node =
    index_of node
      (uniqueify (=)
	 (L.flatten (L.map (fun (x,y) -> [x;y]) execution_graph)))
  in
  let num_to_node = 
    L.nth
      (uniqueify (=)
	 (L.flatten (L.map (fun (x,y) -> [x;y]) execution_graph)))
  in
  let file = open_out output_file in
  let pf = pp in (* s = Printf.fprintf file "%s" s in*)

  (* Print set of propositions *)
  pf "props {";
  let _ = (L.map (fun (id,_,w,_) ->
    (ignore 
       (L.map (fun n -> pf (id ^ string_of_int n ^ ",")) (genseq w))))
	     cfg)
  in
  pf "done}\n\n\n";
  
  let print_string_list l = 
    let rec h l = match l with
      | [] -> ()
      | h1::h2::t -> 
	let _ = pf (h1 ^ ",") in h (h2::t)
      | hd::[] -> 
	let _ = pf hd in ()
    in
    h (uniqueify (=) l)
  in
  
  (* Print initial states *)
  let (start_states : pclessnode list) = nodes_from execution_graph PInit in
  pf "start {";
  print_string_list
    (L.map (fun n -> string_of_int (node_to_num n)) start_states);
  pf "}\n\n";
  
  (* Print graph *)
  let _ = L.map
    (function state -> match state with
	PInit -> () 
      | _ ->
	let this_node = node_to_num state in
	pf ((string_of_int (this_node)) ^ " -> {");
	let next_states = match state with 
	  | PDone _ -> state :: (nodes_from execution_graph state)
	  | PNode _ -> nodes_from execution_graph state 
	in
	print_string_list
	  (L.map 
	     (fun x -> string_of_int (node_to_num x))
	     next_states);
	ignore (pf "}\n"))
    states
  in
  pf "\n";

  (* Print valuation *)
  let _ = L.map 
    (fun state ->
      match state with
	| PInit -> ()
	| _ ->
	  pf ("L(" ^ (string_of_int (node_to_num state)) ^ ") {");
	  print_string_list (state_to_props state);
	  pf "}\n")
    states
  in

  (* Print some output information *)
  ignore (L.map (fun (x,y) -> ()
    (*pp (string_of_int (node_to_num x)
	^ " -> " 
	^ string_of_int (node_to_num y) ^ "\n") *)) execution_graph);
  let i = ref 0 in
  L.iter (fun x -> () (*pp ("State " ^ (string_of_int !i) ^ ":\n" ^ 
		       string_of_pclessnode x ^ "\n") *);
    i := (!i + 1)) states;
  

(* 
props {x0}

start {2}

2 -> {0}
2 -> {1}

L(2) {}
L(0) {}
L(1) {x0}
*)
