(**
   Small utilities used throughout the system.
*)

module L = List

let lift2 = fun f -> function (a,b) -> (f a, f b)

let rec iterateu f = function
  | 0 -> ()
  | n -> f; iterateu f (n-1)

let const = fun c -> (fun _ -> c)

(*let accuml f n = 
  let rec dontimes_l' a = function
    | 0 -> a
    | n -> dontimes_l' (f () :: a) (n-1)
  in
  dontimes_l' [] n*)

let genlst i n = 
  let rec genlst' a = function
    | 0 -> a
    | n -> genlst' (i::a) (n-1)
  in
  genlst' [] n

let string_of_pair (x,y) = "(" ^ x ^ "," ^ y ^ ")"

let cross (x : 'a list) (y : 'b list) = 
  List.flatten (List.map (fun y -> List.map (fun x -> (x,y)) x) y)
    
let crossn s n =
  let rec c (acc : 'a list list) = function
    | 0 -> acc
    | n -> c (L.flatten (L.map (fun x -> L.map (fun y -> x::y) acc) s)) (n-1)
  in
  c ([[]]) n

let pset s =
  let rec h acc = function
    | []     -> acc
    | hd::tl -> h (L.flatten (L.map (fun x -> [x @ [hd];x]) acc)) tl
  in
  h [[]] s

let genseq n = 
  let rec r' acc = function 
    | 0 -> List.rev (0::acc)
    | n -> r' (n::acc) (n-1)
  in r' [] (n-1)

let rec zip = function 
  | (h::t,h'::t') -> (h,h')::zip(t,t')
  | _             -> []

(*let rec unzip = function 
  | (h,h')::t -> (h :: unzip 
  | (h::t,h'::t') -> (h,h')::zip(t,t')
  | _             -> []*)

let fnot f = fun x -> (not (f x))

let lfst ls = L.map (fun x -> fst x) ls

let lsnd ls = L.map (fun x -> snd x) ls

let unzip l = (lfst l, lsnd l)

let subset l l' = 
  List.for_all (fun x -> (List.mem x l')) l

let disjoint l l' = 
  List.for_all (fun x -> (not (List.mem x l'))) l && List.for_all (fun x -> (not (List.mem x l))) l'

let ssuperset l l' = 
  (subset l' l) && (List.exists (fun x -> not (List.mem x l')) l)

let set_minus l l' = 
  L.filter (fun x -> not (List.mem x l')) l

let intersect l l' = 
  L.filter (fun x -> (List.mem x l')) l

let equal_sets l l' = 
  List.for_all (fun x -> (List.mem x l')) l && List.for_all (fun x -> (List.mem x l)) l'

(* join a list together using an associative binary operation *)
(*let joinlst = fun l f i ->
  L.fold_left (fun a e -> f a e) i l*)

let getpos ls v =
  let rec i n = function
    | [] -> n
    | hd::tl when (hd = v) -> n
    | hd::tl               -> i (n+1) tl
  in i 0 ls

(* Make a set unique with respect to the equality measure eq *)
let rec uniqueify (eq) = function 
  | [] -> []
  | hd::tl -> if (List.exists (fun x -> (eq hd x)) tl) then (uniqueify eq tl)
    else (hd :: (uniqueify eq tl))

(* Union of l and l' *)
let union l l' = 
  L.fold_left (fun acc x -> if (List.mem x acc) then acc else (x::acc)) (uniqueify (=) l') l

(* Union of a set of lists. *)
let unions l = List.fold_left (fun acc x -> union x acc) [] l
      
(* *)
let cross_n_lists lists = 
  let rec h acc = function 
    | [] -> acc 
    | hd::tl ->
      if hd = [] then [] 
      else
	h (L.flatten
	     (L.map (fun x -> 
	       L.map (fun lst -> lst @ [x]) acc) hd))
	  tl
  in
  h [[]] lists

let read_file filename = 
  let lines = ref "" in
  let chan = open_in filename in
  try
    while true; do
      lines := !lines ^ "\n" ^ (input_line chan)
    done; ""
  with End_of_file ->
    close_in chan;
    !lines


let index_of e l = 
  let rec index_rec i = function
    | [] -> raise Not_found
    | hd::tl -> if hd = e then i else index_rec (i+1) tl
  in
  index_rec 0 l

