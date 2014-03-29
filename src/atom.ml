
module H = Hashtbl
module L = List

type t = string

type atoms = string list

(* Get them in a proper order. *)
let from_list = L.sort (fun x y -> (H.hash x) - (H.hash y))

let to_list ats = ats

let to_atom x = x

let equal = (=)

let is_true = List.mem

let string_of_atoms al = 
  "(" ^ (L.fold_left (fun acc x -> acc ^ "," ^ x) "" al) ^ ")"

let string_of_atoms_minimal al = 
  let sep = "1" in
  if (al = []) 
  then "e"
  else 
    (L.fold_left (fun acc x -> acc ^ sep ^ x) "" al)

let eta_expand_alist f = 
  fun x -> (from_list (f x))
