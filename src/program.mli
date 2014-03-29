(**
   
  AST representation for parsing programs written in an IMP syntax.

  Program representation for generating LTSs from input, that can
  subsequently be fed into a model checker.

*)

type id = string

(** Expressions *)
type exp = 
  | Const of int
  | Id    of id
  | Plus  of exp * exp
  | Sub   of exp * exp
  | Mul   of exp * exp
  | Div   of exp * exp
  | Lt    of exp * exp
  | Gt    of exp * exp
  | Eq    of exp * exp
  | Not   of exp
  | And   of exp * exp

(** Commands *)
type cmd = 
  | Skip
  | Asn    of id * exp
  | Seq    of cmd * cmd
  | If     of exp * cmd * cmd
  | While  of exp * cmd
  | Choice of cmd list

(** Security levels. *)
type level = H | L

type init =
  | Any
  | Specific of int

(* Identifier, high/low, bit width, init value *)
type var_configuration =
    id * level * int * init

(** A configuration of the program. *)
type configuration =
    var_configuration list

(** Enumeration of the store *)
type store =
    (id * level * int * int) list

(** A program is a list of variables associated with levels and widths *)
type program =
    configuration * cmd

val mk_program : configuration -> cmd -> program

exception NoId of string


