type id = string

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

type cmd = 
  | Skip
  | Asn    of id * exp
  | Seq    of cmd * cmd
  | If     of exp * cmd * cmd
  | While  of exp * cmd
  | Choice of cmd list

type level = H | L

type init =
  | Any
  | Specific of int

type var_configuration =
    id * level * int * init

type configuration =
    var_configuration list

let empty_configuration = []

(* Enumeration of the store: id, level, width, value *)
type store =
    (id * level * int * int) list

(* A program is a list of variables associated with levels and widths *)
type program =
    configuration * cmd

let starting_program = function cmd -> (empty_configuration, cmd)

let mk_program cfg p = cfg,p

exception NoId of string
