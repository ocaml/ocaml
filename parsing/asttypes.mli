(* Auxiliary a.s.t. types used by parsetree and typedtree. *)

type constant =
    Const_int of int
  | Const_char of char
  | Const_string of string
  | Const_float of string

type rec_flag = Nonrecursive | Recursive

type direction_flag = Upto | Downto

type mutable_flag = Immutable | Mutable


