val debug : bool
;;

type label = string
;;

type run_ident =
    Ride_ident of string * int
  | Ride_dot of run_ident * string * int
  | Ride_apply of run_ident * run_ident
;;

type 'a val_type =
  | Rtyp_var of int
  | Rtyp_arrow of label * 'a val_type * 'a val_type
  | Rtyp_tuple of 'a val_type list
  | Rtyp_constr of 'a * 'a val_type list
;;

type run_type = (run_ident * string) val_type
;;

val string_of_digest : string -> string
val string_of_run_type : run_type -> string

val is_instance : ('a -> 'a -> bool) -> 'a val_type -> 'a val_type -> bool

exception Type_match_failure of run_type * run_type * string * int * int
val dynamic_comp : run_type array -> 'a -> 'a * run_type
val fail : run_type -> run_type -> string -> int -> int -> 'a
val coerce_comp : string * int * int -> run_type array -> 'a * run_type -> 'a
(*
val import_comp : string * int * int -> run_type array -> 'a * run_type -> 'a
*)
