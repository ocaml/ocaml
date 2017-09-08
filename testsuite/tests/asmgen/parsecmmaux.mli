(* Auxiliary functions for parsing *)

val bind_ident: string -> Ident.t
val find_ident: string -> Ident.t
val unbind_ident: Ident.t -> unit

val find_label: string -> int

val debuginfo: ?loc:Location.t -> unit -> Debuginfo.t

type error =
    Unbound of string
  | Undefined_continuation of int
  | Wrong_stack_at_poptrap of (int * int option)
  | Inconsistent_stacks of int

exception Error of error

val report_error: error -> unit

val adjust_traps_at_exit: Cmm.phrase -> Cmm.phrase
