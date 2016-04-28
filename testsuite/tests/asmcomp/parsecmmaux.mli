(* Auxiliary functions for parsing *)

val bind_ident: string -> Ident.t
val find_ident: string -> Ident.t
val unbind_ident: Ident.t -> unit

type error =
    Unbound of string

exception Error of error

val report_error: error -> unit
