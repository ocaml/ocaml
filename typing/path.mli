(* Access paths *)

type t =
    Pident of Ident.t
  | Pdot of t * string * int

val same: t -> t -> bool
val isfree: Ident.t -> t -> bool

val nopos: int
