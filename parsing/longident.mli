(* Long identifiers, used in parsetree. *)

type t =
    Lident of string
  | Ldot of t * string
