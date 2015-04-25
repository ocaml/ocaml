
external spawn : (unit -> unit) -> unit = "caml_domain_spawn"

external self : unit -> int = "caml_ml_domain_id"
