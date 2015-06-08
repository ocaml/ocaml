external spawn : (unit -> unit) -> unit = "caml_domain_spawn"

external self : unit -> int = "caml_ml_domain_id"

type mutex

external mutex : unit -> mutex = "caml_domain_mutex"

external lock : mutex -> unit = "caml_domain_lock"

external unlock : mutex -> unit = "caml_domain_unlock"
