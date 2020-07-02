type t

external create: unit -> t = "caml_condition_new"
external wait: t -> Mutex.t -> unit = "caml_condition_wait"
external signal: t -> unit = "caml_condition_signal"
external broadcast: t -> unit = "caml_condition_broadcast"
