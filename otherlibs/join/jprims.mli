type automaton
external exit : unit -> unit = "process_exit"
val create_process : (unit -> unit) -> unit
val create_process_location : location -> (unit -> unit) -> unit
val send_sync : automaton -> int -> 'a -> unit
val send_async : automaton -> int -> 'a -> unit
