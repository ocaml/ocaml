type automaton
type matches
type guard
type thread

external exit : unit -> unit = "process_exit"
external create_location : unit -> location = "create_location"
external create_process : (unit -> unit) -> unit = "create_process"
external create_process_location : location -> (unit -> unit) -> unit = "create_process_location"
external send_sync : automaton -> int -> 'a -> unit = "send_sync"
external send_async : automaton -> int -> 'a -> unit = "send_async"

(* create_automatons nchannels nprocs *)
external create_automaton : int -> int -> automaton = "create_automaton"
external create_automaton_location : location -> int -> int -> automaton = "create_automaton_location"
external patch_match : automaton -> int -> matches -> unit = "patch_match"
external patch_guard : automaton -> int -> guard -> unit = "patch_guard"
external reply_to : 'a -> thread -> unit = "reply_to"
