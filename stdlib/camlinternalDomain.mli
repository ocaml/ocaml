type entry = {key_id: int ref; mutable slot: Obj.t}

type dls_state =
  { mutable random     : Obj.t; (* Domain-local state of Random module *)
    mutable hashtbl    : Obj.t; (* Domain-local state of Hashtbl module *)
    mutable filename   : Obj.t; (* Domain-local state of filename module *)
    mutable entry_list : entry list }

external get_dls_state : unit -> dls_state = "%dls_get"

val default_initialiser : unit -> dls_state

val register_initialiser : (dls_state -> unit) -> unit

val initialise_dls : unit -> unit
