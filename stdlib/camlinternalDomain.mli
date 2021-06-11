type entry = {key_id: int ref; mutable slot: Obj.t}

type dls_state =
  { mutable random     : Obj.t;
    mutable format     : Obj.t;
    mutable entry_list : entry list }

external get_dls_state : unit -> dls_state = "%dls_get"

val register_initialiser : (dls_state -> unit) -> unit

val initialise_dls : unit -> unit
