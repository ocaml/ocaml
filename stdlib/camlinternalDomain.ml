type entry = {key_id: int ref; mutable slot: Obj.t}

type dls_state =
  { mutable random     : Obj.t;
    mutable format     : Obj.t;
    mutable entry_list : entry list }

external get_dls_state : unit -> dls_state = "%dls_get"

external set_dls_state : dls_state -> unit = "caml_domain_dls_set" [@@noalloc]

let default_initialiser () =
  let st = get_dls_state () in
  if Obj.is_int (Obj.repr st) then begin
    let st =
      { random = Obj.repr (); format = Obj.repr (); entry_list = [] }
    in
    set_dls_state st;
    st
  end else st

let initialiser = ref (fun () -> ignore (default_initialiser ()))

(* Called in the top-level of stdlib modules which need to initialise
 * domain-local state *)
let register_initialiser f =
  let current_initialiser = !initialiser in
  let new_initialiser () =
    current_initialiser ();
    let st = get_dls_state () in
    f st
  in
  initialiser := new_initialiser;
  (* Initialise for the main domain *)
  let st = default_initialiser () in
  f st

(* Called when a new domain is spawned *)
let initialise_dls () =
  let f = !initialiser in
  f ()
