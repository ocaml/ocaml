(* TEST
 include runtime_events;
*)
open Runtime_events

let () =
  start ();
  let cursor = create_cursor None in
  let freed = ref false in
  let runtime_begin _ _ phase =
    match phase with
    | EV_MINOR when not !freed ->
        freed := true;
        print_endline "freeing cursor from inside its own event callback";
        (try free_cursor cursor; assert false
        with Failure _ -> ())
    | _ -> ()
  in
  let callbacks = Callbacks.create ~runtime_begin () in
  Gc.full_major ();
  Gc.minor ();
  ignore (read_poll cursor callbacks None)
