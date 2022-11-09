(* TEST
   include runtime_events
   flags = "-runtime-variant=i"

   * instrumented-runtime
   ** native
*)

open Runtime_events

let list_ref = ref []
let total_sizes = ref 0
let total_minors = ref 0
let lost_event_words = ref 0

let alloc domain_id ts sizes =
  let size_accum = Array.fold_left (fun x y -> x + y) 0 sizes in
    total_sizes := !total_sizes + size_accum

let runtime_end domain_id ts phase =
  match phase with
  | EV_MINOR ->
    total_minors := !total_minors + 1
  | _ -> ()

(* lost words of events *)
let lost_events domain_id words =
  lost_event_words := !lost_event_words + words

let () =
    Gc.full_major ();
    start ();
    let cursor = create_cursor None in
    for a = 0 to 1_000_000 do
      list_ref := (Sys.opaque_identity(ref 42)) :: !list_ref
    done;
    Gc.full_major ();
    let callbacks = Callbacks.create ~runtime_end ~alloc ~lost_events () in
    ignore(read_poll cursor callbacks None);
    Printf.printf "lost_event_words: %d, total_sizes: %d, total_minors: %d\n"
      !lost_event_words !total_sizes !total_minors
