(* TEST
 include runtime_events;
 flags = "-runtime-variant=i";
 instrumented-runtime;
 native;
*)

open Runtime_events

let list_ref = ref []
let total_blocks = ref 0
let total_minors = ref 0
let lost_event_words = ref 0

let alloc domain_id ts counts =
  total_blocks := Array.fold_left ( + ) !total_blocks counts

let runtime_end domain_id ts phase =
  match phase with
  | EV_MINOR ->
    total_minors := !total_minors + 1
  | _ -> ()

(* lost words of events *)
let lost_events domain_id words =
  lost_event_words := !lost_event_words + words

let callbacks = Callbacks.create ~runtime_end ~alloc ~lost_events ()

let reset cursor =
  ignore (read_poll cursor callbacks None);
  total_blocks := 0;
  total_minors := 0

let loop n cursor =
  Gc.full_major ();
  reset cursor;
  let minors_before = Gc.((quick_stat ()).minor_collections) in
  for a = 1 to n do
    list_ref := (Sys.opaque_identity(ref 42)) :: !list_ref
  done;
  Gc.full_major ();
  ignore(read_poll cursor callbacks None);
  let minors_after = Gc.((quick_stat ()).minor_collections) in
  minors_after - minors_before

let () =
  start ();
  let cursor = create_cursor None in
  let self_minors_base = loop 0 cursor in
  let blocks_base = !total_blocks in
  let minors_base = !total_minors in
  let self_minors = loop 1_000_000 cursor - self_minors_base in
  let blocks = !total_blocks in
  let minors = !total_minors in
  Printf.printf "lost_event_words: %d, total_blocks: %d, diff_minors: %d\n"
    !lost_event_words (blocks - blocks_base)
    (minors - minors_base - self_minors)
