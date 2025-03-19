(* TEST
 modules = "names_stubs.c";
 include runtime_events;
*)

external get_event_max_ids : unit -> (int * int * int) = "get_event_max_ids"

let () =
  let check_unique_names count f =
    let uniq =
      List.init count (fun i -> f (Obj.magic i))
      |> List.sort_uniq String.compare
      |> List.length
    in
    assert (uniq = count)
  in
  let counters, phases, lifecycles = get_event_max_ids () in
  check_unique_names counters Runtime_events.runtime_counter_name;
  check_unique_names phases Runtime_events.runtime_phase_name;
  check_unique_names lifecycles Runtime_events.lifecycle_name;
  print_endline "ok"
