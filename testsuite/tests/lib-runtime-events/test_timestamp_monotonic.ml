
(* TEST
 include runtime_events;
*)
open Runtime_events

let () =
  start();

  let t1 = Timestamp.current_timestamp() in
  for _i  = 0 to 1_000_000 do
    ignore (Sys.opaque_identity _i : int)
  done;
  let t2 = Timestamp.current_timestamp() in
  assert (t2 > t1)
