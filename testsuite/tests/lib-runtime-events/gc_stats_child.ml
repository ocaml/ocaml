(* Child program that does allocation work.
   Used by test_gc_stats_match.ml.

   With OCAMLRUNPARAM=v=0x400, the runtime prints GC stats to stderr at exit.
   With OCAML_RUNTIME_EVENTS_START=1, runtime events are enabled from startup.
   The parent compares the v=0x400 stats with runtime event counters. *)

let () =
  (* Do allocation that triggers multiple minor GCs *)
  let r = ref [] in
  for _ = 1 to 100 do
    for i = 1 to 1000 do
      r := (Sys.opaque_identity (ref i)) :: !r
    done;
    Gc.minor ()
  done;

  (* Do a major GC *)
  Gc.full_major ();

  (* Keep data alive until exit *)
  let _ = Sys.opaque_identity !r in
  ()
