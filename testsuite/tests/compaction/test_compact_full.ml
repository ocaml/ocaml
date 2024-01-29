(* TEST
*)
let () =
  Gc.full_major (); (* do a major before compaction so there isn't any pending major
                  cycles when we do compaction. *)
  let heap_stats_before = Gc.quick_stat () in
  Gc.compact ();
  let heap_stats_after = Gc.quick_stat () in
  (* assert that we have done an additional three major collections *)
  assert (heap_stats_after.major_collections == heap_stats_before.major_collections+3);
  (* also a compaction! *)
  assert (heap_stats_after.compactions == heap_stats_before.compactions+1)
