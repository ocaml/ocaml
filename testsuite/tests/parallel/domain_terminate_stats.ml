(* TEST
   run_can_skip = "true";
 *)

let alloc_workload () =
  Array.init 10000 (fun _ -> String.make 100 'x') |> ignore

let domain_workload () =
  let domains = Array.init 2 (fun _ -> Domain.spawn alloc_workload)
  in
  Array.iter Domain.join domains

let check name1 v1 (opname, op) name2 v2 =
  if not (op v1 v2) then begin
    Printf.eprintf "Warning: invariant does not hold: %s(%Ld) %s %s(%Ld)!\n" name1 v1 opname name2 v2;
    1
  end else 0

let geq = (">=", (>=))
let leq = ("<=", (<=))
let eq = ("=", (=))

let check_geq0 name v =
  check name v geq "0" 0L

let check_invariants t =
  let open Gc in
  let minor_words = t.minor_words |> Float.round |> Int64.of_float
  and promoted_words = t.promoted_words |> Float.round |> Int64.of_float
  and major_words = t.major_words |> Float.round |> Int64.of_float
  and heap_words = t.heap_words |> Int64.of_int
  and live_words = t.live_words |> Int64.of_int
  and free_words = t.free_words |> Int64.of_int
  and fragments = t.fragments |> Int64.of_int
  and top_heap_words = t.top_heap_words |> Int64.of_int
  in

  check_geq0 "minor_words" minor_words +
  check_geq0 "promoted_words" promoted_words +
  check_geq0 "major_words" major_words +
  check_geq0 "heap_words" heap_words +
  check_geq0 "live_words" live_words +
  check_geq0 "free_words" free_words +
  check_geq0 "fragments" fragments +
  check_geq0 "top_heap_words" top_heap_words +

  (* Number of words allocated in the minor heap that survived a minor collection
     => promoted_words <= minor_words
   *)
  check "promoted_words" promoted_words leq "minor_words" minor_words +

  (* Number of words allocated in the major heap, including the promoted words
     => major_words >= promoted_words
   *)
  check "major_words" major_words geq "promoted_words" promoted_words +

  check "live_words" live_words leq "heap_words" heap_words +
  check "heap_words" heap_words eq "live_words+free_words+fragments"
    Int64.(add (add live_words free_words) fragments) +

  check "top_heap_words" top_heap_words geq "heap_words" heap_words

let gc_and_check f =
  f ();
  Gc.full_major ();
  Gc.minor ();
  let t = Gc.quick_stat () in
  let errors = check_invariants t in
  if errors > 0 then begin
    prerr_endline "---";
    Gc.print_stat stderr;
    prerr_endline "---";
    prerr_endline "";
  end;
  errors

let () =
  let errors = gc_and_check alloc_workload in
  let errors = errors + gc_and_check domain_workload in
  if errors > 0 then begin
    Printf.eprintf "FAIL: %d errors\n" errors;
    exit 125
  end
