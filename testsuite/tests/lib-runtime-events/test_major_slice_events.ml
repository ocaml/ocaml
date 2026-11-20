(* TEST
 include runtime_events;
 native;
*)

(* Tests the runtime events emitted for major GC slices:
   - major slice spans contain at least one sweep/mark/ephemeron subspan
     (slices which find no work emit no events at all);
   - opportunistic slices emit no per-slice events; each spin phase of
     opportunistic slices is instead aggregated into a single
     EV_MAJOR_MARK_OPPORTUNISTIC span containing exactly one
     EV_C_MAJOR_SLICE_WORK_DONE counter, which is positive.

   The workload runs a domain making large allocations with plenty of live
   data (steady major GC work) alongside a domain churning short-lived
   allocations (frequent minor collections, so barrier waits with
   opportunistic slices). *)

open Runtime_events

let max_domains = 256
let in_opp = Array.make max_domains false
let opp_work_counters = Array.make max_domains 0
let in_slice = Array.make max_domains false
let slice_subspans = Array.make max_domains 0
let events_lost = ref false
let violations = ref []

let violation fmt =
  Printf.ksprintf (fun s -> violations := s :: !violations) fmt

let runtime_begin dom _ts phase =
  match phase with
  | EV_MAJOR_MARK_OPPORTUNISTIC ->
    if in_opp.(dom) then violation "domain %d: nested opportunistic span" dom;
    in_opp.(dom) <- true;
    opp_work_counters.(dom) <- 0
  | EV_MAJOR_SLICE ->
    if in_opp.(dom) then violation "domain %d: slice span in opp span" dom;
    if in_slice.(dom) then violation "domain %d: nested slice span" dom;
    in_slice.(dom) <- true;
    slice_subspans.(dom) <- 0
  | EV_MAJOR_SWEEP | EV_MAJOR_MARK | EV_MAJOR_EPHE_MARK | EV_MAJOR_EPHE_SWEEP ->
    if in_opp.(dom) then violation "domain %d: slice subspan in opp span" dom;
    slice_subspans.(dom) <- slice_subspans.(dom) + 1
  | _ -> ()

let runtime_end dom _ts phase =
  match phase with
  | EV_MAJOR_MARK_OPPORTUNISTIC ->
    if not in_opp.(dom) then
      violation "domain %d: unmatched opportunistic span end" dom
    else if opp_work_counters.(dom) <> 1 then
      violation "domain %d: %d work counters in opp span"
        dom opp_work_counters.(dom);
    in_opp.(dom) <- false
  | EV_MAJOR_SLICE ->
    if not in_slice.(dom) then
      violation "domain %d: unmatched slice span end" dom
    else if slice_subspans.(dom) = 0 then
      violation "domain %d: slice span with no subspans" dom;
    in_slice.(dom) <- false
  | _ -> ()

let runtime_counter dom _ts counter v =
  match counter with
  | EV_C_MAJOR_SLICE_WORK_DONE ->
    if in_opp.(dom) then begin
      opp_work_counters.(dom) <- opp_work_counters.(dom) + 1;
      if v = 0 then violation "domain %d: zero-work opp span" dom
    end
  | EV_C_MAJOR_HEAP_WORDS | EV_C_MAJOR_ALLOCATED_WORDS
  | EV_C_MAJOR_DIRECT_ALLOCATED_WORDS | EV_C_MAJOR_SUSPENDED_ALLOCATED_WORDS
  | EV_C_MAJOR_RESUMED_ALLOCATED_WORDS | EV_C_MAJOR_ALLOCATED_WORK
  | EV_C_MAJOR_DEPENDENT_WORK | EV_C_MAJOR_EXTRA_WORK
  | EV_C_MAJOR_WORK_COUNTER | EV_C_MAJOR_ALLOC_COUNTER
  | EV_C_MAJOR_SLICE_TARGET | EV_C_MAJOR_SLICE_BUDGET ->
    if in_opp.(dom) then violation "domain %d: slice counter in opp span" dom
  | _ -> ()

let lost_events _dom _words = events_lost := true

let callbacks =
  Callbacks.create ~runtime_begin ~runtime_end ~runtime_counter ~lost_events ()

let n_live = 50_000
let major_iters = 200_000
let churn_iters = 1_000_000

let () =
  start ();
  let cursor = create_cursor None in
  let churner = Domain.spawn (fun () ->
    for _ = 1 to churn_iters do
      ignore (Sys.opaque_identity (Array.make 100 0.0))
    done)
  in
  let live = Array.make n_live [||] in
  for i = 1 to major_iters do
    live.(i mod n_live) <- Array.make 64 i;
    if i mod 500 = 0 then ignore (read_poll cursor callbacks None)
  done;
  ignore (Sys.opaque_identity live);
  Domain.join churner;
  ignore (read_poll cursor callbacks None);
  (* If the ring overflowed, spans may be missing their begins or ends and
     the checks above misfire, so only report violations from a clean run. *)
  if not !events_lost then
    List.iter print_endline (List.sort_uniq compare !violations);
  print_endline "OK"
