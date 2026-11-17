(* TEST
 include unix;
 include runtime_events;
 libunix; (* the stderr-redirection dance trips Windows file sharing *)
 not tsan; (* timing-sensitive; far too slow under TSan *)
 native;
*)

(* Measure the rates at which the major GC marks and sweeps, the minor GC
   promotes, and the compactor compacts.

   Build a live heap of mixed structure, then churn it, while consuming my own
   runtime-events stream, accumulating time spent inside EV_MAJOR_SWEEP and
   EV_MAJOR_MARK spans. Stderr redirected to a file with the runtime's debug
   log enabled to obtain per-slice sweep and mark work from the "Major
   slice" lines. Finally measure
   compaction three times: at steady state (live data, garbage-laden heap),
   immediately again (live data, compacted heap), and after dropping the data
   (near-empty), timing the EV_COMPACT span and its sub-phases.

   With no arguments (testsuite mode): small heap, a few seconds, terse
   output checking the rates clear generous de-minimis thresholds.  With
   arguments (manual mode): gcrate LIVE-GIB SECONDS [SEED] prints a full
   report.  Steady-state major heap is roughly 2.4x LIVE-GIB at the default
   space_overhead of 80. *)

module RE = Runtime_events

let manual = Array.length Sys.argv > 1

let usage =
  "usage: gcrate [LIVE-GIB [SECONDS [SEED]]]\n\
   \n\
   Estimates the rates at which the OCaml major GC marks and sweeps,\n\
   the minor GC evacuates survivors,\n\
   and the compactor compacts. Builds LIVE-GIB GiB of live\n\
   mixed-structure heap, churns it for SECONDS seconds (default 300),\n\
   and then compacts twice, drops the roots, and compacts one more time.\n\
   \n\
   With no arguments: self-test mode, as run from the testsuite - 32 MiB\n\
   live heap, 3 s churn, terse pass/fail output against de-minimis rate\n\
   thresholds.\n"

let () =
  if manual then
    match Sys.argv.(1) with
    | "help" | "-help" | "--help" | "-h" ->
      print_string usage;
      exit 0
    | _ -> ()

let arg conv i default =
  if Array.length Sys.argv > i then
    match conv Sys.argv.(i) with
    | Some v -> v
    | None ->
      Printf.eprintf "gcrate: bad argument %S (try --help)\n" Sys.argv.(i);
      exit 2
  else default

let live_gib = arg float_of_string_opt 1 0.03125 (* test mode: 32 MiB *)
let duration = arg float_of_string_opt 2 (if manual then 300.0 else 3.0)
let seed = arg int_of_string_opt 3 42

let target_words = int_of_float (live_gib *. (1024. *. 1024. *. 1024. /. 8.))
let slot_words = 2000
let nslots = max 64 (target_words / slot_words)

(* De-minimis thresholds for testsuite mode, 20-40x below rates measured on
   an idle x86-64 dev box in 2026 (mark ~200 Mwords/s, sweep ~1600 Mwords/s,
   roughly independent of heap size): only catastrophic regressions or broken
   measurement plumbing should fail.  Debug/instrumented runtimes are slower
   still. *)
let variant_factor =
  match Sys.runtime_variant () with "" -> 1.0 | _ -> 0.1

let min_mark_rate = 10e6 *. variant_factor
let min_sweep_rate = 40e6 *. variant_factor
let min_compact_rate = 2e6 *. variant_factor (* live words / compact time *)
let min_promote_rate = 2e6 *. variant_factor (* promoted words / minor time *)

(* ---------------- runtime-events consumption ---------------- *)

let phase_total : (RE.runtime_phase, int ref * int ref) Hashtbl.t =
  Hashtbl.create 64

let phase_open : (RE.runtime_phase, int) Hashtbl.t = Hashtbl.create 64

let counter_total : (RE.runtime_counter, int ref * int ref) Hashtbl.t =
  Hashtbl.create 64

let lost_total = ref 0

let ns_of ts = Int64.to_int (RE.Timestamp.to_int64 ts)

let tally tbl key v =
  let count, total =
    match Hashtbl.find_opt tbl key with
    | Some ct -> ct
    | None ->
      let ct = (ref 0, ref 0) in
      Hashtbl.add tbl key ct;
      ct
  in
  incr count;
  total := !total + v

let runtime_begin _ring ts phase = Hashtbl.replace phase_open phase (ns_of ts)

let runtime_end _ring ts phase =
  match Hashtbl.find_opt phase_open phase with
  | None -> () (* span began before we started consuming *)
  | Some t0 ->
    Hashtbl.remove phase_open phase;
    tally phase_total phase (ns_of ts - t0)

let runtime_counter _ring _ts counter v = tally counter_total counter v

let lost_events _ring n = lost_total := !lost_total + n

let callbacks =
  RE.Callbacks.create ~runtime_begin ~runtime_end ~runtime_counter
    ~lost_events ()

let cursor = ref None

let poll () =
  match !cursor with
  | None -> ()
  | Some c -> ignore (RE.read_poll c callbacks None)

let reset_tallies () =
  Hashtbl.reset phase_total;
  Hashtbl.reset phase_open;
  Hashtbl.reset counter_total;
  lost_total := 0

let span phase =
  match Hashtbl.find_opt phase_total phase with
  | Some (c, ns) -> (!c, !ns)
  | None -> (0, 0)

(* ---------------- representative heap ---------------- *)

(* Shared immortal strings, like interned symbols/atoms. *)
let interned =
  let st = Random.State.make [| 0xbeef |] in
  Array.init 4096 (fun _ ->
      String.init
        (8 + Random.State.int st 24)
        (fun _ -> Char.chr (97 + Random.State.int st 26)))

type node =
  | Leaf
  | Str of string
  | Floats of floatarray
  | Ints of int array
  | Cons of node * node
  | Pair of node * node
  | Rec of record

and record =
  { id : int;
    name : string;
    mutable hits : int;
    mutable score : float; (* boxed *)
    kids : node array
  }

(* Builders track an approximate word budget so a "slot" lands near
   [slot_words] total. *)
type builder =
  { rng : Random.State.t;
    mutable budget : int
  }

let charge b w = b.budget <- b.budget - w

(* Uniform in [1, 2*mean-1]; mean is [mean]. *)
let rand_len b mean = 1 + Random.State.int b.rng ((2 * mean) - 1)

let pick_interned b =
  interned.(Random.State.int b.rng (Array.length interned))

(* Smart constructors: each charges the builder for the words it
   allocates (header included) as it constructs.  [build_leaf]
   allocates nothing but still charges a word, so that budgets always
   make progress; [build_interned] shares an existing string and
   charges nothing. *)

let build_leaf b = charge b 1; Leaf

let build_interned b = Str (pick_interned b)

let build_cons b x xs = charge b 3; Cons (x, xs)

let build_pair b x y = charge b 3; Pair (x, y)

let build_string b =
  let len = rand_len b 40 in
  charge b (2 + (len / 8));
  Str (String.make len (Char.chr (97 + Random.State.int b.rng 26)))

let build_floats b =
  let len = rand_len b 24 in
  charge b (1 + len);
  Floats (Float.Array.init len (fun i -> float_of_int i *. 1.5))

let build_ints b =
  let len = rand_len b 16 in
  charge b (1 + len);
  Ints (Array.init len (fun i -> i * 7))

let build_rec b ~id ~name ~score ~kids =
  charge b (6 + 2 + 1 + Array.length kids)
    (* record + boxed float + kids array *);
  Rec { id; name; hits = 0; score; kids }

let rec build b depth =
  if b.budget <= 0 || depth > 14 then build_leaf b
  else
    match Random.State.int b.rng 100 with
    | n when n < 25 ->
      (* a run of list cells holding small elements *)
      let len = rand_len b 24 in
      let tail = ref Leaf in
      for _ = 1 to len do
        let elt =
          match Random.State.int b.rng 10 with
          | 0 | 1 | 2 | 3 -> build_interned b
          | 4 -> build_string b
          | 5 -> build_pair b Leaf Leaf
          | _ -> Leaf
        in
        tail := build_cons b elt !tail
      done;
      !tail
    | n when n < 40 -> build_string b
    | n when n < 50 -> build_floats b
    | n when n < 57 -> build_ints b
    | n when n < 87 ->
      let nkids = 1 + Random.State.int b.rng 7 in
      build_rec b
        ~id:(Random.State.int b.rng 1000000)
        ~name:(pick_interned b)
        ~score:(Random.State.float b.rng 1.0)
        ~kids:(Array.init nkids (fun _ -> build b (depth + 1)))
    | _ ->
      build_pair b (build b (depth + 1)) (build b (depth + 1))

let build_slot rng budget =
  let b = { rng; budget } in
  let acc = ref Leaf in
  while b.budget > 0 do
    acc := build_cons b (build b 0) !acc
  done;
  !acc

let rec first_record node depth =
  if depth > 8 then None
  else
    match node with
    | Rec r -> Some r
    | Cons (a, b) | Pair (a, b) -> (
      match first_record a (depth + 1) with
      | Some _ as s -> s
      | None -> first_record b (depth + 1))
    | Leaf | Str _ | Floats _ | Ints _ -> None

(* Shallow walk: consumes freshly built ephemeral data so it can't be
   optimized away, and touches it like a real mutator would. *)
let rec checksum node depth acc =
  if depth > 4 then acc
  else
    match node with
    | Leaf -> acc + 1
    | Str s -> acc + String.length s
    | Floats f -> acc + Float.Array.length f
    | Ints a -> acc + Array.length a
    | Cons (a, b) | Pair (a, b) ->
      checksum b (depth + 1) (checksum a (depth + 1) acc)
    | Rec r -> acc + r.id

(* ---------------- GC log capture and parsing ---------------- *)

let log_path = Filename.temp_file "gcrate" ".gclog"
let compact_log_path = Filename.temp_file "gcrate" ".compactlog"
let saved_stderr = Unix.dup Unix.stderr

let redirect_stderr_to path =
  flush stderr;
  let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  Unix.dup2 fd Unix.stderr;
  Unix.close fd

let restore_stderr () =
  flush stderr;
  Unix.dup2 saved_stderr Unix.stderr

(* Disable auto-compaction: Gc.full_major runs its last cycle with
   Compaction_auto, which fires once the roots are dropped and would compact
   the heap before the measured Gc.compact sees it.  Must precede
   [set_verbose] capturing the control record below. *)
let () = Gc.set { (Gc.get ()) with Gc.max_overhead = 1000000 }

(* Prebuilt controls: Gc.set at a measurement boundary must not allocate.
   The per-slice "Major slice" lines are caml_gc_log output, enabled by
   CAML_GC_MSG_DEBUG (0x800); compaction messages are 0x10.  The selector
   values 4 and 8 are symbolic. *)
let set_verbose =
  let ctl = Gc.get () in
  let slice_on = { ctl with Gc.verbose = 0x800 } in
  let compact_on = { ctl with Gc.verbose = 0x10 } in
  let off = { ctl with Gc.verbose = 0 } in
  fun v ->
    Gc.set (if v = 4 then slice_on else if v = 8 then compact_on else off)

let find_sub line sub =
  let n = String.length line and m = String.length sub in
  let rec go i =
    if i + m > n then None
    else if String.sub line i m = sub then Some i
    else go (i + 1)
  in
  go 0

type work =
  { mutable slices : int;
    mutable sweep : int;
    mutable mark : int;
    mutable blocks : int
  }

let parse_gclog path =
  let w =
    { slices = 0; sweep = 0; mark = 0; blocks = 0 }
  in
  let ic = open_in path in
  (try
     while true do
       let line = input_line ic in
       match find_sub line "Major slice [" with
       | None -> ()
       | Some i -> (
         match String.index_from_opt line i ':' with
         | None -> ()
         | Some j ->
           let rest = String.sub line (j + 1) (String.length line - j - 1) in
           (try
              Scanf.sscanf rest
                " %d sweep, %d mark (%d blocks)"
                (fun s m b ->
                  w.slices <- w.slices + 1;
                  w.sweep <- w.sweep + s;
                  w.mark <- w.mark + m;
                  w.blocks <- w.blocks + b)
            with Scanf.Scan_failure _ | Failure _ | End_of_file -> ()))
     done
   with End_of_file -> close_in ic);
  w

(* ---------------- reporting ---------------- *)

let secs ns = float_of_int ns /. 1e9
let rate words ns = if ns = 0 then 0.0 else float_of_int words /. secs ns

let report_stat prefix (s : Gc.stat) =
  Printf.printf
    "META %s: minor_words=%.0f promoted_words=%.0f major_words=%.0f \
     minor_collections=%d major_collections=%d heap_words=%d \
     top_heap_words=%d compactions=%d forced_major_collections=%d\n"
    prefix s.minor_words s.promoted_words s.major_words s.minor_collections
    s.major_collections s.heap_words s.top_heap_words s.compactions
    s.forced_major_collections

let tallies_snapshot () =
  let phases =
    Hashtbl.fold
      (fun p (c, t) acc -> (RE.runtime_phase_name p, !c, !t) :: acc)
      phase_total []
  and counters =
    Hashtbl.fold
      (fun k (c, t) acc -> (RE.runtime_counter_name k, !c, !t) :: acc)
      counter_total []
  in
  (List.sort compare phases, List.sort compare counters)

let report_tallies (phases, counters) =
  List.iter
    (fun (name, c, t) -> Printf.printf "PHASE %s count=%d ns=%d\n" name c t)
    phases;
  List.iter
    (fun (name, c, t) ->
      Printf.printf "COUNTER %s count=%d total=%d\n" name c t)
    counters

let report_rate what words ns =
  Printf.printf "%-8s %16d words in %9.3f s -> %7.1f Mwords/s (%.2f GiB/s)\n"
    what words (secs ns)
    (rate words ns /. 1e6)
    (rate words ns *. 8. /. (1024. *. 1024. *. 1024.))

(* Compaction measurement.  [mapped_before] estimates the mapped heap
   extent: sweeping parks fully-empty pools on an OS freelist that
   heap_words no longer counts, so we carry forward the previous
   compaction's heap_after (pools mapped then stay mapped until a
   compaction releases them). *)
type compaction =
  { clabel : string;
    live_before : int;
    heap_before : int;
    mapped_before : int;
    heap_after : int;
    runs : int; (* Gc.stat compactions delta: 1 if it really ran *)
    wall : float; (* around Gc.compact, including its full major *)
    compact_ns : int;
    evacuate_ns : int;
    forward_ns : int;
    release_ns : int;
    (* span counts: >1 forward pass means phase two of the default
       algorithm ran its own fixup *)
    evacuate_spans : int;
    forward_spans : int;
    release_spans : int
  }

let measure_compaction clabel mapped_before =
  if manual then Printf.printf "# compacting (%s)\n%!" clabel;
  Gc.full_major ();
  let b = Gc.quick_stat () in
  poll ();
  reset_tallies ();
  let t0 = Unix.gettimeofday () in
  Gc.compact ();
  let t1 = Unix.gettimeofday () in
  poll ();
  let a = Gc.quick_stat () in
  let evacuate_spans, evacuate_ns = span RE.EV_COMPACT_EVACUATE in
  let forward_spans, forward_ns = span RE.EV_COMPACT_FORWARD in
  let release_spans, release_ns = span RE.EV_COMPACT_RELEASE in
  { clabel;
    live_before = b.live_words;
    heap_before = b.heap_words;
    mapped_before = max mapped_before b.heap_words;
    heap_after = a.heap_words;
    runs = a.compactions - b.compactions;
    wall = t1 -. t0;
    compact_ns = snd (span RE.EV_COMPACT);
    evacuate_ns;
    forward_ns;
    release_ns;
    evacuate_spans;
    forward_spans;
    release_spans
  }

let report_compaction c =
  Printf.printf
    "COMPACT %s: live_before=%d heap_before=%d mapped_before=%d \
     heap_after=%d runs=%d wall=%.3f compact_ns=%d evacuate_ns=%d \
     forward_ns=%d release_ns=%d evacuate_spans=%d forward_spans=%d \
     release_spans=%d\n"
    c.clabel c.live_before c.heap_before c.mapped_before c.heap_after c.runs
    c.wall c.compact_ns c.evacuate_ns c.forward_ns c.release_ns
    c.evacuate_spans c.forward_spans c.release_spans

let compaction_summary c1 c2 c3 =
  List.iter
    (fun c ->
      Printf.printf
        "compact %-9s: %6.1f Mw live, %7.1f -> %6.1f Mw heap, \
         %7.3f s (evac %.3f fwd %.3f rel %.3f), %6.1f Mwords(live)/s\n"
        c.clabel
        (float_of_int c.live_before /. 1e6)
        (float_of_int c.mapped_before /. 1e6)
        (float_of_int c.heap_after /. 1e6)
        (secs c.compact_ns) (secs c.evacuate_ns) (secs c.forward_ns)
        (secs c.release_ns)
        (rate c.live_before c.compact_ns /. 1e6))
    [ c1; c2; c3 ];
  (* The phases separate the factors directly: forward (pointer fixup)
     scans all live words; evacuate copies live blocks out of mostly-empty
     pools (cost tracks the garbage-laden heap; moved words are not
     instrumented); release returns pools to the OS. *)
  Printf.printf
    "live-size factor, forward fixup: %.1f Mwords(live)/s per pass steady \
     (%d passes), %.1f compacted (%d passes)\n"
    (rate (c1.live_before * max 1 c1.forward_spans) c1.forward_ns /. 1e6)
    c1.forward_spans
    (rate (c2.live_before * max 1 c2.forward_spans) c2.forward_ns /. 1e6)
    c2.forward_spans;
  Printf.printf
    "heap-size factor, release: %.0f Mwords(freed)/s steady, %.0f emptied\n"
    (rate (c1.mapped_before - c1.heap_after) c1.release_ns /. 1e6)
    (rate (c3.mapped_before - c3.heap_after) c3.release_ns /. 1e6);
  Printf.printf
    "evacuate (copy): %.3f s steady vs %.3f s freshly compacted\n"
    (secs c1.evacuate_ns) (secs c2.evacuate_ns);
  (* Crude two-point fit T = a*live + b*mapped from the steady and emptied
     compactions; the middle compaction checks it (expect a poor fit: the
     evacuate cost depends on garbage, which this model cannot see). *)
  let l1 = float_of_int c1.live_before
  and h1 = float_of_int c1.mapped_before
  and t1 = secs c1.compact_ns in
  let l3 = float_of_int c3.live_before
  and h3 = float_of_int c3.mapped_before
  and t3 = secs c3.compact_ns in
  let det = (l1 *. h3) -. (l3 *. h1) in
  if det <> 0. then begin
    let a = ((t1 *. h3) -. (t3 *. h1)) /. det in
    let b = ((l1 *. t3) -. (l3 *. t1)) /. det in
    Printf.printf
      "two-point fit: %.2f ns/live word (%.1f Mwords/s) + %.3f ns/heap word \
       (%.1f Mwords/s); predicts %s = %.3f s (measured %.3f s)\n"
      (a *. 1e9) (1e-6 /. a) (b *. 1e9) (1e-6 /. b) c2.clabel
      ((a *. float_of_int c2.live_before)
       +. (b *. float_of_int c2.mapped_before))
      (secs c2.compact_ns)
  end

let check name ok detail =
  if ok then Printf.printf "%s: ok\n" name
  else Printf.printf "%s: FAIL (%s)\n" name detail

(* ---------------- main ---------------- *)

let () =
  RE.start ();
  cursor := Some (RE.create_cursor None);
  let rng = Random.State.make [| seed |] in
  if manual then
    Printf.printf "# building %.3f GiB live: %d slots of ~%d words\n%!"
      live_gib nslots slot_words;
  let t0 = Unix.gettimeofday () in
  let slots = Array.make nslots Leaf in
  let recs = Array.make nslots None in
  let tick = max 1 (nslots / 10) in
  for i = 0 to nslots - 1 do
    let s = build_slot rng slot_words in
    slots.(i) <- s;
    recs.(i) <- first_record s 0;
    if i mod tick = 0 then begin
      poll ();
      if manual && i / tick < 10 then
        Printf.printf "# build %d%%\n%!" (i / tick * 10)
    end
  done;
  if manual then
    Printf.printf "# build done in %.1f s\n%!" (Unix.gettimeofday () -. t0);
  Gc.full_major ();
  Gc.full_major ();
  let stat0 = Gc.quick_stat () in
  redirect_stderr_to log_path;
  poll ();
  reset_tallies ();
  set_verbose 4;
  let wall0 = Unix.gettimeofday () in
  let cpu0 = Sys.time () in
  let iters = ref 0 in
  let sink = ref 0 in
  let deadline = wall0 +. duration in
  let next_decade = ref 1 in
  if manual then
    Printf.printf "# churn 0%% (heap %d Mwords)\n%!"
      ((Gc.quick_stat ()).heap_words / 1048576);
  (try
     while true do
       incr iters;
       (* replace one slot: ~slot_words of allocation that survives *)
       let i = Random.State.int rng nslots in
       let s = build_slot rng slot_words in
       slots.(i) <- s;
       recs.(i) <- first_record s 0;
       (* ~4x that in short-lived garbage *)
       for _ = 1 to 4 do
         sink := checksum (build_slot rng slot_words) 0 !sink
       done;
       (* mutation traffic on old records *)
       for _ = 1 to 8 do
         match recs.(Random.State.int rng nslots) with
         | None -> ()
         | Some r ->
           r.hits <- r.hits + 1;
           r.score <- r.score +. 0.5
       done;
       if !iters land 31 = 0 then poll ();
       if !iters land 255 = 0 then begin
         let now = Unix.gettimeofday () in
         if now > deadline then raise Exit;
         if manual && !next_decade <= 9
            && now >= wall0 +. (duration *. float_of_int !next_decade /. 10.)
         then begin
           Printf.printf "# churn %d%% (heap %d Mwords)\n%!"
             (!next_decade * 10)
             ((Gc.quick_stat ()).heap_words / 1048576);
           incr next_decade
         end
       end
     done
   with Exit -> ());
  poll ();
  set_verbose 0;
  poll ();
  let wall1 = Unix.gettimeofday () in
  let cpu1 = Sys.time () in
  let stat1 = Gc.quick_stat () in
  restore_stderr ();
  let w = parse_gclog log_path in
  let sweep_spans, sweep_ns = span RE.EV_MAJOR_SWEEP in
  let mark_spans, mark_ns = span RE.EV_MAJOR_MARK in
  let minor_spans, minor_ns = span RE.EV_MINOR in
  let minor_allocated =
    int_of_float (stat1.minor_words -. stat0.minor_words) in
  let promoted = int_of_float (stat1.promoted_words -. stat0.promoted_words) in
  (* Snapshot the churn tallies and losses: the compaction measurements
     below reset them. *)
  let churn_tallies = tallies_snapshot () in
  let churn_lost = !lost_total in
  redirect_stderr_to compact_log_path;
  set_verbose 8;
  let c1 = measure_compaction "steady" 0 in
  let c2 = measure_compaction "compacted" c1.heap_after in
  Array.fill slots 0 nslots Leaf;
  Array.fill recs 0 nslots None;
  let c3 = measure_compaction "emptied" c2.heap_after in
  set_verbose 0;
  restore_stderr ();
  if manual then begin
    Printf.printf
      "META config: live_gib=%.3f duration=%.1f seed=%d slots=%d \
       slot_words=%d minor_heap_size=%d space_overhead=%d\n"
      live_gib duration seed nslots slot_words (Gc.get ()).minor_heap_size
      (Gc.get ()).space_overhead;
    Printf.printf "META churn: wall=%.3f cpu=%.3f iters=%d sink=%d \
                   lost_events=%d\n"
      (wall1 -. wall0) (cpu1 -. cpu0) !iters !sink churn_lost;
    report_stat "stat0" stat0;
    report_stat "stat1" stat1;
    report_tallies churn_tallies;
    Printf.printf "\nslices=%d blocks_marked=%d\n" w.slices w.blocks;
    report_rate "sweep" w.sweep sweep_ns;
    report_rate "mark" w.mark mark_ns;
    Printf.printf
      "minor: %d collections in %.3f s (mean pause %.0f us), %d words \
       allocated, %d promoted (%.1f%% survival)\n"
      minor_spans (secs minor_ns)
      (if minor_spans = 0 then 0.
       else secs minor_ns *. 1e6 /. float_of_int minor_spans)
      minor_allocated promoted
      (if minor_allocated = 0 then 0.
       else 100. *. float_of_int promoted /. float_of_int minor_allocated);
    report_rate "promotion (minor live evacuation)" promoted minor_ns;
    report_rate "minor absorption (allocated/pause)" minor_allocated minor_ns;
    if w.mark > 0 && mark_ns > 0 && sweep_ns > 0 then
      Printf.printf
        "sweep/mark rate ratio: %.2f (runtime assumes \
         percent_sweep_per_mark=120, i.e. 1.20)\n"
        (rate w.sweep sweep_ns /. rate w.mark mark_ns);
    Printf.printf "GC logs kept in %s (churn) and %s (compaction)\n"
      log_path compact_log_path;
    print_newline ();
    report_compaction c1;
    report_compaction c2;
    report_compaction c3;
    compaction_summary c1 c2 c3;
    let gib_s words ns = rate words ns *. 8. /. (1024. *. 1024. *. 1024.) in
    Printf.printf "\nsummary:\n";
    Printf.printf "  sweep      %7.2f GiB/s\n" (gib_s w.sweep sweep_ns);
    Printf.printf "  mark       %7.2f GiB/s\n" (gib_s w.mark mark_ns);
    Printf.printf
      "  compaction %7.2f GiB/s (steady: %.2f GiB heap compacted in \
       %.3f s)\n"
      (gib_s c1.heap_before c1.compact_ns)
      (float_of_int c1.heap_before *. 8. /. (1024. *. 1024. *. 1024.))
      (secs c1.compact_ns)
  end
  else begin
    check "heap built" (stat1.heap_words >= target_words)
      (Printf.sprintf "heap_words=%d < target=%d" stat1.heap_words
         target_words);
    check "slices measured"
      (w.slices >= 10 && w.sweep > 0 && w.mark > 0 && sweep_spans > 0
       && mark_spans > 0 && churn_lost = 0)
      (Printf.sprintf
         "slices=%d sweep_words=%d mark_words=%d sweep_spans=%d \
          mark_spans=%d lost=%d"
         w.slices w.sweep w.mark sweep_spans mark_spans churn_lost);
    check "sweep rate"
      (rate w.sweep sweep_ns >= min_sweep_rate)
      (Printf.sprintf "%.1f Mwords/s < %.1f" (rate w.sweep sweep_ns /. 1e6)
         (min_sweep_rate /. 1e6));
    check "mark rate"
      (rate w.mark mark_ns >= min_mark_rate)
      (Printf.sprintf "%.1f Mwords/s < %.1f" (rate w.mark mark_ns /. 1e6)
         (min_mark_rate /. 1e6));
    check "minor collections measured"
      (minor_spans >= 10 && minor_ns > 0 && promoted > 0)
      (Printf.sprintf "spans=%d ns=%d promoted=%d" minor_spans minor_ns
         promoted);
    check "promotion rate"
      (rate promoted minor_ns >= min_promote_rate)
      (Printf.sprintf "%.1f Mwords/s < %.1f" (rate promoted minor_ns /. 1e6)
         (min_promote_rate /. 1e6));
    check "compaction runs"
      (c1.runs = 1 && c2.runs = 1 && c3.runs = 1
       && c1.heap_after < c1.heap_before
       && c3.live_before * 4 < c1.live_before)
      (Printf.sprintf "runs=%d,%d,%d heap=%d->%d live=%d->%d" c1.runs c2.runs
         c3.runs c1.heap_before c1.heap_after c1.live_before c3.live_before);
    check "compaction rate"
      (rate c1.live_before c1.compact_ns >= min_compact_rate)
      (Printf.sprintf "%.1f Mwords/s < %.1f"
         (rate c1.live_before c1.compact_ns /. 1e6)
         (min_compact_rate /. 1e6));
    Sys.remove log_path;
    Sys.remove compact_log_path
  end
