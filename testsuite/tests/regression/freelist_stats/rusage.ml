(* TEST
 ocamlrunparam += "b=1";
 modules = "rusage_stubs.c";
 hasunix;
 not target-windows;
 {
   bytecode;
 }
 {
   native;
 }
*)

(* unit is not standard, macOS seems to return bytes,
   but Linux and BSD returns KiB *)
external get_rusage_maxrss : unit -> int = "get_rusage_maxrss"

let word_size_in_bytes = Sys.word_size / 8

(** [alloc_increase_rss ()] allocates memory until the process's ru_maxrss increases. *)
let alloc_increase_rss =
  let global = Stack.create () in
  let rec loop baseline bytes =
    (* can only increase maxrss, so we only push into a global,
       and never pop *)
    Stack.push (String.make bytes 'a') global;
    let n = get_rusage_maxrss () in
    if n = baseline then
      loop baseline (bytes lsl 1)
  in
  fun () ->
  loop (get_rusage_maxrss()) (1 lsl 20);
  (Obj.repr global |> Obj.reachable_words) * word_size_in_bytes

let detect_unit () =
  Gc.compact ();
  (* establish a baseline, after this call currentRSS == maxRSS *)
  let alloc0 = alloc_increase_rss () in
  let rss0 = get_rusage_maxrss () in
  (* allocating more will further increase RSS *)
  let alloc1 = alloc_increase_rss () in
  let rss1 = get_rusage_maxrss () in
  let delta_alloc = alloc1 - alloc0
  and delta_rss = rss1 - rss0 in
  let ratio_log2 = Float.log2 (float_of_int delta_alloc /. float_of_int delta_rss) |> Float.round |> int_of_float in
  (* won't be an exact match, rss increase could be less than the allocation size if already allocated memory is reused *)
  if ratio_log2 = 10 then 1024
  else if ratio_log2 <= 1 then 1
  else
    failwith (Printf.sprintf "Cannot determine unit of ru_maxrss: RSS delta %d, allocation delta %d, ratio_log2 %d"
      delta_rss delta_alloc ratio_log2)

let detected_unit = detect_unit ()

let get_rusage_maxrss_words () =
  let word_size_in_bytes = Sys.word_size / 8 in
  get_rusage_maxrss () * detected_unit / word_size_in_bytes

let sample () =
  (* refresh stats *)
  Gc.minor ();
  let q = Gc.quick_stat () in
  (* assert invariant *)
  if q.heap_words > q.top_heap_words then
    failwith (Printf.sprintf "Top_heap_words: %d, heap_words: %d\n"
      q.top_heap_words q.heap_words);
  q.top_heap_words, get_rusage_maxrss_words ()

let compare_delta =
  Gc.compact ();
  let last = ref (sample ()) in
  fun () ->
    let last_top_heap_words, last_max_rss_words = !last in
    let (current_top_heap_words, current_max_rss_words) as current = sample () in
    let delta_top_heap_words = current_top_heap_words - last_top_heap_words
    and delta_max_rss_words = current_max_rss_words - last_max_rss_words in
    last := current;
    (* check top_heap_words invariant: can only remain the same or increase *)
    if delta_top_heap_words < 0 then
      failwith (Printf.sprintf "Top heap words decreased: %d -> %d\n"
                last_top_heap_words current_top_heap_words);
    if (delta_max_rss_words > 0) && delta_top_heap_words = 0 then
      failwith (Printf.sprintf "Top heap words increased by %d words,\
                                but Max RSS increased by %d words"
                delta_top_heap_words delta_max_rss_words)

let with_live f gc =
  let v = f () in
  compare_delta ();
  let _alive = Sys.opaque_identity v in
  gc ()

let[@inline never] check_alloced f =
  let (_ : int) = alloc_increase_rss () in
  with_live f Gc.full_major;
  with_live f Gc.compact

let verbose_alloc n m =
  Printf.eprintf "Allocating %d strings * %d char each\n" n m;
  Array.init n (fun _ -> String.make m 'a')

let () =
  check_alloced (fun () ->
    prerr_endline "Allocate small strings using pools";
    let small = verbose_alloc 100_000 10 in
    Array.iteri (fun i _ -> if i mod 2 = 0 then small.(i) <- "") small;
    Gc.full_major ();
    prerr_endline "GC major cycle complete: we should now have several partial pools";
    prerr_endline "Allocate large string directly";
    let large = verbose_alloc 1 50_000 in
    (small, large)
  )
