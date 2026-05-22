(* TEST *)
type t = unit array

let empty = [||]

let alloc words : t =
  assert (words > 1) ;
  let n = words - 1 in
  Array.make n ()

let pool_wsize = 4096

let pool_header_wsize = 4

let minor_heap_size = Gc.(get ()).minor_heap_size

let pools_per_minor_heap = minor_heap_size / (pool_wsize - pool_header_wsize)

let sizeclasses =
  [ 2
  ; 3
  ; 4
  ; 5
  ; 6
  ; 7
  ; 8
  ; 10
  ; 12
  ; 14
  ; 16
  ; 17
  ; 19
  ; 22
  ; 25
  ; 28
  ; 32
  ; 33
  ; 37
  ; 42
  ; 47
  ; 53
  ; 59
  ; 65
  ; 73
  ; 81
  ; 89
  ; 99
  ; 108
  ; 118
  ; 128 ]

let repeat = 40

let keep =
  Array.init repeat (fun _ ->
      Array.make_matrix List.(length sizeclasses) pools_per_minor_heap empty )

let fragment_sizeclasses () =
  let fragment_sizeclass j sizeclass_idx sizeclass =
    let n = minor_heap_size / sizeclass in
    let pool_entries = (pool_wsize - pool_header_wsize) / sizeclass in
    (* this will trigger an implicit minor GC when n > 256 *)
    let all = Array.init n (fun _ -> alloc sizeclass) in
    for i = 1 to pools_per_minor_heap do
      keep.(j).(sizeclass_idx).(i - 1) <- all.((i * pool_entries) - 1)
    done
    (* [all] becomes freeable here *)
  in
  let fragment_sizeclass sizeclass_idx sizeclass =
    for j = 0 to repeat - 1 do
      fragment_sizeclass j sizeclass_idx sizeclass
    done
  in
  sizeclasses |> List.iteri fragment_sizeclass

let print_delta gc0 gc1 reachable_words =
  let delta_top_heap_words = gc1.Gc.top_heap_words - gc0.Gc.top_heap_words in
  let delta_heap_words = gc1.Gc.heap_words - gc0.Gc.heap_words in
  let delta_frag_words = gc1.Gc.fragments - gc0.Gc.fragments in
  Printf.printf
    "+top_heap_words: %d, +heap_words: %d, +reachable_words: %d, +frag_words: \
     %d, ratio: %.2f\n"
    delta_top_heap_words delta_heap_words reachable_words delta_frag_words
    (float_of_int delta_heap_words /. float_of_int reachable_words)

(* for easier comparison with OCaml 4 turn off deprecation alerts on Gc.stat.
   Gc.quick_stat on OCaml 4 would set live_words to 0, so it cannot be used.
 *)
[@@@alert "-deprecated"]

let full_major_and_top_ratio () =
  Gc.full_major () ;
  Gc.full_major () ;
  let gc = Gc.stat () in
  (* we're interested in the top memory usage here, since this can lead to OOM *)
  float_of_int gc.top_heap_words /. float_of_int gc.live_words

let compact_and_ratio () =
  Gc.compact () ;
  Gc.compact () ;
  let gc = Gc.stat () in
  (* compaction can't decrease top_heap_words, just heap_words *)
  float_of_int gc.heap_words /. float_of_int gc.live_words

(* See c89bff11ab944e868413f446e1444577e7f6cdbb *)
let triage_msg = "Have you changed the GC?"
let triage_msg_compact = "Have you changed the GC compactor?"

let () =
  (* disable automatic compaction for a fairer comparison with OCaml 4 *)
  Gc.(set {(get ()) with max_overhead= 1000000}) ;
  fragment_sizeclasses () ;
  let ratio = full_major_and_top_ratio () in
  (* if you've changed the GC and confirmed that the higher ratio is
     an expected and acceptable trade-off then tweak this constant *)
  if ratio > 16. then begin
    Gc.print_stat stdout ;
    Printf.printf "\n[!] Top heap size to live words ratio is too high: %.2f.\n"
      ratio;
    print_endline triage_msg
  end
  else print_endline "top_heap_words/live_words: OK" ;
  let ratio = compact_and_ratio () in
  (* This is expected to be around 1, so this is not expected to fail.
     There is some memory that cannot freed, so it isn't exactly 1.
   *)
  if ratio > 3. then begin
    Gc.print_stat stdout ;
    Printf.printf
      "\n[!] Heap size after compaction to live words ratio is too high: %.2f\n"
      ratio;
    print_endline triage_msg_compact
  end
  else print_endline "\ncompact: OK"
