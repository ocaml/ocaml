(* TEST *)

(* Pools that become empty during sweeping are moved to a global freelist.
   They stay mapped until the next compaction, so they must be counted in
   [heap_words]; and they were already counted in [top_heap_words] while
   in use, so they must not be counted there a second time. *)

let n = 100_000

(* About 5 words per element, all in pools: a cons cell and a ref. *)
let[@inline never] alloc () = List.init n (fun i -> ref i)

(* Global, so that it is a GC root until we clear it. *)
let l = ref []

let stat () = Gc.full_major (); Gc.quick_stat ()

let check name cond =
  if not cond then Printf.printf "FAIL: %s\n%!" name

(* Separate [inline never] function needed for flambda
   see https://ocaml.org/manual/5.5/flambda.html#ss:flambda-lift-toplevel-let *)
let[@inline never] main () =
  let pool_words = 5 * n in
  (* Slack for unrelated allocations (channels, Printf, ...). *)
  let slack = pool_words / 10 in
  l := alloc ();
  let s0 = stat () in
  l := [];
  (* The pools are now empty and on the freelist. *)
  let s1 = stat () in
  check "freed pools are still counted in heap_words"
    (s1.heap_words > s0.heap_words - slack);
  check "freed pools are not counted twice in top_heap_words"
    (s1.top_heap_words < s0.top_heap_words + slack);
  (* Allocating again reuses the freelist rather than mapping new pools. *)
  l := alloc ();
  let s2 = stat () in
  check "reusing freelist pools does not grow heap_words"
    (s2.heap_words < s0.heap_words + slack);
  check "reusing freelist pools does not grow top_heap_words"
    (s2.top_heap_words < s0.top_heap_words + slack);
  l := [];
  ignore (stat ());
  (* Compaction unmaps the freelist. *)
  Gc.compact ();
  let s3 = Gc.quick_stat () in
  check "compaction releases freelist pools from heap_words"
    (s3.heap_words < s0.heap_words - pool_words / 2);
  List.iter (fun (name, (s : Gc.stat)) ->
      check ("heap_words <= top_heap_words " ^ name)
        (s.heap_words <= s.top_heap_words))
    ["s0", s0; "s1", s1; "s2", s2; "s3", s3];
  print_endline "done"

let () = main ()
