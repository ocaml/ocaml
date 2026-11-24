(* TEST *)

(* The stacks of a continuation dropped without being resumed are
   reclaimed, young or promoted, also across a compaction; those of a kept
   or resumed continuation are not. *)

open Effect
open Effect.Deep

type _ t += Yield : unit t

let stacks () = (Gc.quick_stat ()).Gc.live_stacks_words

let collect () = Gc.full_major (); Gc.compact (); Gc.full_major ()

(* A fiber stopped at its first [Yield], [depth] frames deep. *)
let gen depth : (unit, int) continuation option =
  let r : (unit, int) continuation option ref = ref None in
  let rec deep n = if n = 0 then (perform Yield; 1) else 1 + deep (n - 1) in
  ignore (match deep depth with
    | x -> x
    | effect Yield, k -> r := Some k; 0);
  !r

let n = 10_000

let () =
  let before = stacks () in
  for _ = 1 to n do ignore (gen 0) done;
  collect ();
  Printf.printf "dropped young: reclaimed %b\n" (stacks () - before < 10 * n);
  let a = Array.init n (fun _ -> gen 0) in
  collect ();
  (* Each kept continuation holds a stack of at least 64 words. *)
  Printf.printf "kept: not reclaimed %b\n" (stacks () - before >= 50 * n);
  let sum = ref 0 in
  Array.iteri (fun i k ->
    if i mod 2 = 0 then begin
      sum := !sum + continue (Option.get k) ();
      a.(i) <- None
    end) a;
  Printf.printf "resumed: %d\n" !sum;
  Array.fill a 0 n None;
  collect ();
  Printf.printf "dropped after promotion: reclaimed %b\n"
    (stacks () - before < 10 * n);
  (* Deep fibers dropped after surviving some minor GCs, with no explicit
     GC call: their stack memory drives the GC (about 4000 words each). *)
  let ring = Array.make 100 None and peak = ref 0 in
  for i = 1 to 3000 do
    ring.(i mod 100) <- gen 2000;
    peak := max !peak (stacks ())
  done;
  ignore (Sys.opaque_identity ring);
  Printf.printf "paced: bounded %b\n" (!peak < 4_000_000)
