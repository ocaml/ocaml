(* TEST
 multicore;
 {
   bytecode;
 }{
   native;
 }
*)

(* Continuations made on a domain that terminates and resumed on two
   others: a resumed continuation keeps its stacks, and the dropped ones
   are reclaimed. *)

open Effect
open Effect.Deep

type _ t += Y : unit t

(* Resuming performs again: that continuation is dropped where it runs. *)
let gen () : (unit, unit) continuation option =
  let r : (unit, unit) continuation option ref = ref None in
  (match perform Y; perform Y with
   | () -> ()
   | effect Y, k -> r := Some k);
  !r

let stacks () = (Gc.quick_stat ()).Gc.live_stacks_words

let rounds = 20 and per_round = 2000
let resumed = Atomic.make 0

let resume ks lo hi = Domain.spawn (fun () ->
  for i = lo to hi - 1 do
    if i mod 2 = 0 then begin
      continue (Option.get ks.(i)) ();
      Atomic.incr resumed
    end
  done)

let () =
  let before = stacks () in
  for _ = 1 to rounds do
    let ks = Domain.join (Domain.spawn (fun () ->
      let ks = Array.init per_round (fun _ -> gen ()) in
      Gc.minor ();
      ks)) in
    let half = per_round / 2 in
    let d1 = resume ks 0 half in
    let d2 = resume ks half per_round in
    Domain.join d1;
    Domain.join d2
  done;
  Gc.full_major ();
  Gc.full_major ();
  Printf.printf "resumed %d of %d\n" (Atomic.get resumed)
    (rounds * per_round / 2);
  (* The dropped continuations hold about 40000 stacks of at least 64
     words. *)
  Printf.printf "reclaimed: %b\n" (stacks () - before < 200_000)
