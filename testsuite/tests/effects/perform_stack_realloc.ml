(* TEST *)

(* The continuation allocated by [perform] is sampled by memprof, and the
   callback grows the fiber's stack, reallocating it. *)

[@@@alert "-unsafe_effects"]

open Effect
open Effect.Deep

type _ t += E : unit t

let rec deep n = if n = 0 then 0 else 1 + deep (n - 1)

let () =
  let _ : Gc.Memprof.t =
    Gc.Memprof.start ~sampling_rate:1.0
      { Gc.Memprof.null_tracker with
        alloc_minor = (fun _ -> ignore (deep 1000 : int); None) }
  in
  try_with perform E
    { effc = (fun (type a) (e : a t) ->
        match e with
        | E -> Some (fun (k : (a, _) continuation) -> continue k ())
        | _ -> None) };
  Gc.Memprof.stop ();
  print_endline "ok"
