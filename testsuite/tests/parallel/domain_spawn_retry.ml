(* TEST
 ocamlrunparam += ",d=2";
*)

(* A failed Domain.spawn must not prevent a later spawn. Here the failure is
   slot exhaustion: OCAMLRUNPARAM d=2 leaves room for one child only. This is
   a smoke test for the retry path; a failure during domain startup (for
   example, creating the backup thread) would need a fault injection point. *)

let () =
  let finish = Atomic.make false in
  let child = Domain.spawn (fun () ->
    while not (Atomic.get finish) do Domain.cpu_relax () done)
  in
  let spawn_failed =
    match Domain.spawn (fun () -> ()) with
    | exception Failure _ -> true
    | extra -> Domain.join extra; false
  in
  Atomic.set finish true;
  Domain.join child;
  assert spawn_failed;
  Gc.full_major ();
  assert (Domain.join (Domain.spawn (fun () -> 42)) = 42)
