(* TEST *)

(* Domain 0 registers [Gc.finalise_last] on a young value of another
   domain. Then that domain terminates: it empties and frees its minor
   heap. *)

let () =
  let n = 5000 and count = ref 0 in
  for _ = 1 to n do
    let cell = Atomic.make None and taken = Atomic.make false in
    let d =
      Domain.spawn (fun () ->
        Atomic.set cell (Some (ref 0));
        while not (Atomic.get taken) do Domain.cpu_relax () done)
    in
    let rec get () =
      match Atomic.get cell with
      | Some v -> v
      | None -> Domain.cpu_relax (); get ()
    in
    Gc.finalise_last (fun () -> incr count) (get ());
    Atomic.set cell None;
    Atomic.set taken true;
    Domain.join d
  done;
  Gc.full_major ();
  assert (!count = n);
  print_endline "ok"
