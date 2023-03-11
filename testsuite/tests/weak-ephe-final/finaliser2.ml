(* TEST *)

let () = Out_channel.set_buffered stdout false

(* If minor heap values are finalised on a domain, then the callbacks are run
   at the end of a minor collection triggered by that domain. *)
let[@inline never][@local never] test1 () =
  let r' = ref 0 in
  let rec foo () =
    let r = ref 0 in
    Gc.finalise_last (fun _ -> r' := 1; print_endline "test1") r
  in
  foo();
  Gc.minor();
  assert (!r' = 1)

(* Check that the finalisation functions will be called in the reverse order of
   the corresponding calls to finalise. *)
let[@inline never][@local never] test2 () =
  let r = ref 0 in
  Gc.finalise (fun r ->
    assert (!r = 1); print_endline "test2: installed 1st") r;
  Gc.finalise (fun r ->
    assert (!r = 0); r := 1; print_endline "test2: installed 2nd") r;
  Gc.full_major()

(* The finalisation functions attached with Gc.finalise are always called
   before the finalisation functions attached with Gc.finalise_last. *)
let[@inline never][@local never] test3 () =
  Gc.full_major ();
  let c = ref 0 in
  let d = Domain.spawn (fun _ ->
    let r = ref 0 in
    let r' = ref 0 in
    Gc.full_major ();
    Gc.finalise_last (fun r ->
      assert (!c = 1); c := 2;
      print_endline "test3: child.finalise_last") r';
    Gc.finalise (fun r ->
      assert (!c = 0); c := 1;
      print_endline "test3: child.finalise") r)
  in
  Domain.join d;
  (* Now this domain takes over the finalisers from d *)
  Gc.full_major();
  assert (!c = 2)

let _ =
  test1 ();
  test2 ();
  test3 ()
