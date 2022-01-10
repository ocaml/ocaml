(* TEST *)

let test1 =
  let r' = ref 0 in
  let rec foo () =
    let r = ref 0 in
    Gc.finalise_last (fun _ -> r' := 1; print_endline "test1") r
  in
  foo();
  Gc.minor();
  assert (!r' = 1)

let test2 =
  let r = ref 0 in
  Gc.finalise (fun r -> assert (!r = 1); print_endline "test2: 1") r;
  Gc.finalise (fun r -> assert (!r = 0); print_endline "test2: 2"; r := 1) r;
  Gc.full_major()

let test3 =
  Gc.full_major ();
  let rec foo () =
    let r = ref 0 in
    Gc.finalise (fun r -> print_endline "test3: parent.1") r;
  in
  foo ();
  let d = Domain.spawn (fun _ ->
    let r = ref 0 in
    let r' = ref 0 in
    Gc.full_major ();
    Gc.finalise (fun r -> print_endline "test3: child.1") r;
    Gc.finalise_last (fun r -> print_endline "test3: child.2") r')
  in
  Domain.join d;
  print_endline "test3: joined";
  (* Now this domain takes over the finalisers from d *)
  Gc.full_major()
