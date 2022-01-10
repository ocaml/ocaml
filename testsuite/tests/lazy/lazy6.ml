(* TEST
   ocamlopt_flags += " -O3 "
*)

let flag1 = Atomic.make false
let flag2 = Atomic.make false

let rec wait_for_flag f =
  if Atomic.get f then ()
  else (Domain.cpu_relax (); wait_for_flag f)

let l1 = Lazy.from_fun (fun () ->
  Atomic.set flag1 true;
  wait_for_flag flag2)

let first_domain () =
  Lazy.force l1

let second_domain () =
  wait_for_flag flag1;
  let l2 = Lazy.from_fun (fun () -> Lazy.force l1) in
  let rec loop () =
    try Lazy.force l2 with
    | Lazy.Undefined -> Atomic.set flag2 true
  in
  loop ()

let _ =
  let d = Domain.spawn first_domain in
  second_domain ();
  Domain.join d;
  print_endline "OK"
