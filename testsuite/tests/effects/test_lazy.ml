(* TEST *)

effect Stop : unit

let f count =
  let r = ref 0 in
  for i = 1 to count do
    incr r;
    if i = count / 2 then perform Stop
  done;
  !r

let _ =
  let l = lazy (f 1_000) in
  let v1 =
    try Lazy.force l with
    effect Stop k -> continue k () in
    Printf.printf "%d\n" v1;
  let l2 = lazy (f 2_000) in
  let v2 =
    try Lazy.force l2 with
    effect Stop k ->
      let d = Domain.spawn(fun () -> continue k ()) in
      Domain.join d in
    Printf.printf "%d\n" v2;
  let l3 = lazy (f 3_000) in
  let _ =
    try Lazy.force l3 with
    effect Stop _ ->
      try let d = Domain.spawn(fun () -> Lazy.force l3)
      in Domain.join d with
      CamlinternalLazy.RacyLazy -> Printf.printf "RacyLazy\n"; 0
  in ()
