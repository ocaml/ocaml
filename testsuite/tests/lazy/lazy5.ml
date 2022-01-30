(* TEST
   ocamlopt_flags += " -O3 "
*)
let rec safe_force l =
  try Lazy.force l with
  | Lazy.Undefined ->
      Domain.cpu_relax ();
      safe_force l

let f count =
  let _n = (Domain.self ():> int) in
  let r = ref 0 in
  for i = 1 to count do
    incr r;
  done;
  !r

let l = lazy (f 1_000_000_000)
let d1 =
  Domain.spawn (fun () ->
      let _n = (Domain.self ():> int) in
      safe_force l)
let n2 = safe_force l
let n1 = Domain.join d1

let _ = Printf.printf "n1=%d n2=%d\n" n1 n2
