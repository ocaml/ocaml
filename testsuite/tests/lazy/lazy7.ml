(* TEST
   ocamlopt_flags += " -O3 "
*)

let num_domains = 4

let rec safe_force l =
  try Lazy.force l with
  | Lazy.Undefined ->
      Domain.cpu_relax ();
      safe_force l

let f count =
  let _n = (Domain.self ():> int) in
  let r = ref 0 in
  for _ = 1 to count do
    incr r;
  done;
  !r

let go = Atomic.make false

let l = lazy (f 1_000_000_000)
let d1 = Array.init (num_domains - 1) (fun _->
  Domain.spawn (fun () ->
      let rec wait () =
        if Atomic.get go then ()
        else wait ()
      in
      wait ();
      let _n = (Domain.self ():> int) in
      safe_force l))
let _ = Atomic.set go true
let n2 = safe_force l
let n1 = Array.map Domain.join d1

let _ = Printf.printf "n1=%d n2=%d\n" n1.(0) n2
