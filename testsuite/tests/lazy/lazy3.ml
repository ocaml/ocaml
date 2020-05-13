(* TEST
   ocamlopt_flags += " -O3 "
*)

let f count =
  let _n = (Domain.self ():> int) in
  let r = ref 0 in
  for i = 1 to count do
    incr r;
  done;
  !r

let main () =
  let l = lazy (f 1_000_000_000) in
  let d1 =
    Domain.spawn (fun () ->
        let _n = (Domain.self ():> int) in
        Lazy.force l)
  in
  let n2 = Lazy.force l in
  let n1 = Domain.join d1 in
  (n1, n2)

let _ =
  match main () with
  | (n1, n2) -> Printf.printf "n1=%d n2=%d\n" n1 n2
  | exception Lazy.Undefined -> print_endline "Undefined"
