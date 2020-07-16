(* TEST
* hasunix
include unix
** bytecode
** native
*)

let check () =
  let k1 : int Domain.TLS.key = Domain.TLS.new_key () in
  Domain.TLS.set k1 100;
  let v = Option.get (Domain.TLS.get k1) in
  (v + 1) |>ignore

let _ =
  let domains = Array.init 3 (fun _ -> Domain.spawn(fun _ -> check ())) in
  Array.iter Domain.join domains;
  print_endline "ok"
