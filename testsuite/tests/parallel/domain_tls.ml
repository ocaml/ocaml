(* TEST
* hasunix
include unix
** bytecode
** native
*)

let check () =
  let k1 = Domain.TLS.new_key () in
  Domain.TLS.set k1 5;
  let _ = Domain.TLS.get k1 in ()

let _ =
  let domains = Array.init 3 (fun _ -> Domain.spawn(fun _ -> check ())) in
  Array.iter Domain.join domains;
  print_endline "ok"
