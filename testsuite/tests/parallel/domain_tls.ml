(* TEST
* hasunix
include unix
** bytecode
** native
*)

let check () =
  let k1 = Domain.TLS.new_key () in
  let my_state = Random.State.make_self_init () in
  Domain.TLS.set k1 my_state;
  let k2 = Domain.TLS.new_key () in
  let my_int = 500 in
  Domain.TLS.set k2 my_int;
  let s : Random.State.t = Obj.magic @@ Option.get @@ Domain.TLS.get k1 in
  let v : int = Obj.magic @@ Option.get @@ Domain.TLS.get k2 in
  ignore @@ (Random.State.int s v)

let _ =
  let domains = Array.init 3 (fun _ -> Domain.spawn(fun _ -> check ())) in
  Array.iter Domain.join domains;
  print_endline "ok"
