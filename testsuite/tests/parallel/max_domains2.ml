(* TEST
 ocamlrunparam += ",d=129";
 { native; }
*)

let m = Mutex.create ()

let _ =
  Mutex.lock m;
  (* The default max domains limit is 128. In this test, we make this limit 129
     and spawn 128 domains in addition to the main domain. *)
  for i = 1 to 128 do
    let d = Domain.spawn (fun _ -> Mutex.lock m; Mutex.unlock m) in
    at_exit (fun () -> Domain.join d)
  done;
  Mutex.unlock m;
  print_endline "ok"
