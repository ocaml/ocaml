(* TEST
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
*)

(* In addition to printing all of the messages, the order in which the messages
   are printed is important. [Domain.at_exit] specifies that the registed
   functions are called in 'last in, first out' order: the functions most
   recently added with [at_exit] is called first. *)
let _ =
  Domain.at_exit (fun _ -> print_endline "(4) main domain; main thread");
  let d = Domain.spawn (fun _ ->
    let t = Thread.create (fun _ -> Domain.at_exit (fun _ ->
      print_endline "(2) child domain; child thread")) ()
    in
    Thread.join t;
    Domain.at_exit (fun _ -> print_endline "(1) child domain; main thread"))
  in
  let t = Thread.create (fun _ -> Domain.at_exit (fun _ ->
    print_endline "(3) main domain; child thread")) ()
  in
  Domain.join d;
  Thread.join t
