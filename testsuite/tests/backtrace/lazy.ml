(* TEST
   flags = "-g"
   * native
*)


let l1 : unit lazy_t = lazy (raise Not_found)

let test1 () =
  let () = Lazy.force l1 in ()

let l2 : unit lazy_t = lazy (raise Not_found)

let test2 () =
  let (lazy ()) = l2 in ()

let run test =
  try
    test ();
  with exn ->
    Printf.printf "Uncaught exception %s\n" (Printexc.to_string exn);
    Printexc.print_backtrace stdout

let () =
  Printexc.record_backtrace true;
  run test1;
  run test2
