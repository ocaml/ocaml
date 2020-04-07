(* TEST *)

(* MPR#7253: "at_exit functions get called twice if a callback raises
   and prevents earlier handlers to execute." *)

exception My_exception

let () =
  Printexc.set_uncaught_exception_handler (fun exn bt ->
    match exn with
    | My_exception -> print_endline "Caught"; exit 0
    | _ -> print_endline "Unexpected uncaught exception");
  at_exit (fun () -> print_endline "Last");
  at_exit (fun () -> print_endline "Raise"; raise My_exception);
  at_exit (fun () -> print_endline "First")
