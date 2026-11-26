(* TEST *)

let () =
  Stdlib.at_exit (fun () -> print_endline "stdlib"; exit 0);
  Domain.at_exit (fun () -> print_endline "domain");
  print_endline "main body"
