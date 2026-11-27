(* TEST
 arch64;
 not windows;
 bytecode;
 native;
*)

let show f =
  match f () with
  | s -> Printf.printf "ok, length %d\n" (String.length s)
  | exception e -> print_endline (Printexc.to_string e)

let () =
  show (fun () -> Printf.sprintf "%.*f" max_int 1.0);
  show (fun () -> Printf.sprintf "%20.*f" max_int 1.0)
