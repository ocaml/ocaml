(* TEST
 ocamlrunparam += ",d=1";
*)

let _ =
  try
    Domain.spawn (fun _ -> print_endline "Expect failure")
    |> Domain.join
  with Failure _ -> print_string "ok\n"
