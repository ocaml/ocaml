(* TEST
*)

exception String of string

let _ =
  match "foo" with
  | str | exception (String str) -> print_endline str
  | exception _ -> print_endline "unexpected exception!"
