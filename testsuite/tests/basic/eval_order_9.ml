(* TEST *)

(* From #12440, by Jeremy Yallop *)
let _ =
  let r = ref true in
  match Fun.id ((r := false), !r) with
  | _, true  -> print_endline "Ok"
  | _, false -> print_endline "ERROR"
