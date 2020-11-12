(* TEST
 *)

effect E : string

let _ =
  try
    try
      print_endline @@ "Hello" ^ perform E
    with
    | effect E k ->
        print_endline "intercepting request..";
        continue k (perform E)
  with
  | Continuation_in_use -> Printf.printf "raised Continuation_in_use\n"
  | effect E k ->
      let k' = Obj.clone_continuation k in
      continue k "";
      continue k' ", again!"
