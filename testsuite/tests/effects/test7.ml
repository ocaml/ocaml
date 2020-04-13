(* TEST
 *)

effect E : string

let _ =
  try print_endline @@ "Hello" ^ perform E with
  | effect E k ->
      let k' = Obj.clone_continuation k in
      continue k "";
      continue k' ", again!"
