(* TEST
 *)

effect E : string
effect F : unit

let _ =
  try
    try
      print_endline @@ "Hello" ^ perform E
    with
    | effect F _ -> failwith "impossible.."
  with
  | effect E k ->
      let k' = Obj.clone_continuation k in
      continue k "";
      continue k' ", again!"
