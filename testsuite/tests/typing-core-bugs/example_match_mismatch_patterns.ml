let f x () = Some x

let _ =
  match f 2 with
  | None -> ()
  | Some _ -> ()
