(* TEST
*)

let () =
  let ic = open_in Filename.null in
  match input_char ic with
  | exception End_of_file -> close_in ic
  | _ -> assert false
