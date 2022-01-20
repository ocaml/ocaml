(* A reduced version of the example from the manual *)
let _ =
  let s = read_line () in
  match Array.to_list (Array.init (String.length s) (String.get s)) with
    ['x'] -> failwith "uh oh"
  | _ -> ()
