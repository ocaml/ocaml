(* TEST *)

let first x =
  print_endline "First";
  x

let second x =
  print_endline "Second";
  x

let reference () =
  let c = (first 0, second 1) in
  match c with
  | 0, 1 -> ()
  | _, _ -> assert false

let match_no_exn () =
  match first 0, second 1 with
  | 0, 1 -> ()
  | _, _ -> assert false

let match_exn () =
  match first 0, second 1 with
  | 0, 1 -> ()
  | _, _ -> assert false
  | exception _ -> assert false

let () =
  reference ();
  match_no_exn ();
  match_exn ();
  ()
