(* TEST_BELOW
(* Blank lines added here to preserve locations. *)
*)
let rec x =
  let y = if false then (fun z -> 1) else (fun z -> x 4 + 1) in
  y;;

let () = ignore (x 42);;

(* TEST
 toplevel;
*)
