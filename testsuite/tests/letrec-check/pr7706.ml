(* TEST
   * toplevel
*)
let rec x =
  let y = if false then (fun z -> 1) else (fun z -> x 4 + 1) in
  y;;

let () = ignore (x 42);;
