(* Fragile, should warn *)
let () = if true then (); ()

let () = if false then () else (); ()

(* Warning free *)
let () = if true do () done; ()

let () = if false do () done else do () done; ()

let () = if true then ()
let () = if false then () else ()
