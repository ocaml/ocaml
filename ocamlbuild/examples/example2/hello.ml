open Greet

let _ =
  let name =
    if Array.length Sys.argv > 1 then
      Sys.argv.(1)
    else
      "stranger"
  in
  greet
    (if name = "Caesar" then Nicely else Badly)
    name;
  Printf.printf "My name is %s\n" Sys.argv.(0)
;;
