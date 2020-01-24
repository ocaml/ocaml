open Domain

let () =
  let l = lazy (print_string "Lazy Forced\n") in
  let d = spawn (fun () -> Lazy.force l) in
  join d
