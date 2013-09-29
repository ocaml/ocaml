open A
let f =
  L.map S.capitalize

let () =
  L.iter print_endline (f ["jacques"; "garrigue"])
