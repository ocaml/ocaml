let f : string A.t -> unit = function
    A.X s -> print_endline s

let () = f A.y
