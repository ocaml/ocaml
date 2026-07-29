type t = T of {string: string; int: int; bool: bool}

let () =
  let open Introspect.Print in
  print_any_endline (~x:1, 2, ~y:"hello");
  print_any_endline (T{string = "hello"; int = 42; bool = true})

