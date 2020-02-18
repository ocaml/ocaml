let g () =
  if true then failwith "Plugin error";
  print_endline "xxx"

let f () =
  g ();
  print_endline "xxx"

let () =
  f ();
  print_endline "xxx"
