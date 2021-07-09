(* TEST *)

let p i x =
  print_int i;
  print_newline ();
  x

let _ =
  for i = (p 13 0) to (p 25 3) do
    p i ()
  done
