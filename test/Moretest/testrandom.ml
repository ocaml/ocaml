open Random

let _ = 
  for i = 0 to 20 do
    print_float (float 1000.); print_char ' '
  done;
  print_newline ();  print_newline ();
  for i = 0 to 20 do
    print_int (int 1000); print_char ' '
  done

let _ = exit 0

