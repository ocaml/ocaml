let () =
  for i = 1 to 10 do
    print_float (2.0 *. float_of_int i);
    print_newline ()
  done;
  for i = 1 to 10 do
    print_float (float_of_int i /. 1000. /. 2. ** -.1023.);
    print_newline ()
  done;
  for i = 1 to 10 do
    print_float (float_of_int i /. 2. ** 1023.);
    print_newline ()
  done;
  for i = 1 to 10 do
    print_float (float_of_int i /. 1000. /. 2. ** -.1024.);
    print_newline ()
  done;
  for i = 1 to 10 do
    print_float (float_of_int i /. 2. ** 1024.);
    print_newline ()
  done;
