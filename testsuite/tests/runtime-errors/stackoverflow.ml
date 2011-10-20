let rec f x =
  if x land 0xFFFF <> 0
  then 1 + f (x + 1)
  else
    try
      1 + f (x + 1)
    with Stack_overflow ->
      print_string "x = "; print_int x; print_newline();
      raise Stack_overflow

let _ =
  try
    ignore(f 0)
  with Stack_overflow ->
    print_string "Stack overflow caught"; print_newline()
