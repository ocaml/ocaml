let manyargs a b c d e f g h i j k =
  print_string "a = "; print_int a; print_newline();
  print_string "b = "; print_int b; print_newline();
  print_string "c = "; print_int c; print_newline();
  print_string "d = "; print_int d; print_newline();
  print_string "e = "; print_int e; print_newline();
  print_string "f = "; print_int f; print_newline();
  print_string "g = "; print_int g; print_newline();
  print_string "h = "; print_int h; print_newline();
  print_string "i = "; print_int i; print_newline();
  print_string "j = "; print_int j; print_newline();
  print_string "k = "; print_int k; print_newline()

let _ = manyargs 1 2 3 4 5 6 7 8 9 10 11

external manyargs_ext: int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int = "manyargs_argv" "manyargs"

let _ = manyargs_ext 1 2 3 4 5 6 7 8 9 10 11
