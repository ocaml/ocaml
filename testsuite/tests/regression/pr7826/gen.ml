(* Used as the preprocessor of nested.ml (ocamlopt -pp), which is why it
   ignores its argument: the preprocessor's output replaces that file.  It
   prints a chain of [n] nested local functions, each enclosing the next, with
   the innermost one closing over the outer parameter -- the shape whose
   compilation used to be quadratic in the nesting depth (see #7826). *)

let n = 8000

let () =
  ignore Sys.argv.(1);
  print_string "let f x =\n";
  for i = 0 to n - 1 do Printf.printf "let rec g%d y =\n" i done;
  print_string "x + y + 1\n";
  for i = n - 1 downto 1 do Printf.printf "in g%d y\n" i done;
  print_string "in g0 x\n";
  print_string "let () = print_int (f 0); print_newline ()\n"
