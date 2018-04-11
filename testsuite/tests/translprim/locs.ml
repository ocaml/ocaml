(* TEST *)

let print_loc loc =
  print_endline loc

let print_file file =
  print_endline file

let print_line line =
  print_endline (string_of_int line)

let print_module md =
  print_endline md

let print_pos (file, line, col1, col2) =
  Printf.printf "%s, %d, %d, %d\n" file line col1 col2

let () = print_loc __LOC__

let () = print_file __FILE__

let () = print_line __LINE__

let () = print_module __MODULE__

let () = print_pos __POS__

let loc, s1 = __LOC_OF__ "an expression"

let () = print_loc loc

let () = print_endline s1

let line, s2 = __LINE_OF__ "another expression"

let () = print_line line

let () = print_endline s2

let pos, s3 = __POS_OF__ "yet another expression"

let () = print_pos pos

let () = print_endline s3
