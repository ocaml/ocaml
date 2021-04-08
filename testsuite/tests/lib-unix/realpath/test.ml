(* TEST
* hasunix
include unix
** bytecode
** native
*)

let main () =
  (* On Windows this tests that we strip \\?\ *)
  let cwd = Sys.getcwd () in
  assert (String.lowercase_ascii cwd = String.lowercase_ascii (Unix.realpath cwd));
  Unix.mkdir "test_dir" 0o755;
  close_out (open_out "test_dir/test_file");
  let p0 = Unix.realpath "test_dir/.//test_file" in
  let p1 = Unix.realpath "test_dir/../test_dir/test_file" in
  assert (p0 = p1 &&
          not (Filename.is_relative p0) &&
          not (Filename.is_relative p1));
  print_endline "Unix.realpath works with files";
  let p2 = Unix.realpath "./test_dir/../test_dir/.." in
  let p3 = Unix.realpath "." in
  assert (p2 = p3 &&
          not (Filename.is_relative p2) &&
          not (Filename.is_relative p3));
  print_endline "Unix.realpath works with directories";
  ()

let () = Unix.handle_unix_error main ()
