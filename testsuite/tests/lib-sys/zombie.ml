(* TEST
* libwin32unix
include unix
** bytecode
** native
*)

let test = Printf.printf "%s: %b\n%!"

(* Tests for O_SHARE_DELETE and Sys.file_exists and Sys.directory_exists *)
let () =
  let h = Unix.(openfile "file" [O_CREAT; O_RDWR; O_SHARE_DELETE] 0o666) in
  assert (Unix.write_substring h "test" 0 4 = 4);
  test "Sys.file_exists \"file\"" @@ Sys.file_exists "file";
  Sys.remove "file";
  test "Sys.file_exists \"file\"" @@ Sys.file_exists "file";
  Unix.close h;
  test "Sys.file_exists \"file\"" @@ Sys.file_exists "file"
