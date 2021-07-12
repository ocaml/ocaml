(* TEST
* has_symlink
** libwin32unix
include unix
*** bytecode
*** native
*)

let test = Printf.printf "%s: %b\n%!"

(* Tests for O_SHARE_DELETE and Unix.stat *)
let () =
  let h = Unix.(openfile "file" [O_CREAT; O_RDWR; O_SHARE_DELETE] 0o666) in
  assert (Unix.write_substring h "test" 0 4 = 4);
  test "Unix.stat \"file\"" (Unix.((stat "file").st_size = 4));
  test "Unix.fstat h" (Unix.((fstat h).st_size = 4));
  Sys.remove "file";
  test "Unix.stat \"file\"" (Unix.((stat "file").st_size = 4));
  test "Unix.fstat h" (Unix.((fstat h).st_size = 4));
  Unix.close h;
  test "Unix.stat \"file\"" (try let _ = Unix.((stat "file").st_size) in false with Unix.Unix_error(Unix.ENOENT, _, _) -> true);
  test "Unix.fstat h" (try let _ = Unix.((fstat h).st_size) in false with Unix.Unix_error(Unix.EBADF, _, _) -> true)
