(* TEST

* libwin32unix
   include unix
** has_symlink
*** bytecode
*** native

*)

let link1 = "link1"
let link2 = "link2"
let link3 = "link3"
let link_dir = "link_directory"
let dir = "directory"
let did_raise = ref false

let link_exists s =
  try (Unix.lstat s).Unix.st_kind = Unix.S_LNK with _ -> false

let directory_exists s =
  try (Unix.lstat s).Unix.st_kind = Unix.S_DIR with _ -> false

let main () =
  close_out (open_out "test.txt");
  if link_exists link1 then Sys.remove link1;
  if link_exists link2 then Sys.remove link2;
  Unix.symlink ~to_dir:false ".\\test.txt" link1;
  assert ((Unix.stat link1).Unix.st_kind = Unix.S_REG);
  print_endline "Unix.symlink works with backwards slashes";
  Unix.symlink ~to_dir:false "./test.txt" link2;
  assert ((Unix.stat link2).Unix.st_kind = Unix.S_REG);
  print_endline "Unix.symlink works with forward slashes";

  did_raise := false;
  if not (directory_exists dir) then
    Unix.mkdir dir 0o644;
  begin try Unix.unlink dir with
  | Unix.Unix_error((EISDIR (* Linux *) | EPERM (* POSIX *) | EACCES (* Windows *)), _, _) ->
    did_raise := true end;
  assert (!did_raise);
  assert (directory_exists dir);
  print_endline "Unix.unlink cannot delete directories";

  did_raise := false;
  if not (directory_exists dir) then
    Unix.mkdir dir 0o644;
  begin try Sys.remove dir with Sys_error _ -> did_raise := true end;
  assert (!did_raise);
  assert (directory_exists dir);
  print_endline "Sys.remove cannot delete directories";

  if not (directory_exists dir) then
    Unix.mkdir dir 0o644;
  if not (link_exists link_dir) then
    Unix.symlink ~to_dir:true dir link_dir;
  Unix.unlink link_dir;
  print_endline "Unix.unlink can delete symlinks to directories";

  if not (link_exists link3) then
    Unix.symlink ~to_dir:false "test.txt" link3;
  Unix.unlink link3;
  print_endline "Unix.unlink can delete symlinks to files";

  if not (directory_exists dir) then
    Unix.mkdir dir 0o644;
  if not (link_exists link_dir) then
    Unix.symlink ~to_dir:true dir link_dir;
  Sys.remove link_dir;
  print_endline "Sys.remove can delete symlinks to directories";

  if not (link_exists link3) then
    Unix.symlink ~to_dir:false "test.txt" link3;
  Sys.remove link3;
  print_endline "Sys.remove can delete symlinks to files"

let () =
  Unix.handle_unix_error main ()
