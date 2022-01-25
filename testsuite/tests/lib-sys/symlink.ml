(* TEST
* hasunix
include unix
** has_symlink
*** bytecode
*** native
*)

let test = Printf.printf "%s: %b\n%!"

(* Returns false if f returns any value and true if Sys_error is raised *)
let fails text f =
  let outcome =
    try f () && false
    with Sys_error _ -> true
  in
    test (text ^ " raises Sys_error") outcome

(* Tests for Sys.file_exists and Sys.directory_exists *)
let () =
  Unix.mkdir "realdir" 0o777;
  let h = Unix.(openfile "realfile" [O_CREAT; O_RDWR] 0o666) in
  assert (Unix.write_substring h "test" 0 4 = 4);
  Unix.close h;
  Unix.symlink "realdir" "linkdir";
  Unix.symlink "realfile" "linkfile";
  Unix.symlink ~to_dir:true "fakedir" "deadlinkdir";
  Unix.symlink "fakefile" "deadlinkfile";
  test "Sys.file_exists \"realdir\"" @@ Sys.file_exists "realdir";
  test "Sys.file_exists \"realfile\"" @@ Sys.file_exists "realfile";
  test "Sys.is_directory \"realdir\"" @@ Sys.is_directory "realdir";
  test "Sys.is_directory \"realfile\"" @@ Sys.is_directory "realfile";
  test "Sys.file_exists \"linkdir\"" @@ Sys.file_exists "linkdir";
  test "Sys.file_exists \"linkfile\"" @@ Sys.file_exists "linkfile";
  test "Sys.is_directory \"linkdir\"" @@ Sys.is_directory "linkdir";
  test "Sys.is_directory \"linkfile\"" @@ Sys.is_directory "linkfile";
  test "Sys.file_exists \"deadlinkdir\"" @@ Sys.file_exists "deadlinkdir";
  test "Sys.file_exists \"deadlinkfile\"" @@ Sys.file_exists "deadlinkfile";
  fails "Sys.is_directory \"deadlinkdir\"" (fun () -> Sys.is_directory "deadlinkdir");
  fails "Sys.is_directory \"deadlinkfile\"" (fun () -> Sys.is_directory "deadlinkfile")
