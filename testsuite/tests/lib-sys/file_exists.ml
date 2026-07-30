(* TEST
 include unix;
 hasunix;
 not windows;
 bytecode;
 native;
*)

(* Test Sys.file_exists and Sys.filepath_exists *)

let writefile filename contents =
  let oc = open_out_bin filename in
  output_string oc contents;
  close_out oc

let safe_remove filename =
  try Sys.remove filename with Sys_error _ -> ()

let safe_rmdir dirname =
  try Sys.rmdir dirname with Sys_error _ -> ()

let show name f =
  print_string name; print_string ": ";
  (try print_string (string_of_bool (f ()))
   with Sys_error msg -> print_string "Sys_error: "; print_string msg);
  print_newline ()

let () =
  (* Existing regular file *)
  let f = "exists.txt" in
  writefile f "some content";
  show "file_exists on existing file" (fun () -> Sys.file_exists f);
  show "filepath_exists on existing file" (fun () -> Sys.filepath_exists f);

  (* Non-existent file (ENOENT) *)
  let nf = "does_not_exist.txt" in
  show "file_exists on nonexistent file" (fun () -> Sys.file_exists nf);
  show "filepath_exists on nonexistent file" (fun () -> Sys.filepath_exists nf);

  (* Path component that is not a directory (ENOTDIR) *)
  let notdir = "notadir.txt" in
  writefile notdir "some content";
  let bad_path = Filename.concat notdir "child" in
  show "file_exists with ENOTDIR" (fun () -> Sys.file_exists bad_path);
  show "filepath_exists with ENOTDIR" (fun () -> Sys.filepath_exists bad_path);

  (* Unreadable directory (EACCES) *)
  let dir = "noaccess_dir" in
  let inner = Filename.concat dir "inner" in

  (* In case there's some leftover from a previous failed/crashed run *)
  if Sys.file_exists dir
  then begin
      Unix.chmod dir 0o755;
      safe_remove inner;
      safe_rmdir dir;
  end;

  Sys.mkdir dir 0o755;
  writefile inner "some content";
  Unix.chmod dir 0o000;
  show "file_exists with EACCES" (fun () -> Sys.file_exists inner);
  show "filepath_exists with EACCES" (fun () -> Sys.filepath_exists inner);
  Unix.chmod dir 0o755;
  safe_remove inner;
  safe_rmdir dir
