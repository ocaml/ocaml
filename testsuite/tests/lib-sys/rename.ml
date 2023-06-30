(* TEST
*)

(* Test the Sys.rename function *)

let writefile filename contents =
  let oc = open_out_bin filename in
  output_string oc contents;
  close_out oc

let readfile filename =
  let ic = open_in_bin filename in
  let sz = in_channel_length ic in
  let contents = really_input_string ic sz in
  close_in ic;
  contents

let safe_remove filename =
  try Sys.remove filename with Sys_error _ -> ()

let safe_remove_dir dirname =
  try Sys.rmdir dirname with Sys_error _ -> ()

let testrename f1 f2 contents =
  try
    Sys.rename f1 f2;
    if readfile f2 <> contents then print_string "wrong contents!"
    else if Sys.file_exists f1 then print_string "initial file still exists!"
    else print_string "passed"
  with Sys_error msg ->
    print_string "Sys_error exception: "; print_string msg

let testfailure f1 f2 =
  try
    Sys.rename f1 f2; print_string "should fail but doesn't!"
  with Sys_error _ ->
    print_string "fails as expected"

let testrenamedir d1 d2 =
  try
    Sys.rename d1 d2;
    try
      if Sys.is_directory d1 then print_string "source directory still exists!"
    with Sys_error msg ->
      if not (Sys.is_directory d2) then print_string "target directory not created!"
      else print_string "passed"
  with Sys_error msg ->
    print_string "Sys_error exception: "; print_string msg

let _ =
  let f1 = "file1.dat" and f2 = "file2.dat" in
  safe_remove f1; safe_remove f2;
  print_string "Rename to nonexisting file: ";
  writefile f1 "abc";
  testrename f1 f2 "abc";
  print_newline();
  print_string "Rename to existing file: ";
  writefile f1 "def";
  writefile f2 "xyz";
  testrename f1 f2 "def";
  print_newline();
  print_string "Renaming a nonexisting file: ";
  testfailure f1 f2;
  print_newline();
  print_string "Renaming to a nonexisting directory: ";
  writefile f1 "abc";
  testfailure f1 (Filename.concat "nosuchdir" f2);
  print_newline();
  safe_remove f1; safe_remove f2;
  print_string "Rename directory to a nonexisting directory: ";
  Sys.mkdir "foo" 0o755;
  testrenamedir "foo" "bar";
  print_newline();
  safe_remove_dir "bar";
  print_string "Rename a nonexisting directory: ";
  testfailure "foo" "bar";
  print_newline();
  print_string "Rename directory to a non-empty directory: ";
  Sys.mkdir "foo" 0o755;
  Sys.mkdir "bar" 0o755;
  let f1 = Filename.concat "bar" "file1.dat" in
  writefile f1 "abc";
  testfailure "foo" "bar";
  print_newline();
  safe_remove f1; safe_remove_dir "foo"; safe_remove_dir "bar";
  print_string "Rename directory to existing empty directory: ";
  Sys.mkdir "foo" 0o755;
  Sys.mkdir "bar" 0o755;
  testrenamedir "foo" "bar";
  print_newline();
  safe_remove_dir "foo";
  safe_remove_dir "bar";
  print_string "Rename existing empty directory to itself: ";
  Sys.mkdir "foo" 0o755;
  testrenamedir "foo" "foo";
  print_newline();
  safe_remove_dir "foo";
  print_string "Rename directory to existing file: ";
  Sys.mkdir "foo" 0o755;
  writefile f2 "xyz";
  testfailure "foo" f2;
  print_newline();
  safe_remove_dir "foo";
  safe_remove f2;
  safe_remove_dir f2;
