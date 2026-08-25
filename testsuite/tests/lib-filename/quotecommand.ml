(* TEST
 readonly_files = "myecho.ml";
 {
   program = "${test_build_directory}/quotecommand.byte";
   setup-ocamlc.byte-build-env;
   program = "${test_build_directory}/myecho.exe";
   all_modules = "myecho.ml";
   ocamlc.byte;
   program = "${test_build_directory}/quotecommand.byte";
   all_modules = "quotecommand.ml";
   ocamlc.byte;
   check-ocamlc.byte-output;
   run;
   check-program-output;
 }{
   program = "${test_build_directory}/quotecommand.opt";
   setup-ocamlopt.byte-build-env;
   program = "${test_build_directory}/myecho.exe";
   all_modules = "myecho.ml";
   ocamlopt.byte;
   program = "${test_build_directory}/quotecommand.opt";
   all_modules = "quotecommand.ml";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   run;
   check-program-output;
 }
*)

open Printf

let copy_channels ic oc =
  let sz = 1024 in
  let buf = Bytes.create sz in
  let rec copy () =
    let n = input ic buf 0 sz in
    if n > 0 then (output oc buf 0 n; copy()) in
  copy()

let copy_file src dst =
  let ic = open_in_bin src in
  let oc = open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary]
                        0o777 dst in
  copy_channels ic oc;
  close_in ic;
  close_out oc

let cat_and_rm_file f =
  match open_in f with
  | ic ->
      copy_channels ic stdout;
      close_in ic;
      Sys.remove f
  | exception Sys_error _ ->
      printf "Could not open %S\n" f

let myecho =
  Filename.concat Filename.current_dir_name "my echo.exe"

let run prog ?stdin ?stdout ?stderr args =
  flush Stdlib.stdout;
  let rc =
   Sys.command (Filename.quote_command prog ?stdin ?stdout ?stderr args) in
  if rc > 0 then
    printf "!!! %s failed\n" prog

let _ =
  let run = run myecho in
  copy_file "myecho.exe" "my echo.exe";
  printf "-------- Spaces\n";
  run ["Lorem ipsum dolor"; "sit amet,"; "consectetur adipiscing elit,"];
  printf "-------- All ASCII characters\n";
  run ["!\"#$%&'()*+,-./";
       "0123456789";
       ":;<=>?@";
       "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
       "[\\]^_`";
       "abcdefghijklmnopqrstuvwxyz";
       "{~|~}"
  ];
  printf "-------- Output redirection\n";
  run ~stdout:"my 'file'.tmp" ["sed do eiusmod tempor incididunt";
                               "ut labore et dolore magna aliqua."];
  printf "-------- Input redirection\n";
  run ~stdin:"my 'file'.tmp" [];
  Sys.remove "my 'file'.tmp";
  printf "-------- Error redirection\n";
  run ~stderr:"my 'file'.tmp"
              ["Exceptur sint"; "-err"; "occaecat"; "cupidatat";
               "-out"; "non proident"; "-err"; "sunt in culpa"];
  printf "-- stderr:\n";
  cat_and_rm_file "my 'file'.tmp";
  printf "-------- Output and error redirections (different files)\n";
  run ~stdout:"my stdout.tmp" ~stderr:"my stderr.tmp"
              ["qui officia"; "-err"; "deserunt"; "mollit";
               "-out"; "anim id est"; "-err"; "laborum."];
  printf "-- stdout:\n"; cat_and_rm_file "my stdout.tmp";
  printf "-- stderr:\n"; cat_and_rm_file "my stderr.tmp";
  printf "-------- Output and error redirections (same file)\n";
  run ~stdout:"my file.tmp" ~stderr:"my file.tmp"
              ["Duis aute"; "irure dolor"; "-err"; "in reprehenderit";
               "in voluptate"; "-out"; "velit esse cillum"; "-err"; "dolore"];
  cat_and_rm_file "my file.tmp";
  Sys.remove "my echo.exe"

let _ =
  printf "-------- Forward slashes in program position\n";
  run "./myecho.exe" ["alea iacta est"]

let echo_exe = "./myecho.exe"

(* The program name and the redirection files go through quote_cmd_filename,
   which must double-quote cmd.exe metacharacters that are legal in a Windows
   file name (& ( ) ^), not just spaces.  These check the right program is
   found and the right (literally named) file is written -- i.e. both that no
   command is injected and that the result is correct. *)

let _ =
  printf "-------- Metacharacters in program name\n";
  List.iter (fun name ->
    let prog = Filename.concat Filename.current_dir_name name in
    copy_file "myecho.exe" name;
    printf "-- %s\n" name;
    run prog ["per aspera"; "ad astra"];
    Sys.remove name)
    ["my&echo.exe"; "my(echo).exe"; "my(echo.exe"; "my)echo.exe"; "my^echo.exe"; "my!echo.exe"]

let _ =
  printf "-------- Redirection to names with metacharacters\n";
  List.iter (fun fn ->
    run echo_exe ~stdout:fn ["lux et veritas"];
    printf "-- %s:\n" fn;
    cat_and_rm_file fn)
    ["out&put.tmp"; "out(put).tmp"; "out(put.tmp"; "out)put.tmp"; "out^put.tmp"; "out!put.tmp"; "a & b.tmp"]

(* Arguments go through quote then quote_cmd.  Check that variable expansion is
   suppressed and that trailing backslashes survive the C-runtime quoting. *)
let _ =
  printf "-------- Adversarial arguments\n";
  run echo_exe ["%PATH%"; {|"&whoami&"|}; {|C:\Program Files\|};
                {|a\|}; {|a\\|}; ""]

(* The two characters quote_cmd_filename cannot quote (double-quote and %) must
   be rejected with Failure, in the program name and in any redirection file. *)
let check_raises descr f =
  let raised_failure =
    match f () with
    | _ -> false
    | exception Failure _ -> true
  in
  if raised_failure = Sys.win32 then
    printf "%s: OK\n" descr
  else
    printf "%s: ERROR\n" descr

let _ =
  printf "-------- Rejected (unquotable) characters\n";
  check_raises "stdout %"
    (fun () -> Filename.quote_command "true" ~stdout:"a%b" []);
  check_raises {|stdout "|}
    (fun () -> Filename.quote_command "true" ~stdout:{|a"b|} []);
  check_raises "program %"
    (fun () -> Filename.quote_command "a%b" []);
  check_raises {|program "|}
    (fun () -> Filename.quote_command {|a"b|} [])
