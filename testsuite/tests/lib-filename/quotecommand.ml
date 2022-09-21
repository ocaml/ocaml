(* TEST

readonly_files = "myecho.ml"

* setup-ocamlc.byte-build-env
program = "${test_build_directory}/quotecommand.byte"
** ocamlc.byte
program = "${test_build_directory}/myecho.exe"
all_modules = "myecho.ml"
*** ocamlc.byte
program = "${test_build_directory}/quotecommand.byte"
all_modules= "quotecommand.ml"
**** check-ocamlc.byte-output
***** run
****** check-program-output

* setup-ocamlopt.byte-build-env
program = "${test_build_directory}/quotecommand.opt"
** ocamlopt.byte
program = "${test_build_directory}/myecho.exe"
all_modules = "myecho.ml"
*** ocamlopt.byte
program = "${test_build_directory}/quotecommand.opt"
all_modules= "quotecommand.ml"
**** check-ocamlopt.byte-output
***** run
****** check-program-output

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

let cat_file f =
  let ic = open_in f in
  copy_channels ic stdout;
  close_in ic

let myecho =
  Filename.concat Filename.current_dir_name "my echo.exe"

let run ?stdin ?stdout ?stderr args =
  flush Stdlib.stdout;
  let rc =
   Sys.command (Filename.quote_command myecho ?stdin ?stdout ?stderr args) in
  if rc > 0 then begin
    printf "!!! my echo failed\n";
    exit 2
  end

let _ =
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
  cat_file "my 'file'.tmp";
  Sys.remove "my 'file'.tmp";
  printf "-------- Output and error redirections (different files)\n";
  run ~stdout:"my stdout.tmp" ~stderr:"my stderr.tmp"
              ["qui officia"; "-err"; "deserunt"; "mollit";
               "-out"; "anim id est"; "-err"; "laborum."];
  printf "-- stdout:\n"; cat_file "my stdout.tmp"; Sys.remove "my stdout.tmp";
  printf "-- stderr:\n"; cat_file "my stderr.tmp"; Sys.remove "my stderr.tmp";
  printf "-------- Output and error redirections (same file)\n";
  run ~stdout:"my file.tmp" ~stderr:"my file.tmp"
              ["Duis aute"; "irure dolor"; "-err"; "in reprehenderit";
               "in voluptate"; "-out"; "velit esse cillum"; "-err"; "dolore"];
  cat_file "my file.tmp"; Sys.remove "my file.tmp";
  Sys.remove "my echo.exe"
