(* TEST

files = "cmdline_prog.ml"

* setup-ocamlc.byte-build-env
program = "${test_build_directory}/test_unix_cmdline.byte"
** ocamlc.byte
program = "${test_build_directory}/cmdline_prog.exe"
all_modules = "cmdline_prog.ml"
*** ocamlc.byte
include unix
program = "${test_build_directory}/test_unix_cmdline.byte"
all_modules= "test_unix_cmdline.ml"
**** check-ocamlc.byte-output
***** run
****** check-program-output

* setup-ocamlopt.byte-build-env
program = "${test_build_directory}/test_unix_cmdline.opt"
** ocamlc.byte
program = "${test_build_directory}/cmdline_prog.exe"
all_modules = "cmdline_prog.ml"
*** ocamlopt.byte
include unix
program = "${test_build_directory}/test_unix_cmdline.opt"
all_modules= "test_unix_cmdline.ml"
**** check-ocamlopt.byte-output
***** run
****** check-program-output

*)

open Unix

let prog_name = "cmdline_prog.exe"

let run args =
  let out, inp = pipe () in
  let in_chan = in_channel_of_descr out in
  set_binary_mode_in in_chan false;
  let pid = create_process ("./" ^ prog_name) (Array.of_list (prog_name :: args)) Unix.stdin inp Unix.stderr in
  List.iter (fun arg ->
      let s = input_line in_chan in
      Printf.printf "%S -> %S [%s]\n" arg s (if s = arg then "OK" else "FAIL")
    ) args;
  close_in in_chan;
  let _, exit = waitpid [] pid in
  assert (exit = WEXITED 0)

let () =
  List.iter run
    [
      [""; ""; "\t \011"];
      ["a"; "b"; "c.txt@!"];
      ["\""];
      [" "; " a "; "  \" \\\" "];
      [" \\ \\ \\\\\\"];
      [" \"hola \""];
      ["a\tb"];
    ]
