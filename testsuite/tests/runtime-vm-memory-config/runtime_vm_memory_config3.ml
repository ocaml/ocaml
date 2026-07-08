(* TEST
 arch64;
 exit_status = "0";
 {
   set EXPECTED_MINOR_BYTES = "4194304";
   set EXPECTED_STACK_BYTES = "8589934592";
   ocamlrunparam = "s=4MB,l=1G,b=0";
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   check-ocamlc.byte-output;
   run;
   check-program-output;
 }
 {
   set EXPECTED_MINOR_BYTES = "4194304";
   set EXPECTED_STACK_BYTES = "1073741824";
   ocamlrunparam = "s=4MB,l=1GB,b=0";
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   check-ocamlc.byte-output;
   run;
   check-program-output;
 }
 {
   set EXPECTED_MINOR_BYTES = "4194304";
   set EXPECTED_STACK_BYTES = "8589934592";
   ocamlrunparam = "s=4MB,l=1Gw,b=0";
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   check-ocamlc.byte-output;
   run;
   check-program-output;
 }
*)

let bytes_per_word = Sys.word_size / 8
let control = Gc.get ()
let minor_heap_size_bytes = control.Gc.minor_heap_size * bytes_per_word
let stack_limit_bytes = control.Gc.stack_limit * bytes_per_word

let check_opt env_name actual =
  match Sys.getenv_opt env_name with
  | None -> failwith "Unexpected environment variable"
  | Some expected_str ->
      let expected = int_of_string expected_str in
      if actual <> expected then
        failwith
          (Printf.sprintf "%s mismatch: got %d expected %d"
             env_name actual expected)

let () =
  check_opt "EXPECTED_MINOR_BYTES" minor_heap_size_bytes;
  check_opt "EXPECTED_STACK_BYTES" stack_limit_bytes;
  print_endline "ok"
