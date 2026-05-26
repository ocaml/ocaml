(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let x =
  let (~##) x = x + 1 and (#~#) x y = x + y in
  (~##10) #~# 10
