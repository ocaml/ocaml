(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let t = 99999999999999999999999999999999999999999999999999999999999
let u = 99999999999999999999999999999999999999999999999999999999999l
let k = 99999999999999999999999999999999999999999999999999999999999L

let r =
  let (+) = Int64.add in
  Int64.(of_int t + of_int32 u + k)

let s : string = r
