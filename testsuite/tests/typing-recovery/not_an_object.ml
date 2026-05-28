(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let f (x : int) obj =
  let r = x # foo in
  let o = obj # bar x in
  (o, r)

let t = 10

let g (x : int) obj =
  let r = x # foo
  and o = obj # bar x in
  (r, o)

let r : string = 10
