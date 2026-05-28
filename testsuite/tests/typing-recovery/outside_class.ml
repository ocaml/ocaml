(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let x = {< y = 12 >}

let t =
  let o = {< foo = 10 >} in
  let b = object
    val x = 10
      val y = {<x = 15>}
    method foo = {<x = 11>}
  end in o, b

let x : string = 10
