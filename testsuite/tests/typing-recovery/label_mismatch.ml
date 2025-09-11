(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type a = {
  x : int;
  y : int;
}

type b = {
  x : int;
  z : string;
}

let _ =
  { x = 1; y = 2; z = "oops" }

let x : int = "string"

type d = {
  u : int;
  v : int;
}

type e = {
  u : int;
  w : string;
}

let r = { u = 1; v = 2 }

let _ =
  { r with w = "oops" }
