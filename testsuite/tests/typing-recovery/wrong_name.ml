(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type person = { name : string; age : int }

let _ =
  let p = { name = "Alice"; age = 30 } in
  p.height

type t =
  | A of { foo: int; bar: string }

let f (A {foo; bar; baz}) = ()

let x = 10

let t : string = 10
