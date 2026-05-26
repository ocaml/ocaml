(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type t =  private {  mutable foo: int}

let f (x : t) =
  x.foo <- 10

type e = private A of { mutable bar : int}

let e x y =
  (* Should fail *)
  let () = x.foo <- y in

  (* Should work *)
  let () = (f x) in
  10

let g = function
  | A x -> x.bar <- 10
