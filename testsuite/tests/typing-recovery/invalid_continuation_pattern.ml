(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type _ eff += A: 'a -> int eff

let handle_1 f =
  match f () with
  | "foo" -> "foo"
  | x -> x
  | effect A, [k] -> "bar"

let handle_2 f =
  match f () with
  | "foo" -> "foo"
  | x -> x
  | effect A, (Some k) -> "bar"

let t = (handle_1 (Fun.const "foo")) ^ (handle_2 (Fun.const "bar"))

let u : int = t
