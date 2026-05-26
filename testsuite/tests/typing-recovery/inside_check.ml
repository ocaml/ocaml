(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type (_, _) eq = Eq : ('a, 'a) eq

let bad : type a. ?opt:(a, int -> int -> int) eq -> unit -> a =
  fun ?opt:(Eq = assert false) () x y -> x + y

let x:int = "hello"
