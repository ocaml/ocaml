(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

module E = struct
  type t = { x : int; mutable y : string [@atomic] }
  let f t = [%atomic.loc t.x], [%atomic.loc t.y]
end

let f () =
  let (a, b) = E.f E.{x = 10; y = "11"} in
  a + (int_of_string b)

let t : string = f ()
