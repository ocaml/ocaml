(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

module E = struct
  type t = { x : int; mutable y : int [@atomic] }

  let forbidden { x; y } = x + y
end

let k = E.forbidden E.{x = 10; y = 11} + 12
let r : string = k
