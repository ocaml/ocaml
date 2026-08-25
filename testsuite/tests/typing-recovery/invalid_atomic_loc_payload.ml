(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

module E = struct
  type t = { mutable x : int [@atomic] }
  let f t = [%atomic.loc t]
end

let _ = E.f None
let t : string = 10
