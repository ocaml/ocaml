(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let (module M) = 0

let c =
  let module M = struct
    let x = 10

    class t =
      let (module Ob) = m in
      object end
  end
  in M.x + 1

let t : int = true
