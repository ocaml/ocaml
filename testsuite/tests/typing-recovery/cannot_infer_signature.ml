(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

module type S = sig
  val x : int
end

module U = struct
  let f =
    let m = (module struct let x = 10 end) in
    let n = (module struct let x = 10 end : S) in
    m, n
end

let k () =
  let module A = (val fst (U.f)) in
  let module D = (val fst (U.f) : S) in
  D.x + D.x + 12

let r : string = k ()
